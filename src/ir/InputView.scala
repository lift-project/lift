package ir

import opencl.ir._

import scala.collection.immutable.Stack

sealed abstract class Operation

object NoOperation extends Operation

class InputAccess(val name: String) extends Operation

class ArrayCreation(val v: InputView, val len: ArithExpr, val itVar: Var) extends Operation
class ArrayAccess(var av: ArrayView, val idx: ArithExpr) extends Operation
class ArrayReorder(val av: ArrayView, val f: (ArithExpr) => ArithExpr) extends Operation
class ArraySplit(val av: ArrayView, val chunkSize: ArithExpr) extends Operation
class ArrayJoin(val av: ArrayView, val chunkSize: ArithExpr) extends Operation
class ArrayZip(val tv: TupleView) extends Operation
class ArrayAsVector(val av: ArrayView, val n: ArithExpr) extends Operation
class ArrayAsScalar(val av: ArrayView, val n: ArithExpr) extends Operation
class SubArray(val in: ArrayView, val ids: ArrayView) extends Operation

class TupleCreation(val views: Seq[InputView]) extends Operation
class TupleAccess(val tv: TupleView, val i: Int) extends Operation


abstract class InputView(val operation: Operation, var t: Type = UndefType) {
  def replaced(oldExpr: ArithExpr, newExpr: ArithExpr): InputView = {
    val subst = new scala.collection.mutable.HashMap[ArithExpr,ArithExpr]()
    subst.put(oldExpr, newExpr)

    val newOperation = this.operation match {
      case ac: ArrayCreation => new ArrayCreation(ac.v.replaced(oldExpr,newExpr), ArithExpr.substitute(ac.len, subst.toMap), ArithExpr.substitute(ac.itVar, subst.toMap).asInstanceOf[Var])
      case aa: ArrayAccess => new ArrayAccess(aa.av.replaced(oldExpr,newExpr).asInstanceOf[ArrayView], ArithExpr.substitute(aa.idx, subst.toMap))
      case az: ArrayZip => new ArrayZip(az.tv.replaced(oldExpr,newExpr).asInstanceOf[TupleView])
      case as: ArraySplit => new ArraySplit(as.av.replaced(oldExpr,newExpr).asInstanceOf[ArrayView], ArithExpr.substitute(as.chunkSize, subst.toMap))
      case aj: ArrayJoin => new ArrayJoin(aj.av.replaced(oldExpr,newExpr).asInstanceOf[ArrayView], ArithExpr.substitute(aj.chunkSize, subst.toMap))
      case ar: ArrayReorder => new ArrayReorder(ar.av.replaced(oldExpr, newExpr).asInstanceOf[ArrayView], ar.f)

      case tc: TupleCreation => new TupleCreation(tc.views.map(_.replaced(oldExpr,newExpr)))
      case ta: TupleAccess => new TupleAccess(ta.tv.replaced(oldExpr,newExpr).asInstanceOf[TupleView],ta.i)

      case  _ => this.operation
    }

    this match {
      case av: ArrayView => new ArrayView(av.elemT, newOperation)
      case sv: PrimitiveView => new PrimitiveView(newOperation)
      case tv: TupleView => new TupleView(tv.tupleType, newOperation)
    }
  }




  def access(idx: ArithExpr): InputView = {
    val access = new InputViewAccess(idx, this)
    access.t = this.t.asInstanceOf[ArrayType].elemT
    access
  }

  def split(chunkSize : ArithExpr): InputView = {
    val split = new InputViewSplit(chunkSize, this)
    split.t = this.t match {
      case ArrayType(t, n) => ArrayType(ArrayType(t, chunkSize), n div chunkSize)
    }
    split
  }

  def join(chunkSize: ArithExpr): InputView = {
    val join = new InputViewJoin(chunkSize, this)
    join.t = this.t match {
      case ArrayType(ArrayType(t, n), m) => ArrayType(t, n*m)
    }
    join
  }

  def reorder(f: (ArithExpr) => ArithExpr): InputView = {
    val gather = new InputViewGather(f, this)
    gather.t = this.t
    gather
  }

  def asVector(n: ArithExpr): InputView = {
    t match {
      case st: ScalarType =>
        val vectorize = new InputViewAsVector(n, this)
        vectorize.t = Type.vectorize(st, n)
        vectorize
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into vector types")
    }
  }

  def asScalar(): InputView = {
    t match {
      case VectorType(st, n) =>
        val scalar = new InputViewAsScalar(this)
        scalar.t = st
        scalar
      case st: ScalarType => this
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into scalar types")
    }
  }

  def get(i: Int): InputView = {
    val get = new InputViewZipComponent(i, this)
    get.t = this.t.asInstanceOf[TupleType].elemsT(i)
    get
  }
}

class InputViewMem(name: String, t: Type) extends InputView(NoOperation, t)

class InputViewAccess(i: ArithExpr, iv: InputView) extends InputView(NoOperation)

class InputViewSplit(n: ArithExpr, iv: InputView) extends InputView(NoOperation)

class InputViewJoin(n: ArithExpr, iv: InputView) extends InputView(NoOperation)

class InputViewZip(ivs: InputView*) extends InputView(NoOperation)

class InputViewGather(f: ArithExpr => ArithExpr, iv: InputView) extends InputView(NoOperation)

class InputViewAsVector(n: ArithExpr, iv: InputView) extends InputView(NoOperation)

class InputViewAsScalar(iv: InputView) extends InputView(NoOperation)

class InputViewMap(iv: InputView) extends InputView(NoOperation)

class InputViewZipComponent(i: Int, iv: InputView) extends InputView(NoOperation)

object NoView extends InputView(NoOperation)



// TODO: Remove OLD!!!! views

class ArrayView(var elemT: Type, override val operation: Operation) extends InputView(operation) {

  def access(idx: ArithExpr): InputView = {
    val aa = new ArrayAccess(this, idx)
    InputView(elemT, aa)
  }

  def split(chunkSize : ArithExpr): ArrayView = {
    val as = new ArraySplit(this, chunkSize)
    new ArrayView(new ArrayType(elemT, chunkSize), as)
  }

  def join(chunkSize: ArithExpr): ArrayView = {
    val aj = new ArrayJoin(this, chunkSize)
    new ArrayView(elemT.asInstanceOf[ArrayType].elemT, aj)
  }

  def reorder(f: (ArithExpr) => ArithExpr): ArrayView = {
    val ar = new ArrayReorder(this, f)
    new ArrayView(elemT, ar)
  }

  def asVector(n: ArithExpr): ArrayView = {
    elemT match {
      case st: ScalarType =>
        val av = new ArrayAsVector(this, n)
        new ArrayView(VectorType(st, n), av)
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + elemT + " into vector types")
    }
  }

  def asScalar(): ArrayView = {
    elemT match  {
      case VectorType(st, n) =>
        val av = new ArrayAsScalar(this, n)
        new ArrayView(st, av)
      case st: ScalarType =>
        this.operation match {
          case aav: ArrayAsVector => aav.av
          case _ => this
        }
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + elemT + " into scalar types")
    }
  }

  override def toString = {
    elemT.toString + " " + operation.toString
  }

}


class TupleView(val tupleType: TupleType, override val operation: Operation) extends InputView(operation) {

  def access(i: Int) : InputView = {
    val ta = new TupleAccess(this, i)
    val elemT = tupleType.elemsT(i)
    InputView(elemT, ta)
  }

}

object TupleView {
  def apply(t: Type, op: Operation): TupleView = {
    assert(t.isInstanceOf[TupleType])
    new TupleView(t.asInstanceOf[TupleType], op)
  }
}


class PrimitiveView(override val operation: Operation) extends InputView(operation)

// TODO: here is the new stuff

object InputView {
  // create new view based on the given type
  def apply(t: Type, name: String): InputView = {
    InputView.create(name, t)
  }

  def zip(ivs: InputView*): InputView = {
    val zip = new InputViewZip(ivs:_*)
    zip.t = ArrayType(TupleType(ivs.map(_.t.asInstanceOf[ArrayView].elemT)),
      ivs(0).t.asInstanceOf[ArrayType].len)
    zip
  }

  def create(name: String, t: Type): InputView = {
    new InputViewMem(name, t)
  }

  def visitAndBuildViews(expr: Expr, f:  List[(ArithExpr, ArithExpr)] = List()): InputView = {
    val result = expr match {
      case pr: ParamReference => getViewAtIndex(pr.p.view, pr.i)
      case p: Param => p.view //View(expr.t, new InputAccess())
      case call: FunCall => buildViewFunCall(call, f)
    }
    expr.view = result
    result
  }

  private def getViewAtIndex(v: InputView, index: Int): InputView = {
    v.get(index)
  }

  private def getViewFromArgs(call: FunCall, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    if (call.args.isEmpty) {
      NoView
    } else if (call.args.length == 1) {
      visitAndBuildViews(call.args(0), outputAccessInf)
    } else {
      InputView.zip(call.args.map((expr: Expr) => visitAndBuildViews(expr, outputAccessInf)):_*)
    }
  }

  private def buildViewFunCall(call: FunCall, outputAccessInf: List[(ArithExpr, ArithExpr)]): InputView = {
    val argView = getViewFromArgs(call, outputAccessInf)

    call match {
      case call: MapCall => buildMapView(call, argView, outputAccessInf)
      case call: ReduceCall => buildReduceCall(call, argView, outputAccessInf)
      case call: FunCall =>
        call.f match {
          case l: Lambda => buildViewLambda(l, call, argView, outputAccessInf)
          case cf: CompFunDef => buildViewCompFunDef(cf, argView, outputAccessInf)
          case z: Zip => createViewZip(z, call, argView)
          case Split(n) => createViewSplit(n, argView)
          case _: Join => createViewJoin(call, argView)
          case uf: UserFunDef => createViewUserFunDef(uf, argView, outputAccessInf)
          case ReorderStride(s) => createViewReorderStride(s, call, argView)
          case g: Gather => createViewGather(g, call, argView, outputAccessInf)
          case s: Scatter => createViewScatter(s, call, argView, outputAccessInf)
          case tL: toLocal => buildViewToLocal(tL, argView, outputAccessInf)
          case tG: toGlobal => buildViewToGlobal(tG, argView, outputAccessInf)
          case i: Iterate => buildViewIterate(i, call, argView, outputAccessInf)
          case t: Transpose => createViewTranspose(t, call, argView)
          case tw: TransposeW => createViewTransposeW(tw, call, argView, outputAccessInf)
          case asVector(n) => createViewAsVector(n, argView)
          case _: asScalar => createViewAsScalar(argView)
          case f: Filter => createViewFilter(f, call, argView)
          /*case uz: Unzip =>
          case SplitDim2(n) =>
          case j: JoinDim2 =>

          case _: Swap =>
          */
          case _ => argView
        }
    }
  }

  private def buildViewIterate(i: Iterate, call:FunCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    i.f.params(0).view = argView
    visitAndBuildViews(i.f.body, outputAccessInf)
    initialiseNewView(call.t, outputAccessInf)
  }

  private def buildViewToGlobal(tG: toGlobal, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    tG.f.params(0).view = argView
    visitAndBuildViews(tG.f.body, outputAccessInf)
  }

  private def buildViewToLocal(tL: toLocal, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    tL.f.params(0).view = argView
    visitAndBuildViews(tL.f.body, outputAccessInf)
  }

  private def buildMapView(call: MapCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    // pass down input view
    call.f.f.params(0).view = argView.access(call.loopVar)
    // build information where call.f should write
    val newOutputAccessInf = (Type.getLength(call.t), call.loopVar) :: outputAccessInf

    // traverse into call.f
    val innerView = visitAndBuildViews(call.f.f.body, newOutputAccessInf)

    if (call.isConcrete) {
      // get full type of the output memory object
      val fullReturnType = getFullType(call.t, outputAccessInf)
      // create fresh input view for following function
      return InputView.create(call.mem.variable.name, fullReturnType)
    } else { // call.isAbstract and return input map view
      new InputViewMap(innerView)
    }
  }


  private def buildReduceCall(call: ReduceCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    // pass down input view
    call.f.f.params(0).view = argView.access(0)
    call.f.f.params(1).view = argView.access(1).asInstanceOf[ArrayView].access(call.loopVar)
    // build information where call.f should write
    val newOutputAccessInf = (Type.getLength(call.t), Cst(0)) :: outputAccessInf
    // traverse into call.f
    visitAndBuildViews(call.f.f.body, newOutputAccessInf)
    // get full type of the output memory object
    val fullReturnType = getFullType(call.t, outputAccessInf)
    // create fresh input view for following function
    InputView.create(call.mem.variable.name, fullReturnType)
  }

  private def buildViewLambda(l: Lambda, call: FunCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    assert(call.args.nonEmpty)
    if (call.args.length == 1) {
      if (l.params.length != 1) throw new NumberOfArgumentsException
      l.params(0).view = argView
    } else {
      l.params.zipWithIndex.map({ case (p, i) => p.view = argView.access(i) })
    }
    visitAndBuildViews(l.body, outputAccessInf)
  }

  private def buildViewCompFunDef(cf: CompFunDef, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {

    val outputAccessInfs = scala.collection.mutable.Stack(outputAccessInf)

    cf.funs.foldRight(argView)((f, v) => {
      if (f.params.length != 1) throw new NumberOfArgumentsException
      f.params(0).view = v

      f.body match {
        case call: FunCall =>
          call.f match {
            case tL: toLocal => outputAccessInfs.push(List())
            case tG: toGlobal => if (outputAccessInfs.length > 1) outputAccessInfs.pop()
            case _ =>
          }
        case _ =>
      }

      visitAndBuildViews(f.body, outputAccessInfs.top)
    })
  }


  // TODO: continue refactoring here on monday ....

  private def createViewZip(z: Zip, call: FunCall, argView: InputView): InputView = {
    argView match {
      case tv: TupleView => new ArrayView(Type.getElemT(call.t), new ArrayZip(tv))
      // new ArrayCreation(tv, Type.getLength(call.t), ???))
      //case tv: TupleView => tv
      case _ => throw new IllegalArgumentException("PANIC! Expected tuple, found " + argView.getClass)
    }
  }

  private def createViewFilter(f: Filter, call: FunCall, argView: InputView): InputView = {
    argView match {
      case tv: TupleView =>
        new ArrayView(Type.getElemT(call.t),
          new SubArray(tv.access(0).asInstanceOf[ArrayView], tv.access(1).asInstanceOf[ArrayView])
        )
      case _ => throw new IllegalArgumentException("PANIC! Expected tuple, found " + argView.getClass)
    }
  }

  private def createViewJoin(call: FunCall, argView: InputView): InputView = {
    val chunkSize = call.argsType match {
      case ArrayType(ArrayType(_, n), _) => n
      case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + call.argsType)
    }

    argView match {
      case av: ArrayView => av.join(chunkSize)
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def createViewSplit(n: ArithExpr, argView: InputView): InputView = {
    argView match {
      case av: ArrayView => av.split(n)
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def createViewAsVector(n: ArithExpr, argView: InputView): InputView = {
    argView match {
      case av: ArrayView => av.asVector(n)
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def createViewAsScalar(argView: InputView): InputView = {
    argView match {
      case av: ArrayView => av.asScalar()
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def createViewUserFunDef(uf: UserFunDef, argView: InputView, f:  List[(ArithExpr, ArithExpr)]): InputView = {
    initialiseNewView(uf.outT, f)
  }

  private def initialiseNewView(t: Type, f: List[(ArithExpr, ArithExpr)], name: String = ""): InputView = {
    // Use the lengths and iteration vars to mimic inputs
    val outArray = f.foldLeft(t)((t, len) => ArrayType(t, len._1))
    val outView = InputView(outArray, new InputAccess(name))
    f.foldRight(outView)((idx, view) => view.asInstanceOf[ArrayView].access(idx._2))
  }

  private def createViewReorderStride(s: ArithExpr, call: FunCall, argView: InputView): InputView = {
    val n = Type.getLength(call.argsType) / s

    argView match {
      case av: ArrayView => av.reorder( (i:ArithExpr) => { (i div n) + s * ( i % n) } )
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def createViewTranspose(t: Transpose, call: FunCall, argView: InputView): InputView = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        argView.asInstanceOf[ArrayView].
          join(n).
          reorder((i:ArithExpr) => { IndexFunction.transpose(i, call.t) }).
          split(m)
    }
  }

  private def createViewTransposeW(tw: TransposeW, call: FunCall, argView: InputView, ids: List[(ArithExpr, ArithExpr)]): InputView = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        findAccessAndReorder(argView, IndexFunction.transpose, call.t, 0, transpose = true)
    }

    initialiseNewView(call.t, ids)
  }

  private def createViewGather(gather: Gather, call: FunCall, argView: InputView, f:  List[(ArithExpr, ArithExpr)]): InputView = {
    argView match {
      case av: ArrayView =>
        gather.f.params(0).view = av.reorder( (i:ArithExpr) => { gather.idx.f(i, call.t) } )
        visitAndBuildViews(gather.f.body, f)
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def createViewScatter(scatter: Scatter, call: FunCall, argView: InputView, f:  List[(ArithExpr, ArithExpr)]): InputView = {
    argView match {
      case av: ArrayView =>
        scatter.f.params(0).view = av
        visitAndBuildViews(scatter.f.body, f)

        // Find the matching ArrayAccess to the first ArrayCreation,
        // and reorder the ArrayView in the access
        findAccessAndReorder(scatter.f.body.view, scatter.idx, call.t, 0)

        scatter.f.body.view
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def findAccessAndReorder(view: InputView, idx: IndexFunction, t:Type, count: scala.Int, transpose: Boolean = false): Unit = {
    view.operation match {
      case access: ArrayAccess =>
        if (count == 1) {
          if (!transpose)
            access.av = access.av.reorder( (i:ArithExpr) => { idx.f(i, t) } )
          else
            access.av = t match {
              case ArrayType(ArrayType(typ, m), n) =>
                access.av.elemT = ArrayType(access.av.elemT.asInstanceOf[ArrayType].elemT, m)

                access.av.
                  join(m).
                  reorder((i:ArithExpr) => { IndexFunction.transpose(i, ArrayType(ArrayType(typ, n), m)) }).
                  split(n)
            }
        } else {
          findAccessAndReorder(access.av, idx, t, count-1, transpose)
        }
      case ar: ArrayReorder => findAccessAndReorder(ar.av, idx, t, count, transpose)
      case as: ArraySplit => findAccessAndReorder(as.av, idx, t, count, transpose)
      case aj: ArrayJoin => findAccessAndReorder(aj.av, idx, t, count, transpose)
      case ac: ArrayCreation => findAccessAndReorder(ac.v, idx, t, count+1, transpose)
      case op => throw new NotImplementedError(op.getClass.toString)
    }
  }


  private def getFullType(outputType: Type, outputAccessInf: List[(ArithExpr, ArithExpr)): Type = {
    outputAccessInf.foldLeft(outputType)((t, len) => ArrayType(t, len._1))
  }



  def getInputAccess(sv : InputView, tupleAccessStack : Stack[Int] = new Stack()) : InputAccess = {
    sv.operation match {
      case ia : InputAccess => ia
      case aa : ArrayAccess => InputView.getInputAccess(aa.av, tupleAccessStack)
      case ac : ArrayCreation =>InputView.getInputAccess(ac.v, tupleAccessStack)
      case as : ArraySplit => InputView.getInputAccess(as.av, tupleAccessStack)
      case aj : ArrayJoin => InputView.getInputAccess(aj.av, tupleAccessStack)
      case ar : ArrayReorder => InputView.getInputAccess(ar.av, tupleAccessStack)
      case sa : SubArray => InputView.getInputAccess(sa.in, tupleAccessStack)
      case aav: ArrayAsVector => InputView.getInputAccess(aav.av, tupleAccessStack)
      case aas: ArrayAsScalar => InputView.getInputAccess(aas.av, tupleAccessStack)

      case ta : TupleAccess =>
        val newTAS = tupleAccessStack.push(ta.i)
        getInputAccess(ta.tv,newTAS)

      case tc : TupleCreation =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        getInputAccess(tc.views(i),newTAS)

      case az : ArrayZip =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        getInputAccess(az.tv.access(i), newTAS)

      case op => throw new NotImplementedError(op.getClass.toString)
    }
  }

}

object ViewPrinter {

  def emit(sv : InputView) : ArithExpr = {
    sv match {
      case _: PrimitiveView => emitView(sv, new Stack(), new Stack())
      case _: TupleView => emitView(sv, new Stack(), new Stack())
      case t => throw new IllegalArgumentException(t.getClass + " found, TupleView/PrimitiveView expected")
    }
  }


  private def emitView(sv : InputView,
                       arrayAccessStack : Stack[(ArithExpr, ArithExpr)], // id, dimension size
                       tupleAccessStack : Stack[Int]) : ArithExpr = {
    sv.operation match {
      case ia : InputAccess =>
        assert(tupleAccessStack.isEmpty)
        arrayAccessStack.map(x => (x._1*x._2).asInstanceOf[ArithExpr]).foldLeft(Cst(0).asInstanceOf[ArithExpr])((x, y) => (x+y).asInstanceOf[ArithExpr])

      case aa : ArrayAccess =>
        val length: ArithExpr = getLengthForArrayAccess(aa.av.elemT, tupleAccessStack)
        val newAAS = arrayAccessStack.push((aa.idx, length))
        emitView(aa.av,newAAS, tupleAccessStack)

      case ac : ArrayCreation =>
        val idx = arrayAccessStack.top._1
        val newAAS = arrayAccessStack.pop
        val newV = ac.v.replaced(ac.itVar, idx)
        emitView(newV,newAAS,tupleAccessStack)

      case as : ArraySplit =>
        val (chunkId,stack1) = arrayAccessStack.pop2
        val (chunkElemId,stack2) = stack1.pop2
        val newIdx = chunkId._1*as.chunkSize+chunkElemId._1
        val newAAS = stack2.push((newIdx, chunkElemId._2))
        emitView(as.av, newAAS,tupleAccessStack)

      case aj : ArrayJoin =>
        val (idx,stack) = arrayAccessStack.pop2
        val chunkSize: ArithExpr = aj.chunkSize
        val chunkId = idx._1 div chunkSize
        val chunkElemId = idx._1 % chunkSize
        val newAS = stack.push((chunkElemId, Type.getLengths(sv.asInstanceOf[ArrayView].elemT).reduce(_*_))).push((chunkId, Type.getLengths(aj.av.elemT).reduce(_*_)))
        emitView(aj.av,newAS,tupleAccessStack)

      case ar : ArrayReorder =>
        val (idx,stack) = arrayAccessStack.pop2
        val newIdx = ar.f(idx._1)
        val newAS = stack.push((newIdx, idx._2))
        emitView(ar.av,newAS,tupleAccessStack)

      case sa : SubArray =>
        val ((idx, len), stack) = arrayAccessStack.pop2

        val newIdx = emit(sa.ids.access(idx))
        val indirection = new AccessVar(InputView.getInputAccess(sa.ids).name, newIdx)

        emitView(sa.in, stack.push((indirection, len)), tupleAccessStack)

      case ta : TupleAccess =>
        val newTAS = tupleAccessStack.push(ta.i)
        emitView(ta.tv,arrayAccessStack,newTAS)

      case tc : TupleCreation =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        emitView(tc.views(i),arrayAccessStack,newTAS)

      case az : ArrayZip =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        emitView(az.tv.access(i) ,arrayAccessStack,newTAS)

      case aav: ArrayAsVector =>
        val top = arrayAccessStack.top
        val newAAS = arrayAccessStack.pop.push((top._1 * aav.n, top._2)).map(x => (x._1, x._2 / aav.n))
        emitView(aav.av, newAAS, tupleAccessStack)

      case aas: ArrayAsScalar =>
        val top = arrayAccessStack.top
        val newAAS = arrayAccessStack.pop.push((top._1 / aas.n, top._2)).map(x => (x._1, x._2 * aas.n))
        emitView(aas.av, newAAS, tupleAccessStack)

      case op => throw new NotImplementedError(op.getClass.toString)
     }
  }

  private def getLengthForArrayAccess(t: Type, tupleAccesses: Stack[Int]): ArithExpr = {

    if (tupleAccesses.isEmpty) {
      Type.getLengths(t).reduce(_*_)
    } else {
      t match {
        case tt: TupleType => getLengthForArrayAccess(Type.getTypeAtIndex(tt, tupleAccesses.top), tupleAccesses.pop)
        case ArrayType(elemT, n) => getLengthForArrayAccess(elemT, tupleAccesses) * n
      }
    }
  }
}


