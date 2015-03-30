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
      case ArrayType(elemT, n) => ArrayType(ArrayType(elemT, chunkSize), n div chunkSize)
    }
    split
  }

  def join(chunkSize: ArithExpr): InputView = {
    val join = new InputViewJoin(chunkSize, this)
    join.t = this.t match {
      case ArrayType(ArrayType(elemT, n), m) => ArrayType(elemT, n*m)
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

  def filter(ids: InputView): InputView = {
    val filter = new InputViewFilter(this, ids)
    filter.t = ArrayType(this.t.asInstanceOf[ArrayType].elemT, ids.t.asInstanceOf[ArrayType].len)
    filter
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

class InputViewFilter(iv: InputView, ids: InputView) extends InputView(NoOperation)

class InputViewMap(iv: InputView) extends InputView(NoOperation)

class InputViewZipComponent(i: Int, iv: InputView) extends InputView(NoOperation)

object NoView extends InputView(NoOperation)



// TODO: Remove OLD!!!! views

class ArrayView(val elemT: Type, override val operation: Operation) extends InputView(operation)

class TupleView(val tupleType: TupleType, override val operation: Operation) extends InputView(operation)

class PrimitiveView(override val operation: Operation) extends InputView(operation)

// TODO: here is the new stuff

object InputView {
  // create new view based on the given type
  def apply(t: Type, name: String): InputView = {
    InputView.create(name, t)
  }

  def zip(ivs: InputView*): InputView = {
    val zip = new InputViewZip(ivs:_*)
    zip.t = ArrayType(TupleType(ivs.map(_.t.asInstanceOf[ArrayType].elemT):_*),
      ivs(0).t.asInstanceOf[ArrayType].len)
    zip
  }

  def create(name: String, t: Type): InputView = {
    new InputViewMem(name, t)
  }

  def visitAndBuildViews(expr: Expr, outputAccessInf:  List[(ArithExpr, ArithExpr)] = List()): InputView = {
    val result = expr match {
      case pr: ParamReference => getViewAtIndex(pr.p.view, pr.i)
      case p: Param => p.view //View(expr.t, new InputAccess())
      case call: FunCall => buildViewFunCall(call, outputAccessInf)
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
      case call: ReduceCall => buildViewReduceCall(call, argView, outputAccessInf)
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
      InputView.create(call.mem.variable.name, fullReturnType)
    } else { // call.isAbstract and return input map view
      new InputViewMap(innerView)
    }
  }


  private def buildViewReduceCall(call: ReduceCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    // pass down input view
    call.f.f.params(0).view = argView.access(0)
    call.f.f.params(1).view = argView.access(1).access(call.loopVar)
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
      l.params.zipWithIndex.foreach({ case (p, i) => p.view = argView.access(i) })
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

  private def createViewZip(z: Zip, call: FunCall, argView: InputView): InputView = {
    argView
  }

  private def createViewFilter(f: Filter, call: FunCall, argView: InputView): InputView = {
    argView.get(0).filter(argView.get(1))
  }

  private def createViewJoin(call: FunCall, argView: InputView): InputView = {
    val chunkSize = call.argsType match {
      case ArrayType(ArrayType(_, n), _) => n
      case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + call.argsType)
    }

    argView.join(chunkSize)
  }

  private def createViewSplit(n: ArithExpr, argView: InputView): InputView = {
    argView.split(n)
  }

  private def createViewAsVector(n: ArithExpr, argView: InputView): InputView = {
    argView.asVector(n)
  }

  private def createViewAsScalar(argView: InputView): InputView = {
    argView.asScalar()
  }

  private def createViewUserFunDef(uf: UserFunDef, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    initialiseNewView(uf.outT, outputAccessInf)
  }

  private def initialiseNewView(t: Type, outputAccessInf: List[(ArithExpr, ArithExpr)], name: String = ""): InputView = {
    // Use the lengths and iteration vars to mimic inputs
    val outArray = getFullType(t, outputAccessInf)
    val outView = InputView.create(name, outArray)
    outputAccessInf.foldRight(outView)((idx, view) => view.asInstanceOf[ArrayView].access(idx._2))
  }

  private def createViewReorderStride(s: ArithExpr, call: FunCall, argView: InputView): InputView = {
    val n = Type.getLength(call.argsType) / s

    argView.reorder( (i:ArithExpr) => { (i div n) + s * ( i % n) } )
  }

  private def createViewTranspose(t: Transpose, call: FunCall, argView: InputView): InputView = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        argView.
          join(n).
          reorder((i:ArithExpr) => { IndexFunction.transpose(i, call.t) }).
          split(m)
    }
  }

  // TODO: Implement in OutputView
  private def createViewTransposeW(tw: TransposeW, call: FunCall, argView: InputView, outputAccessInf: List[(ArithExpr, ArithExpr)]): InputView = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
//        findAccessAndReorder(argView, IndexFunction.transpose, call.t, 0, transpose = true)
    }

    initialiseNewView(call.t, outputAccessInf)
  }

  private def createViewGather(gather: Gather, call: FunCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    argView.reorder( (i:ArithExpr) => { gather.idx.f(i, call.t) } )
  }

  // TODO: Implement in OutputView
  private def createViewScatter(scatter: Scatter, call: FunCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    scatter.f.params(0).view = argView
    visitAndBuildViews(scatter.f.body, outputAccessInf)

    // Find the matching ArrayAccess to the first ArrayCreation,
    // and reorder the ArrayView in the access
//    findAccessAndReorder(scatter.f.body.view, scatter.idx, call.t, 0)

    argView
  }

  // TODO: Implement in OutputView
  /*private def findAccessAndReorder(view: InputView, idx: IndexFunction, t:Type, count: scala.Int, transpose: Boolean = false): Unit = {
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
  }*/

  private def getFullType(outputType: Type, outputAccessInf: List[(ArithExpr, ArithExpr)]): Type = {
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


