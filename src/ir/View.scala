package ir

import opencl.ir._

import scala.collection.immutable.Stack

sealed abstract class Operation

object NoOperation extends Operation

class InputAccess(val name: String) extends Operation

class ArrayCreation(val v: View, val len: ArithExpr, val itVar: Var) extends Operation
class ArrayAccess(var av: ArrayView, val idx: ArithExpr) extends Operation
class ArrayReorder(val av: ArrayView, val f: (ArithExpr) => ArithExpr) extends Operation
class ArraySplit(val av: ArrayView, val chunkSize: ArithExpr) extends Operation
class ArrayJoin(val av: ArrayView, val chunkSize: ArithExpr) extends Operation
class ArrayZip(val tv: TupleView) extends Operation
class ArrayAsVector(val av: ArrayView, val n: ArithExpr) extends Operation
class ArrayAsScalar(val av: ArrayView, val n: ArithExpr) extends Operation
class SubArray(val in: ArrayView, val ids: ArrayView) extends Operation
class ArrayTail(val in: ArrayView) extends Operation

//class MatrixCreation(val v: View, val dx: ArithExpr, val dy: ArithExpr, val itVar: Var) extends Operation
//class MatrixAccess(val mv:MatrixView, val idx: ArithExpr, val idy: ArithExpr) extends Operation

class TupleCreation(val views: Seq[View]) extends Operation
class TupleAccess(val tv: TupleView, val i: Int) extends Operation


abstract class View(val operation: Operation) {
  def replaced(oldExpr: ArithExpr, newExpr: ArithExpr): View = {
    val subst = new scala.collection.mutable.HashMap[ArithExpr,ArithExpr]()
    subst.put(oldExpr, newExpr)

    val newOperation = this.operation match {
      case ac: ArrayCreation => new ArrayCreation(ac.v.replaced(oldExpr,newExpr), ArithExpr.substitute(ac.len, subst.toMap), ArithExpr.substitute(ac.itVar, subst.toMap).asInstanceOf[Var])
      case aa: ArrayAccess => new ArrayAccess(aa.av.replaced(oldExpr,newExpr).asInstanceOf[ArrayView], ArithExpr.substitute(aa.idx, subst.toMap))
      case az: ArrayZip => new ArrayZip(az.tv.replaced(oldExpr,newExpr).asInstanceOf[TupleView])
      case as: ArraySplit => new ArraySplit(as.av.replaced(oldExpr,newExpr).asInstanceOf[ArrayView], ArithExpr.substitute(as.chunkSize, subst.toMap))
      case aj: ArrayJoin => new ArrayJoin(aj.av.replaced(oldExpr,newExpr).asInstanceOf[ArrayView], ArithExpr.substitute(aj.chunkSize, subst.toMap))
      case ar: ArrayReorder => new ArrayReorder(ar.av.replaced(oldExpr, newExpr).asInstanceOf[ArrayView], ar.f)

      case at: ArrayTail => new ArrayTail(at.in.replaced(oldExpr, newExpr).asInstanceOf[ArrayView])

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
}

object NoView extends View(NoOperation)

class ArrayView(var elemT: Type, override val operation: Operation) extends View(operation) {

  def access(idx: ArithExpr): View = {
    val aa = new ArrayAccess(this, idx)
    View(elemT, aa)
  }

  def split(chunkSize : ArithExpr): ArrayView = {
    val as = new ArraySplit(this, chunkSize)
    new ArrayView(new ArrayType(elemT, chunkSize), as)
  }

  def join(chunkSize: ArithExpr): ArrayView = {
    val aj = new ArrayJoin(this, chunkSize)
    new ArrayView(elemT.asInstanceOf[ArrayType].elemT, aj)
  }

  def tail() : ArrayView = {
    val at = new ArrayTail(this)
    new ArrayView(elemT, at)
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

//class MatrixView(val elemT: Type, override val operation: Operation) extends View(operation) {
//  def access(idx: ArithExpr, idy: ArithExpr): View = {
//    val ma = new MatrixAccess(this, idx,idy)
//    View(elemT, ma)
//  }
//}

class TupleView(val tupleType: TupleType, override val operation: Operation) extends View(operation) {

  def access(i: Int) : View = {
    val ta = new TupleAccess(this, i)
    val elemT = tupleType.elemsT(i)
    View(elemT, ta)
  }

}

object TupleView {
  def apply(t: Type, op: Operation): TupleView = {
    assert(t.isInstanceOf[TupleType])
    new TupleView(t.asInstanceOf[TupleType], op)
  }
}


class PrimitiveView(override val operation: Operation) extends View(operation)


object View {
  // create new view based on the given type
  def apply(t: Type, op: Operation): View = {
    t match {
      case at: ArrayType => new ArrayView(at.elemT, op)
//      case mt: MatrixType => new MatrixView(mt.elemT, op)
      case st: ScalarType => new PrimitiveView(op)
      case tt: TupleType => new TupleView(tt, op)
      case vt: VectorType => new PrimitiveView(op)
      case _ => throw new IllegalArgumentException("PANIC " + t.getClass)
    }
  }

  def createView(expr: Expr, f:  List[(ArithExpr, ArithExpr)] = List()): View = {
    val result = expr match {
      case pr: ParamReference => getViewAtIndex(pr.p.view, pr.i)
      case p: Param => p.view //View(expr.t, new InputAccess())
      case call: FunCall => createViewFunCall(call, f)
    }
    expr.view = result
    result
  }

  private def getViewAtIndex(v: View, index: Int): View = {
    v match {
      case tv: TupleView =>
        assert(index < tv.tupleType.elemsT.length)
        tv.access(index)
      case _ => throw new IllegalArgumentException("PANIC " + v.getClass)
    }
  }

  def getInputAccess(sv : View, tupleAccessStack : Stack[Int] = new Stack()) : InputAccess = {
    sv.operation match {
      case ia : InputAccess => ia
      case aa : ArrayAccess => View.getInputAccess(aa.av, tupleAccessStack)
      case ac : ArrayCreation =>View.getInputAccess(ac.v, tupleAccessStack)
      case as : ArraySplit => View.getInputAccess(as.av, tupleAccessStack)
      case aj : ArrayJoin => View.getInputAccess(aj.av, tupleAccessStack)
      case ar : ArrayReorder => View.getInputAccess(ar.av, tupleAccessStack)
      case sa : SubArray => View.getInputAccess(sa.in, tupleAccessStack)
      case at : ArrayTail => View.getInputAccess(at.in, tupleAccessStack)

      case aav: ArrayAsVector => View.getInputAccess(aav.av, tupleAccessStack)
      case aas: ArrayAsScalar => View.getInputAccess(aas.av, tupleAccessStack)

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

  private def createViewFunCall(call: FunCall, ids: List[(ArithExpr, ArithExpr)]): View = {
    val argView = getViewFromArgs(call, ids)

    call match {
      case call: MapCall => createViewMap(call, argView, ids)
      case call: ReduceCall => createViewReduce(call, argView, ids)
      case call: HeadCall => createViewHead(call, argView, ids)
      case call: TailCall => createViewTailCall(call, argView, ids)

      case call: FunCall =>
        call.f match {
          case l: Lambda => createViewLambda(l, call, argView, ids)
          case cf: CompFunDef => createViewCompFunDef(cf, argView, ids)
          case z: Zip => createViewZip(z, call, argView)
          case Split(n) => createViewSplit(n, argView)
          case _: Join => createViewJoin(call, argView)
          case uf: UserFunDef => createViewUserFunDef(uf, argView, ids)
          case ReorderStride(s) => createViewReorderStride(s, call, argView)
          case g: Gather => createViewGather(g, call, argView, ids)
          case s: Scatter => createViewScatter(s, call, argView, ids)
          case tL: toLocal => createViewToLocal(tL, argView, ids)
          case tG: toGlobal => createViewToGlobal(tG, argView, ids)
          case i: Iterate => createViewIterate(i, call, argView, ids)
          case t: Transpose => createViewTranspose(t, call, argView)
          case tw: TransposeW => createViewTransposeW(tw, call, argView, ids)
          case asVector(n) => createViewAsVector(n, argView)
          case _: asScalar => createViewAsScalar(argView)
          case f: Filter => createViewFilter(f, call, argView)
          case VTail() => createViewTail(argView)

          /*case uz: Unzip =>
          case SplitDim2(n) =>
          case j: JoinDim2 =>

          case _: Swap =>
          */
          case _ => argView
        }
    }
  }

  private def createViewIterate(i: Iterate, call:FunCall, argView: View, f:  List[(ArithExpr, ArithExpr)]): View = {
    i.f.params(0).view = argView
    createView(i.f.body, f)
    initialiseNewView(call.t, f)
  }

  private def createViewToGlobal(tG: toGlobal, argView: View, f:  List[(ArithExpr, ArithExpr)]): View = {
    tG.f.params(0).view = argView
    createView(tG.f.body, f)
  }

  private def createViewToLocal(tL: toLocal, argView: View, f:  List[(ArithExpr, ArithExpr)]): View = {
    tL.f.params(0).view = argView
    createView(tL.f.body, f)
  }

  private def getViewFromArgs(call: FunCall, f:  List[(ArithExpr, ArithExpr)]): View = {
    if (call.args.isEmpty) {
      NoView
    } else if (call.args.length == 1) {
      createView(call.args(0), f)
    } else {
      TupleView(call.argsType, new TupleCreation(call.args.map((expr: Expr) => createView(expr, f))))
    }
  }

  private def createViewMap(call: MapCall, argView: View, f:  List[(ArithExpr, ArithExpr)]): View = {
    argView match {
      case av: ArrayView =>
        call.f.f.params(0).view = av.access(call.loopVar)

        val newF = (Type.getLength(call.t), call.loopVar) :: f

        val innerView = createView(call.f.f.body, newF)
        new ArrayView(Type.getElemT(call.t), new ArrayCreation(innerView, Type.getLength(call.t), call.loopVar))
//        new MatrixView(Type.getElemT(call.t), new MatrixCreation(innerView, Type.getWidth(call.t), Type.getHeight(call.t), call.loopVar))
        case _ => throw new IllegalArgumentException("PANIC " + argView.getClass)
    }
  }

  private def createViewReduce(call: ReduceCall, argView: View, f:  List[(ArithExpr, ArithExpr)]): View = {
    argView match {
      case tv: TupleView =>
        call.f.f.params(0).view = tv.access(0)
        call.f.f.params(1).view = tv.access(1).asInstanceOf[ArrayView].access(call.loopVar)

        val newF = (Type.getLength(call.t), Cst(0)) :: f

        val innerView = createView(call.f.f.body, newF)
        new ArrayView(Type.getElemT(call.t), new ArrayCreation(innerView, Type.getLength(call.t), call.loopVar))
      case _ => throw new IllegalArgumentException("PANIC " + argView.getClass)
    }
  }

  private def createViewLambda(l: Lambda, call: FunCall, argView: View, f:  List[(ArithExpr, ArithExpr)]): View = {
    assert(call.args.nonEmpty)
    if (call.args.length == 1) {
      if (l.params.length != 1) throw new NumberOfArgumentsException
      l.params(0).view = argView
    } else {
      val tv = argView match { case tv: TupleView => tv }

      l.params.zipWithIndex.map({ case (p, i) => p.view = tv.access(i) })
    }
    createView(l.body, f)
  }

  private def createViewCompFunDef(cf: CompFunDef, argView: View, f:  List[(ArithExpr, ArithExpr)]): View = {

    val funs = scala.collection.mutable.Stack(f)

    cf.funs.foldRight(argView)((f, v) => {
      if (f.params.length != 1) throw new NumberOfArgumentsException
      f.params(0).view = v

      f.body match {
        case call: FunCall =>
          call.f match {
            case tL: toLocal => funs.push(List())
            case tG: toGlobal => if (funs.length > 1) funs.pop()
            case _ =>
          }
        case _ =>
      }

      createView(f.body, funs.top)
    })
  }

  private def createViewZip(z: Zip, call: FunCall, argView: View): View = {
    argView match {
      case tv: TupleView => new ArrayView(Type.getElemT(call.t), new ArrayZip(tv))
      // new ArrayCreation(tv, Type.getLength(call.t), ???))
      //case tv: TupleView => tv
      case _ => throw new IllegalArgumentException("PANIC! Expected tuple, found " + argView.getClass)
    }
  }

  private def createViewFilter(f: Filter, call: FunCall, argView: View): View = {
    argView match {
      case tv: TupleView =>
        new ArrayView(Type.getElemT(call.t),
          new SubArray(tv.access(0).asInstanceOf[ArrayView], tv.access(1).asInstanceOf[ArrayView])
        )
      case _ => throw new IllegalArgumentException("PANIC! Expected tuple, found " + argView.getClass)
    }
  }


  private def createViewJoin(call: FunCall, argView: View): View = {
    val chunkSize = call.argsType match {
      case ArrayType(ArrayType(_, n), _) => n
      case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + call.argsType)
    }

    argView match {
      case av: ArrayView => av.join(chunkSize)
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def createViewSplit(n: ArithExpr, argView: View): View = {
    argView match {
      case av: ArrayView => av.split(n)
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def createViewHead(call: HeadCall, argView: View, f:  List[(ArithExpr, ArithExpr)]) : View = {
    argView match {
      case av: ArrayView =>
        call.arg.view = av.access(Cst(0))
        av.access(Cst(0))
//        argView
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def createViewTail(argView: View) : View = {
    argView match {
      case av: ArrayView =>
        av.tail()
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found "+ argView.getClass)
    }
  }

  private def createViewTailCall(call: TailCall, argView: View, f: List[(ArithExpr,ArithExpr)]) :View ={
    argView match {
      case av: ArrayView =>
        call.arg.view = av.access(call.loopVar + 1)
        val elemT = call.t match { case at : ArrayType => at.elemT }

        val newF = (Type.getLength(call.t), call.loopVar) :: f

        val innerView = initialiseNewView(call.t, newF)

        call.view = new ArrayView(elemT, new ArrayCreation(innerView, Type.getLength(call.t)-1, call.loopVar))

        call.view
//        av.access(call.loopVar)
//        val newF = (Type.getLength(call.t), call.loopVar) :: f
//        new ArrayView(Type.getElemT(call.t), new ArrayCreation(argView, Type.getLength(call.t) -1, call.loopVar))
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found "+ argView.getClass)
    }
  }

  private def createViewAsVector(n: ArithExpr, argView: View): View = {
    argView match {
      case av: ArrayView => av.asVector(n)
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def createViewAsScalar(argView: View): View = {
    argView match {
      case av: ArrayView => av.asScalar()
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def createViewUserFunDef(uf: UserFunDef, argView: View, f:  List[(ArithExpr, ArithExpr)]): View = {
    initialiseNewView(uf.outT, f)
  }

  private def initialiseNewView(t: Type, f: List[(ArithExpr, ArithExpr)], name: String = ""): View = {
    // Use the lengths and iteration vars to mimic inputs
    val outArray = f.foldLeft(t)((t, len) => ArrayType(t, len._1))
    val outView = View(outArray, new InputAccess(name))
    f.foldRight(outView)((idx, view) => view.asInstanceOf[ArrayView].access(idx._2))
  }

  private def createViewReorderStride(s: ArithExpr, call: FunCall, argView: View): View = {
    val n = Type.getLength(call.argsType) / s

    argView match {
      case av: ArrayView => av.reorder( (i:ArithExpr) => { (i div n) + s * ( i % n) } )
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def createViewTranspose(t: Transpose, call: FunCall, argView: View): View = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        argView.asInstanceOf[ArrayView].
          join(n).
          reorder((i:ArithExpr) => { IndexFunction.transpose(i, call.t) }).
          split(m)
    }
  }

  private def createViewTransposeW(tw: TransposeW, call: FunCall, argView: View, ids: List[(ArithExpr, ArithExpr)]): View = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        findAccessAndReorder(argView, IndexFunction.transpose, call.t, 0, transpose = true)
    }

    initialiseNewView(call.t, ids)
  }

  private def createViewGather(gather: Gather, call: FunCall, argView: View, f:  List[(ArithExpr, ArithExpr)]): View = {
    argView match {
      case av: ArrayView =>
        gather.f.params(0).view = av.reorder( (i:ArithExpr) => { gather.idx.f(i, call.t) } )
        createView(gather.f.body, f)
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def createViewScatter(scatter: Scatter, call: FunCall, argView: View, f:  List[(ArithExpr, ArithExpr)]): View = {
    argView match {
      case av: ArrayView =>
        scatter.f.params(0).view = av
        createView(scatter.f.body, f)

        // Find the matching ArrayAccess to the first ArrayCreation,
        // and reorder the ArrayView in the access
        findAccessAndReorder(scatter.f.body.view, scatter.idx, call.t, 0)

        scatter.f.body.view
      case _ => throw new IllegalArgumentException("PANIC! Expected array, found " + argView.getClass)
    }
  }

  private def findAccessAndReorder(view: View, idx: IndexFunction, t:Type, count: scala.Int, transpose: Boolean = false): Unit = {
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

}

object ViewPrinter {

  def emit(sv : View) : ArithExpr = {
    sv match {
      case _: PrimitiveView => emitView(sv, new Stack(), new Stack())
      case _: TupleView => emitView(sv, new Stack(), new Stack())
      case t => throw new IllegalArgumentException(t.getClass + " found, TupleView/PrimitiveView expected")
    }
  }


  private def emitView(sv : View,
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
        val indirection = new AccessVar(View.getInputAccess(sa.ids).name, newIdx)

        emitView(sa.in, stack.push((indirection, len)), tupleAccessStack)

      case at: ArrayTail =>
        val ((idx, len), stack) = arrayAccessStack.pop2
        val newIdx = idx - 1
        val newLen = len - 1
        val newAS = stack.push((newIdx, newLen))
        emitView(at.in, newAS, tupleAccessStack)

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


