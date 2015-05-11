package ir.view

import arithmetic._
import ir._
import opencl.ir._

import scala.collection.immutable.Stack

abstract class View(val t: Type = UndefType) {

  def replaced(oldExpr: ArithExpr, newExpr: ArithExpr): View = {
    val subst = new scala.collection.mutable.HashMap[ArithExpr,ArithExpr]()
    subst.put(oldExpr, newExpr)

    this match {
      case map: ViewMap => new ViewMap(map.iv.replaced(oldExpr,newExpr), map.itVar, t)
      case access: ViewAccess => new ViewAccess(ArithExpr.substitute(access.i, subst.toMap), access.iv.replaced(oldExpr,newExpr), t)
      case zip: ViewZip => new ViewZip(zip.ivs.map(_.replaced(oldExpr,newExpr)), t)
      case split: ViewSplit => new ViewSplit(ArithExpr.substitute(split.n, subst.toMap), split.iv.replaced(oldExpr,newExpr), t)
      case join: ViewJoin => new ViewJoin(ArithExpr.substitute(join.n, subst.toMap), join.iv.replaced(oldExpr,newExpr), t)
      case gather: ViewReorder => new ViewReorder(gather.f, gather.iv.replaced(oldExpr, newExpr), t)
      case asVector: ViewAsVector => new ViewAsVector(asVector.n, asVector.iv.replaced(oldExpr, newExpr), t)
      case asScalar: ViewAsScalar => new ViewAsScalar(asScalar.iv.replaced(oldExpr, newExpr), asScalar.n, t)
      case filter: ViewFilter => new ViewFilter(filter.iv.replaced(oldExpr, newExpr), filter.ids.replaced(oldExpr, newExpr), t)
      case tuple: ViewTuple => new ViewTuple(tuple.ivs.map(_.replaced(oldExpr, newExpr)), t)
      case component: ViewTupleComponent => new ViewTupleComponent(component.i, component.iv.replaced(oldExpr,newExpr), t)
      case group: ViewGroup => new ViewGroup(group.iv.replaced(oldExpr, newExpr), group.group, group.t)
      case  _ => this
    }
  }

  def access(idx: ArithExpr): View = {
    new ViewAccess(idx, this, t.asInstanceOf[ArrayType].elemT)
  }

  def split(chunkSize : ArithExpr): View = {
    this.t match {
      case ArrayType(elemT, n) => new ViewSplit(chunkSize, this,
        ArrayType(ArrayType(elemT, chunkSize), n div chunkSize))
    }
  }

  def join(chunkSize: ArithExpr): View = {
    this.t match {
      case ArrayType(ArrayType(elemT, n), m) =>
        new ViewJoin(chunkSize, this, ArrayType(elemT, n*m))
    }
  }

  def reorder(f: (ArithExpr) => ArithExpr): View = {
    new ViewReorder(f, this, this.t)
  }

  def asVector(n: ArithExpr): View = {
    t match {
      case ArrayType(st: ScalarType, len) =>
        new ViewAsVector(n, this, ArrayType(Type.vectorize(st, n), len))
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into vector types")
    }
  }

  def filter(ids: View): View = {
    new ViewFilter(this, ids,
      ArrayType(this.t.asInstanceOf[ArrayType].elemT, ids.t.asInstanceOf[ArrayType].len))
  }

  def asScalar(): View = {
    t match {
      case ArrayType(VectorType(st, n), len) =>
        new ViewAsScalar(this, n, ArrayType(st, len))
      case st: ScalarType => this
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into scalar types")
    }
  }

  def get(i: Int): View = {
    new ViewTupleComponent(i, this, t.asInstanceOf[TupleType].elemsT(i))
  }

  def zip(): View = {
    this match {
      case tuple: ViewTuple =>
        new ViewZip(tuple.ivs, ArrayType(TupleType(tuple.ivs.map(_.t.asInstanceOf[ArrayType].elemT):_*),
          tuple.ivs.head.t.asInstanceOf[ArrayType].len))
      case other => throw new IllegalArgumentException("Can't zip " + other.getClass)
    }
  }

  def group(g: Group): View = {
    this.t match {
      case ArrayType(elemT, len) =>
        new ViewGroup(this, g, ArrayType(ArrayType(elemT, g.relIndices.length), len))
      case other => throw new IllegalArgumentException("Can't group " + other)
    }
  }
//        new MatrixView(Type.getElemT(call.t), new MatrixCreation(innerView, Type.getWidth(call.t), Type.getHeight(call.t), call.loopVar))
}

class ViewMem(val name: String, override val t: Type) extends View(t)

class ViewAccess(val i: ArithExpr, val iv: View, override val t: Type) extends View(t)

class ViewSplit(val n: ArithExpr, val iv: View, override val t: Type) extends View(t)

class ViewJoin(val n: ArithExpr, val iv: View, override val t: Type) extends View(t)

class ViewZip(val ivs: Seq[View], override val t: Type) extends View(t)

class ViewReorder(val f: ArithExpr => ArithExpr, val iv: View, override val t: Type) extends View(t)

class ViewAsVector(val n: ArithExpr, val iv: View, override val t: Type) extends View(t)

class ViewAsScalar(val iv: View, val n: ArithExpr, override val t: Type) extends View(t)

class ViewFilter(val iv: View, val ids: View, override val t: Type) extends View(t)

class ViewMap(val iv: View, val itVar: Var, override val t: Type) extends View(t)

class ViewTupleComponent(val i: Int, val iv: View, override val t: Type) extends View(t)

class ViewTuple(val ivs: Seq[View], override val t: Type) extends View(t)

class ViewGroup(val iv: View, val group: Group, override val t: Type) extends View(t)

class ViewHead(val iv: View, override val t: Type) extends View(t)

class ViewTail(val iv: View, override val t: Type) extends View(t)

//class DynamicView(val iv: View, val viewVar: Var, override t:Type) extends View(t)

object NoView extends View()

object View {
  /** create new view based on the given type*/
  def apply(t: Type, name: String): View = new ViewMem(name, t)

  def tuple(ivs: View*) = new ViewTuple(ivs, TupleType(ivs.map(_.t):_*))

  def visitAndBuildViews(expr: Expr): Unit = {
    BuildDepthInfo(expr)
    InputView(expr)
    OutputView(expr)
  }

  private def getFullType(outputType: Type, outputAccessInf: List[(ArithExpr, ArithExpr)]): Type = {
    outputAccessInf.foldLeft(outputType)((t, len) => ArrayType(t, len._1))
  }

  private[view] def initialiseNewView(t: Type, outputAccessInf: List[(ArithExpr, ArithExpr)], name: String = ""): View = {
    // Use the lengths and iteration vars to mimic inputs
    val outArray = getFullType(t, outputAccessInf)
    val outView = View(outArray, name)
    outputAccessInf.foldRight(outView)((idx, view) => view.access(idx._2))
  }

}

object ViewPrinter {

  def emit(sv : View) : ArithExpr = {
    assert(!sv.t.isInstanceOf[ArrayType])
    emitView(sv, new Stack(), new Stack())
  }

  private def emitView(sv : View,
                       arrayAccessStack : Stack[(ArithExpr, ArithExpr)], // id, dimension size
                       tupleAccessStack : Stack[Int]) : ArithExpr = {
    sv match {
      case mem : ViewMem =>
        assert(tupleAccessStack.isEmpty)
        arrayAccessStack.map(x => (x._1*x._2).asInstanceOf[ArithExpr]).foldLeft(Cst(0).asInstanceOf[ArithExpr])((x, y) => (x+y).asInstanceOf[ArithExpr])

      case access : ViewAccess =>
        val length: ArithExpr = getLengthForArrayAccess(sv.t, tupleAccessStack)
        val newAAS = arrayAccessStack.push((access.i, length))
        emitView(access.iv,newAAS, tupleAccessStack)

      case map : ViewMap =>
        val idx = arrayAccessStack.top._1
        val newAAS = arrayAccessStack.pop
        val newV = map.iv.replaced(map.itVar, idx)
        emitView(newV,newAAS,tupleAccessStack)

      case split : ViewSplit =>
        val (chunkId,stack1) = arrayAccessStack.pop2
        val (chunkElemId,stack2) = stack1.pop2
        val newIdx = chunkId._1*split.n+chunkElemId._1
        val newAAS = stack2.push((newIdx, chunkElemId._2))
        emitView(split.iv, newAAS,tupleAccessStack)

      case ag : ViewGroup =>
        val (outerId,stack1) = arrayAccessStack.pop2
        val (innerId,stack2) = stack1.pop2

        ag.group.params(0).t match {
          case ArrayType(t, len) =>
            val newIdx = new GroupCall(ag.group, outerId._1, innerId._1, len)
            val newAAS = stack2.push((newIdx, innerId._2))
            emitView(ag.iv, newAAS, tupleAccessStack)
          case _ => throw new IllegalArgumentException()
        }

      case join : ViewJoin =>
        val (idx,stack) = arrayAccessStack.pop2
        val chunkSize: ArithExpr = join.n
        val chunkId = idx._1 div chunkSize
        val chunkElemId = idx._1 % chunkSize
        val newAS = stack.push((chunkElemId, Type.getLengths(sv.t.asInstanceOf[ArrayType].elemT).reduce(_*_))).
          push((chunkId, Type.getLengths(join.t.asInstanceOf[ArrayType].elemT).reduce(_*_)*join.n))
        emitView(join.iv,newAS,tupleAccessStack)

      case gather : ViewReorder =>
        val (idx,stack) = arrayAccessStack.pop2
        val newIdx = gather.f(idx._1)
        val newAS = stack.push((newIdx, idx._2))
        emitView(gather.iv,newAS,tupleAccessStack)

      case filter : ViewFilter =>
        val ((idx, len), stack) = arrayAccessStack.pop2

        val newIdx = emit(filter.ids.access(idx))
        val indirection = new AccessVar(getInputAccess(filter.ids).name, newIdx)

        emitView(filter.iv, stack.push((indirection, len)), tupleAccessStack)

      case component : ViewTupleComponent =>
        val newTAS = tupleAccessStack.push(component.i)
        emitView(component.iv,arrayAccessStack,newTAS)

      case zip : ViewZip =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        emitView(zip.ivs(i),arrayAccessStack,newTAS)

      case tuple : ViewTuple =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        emitView(tuple.ivs(i),arrayAccessStack,newTAS)

      case asVector: ViewAsVector =>
        val top = arrayAccessStack.top
        val newAAS = arrayAccessStack.pop.push((top._1 * asVector.n, top._2)).map(x => (x._1, x._2 / asVector.n))
        emitView(asVector.iv, newAAS, tupleAccessStack)

      case asScalar: ViewAsScalar =>
        val top = arrayAccessStack.top
        val newAAS = arrayAccessStack.pop.push((top._1 / asScalar.n, top._2)).map(x => (x._1, x._2 * asScalar.n))
        emitView(asScalar.iv, newAAS, tupleAccessStack)

      case head: ViewHead =>
        val newAAS = arrayAccessStack.pop
        emitView(head.iv, newAAS,tupleAccessStack)

      case tail: ViewTail =>
        val (idx, stack) = arrayAccessStack.pop2
        val newIdx = idx._1 + 1
        val newLen = idx._2
        val newAAS = stack.push((newIdx, newLen))
        emitView(tail.iv,newAAS,tupleAccessStack)


      case op => throw new NotImplementedError(op.getClass.toString)
    }
  }

  private def getInputAccess(sv : View, tupleAccessStack : Stack[Int] = new Stack()) : ViewMem = {
    sv match {
      case map : ViewMem => map
      case access : ViewAccess => getInputAccess(access.iv, tupleAccessStack)
      case map : ViewMap => getInputAccess(map.iv, tupleAccessStack)
      case split : ViewSplit => getInputAccess(split.iv, tupleAccessStack)
      case join : ViewJoin => getInputAccess(join.iv, tupleAccessStack)
      case gather : ViewReorder => getInputAccess(gather.iv, tupleAccessStack)
      case filter : ViewFilter => getInputAccess(filter.iv, tupleAccessStack)
      case asVector: ViewAsVector => getInputAccess(asVector.iv, tupleAccessStack)
      case asScalar: ViewAsScalar => getInputAccess(asScalar.iv, tupleAccessStack)

      case component : ViewTupleComponent =>
        val newTAS = tupleAccessStack.push(component.i)
        getInputAccess(component.iv, newTAS)

      case zip : ViewZip =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        getInputAccess(zip.ivs(i),newTAS)

      case tuple: ViewTuple =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        getInputAccess(tuple.ivs(i),newTAS)

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


