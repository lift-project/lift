package ir.view

import apart.arithmetic._
import ir._
import ir.ast._
import opencl.ir.ast._

/**
 * An arithmetic expression that performs an access to `array[idx]`
 *
 * @param array Array name
 * @param idx Index to access in the array
 */
class AccessVar(val array: String, val idx: ArithExpr) extends Var("")

/**
 * Views are lazy constructs for determining the locations for memory accesses.
 * They are lazy in the sense that if an array should be reordered, they remember the
 * reordering, but do not actually reorder an array to avoid unnecessary copies.
 *
 * @param t Type of the view.
 */
abstract class View(val t: Type = UndefType) {

  /**
   * Construct a new view, where `oldExpr` is replaced by `newExpr`
   *
   * @param oldExpr Expression to replace.
   * @param newExpr Expression to use for replacing
   * @return The new view.
   */
  def replaced(oldExpr: ArithExpr, newExpr: ArithExpr): View = {
    val subst = new scala.collection.mutable.HashMap[ArithExpr, ArithExpr]()
    subst.put(oldExpr, newExpr)

    this match {
      case map: ViewMap => new ViewMap(map.iv.replaced(oldExpr, newExpr), map.itVar, t)
      case access: ViewAccess => new ViewAccess(ArithExpr.substitute(access.i, subst.toMap), access.iv.replaced(oldExpr, newExpr), t)
      case zip: ViewZip => new ViewZip(zip.ivs.map(_.replaced(oldExpr, newExpr)), t)
      case unzip: ViewUnzip => new ViewUnzip(unzip.iv.replaced(oldExpr, newExpr), t)
      case split: ViewSplit => new ViewSplit(ArithExpr.substitute(split.n, subst.toMap), split.iv.replaced(oldExpr, newExpr), t)
      case join: ViewJoin => new ViewJoin(ArithExpr.substitute(join.n, subst.toMap), join.iv.replaced(oldExpr, newExpr), t)
      case gather: ViewReorder => new ViewReorder(gather.f, gather.iv.replaced(oldExpr, newExpr), t)
      case asVector: ViewAsVector => new ViewAsVector(asVector.n, asVector.iv.replaced(oldExpr, newExpr), t)
      case asScalar: ViewAsScalar => new ViewAsScalar(asScalar.iv.replaced(oldExpr, newExpr), asScalar.n, t)
      case filter: ViewFilter => new ViewFilter(filter.iv.replaced(oldExpr, newExpr), filter.ids.replaced(oldExpr, newExpr), t)
      case tuple: ViewTuple => new ViewTuple(tuple.ivs.map(_.replaced(oldExpr, newExpr)), t)
      case component: ViewTupleComponent => new ViewTupleComponent(component.i, component.iv.replaced(oldExpr, newExpr), t)
      case group: ViewGroup => new ViewGroup(group.iv.replaced(oldExpr, newExpr), group.group, group.t)
      case _ => this
    }
  }

  /**
   * Construct a new view for accessing the current view at position `idx`.
   *
   * @param idx Access position
   * @return The view for position `idx`
   */
  def access(idx: ArithExpr): View = {
    new ViewAccess(idx, this, t.asInstanceOf[ArrayType].elemT)
  }

  /**
   * Construct a view for splitting the current view.
   *
   * Corresponds to the Split() pattern for InputView and Join() pattern for OutputView.
   *
   * @param chunkSize The size of chunks the input array should be split in.
   */
  def split(chunkSize: ArithExpr): View = {
    this.t match {
      case ArrayType(elemT, n) => new ViewSplit(chunkSize, this,
        ArrayType(ArrayType(elemT, chunkSize), n / chunkSize))
      case _ => throw new IllegalArgumentException("PANIC: split expects an array type")
    }
  }

  /**
   * Construct a view for joining the current view.
   *
   * Corresponds to the Join() pattern for InputView and Split() pattern for OutputView.
   *
   * @param chunkSize The chunk size of the dimension that should be joined.
   */
  def join(chunkSize: ArithExpr): View = {
    this.t match {
      case ArrayType(ArrayType(elemT, n), m) =>
        new ViewJoin(chunkSize, this, ArrayType(elemT, n * m))
      case _ => throw new IllegalArgumentException("PANIC: join expects an array type")
    }
  }

  /**
   * Construct a view for reordering the current view using function `f`.
   *
   * Corresponds to Scatter and Gather patterns.
   *
   * @param f Function to use for reordering.
   */
  def reorder(f: (ArithExpr) => ArithExpr): View = {
    new ViewReorder(f, this, this.t)
  }

  /**
   * Construct a view for vectorising the current view.
   *
   * Corresponds to the asVector pattern for InputView and the asScalar pattern
   * for OutputView.
   *
   * @param n Vector width.
   */
  def asVector(n: ArithExpr): View = {
    t match {
      case ArrayType(st: ScalarType, len) =>
        new ViewAsVector(n, this, ArrayType(st.vectorize(n), len))
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into vector types")
    }
  }

  /**
   * Construct a view for un-vectorising/scalarising the current view.
   *
   * Corresponds to the asScalar pattern for InputView and the asVector pattern
   * for OutputView.
   *
   * Infers the vector width from the type.
   */
  def asScalar(): View = {
    t match {
      case ArrayType(VectorType(st, n), len) =>
        new ViewAsScalar(this, n, ArrayType(st, len))
      case st: ScalarType => this
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into scalar types")
    }
  }

  /**
   * Construct a view for filtering the current view with `ids`.
   *
   * Corresponds to the Filter pattern.
   *
   * @param ids Indices to use for filtering.
   */
  def filter(ids: View): View = {
    new ViewFilter(this, ids,
      ArrayType(this.t.asInstanceOf[ArrayType].elemT, ids.t.asInstanceOf[ArrayType].len))
  }

  /**
   * Construct a view for projecting the `i`th element out of the current view.
   * The current view has to be a tuple.
   *
   * Corresponds to the Get pattern
   *
   * @param i The index of the element to project.
   */
  def get(i: Int): View = {
    new ViewTupleComponent(i, this, t.asInstanceOf[TupleType].elemsT(i))
  }

  /**
   * Construct a view for zipping the current view. The current view has to be
   * a tuple of arrays.
   *
   * Corresponds to the Zip pattern.

   */
  def zip(): View = {
    this match {
      case tuple: ViewTuple =>
        new ViewZip(tuple.ivs, ArrayType(TupleType(tuple.ivs.map(_.t.asInstanceOf[ArrayType].elemT): _*),
          tuple.ivs.head.t.asInstanceOf[ArrayType].len))
      case other => throw new IllegalArgumentException("Can't zip " + other.getClass)
    }
  }

  /**
   * Construct a view for unzipping the current view. The current view has to be an
   * array of tuples.
   */
  def unzip(): View = {
    t match {
      case ArrayType(TupleType(ts@_*), len) => new ViewUnzip(this, TupleType(ts.map(ArrayType(_, len)): _*))
      case other => throw new IllegalArgumentException("Can't unzip " + other)
    }
  }

  def group(g: Group): View = {
    this.t match {
      case ArrayType(elemT, len) =>
        new ViewGroup(this, g, ArrayType(ArrayType(elemT, g.relIndices.length), len))
      case other => throw new IllegalArgumentException("Can't group " + other)
    }
  }
}

/**
 * A view to memory object.
 *
 * @param name Name of the memory object/array.
 * @param t Type of the view.
 */
private[view] class ViewMem(val name: String, override val t: Type) extends View(t)

/**
 * A view for accessing another view at position `i`.
 *
 * @param i Index to access.
 * @param iv View to access.
 * @param t Type of the view.
 */
private[view] class ViewAccess(val i: ArithExpr, val iv: View, override val t: Type) extends View(t)

/**
 * A view for splitting another view.
 *
 * @param n The size of chunks the input view should be split in.
 * @param iv View to split.
 * @param t Type of the view.
 */
private[view] class ViewSplit(val n: ArithExpr, val iv: View, override val t: Type) extends View(t)

/**
 * A view for joining another view.
 *
 * @param n The chunk size of the dimension that should be joined.
 * @param iv The view to join.
 * @param t Type of the view.
 */
private[view] class ViewJoin(val n: ArithExpr, val iv: View, override val t: Type) extends View(t)

/**
 * A view for zipping a number of views.
 *
 * @param ivs Views to zip.
 * @param t Type of the view.
 */
private[view] class ViewZip(val ivs: Seq[View], override val t: Type) extends View(t)

/**
 * A view for unzipping another view
 *
 * @param iv View to unzip
 * @param t Type of the view
 */
private[view] class ViewUnzip(val iv: View, override val t: Type) extends View(t)

/**
 * A view for reordering.
 *
 * @param f Function to use for reordering.
 * @param iv View to reorder.
 * @param t Type of the view.
 */
private[view] class ViewReorder(val f: ArithExpr => ArithExpr, val iv: View, override val t: Type) extends View(t)

/**
 * A view for vectorisation.
 *
 * @param n Vector width.
 * @param iv View to vectorise.
 * @param t Type of the view.
 */
private[view] class ViewAsVector(val n: ArithExpr, val iv: View, override val t: Type) extends View(t)

/**
 * A view for undoing vectorisation
 *
 * @param iv View to scalarise
 * @param n Vector width
 * @param t Type of the view
 */
private[view] class ViewAsScalar(val iv: View, val n: ArithExpr, override val t: Type) extends View(t)

/**
 * A view for filtering.
 *
 * @param iv View to filter.
 * @param ids A view providing the indices.
 * @param t Type of the View
 */
private[view] class ViewFilter(val iv: View, val ids: View, override val t: Type) extends View(t)

/**
 * A view for exiting a dimension
 *
 * @param iv View to add a dimension to.
 * @param itVar Iteration variable used for the current dimension
 * @param t Type of the view
 */
private[view] class ViewMap(val iv: View, val itVar: Var, override val t: Type) extends View(t)

/**
 * A view for accessing a tuple component.
 *
 * @param i The component to access.
 * @param iv The view to access.
 * @param t Type of the view.
 */
private[view] class ViewTupleComponent(val i: Int, val iv: View, override val t: Type) extends View(t)

/**
 * A view for constructing a tuple.
 *
 * @param ivs The views to construct a tuple of.
 * @param t Type of the view.
 */
private[view] class ViewTuple(val ivs: Seq[View], override val t: Type) extends View(t)

/**
 *  A view for grouping.
 *
 * @param iv View to group.
 * @param group The group function to use.
 * @param t Type of the view.
 */
private[view] class ViewGroup(val iv: View, val group: Group, override val t: Type) extends View(t)

/**
 * Get the head of a view.
 *
 * @param iv The view to get the head of.
 * @param t Type of view
 */
private[view] class ViewHead(val iv: View, override val t: Type) extends View(t)

/**
 * A view for getting the tail of a view.
 *
 * @param iv The view to get the tail of.
 * @param t The type of view.
 */
private[view] class ViewTail(val iv: View, override val t: Type) extends View(t)

/**
 * Placeholder for a view that is not yet created.
 */
object NoView extends View()

object View {

  /**
   * Create new view based on the given type
   *
   * @param t The type of the view.
   * @param name A name for the array.
   * @return
   */
  def apply(t: Type, name: String): View = new ViewMem(name, t)

  private[view] def tuple(ivs: View*) = new ViewTuple(ivs, TupleType(ivs.map(_.t): _*))

  /**
   * Visit the expression and construct all views for all sub-expressions.
   *
   * @param expr The starting expression.
   */
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

/**
 * Helper object for converting views to arithmetic expressions.
 *
 * Incrementally backtracks the links in the views modifying access variables and
 * dimension lengths as necessary.
 *
 * Finally flattens the expression, as arrays are stored in a flattened format.
 */
object ViewPrinter {

  /**
   * Emit the arithmetic expression for accessing an array that corresponds
   * to the view.
   * 
   * @param view The view to emit.
   * @return The arithmetic expression.
   */
  def emit(view: View): ArithExpr = {
    assert(!view.t.isInstanceOf[ArrayType])
    emitView(view, List(), List())
  }

  private def emitView(sv: View,
                       arrayAccessStack: List[(ArithExpr, ArithExpr)], // id, dimension size
                       tupleAccessStack: List[Int]): ArithExpr = {
    sv match {
      case mem: ViewMem =>
        assert(tupleAccessStack.isEmpty)
        arrayAccessStack.map(x => x._1 * x._2).foldLeft(Cst(0).asInstanceOf[ArithExpr])((x, y) => x + y)

      case access: ViewAccess =>
        val length: ArithExpr = getLengthForArrayAccess(sv.t, tupleAccessStack)
        val newAAS = (access.i, length) :: arrayAccessStack
        emitView(access.iv, newAAS, tupleAccessStack)

      case map: ViewMap =>
        val idx = arrayAccessStack.head._1
        val newAAS = arrayAccessStack.tail
        val newV = map.iv.replaced(map.itVar, idx)
        emitView(newV, newAAS, tupleAccessStack)

      case split: ViewSplit =>
        val chunkId = arrayAccessStack.head
        val stack1 = arrayAccessStack.tail
        val chunkElemId = stack1.head
        val stack2 = stack1.tail
        val newIdx = chunkId._1 * split.n + chunkElemId._1
        val newAAS = (newIdx, chunkElemId._2) :: stack2
        emitView(split.iv, newAAS, tupleAccessStack)

      case ag: ViewGroup =>
        val outerId = arrayAccessStack.head
        val stack1 = arrayAccessStack.tail
        val innerId = stack1.head
        val stack2 = stack1.tail

        ag.group.params(0).t match {
          case ArrayType(t, len) =>
            val newIdx = new GroupCall(ag.group, outerId._1, innerId._1, len)
            val newAAS = (newIdx, innerId._2) :: stack2
            emitView(ag.iv, newAAS, tupleAccessStack)
          case _ => throw new IllegalArgumentException()
        }

      case join: ViewJoin =>
        val idx = arrayAccessStack.head
        val stack = arrayAccessStack.tail
        val chunkSize: ArithExpr = join.n
        val chunkId = idx._1 / chunkSize
        val chunkElemId = idx._1 % chunkSize
        val newAS = stack.::((chunkElemId, Type.getLengths(sv.t.asInstanceOf[ArrayType].elemT).reduce(_ * _))).
          ::((chunkId, Type.getLengths(join.t.asInstanceOf[ArrayType].elemT).reduce(_ * _) * join.n))
        emitView(join.iv, newAS, tupleAccessStack)

      case gather: ViewReorder =>
        val idx = arrayAccessStack.head
        val stack = arrayAccessStack.tail
        val newIdx = gather.f(idx._1)
        val newAS = (newIdx, idx._2) :: stack
        emitView(gather.iv, newAS, tupleAccessStack)

      case filter: ViewFilter =>
        val (idx, len) = arrayAccessStack.head
        val stack = arrayAccessStack.tail

        val newIdx = emit(filter.ids.access(idx))
        val indirection = new AccessVar(getInputAccess(filter.ids).name, newIdx)

        emitView(filter.iv, (indirection, len) :: stack, tupleAccessStack)

      case component: ViewTupleComponent =>
        val newTAS = component.i :: tupleAccessStack
        emitView(component.iv, arrayAccessStack, newTAS)

      case zip: ViewZip =>
        val i = tupleAccessStack.head
        val newTAS = tupleAccessStack.tail
        emitView(zip.ivs(i), arrayAccessStack, newTAS)

      case unzip: ViewUnzip =>
        emitView(unzip.iv, arrayAccessStack, tupleAccessStack)

      case tuple: ViewTuple =>
        val i = tupleAccessStack.head
        val newTAS = tupleAccessStack.tail
        emitView(tuple.ivs(i), arrayAccessStack, newTAS)

      case asVector: ViewAsVector =>
        val top = arrayAccessStack.head
        val newAAS = ((top._1 * asVector.n, top._2) :: arrayAccessStack.tail).map(x => (x._1, x._2 /^ asVector.n))
        emitView(asVector.iv, newAAS, tupleAccessStack)

      case asScalar: ViewAsScalar =>
        val top = arrayAccessStack.head
        val newAAS = ((top._1 /^ asScalar.n, top._2) :: arrayAccessStack.tail).map(x => (x._1, x._2 * asScalar.n))
        emitView(asScalar.iv, newAAS, tupleAccessStack)

      case head: ViewHead =>
        val newAAS = arrayAccessStack.tail
        emitView(head.iv, newAAS, tupleAccessStack)

      case tail: ViewTail =>
        val idx = arrayAccessStack.head
        val stack = arrayAccessStack.tail
        val newIdx = idx._1 + 1
        val newLen = idx._2
        val newAAS = (newIdx, newLen) :: stack
        emitView(tail.iv, newAAS, tupleAccessStack)

      case op => throw new NotImplementedError(op.getClass.toString)
    }
  }

  private def getInputAccess(sv: View, tupleAccessStack: List[Int] = List()): ViewMem = {
    sv match {
      case map: ViewMem => map
      case access: ViewAccess => getInputAccess(access.iv, tupleAccessStack)
      case map: ViewMap => getInputAccess(map.iv, tupleAccessStack)
      case split: ViewSplit => getInputAccess(split.iv, tupleAccessStack)
      case join: ViewJoin => getInputAccess(join.iv, tupleAccessStack)
      case gather: ViewReorder => getInputAccess(gather.iv, tupleAccessStack)
      case filter: ViewFilter => getInputAccess(filter.iv, tupleAccessStack)
      case asVector: ViewAsVector => getInputAccess(asVector.iv, tupleAccessStack)
      case asScalar: ViewAsScalar => getInputAccess(asScalar.iv, tupleAccessStack)

      case component: ViewTupleComponent =>
        val newTAS = tupleAccessStack.::(component.i)
        getInputAccess(component.iv, newTAS)

      case zip: ViewZip =>
        val i = tupleAccessStack.head
        val newTAS = tupleAccessStack.tail
        getInputAccess(zip.ivs(i), newTAS)

      case tuple: ViewTuple =>
        val i = tupleAccessStack.head
        val newTAS = tupleAccessStack.tail
        getInputAccess(tuple.ivs(i), newTAS)

      case op => throw new NotImplementedError(op.getClass.toString)
    }
  }

  private def getLengthForArrayAccess(t: Type, tupleAccesses: List[Int]): ArithExpr = {

    if (tupleAccesses.isEmpty) {
      Type.getLengths(t).reduce(_ * _)
    } else {
      t match {
        case tt: TupleType => getLengthForArrayAccess(Type.getTypeAtIndex(tt, tupleAccesses.head), tupleAccesses.tail)
        case ArrayType(elemT, n) => getLengthForArrayAccess(elemT, tupleAccesses) * n
        case _ => throw new IllegalArgumentException("PANIC: cannot compute array access")
      }
    }
  }
}


