package ir.view

import lift.arithmetic._
import ir._
import ir.ast._
import opencl.generator.OpenCLAST.{ArithExpression, Expression, VarRef}

import scala.collection.immutable
import opencl.generator.{OpenCLAST, OpenCLPrinter}

/**
 * An arithmetic expression that performs an access to `array[idx]`
 *
 * @param array Array name
 * @param idx Index to access in the array
 */
case class AccessVar(array: String, idx: ArithExpression, r : Range = RangeUnknown, fixedId: Option[Long] = None) extends ExtensibleVar("",r,fixedId) {
  override def copy(r: Range) = AccessVar(array, idx, r, Some(id))

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr =
    f(AccessVar(array, ArithExpression(idx.content.visitAndRebuild(f)), range.visitAndRebuild(f), Some(id)))
}

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
    val subst = collection.Map[ArithExpr, ArithExpr](oldExpr -> newExpr)

    replaced(subst)
  }

  def replaced(subst: collection.Map[ArithExpr, ArithExpr]): View = {
    this match {
      case map: ViewMap => ViewMap(map.iv.replaced(subst), map.itVar, t)
      case access: ViewAccess => ViewAccess(ArithExpr.substitute(access.i, subst.toMap), access.iv.replaced(subst), t)
      case zip: ViewZip => ViewZip(zip.iv.replaced(subst), t)
      case unzip: ViewUnzip => ViewUnzip(unzip.iv.replaced(subst), t)
      case split: ViewSplit => ViewSplit(ArithExpr.substitute(split.n, subst.toMap), split.iv.replaced(subst), t)
      case join: ViewJoin => ViewJoin(ArithExpr.substitute(join.n, subst.toMap), join.iv.replaced(subst), t)
      case gather: ViewReorder => ViewReorder(gather.f, gather.iv.replaced(subst), t)
      case asVector: ViewAsVector => ViewAsVector(asVector.n, asVector.iv.replaced(subst), t)
      case asScalar: ViewAsScalar => ViewAsScalar(asScalar.iv.replaced(subst), asScalar.n, t)
      case filter: ViewFilter => ViewFilter(filter.iv.replaced(subst), filter.ids.replaced(subst), t)
      case tuple: ViewTuple => ViewTuple(tuple.ivs.map(_.replaced(subst)), t)
      case component: ViewTupleComponent => ViewTupleComponent(component.i, component.iv.replaced(subst), t)
      case slide: ViewSlide => ViewSlide(slide.iv.replaced(subst), slide.slide, slide.t)
      case pad: ViewPad => ViewPad(pad.iv.replaced(subst), pad.left, pad.right, pad.fct, t)
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
    ViewAccess(idx, this, t.asInstanceOf[ArrayType].elemT)
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
      case ArrayType(elemT, n) => ViewSplit(chunkSize, this,
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
        ViewJoin(chunkSize, this, ArrayType(elemT, n * m))
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
    ViewReorder(f, this, this.t)
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
        ViewAsVector(n, this, ArrayType(st.vectorize(n), len /^ n))
      case _ =>
        throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into vector types")
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
        ViewAsScalar(this, n, ArrayType(st, len * n))
      case _: ScalarType => this
      case _ =>
        throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into scalar types")
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
    ViewFilter(this, ids,
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
    ViewTupleComponent(i, this, t.asInstanceOf[TupleType].elemsT(i))
  }

  /**
   * Construct a view for zipping the current view. The current view has to be
   * a tuple of arrays.
   *
   * Corresponds to the Zip pattern.
   */
  def zip(): View = {
    t match {
      case TupleType(ts@_*) if ts.forall(_.isInstanceOf[ArrayType]) =>
        val arrayTs: Seq[ArrayType] = ts.map(_.asInstanceOf[ArrayType])
        val newT = ArrayType(TupleType(arrayTs.map(_.elemT):_*), arrayTs.head.len)
        ViewZip(this, newT)
      case other => throw new IllegalArgumentException("Can't zip " + other)
    }

  }

  /**
   * Construct a view for unzipping the current view. The current view has to be an
   * array of tuples.
   */
  def unzip(): View = {
    t match {
      case ArrayType(TupleType(ts@_*), len) =>
        ViewUnzip(this, TupleType(ts.map(ArrayType(_, len)): _*))
      case other => throw new IllegalArgumentException("Can't unzip " + other)
    }
  }

  def slide(s: Slide): View = {
    this.t match {
      case ArrayType(_, _) =>
        ViewSlide(this, s, s.checkType(this.t, setType=false))
      case other => throw new IllegalArgumentException("Can't group " + other)
    }
  }

  def pad(left: Int, right: Int, boundary: Pad.BoundaryFun): View = {
    this.t match {
      case ArrayType(elemT, len) =>
        ViewPad(this, left, right, boundary, ArrayType(elemT, len + left + right))
      case other => throw new IllegalArgumentException("Can't pad " + other)
    }
  }

  def size(): View = {
    this match {
      case z: ViewZip => z.iv match {
        case t: ViewTuple => t.ivs.head.size()
      }
      case _ if this.t.isInstanceOf[ArrayType] => this.access(0)
    }
  }

}

private[view] case class ViewGeneratorUserFun(f: UserFun, override val t: ArrayType) extends View(t)

private[view] case class View2DGeneratorUserFun(f: UserFun, override val t: ArrayType) extends View(t)

private[view] case class View3DGeneratorUserFun(f: UserFun, override val t: ArrayType) extends View(t)

private[view] case class ViewGenerator(f: (ArithExpr, ArithExpr) => Expression, override val t: ArrayType) extends View(t)

private[view] case class ViewConstant(value: Value, override val t: Type) extends View(t)

/**
 * A view to memory object.
 *
 * @param name Name of the memory object/array.
 * @param t Type of the view.
 */
private[view] case class ViewMem(name: String, override val t: Type) extends View(t)

/**
 * A view for accessing another view at position `i`.
 *
 * @param i Index to access.
 * @param iv View to access.
 * @param t Type of the view.
 */
private[view] case class ViewAccess(i: ArithExpr, iv: View, override val t: Type) extends View(t)

/**
 * A view for splitting another view.
 *
 * @param n The size of chunks the input view should be split in.
 * @param iv View to split.
 * @param t Type of the view.
 */
private[view] case class ViewSplit(n: ArithExpr, iv: View, override val t: Type) extends View(t)

/**
 * A view for joining another view.
 *
 * @param n The chunk size of the dimension that should be joined.
 * @param iv The view to join.
 * @param t Type of the view.
 */
private[view] case class ViewJoin(n: ArithExpr, iv: View, override val t: Type) extends View(t)

/**
 * A view for zipping a number of views.
 *
 * @param iv View to zip.
 * @param t Type of the view.
 */
private[view] case class ViewZip(iv: View, override val t: Type) extends View(t)

/**
 * A view for unzipping another view
 *
 * @param iv View to unzip
 * @param t Type of the view
 */
private[view] case class ViewUnzip(iv: View, override val t: Type) extends View(t)

/**
 * A view for reordering.
 *
 * @param f Function to use for reordering.
 * @param iv View to reorder.
 * @param t Type of the view.
 */
private[view] case class ViewReorder(f: ArithExpr => ArithExpr, iv: View, override val t: Type) extends View(t)

/**
 * A view for vectorisation.
 *
 * @param n Vector width.
 * @param iv View to vectorise.
 * @param t Type of the view.
 */
private[view] case class ViewAsVector(n: ArithExpr, iv: View, override val t: Type) extends View(t)

/**
 * A view for undoing vectorisation
 *
 * @param iv View to scalarise
 * @param n Vector width
 * @param t Type of the view
 */
private[view] case class ViewAsScalar(iv: View, n: ArithExpr, override val t: Type) extends View(t)

/**
 * A view for filtering.
 *
 * @param iv View to filter.
 * @param ids A view providing the indices.
 * @param t Type of the View
 */
private[view] case class ViewFilter(iv: View, ids: View, override val t: Type) extends View(t)

/**
 * A view for exiting a dimension
 *
 * @param iv View to add a dimension to.
 * @param itVar Iteration variable used for the current dimension
 * @param t Type of the view
 */
private[view] case class ViewMap(iv: View, itVar: ArithExpr, override val t: Type) extends View(t)

/**
 * A view for accessing a tuple component.
 *
 * @param i The component to access.
 * @param iv The view to access.
 * @param t Type of the view.
 */
case class ViewTupleComponent(i: Int, iv: View, override val t: Type) extends View(t)

/**
 * A view for constructing a tuple.
 *
 * @param ivs The views to construct a tuple of.
 * @param t Type of the view.
 */
private[view] case class ViewTuple(ivs: Seq[View], override val t: Type) extends View(t)

/**
 *  A view for sliding.
 *
 * @param iv View to Slide.
 * @param slide The slide function to use.
 * @param t Type of the view.
 */
private[view] case class ViewSlide(iv: View, slide: Slide, override val t: Type) extends View(t)

/**
 * Get the head of a view.
 *
 * @param iv The view to get the head of.
 * @param t Type of view
 */
private[view] case class ViewHead(iv: View, override val t: Type) extends View(t)

/**
 * A view for getting the tail of a view.
 *
 * @param iv The view to get the tail of.
 * @param t The type of view.
 */
private[view] case class ViewTail(iv: View, override val t: Type) extends View(t)

/**
 * A view for padding an array.
 *
 * @param iv The view to pad.
 * @param left The number of elements to add on the left
 * @param right The number of elements to add on the right
 * @param fct The boundary handling function.
 * @param t The type of view.
 */
private[view] case class ViewPad(iv: View, left: Int, right: Int, fct: Pad.BoundaryFun,
                   override val t: Type) extends View(t)


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
  def apply(t: Type, name: String): View = ViewMem(name, t)

  private[view] def tuple(ivs: View*) = ViewTuple(ivs, TupleType(ivs.map(_.t): _*))

  /**
   * Visit the expression and construct all views for all sub-expressions.
   *
   * @param lambda The starting expression.
   */
  def apply(lambda: Lambda): Unit = {
   lambda.params.foreach((p) => {
      p.view = View(p.t, OpenCLPrinter.toString(p.mem.variable))
    })
    View(lambda.body)
  }

  /**
   * Visit the expression and construct all views for all sub-expressions.
   *
   * @param expr The starting expression.
   */
  def apply(expr: Expr): Unit = {
    BuildDepthInfo(expr)
    InputView(expr)
    OutputView(expr)
  }

  private[view] def getFullType(outputType: Type, outputAccessInf: List[(ArithExpr, ArithExpr)]): Type = {
    outputAccessInf.foldLeft(outputType)((t, len) => ArrayType(t, len._1))
  }

  private[view] def initialiseNewView(t: Type, outputAccessInf: List[(ArithExpr, ArithExpr)], name: String = ""): View = {
    // Use the lengths and iteration vars to mimic inputs
    val outArray = getFullType(t, outputAccessInf)
    val outView = View(outArray, name)
    outputAccessInf.foldRight(outView)((idx, view) => view.access(idx._2))
  }

}

// OpenCLAST.VarRef(v, arrayIndex = index)
class ViewPrinter(val replacements: immutable.Map[ArithExpr, ArithExpr]) {
    @scala.annotation.tailrec
    private def emitView(v: Var,
                         sv: View,
                         arrayAccessStack: List[(ArithExpr, ArithExpr)], // id, dimension size
                         tupleAccessStack: List[Int]): Expression = {
    sv match {
      case _: ViewMem =>
        assert(tupleAccessStack.isEmpty)
        val index = arrayAccessStack.map(x => x._2 match {
          case Var("unknown_length", _) =>
            AccessVar(v.toString, ArithExpression(x._1))
          case _ => x._1 * x._2
        }).foldLeft(Cst(0): ArithExpr)((x, y) => x + y)
        VarRef(v, arrayIndex = ArithExpression(index))

      case access: ViewAccess =>
        val length: ArithExpr = ViewPrinter.getLengthForArrayAccess(sv.t, tupleAccessStack)
        val newAAS = (access.i, length) :: arrayAccessStack
        emitView(v, access.iv, newAAS, tupleAccessStack)

      case map: ViewMap =>
        val idx = arrayAccessStack.head._1
        val newAAS = arrayAccessStack.tail
        val newV = map.iv.replaced(map.itVar, idx)
        emitView(v, newV, newAAS, tupleAccessStack)

      case split: ViewSplit =>
        val chunkId = arrayAccessStack.head
        val stack1 = arrayAccessStack.tail
        val chunkElemId = stack1.head
        val stack2 = stack1.tail
        val newIdx = chunkId._1 * split.n + chunkElemId._1
        val newAAS = (newIdx, chunkElemId._2) :: stack2
        emitView(v, split.iv, newAAS, tupleAccessStack)

      case join: ViewJoin =>
        val idx = arrayAccessStack.head
        val stack = arrayAccessStack.tail
        val chunkSize: ArithExpr = join.n
        val chunkId = idx._1 / chunkSize
        val chunkElemId = idx._1 % chunkSize
        val newAS = stack.::((chunkElemId, Type.getLengths(sv.t.asInstanceOf[ArrayType].elemT).reduce(_ * _))).
          ::((chunkId, Type.getLengths(join.t.asInstanceOf[ArrayType].elemT).reduce(_ * _) * join.n))
        emitView(v, join.iv, newAS, tupleAccessStack)

      case gather: ViewReorder =>
        val idx = arrayAccessStack.head
        val stack = arrayAccessStack.tail
        val newIdx = gather.f(idx._1)
        val newAS = (newIdx, idx._2) :: stack
        emitView(v, gather.iv, newAS, tupleAccessStack)

      case filter: ViewFilter =>
        val (idx, len) = arrayAccessStack.head
        val stack = arrayAccessStack.tail

        val newIdx = ViewPrinter.emit(v, filter.ids.access(idx), replacements)
        val indirection = newIdx match {
          case VarRef(_, _, index) =>
            new AccessVar(ViewPrinter.getViewMem(filter.ids).name, index)
          case x => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
        }

        emitView(v, filter.iv, (indirection, len) :: stack, tupleAccessStack)

      case component: ViewTupleComponent =>
        val newTAS = component.i :: tupleAccessStack
        emitView(v, component.iv, arrayAccessStack, newTAS)

      case zip: ViewZip =>
        emitView(v, zip.iv, arrayAccessStack, tupleAccessStack)

      case unzip: ViewUnzip =>
        emitView(v, unzip.iv, arrayAccessStack, tupleAccessStack)

      case tuple: ViewTuple =>
        val i = tupleAccessStack.head
        val newTAS = tupleAccessStack.tail
        emitView(v, tuple.ivs(i), arrayAccessStack, newTAS)

      case asVector: ViewAsVector =>
        val top = arrayAccessStack.head
        val newAAS = ((top._1 * asVector.n, top._2) :: arrayAccessStack.tail).map(x => (x._1, x._2 /^ asVector.n))
        emitView(v, asVector.iv, newAAS, tupleAccessStack)

      case asScalar: ViewAsScalar =>
        val top = arrayAccessStack.head
        val newAAS = ((top._1 /^ asScalar.n, top._2) :: arrayAccessStack.tail).map(x => (x._1, x._2 * asScalar.n))
        emitView(v, asScalar.iv, newAAS, tupleAccessStack)

      case head: ViewHead =>
        val newAAS = arrayAccessStack.tail
        emitView(v, head.iv, newAAS, tupleAccessStack)

      case tail: ViewTail =>
        val idx = arrayAccessStack.head
        val stack = arrayAccessStack.tail
        val newIdx = idx._1 + 1
        val newLen = idx._2
        val newAAS = (newIdx, newLen) :: stack
        emitView(v, tail.iv, newAAS, tupleAccessStack)

      case ag: ViewSlide =>
        val outerId = arrayAccessStack.head
        val stack1 = arrayAccessStack.tail
        val innerId = stack1.head
        val stack2 = stack1.tail

        ag.t match {
          case ArrayType(_, _) =>
            val newIdx = outerId._1 * ag.slide.step + innerId._1
            val newAAS = (newIdx, innerId._2) :: stack2
            emitView(v, ag.iv, newAAS, tupleAccessStack)
          case _ => throw new IllegalArgumentException()
        }

      case pad: ViewPad =>
        val idx = arrayAccessStack.head
        val stack = arrayAccessStack.tail
        val currentIdx = idx._1 - pad.left
        val length = pad.iv.t.asInstanceOf[ArrayType].len
        val newIdx = if(ArithExpr.mightBeNegative(currentIdx) || ArithExpr.isSmaller(length -1, currentIdx.max).getOrElse(true))
          pad.fct(currentIdx, length)
        else
          currentIdx

        val newLen = idx._2
        val newAAS = (newIdx, newLen) :: stack
        emitView(v, pad.iv, newAAS, tupleAccessStack)

      case ViewConstant(value, _) =>
        OpenCLAST.OpenCLExpression(value.value)

      case ViewGenerator(f, at) =>
        val index = arrayAccessStack.map(x => x._1 * x._2).foldLeft(Cst(0):ArithExpr)((x, y) => x + y)
        val i = ArithExpr.substitute(index, replacements)
        val l = ArithExpr.substitute(at.len, replacements)
        f(i, l)

      case ViewGeneratorUserFun(f, at) =>
        val index = arrayAccessStack.map(x => x._1 * x._2).foldLeft(Cst(0):ArithExpr)((x, y) => x + y)
        val i = ArithExpr.substitute(index, replacements)
        val l = ArithExpr.substitute(at.len, replacements)
        OpenCLAST.FunctionCall(f.name,
          List(OpenCLAST.ArithExpression(i), OpenCLAST.ArithExpression(l)))

      case View2DGeneratorUserFun(f, at) =>
        (arrayAccessStack, at) match {
          case ( List(first, second, rest @ _*),
          ArrayType(ArrayType(_, n_), m_) ) =>
            val i = ArithExpr.substitute(first._1, replacements)
            val m = ArithExpr.substitute(m_, replacements)
            val j = ArithExpr.substitute(second._1, replacements)
            val n = ArithExpr.substitute(n_, replacements)

            OpenCLAST.FunctionCall(f.name,
              List(OpenCLAST.ArithExpression(i), OpenCLAST.ArithExpression(j),
                OpenCLAST.ArithExpression(m), OpenCLAST.ArithExpression(n)))
        }

      case View3DGeneratorUserFun(f, at) =>
        (arrayAccessStack, at) match {
          case ( List(first, second, third, rest @ _*),
                 ArrayType(ArrayType(ArrayType(_, o_), n_), m_) ) =>
            val i = ArithExpr.substitute(first._1, replacements)
            val m = ArithExpr.substitute(m_, replacements)
            val j = ArithExpr.substitute(second._1, replacements)
            val n = ArithExpr.substitute(n_, replacements)
            val k = ArithExpr.substitute(third._1, replacements)
            val o = ArithExpr.substitute(o_, replacements)

            OpenCLAST.FunctionCall(f.name,
              List(OpenCLAST.ArithExpression(i), OpenCLAST.ArithExpression(j), OpenCLAST.ArithExpression(k),
                   OpenCLAST.ArithExpression(m), OpenCLAST.ArithExpression(n), OpenCLAST.ArithExpression(o)))
        }


      case op => throw new NotImplementedError(op.getClass.toString)
    }
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
  def emit(v: Var,
           view: View,
           replacements: immutable.Map[ArithExpr, ArithExpr] = immutable.Map()
          ): Expression = {
    val vp = new ViewPrinter(replacements)
    assert(!view.t.isInstanceOf[ArrayType])
    vp.emitView(v, view.replaced(replacements), List(), List())
  }


  @scala.annotation.tailrec
  private def getViewMem(sv: View, tupleAccessStack: List[Int] = List()): ViewMem = {
    sv match {
      case map: ViewMem => map
      case access: ViewAccess => getViewMem(access.iv, tupleAccessStack)
      case map: ViewMap => getViewMem(map.iv, tupleAccessStack)
      case split: ViewSplit => getViewMem(split.iv, tupleAccessStack)
      case join: ViewJoin => getViewMem(join.iv, tupleAccessStack)
      case gather: ViewReorder => getViewMem(gather.iv, tupleAccessStack)
      case filter: ViewFilter => getViewMem(filter.iv, tupleAccessStack)
      case asVector: ViewAsVector => getViewMem(asVector.iv, tupleAccessStack)
      case asScalar: ViewAsScalar => getViewMem(asScalar.iv, tupleAccessStack)

      case component: ViewTupleComponent =>
        val newTAS = tupleAccessStack.::(component.i)
        getViewMem(component.iv, newTAS)

      case zip: ViewZip =>
        getViewMem(zip.iv, tupleAccessStack)

      case tuple: ViewTuple =>
        val i = tupleAccessStack.head
        val newTAS = tupleAccessStack.tail
        getViewMem(tuple.ivs(i), newTAS)

      case op => throw new NotImplementedError(op.getClass.toString)
    }
  }

  private def getLengthForArrayAccess(t: Type, tupleAccesses: List[Int]): ArithExpr = {

    if (tupleAccesses.isEmpty) {
      Type.getLengths(t).reduce(_ * _)
    } else {
      t match {
        case tt: TupleType =>
          getLengthForArrayAccess(Type.getTypeAtIndex(tt, tupleAccesses.head), tupleAccesses.tail)
        case ArrayType(elemT, n) => getLengthForArrayAccess(elemT, tupleAccesses) * n
        case _ =>
          throw new IllegalArgumentException("PANIC: cannot compute array access for type " + t)
      }
    }
  }
}


