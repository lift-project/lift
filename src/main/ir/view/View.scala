package ir.view

import backends.c.common.common_ir.Slice
import core.generator.GenericAST
import core.generator.GenericAST._
import ir.Type.size_t
import ir.ast._
import ir._
import lift.arithmetic._
//import opencl.generator.OpenCLAST
//import opencl.generator.OpenCLAST.{ArithExpression, Expression, VarRef}
import opencl.ir.{AddressSpaceCollection, Int, OpenCLAddressSpace, PrivateMemory, UndefAddressSpace}

import scala.collection.immutable

private class IllegalAccess(err: String)
      extends IllegalArgumentException() {
  def this(ty: Type) = this(s"Cannot compute access for type $ty")
  def this(ty: Type, as: OpenCLAddressSpace) = this(s"Cannot compute access for type $ty in $as")
}

private class IllegalView(err: String)
      extends IllegalArgumentException(err) {
  def this(v: View) = this(s"View $v is ill-formed")
}

/**
 * An arithmetic expression that performs an access to `array[idx]`
 *
 * @param array variable referencing the array
 * @param idx index to access in the array
 */
case class AccessVar(array: Var, idx: ArithExpr, r: Range = RangeUnknown, override val fixedId: Option[Long] = None)
  extends ExtensibleVar("", r, fixedId) {

  override def copy(r: Range): AccessVar = AccessVar(array.copy(array.range), idx, r, Some(id))

  override def cloneSimplified() = new AccessVar(array, idx, r, Some(id)) with SimplifiedExpr

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr =
    f(AccessVar(
      array.visitAndRebuild(f).asInstanceOf[Var],
      idx.visitAndRebuild(f),
      range.visitAndRebuild(f),
      Some(id)
    ))
}

/**
 * Variable storing a casted pointer.
 * `CastedPointer(v, type, offset)` generates the following C code: `((type*)(v + offset))`
 */
case class CastedPointer(ptr: Var, ty: ScalarType, offset: ArithExpr, addressSpace: OpenCLAddressSpace,
                         override val fixedId: Option[Long] = None)
  extends ExtensibleVar("", RangeUnknown, fixedId) {

  override def copy(r: Range): CastedPointer = {
    CastedPointer(ptr.copy(ptr.range), ty, offset, addressSpace)
  }

  override def cloneSimplified() = new CastedPointer(ptr, ty, offset, addressSpace, Some(id)) with SimplifiedExpr

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr =
    f(CastedPointer(
      ptr.visitAndRebuild(f).asInstanceOf[Var],
      ty,
      offset.visitAndRebuild(f),
      addressSpace
    ))

  override lazy val toString: String =
    s"#error THE VARIABLE $ptr THIS SHOULD NEVER BE PRINTED. USE Printer" +
      s".ToString(...) INSTEAD!\n" +
      s"($addressSpace ${ty
      .name}*)($ptr" +
      s" + " +
    s"$offset)"
}

/**
 * Hack: a special variable to fetch the size of an array. Should not appear
 * in the generated code.
 *
 * array[SizeIndex()]  ==  array.size
 */
case class SizeIndex(override val fixedId: Option[Long] = None)
  extends ExtensibleVar("SIZE", RangeUnknown, fixedId) {
  override def copy(r: Range) = SizeIndex()

  override def cloneSimplified() = new SizeIndex(Some(id)) with SimplifiedExpr

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr = f(SizeIndex())
}

/**
 * Views are lazy constructs for determining the locations for memory accesses.
 * They are lazy in the sense that if an array should be reordered, they remember the
 * reordering, but do not actually reorder an array to avoid unnecessary copies.
 *
 * @param t Type of the view.
 */
abstract sealed class View(val t: Type = UndefType) {

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
      case ViewMem(memVar, ty) =>
        if (subst.isDefinedAt(memVar))
          ViewMem(subst(memVar).asInstanceOf[Var], ty)
        else this
      case ViewMap(iv, itVar, ty) => ViewMap(iv.replaced(subst), itVar, ty)
      case ViewAccess(i, iv, ty) => ViewAccess(ArithExpr.substitute(i, subst), iv.replaced(subst), ty)
      case ViewArrayWrapper(iv, ty) => ViewArrayWrapper(iv.replaced(subst), ty)
      case ViewZip(iv, ty) => ViewZip(iv.replaced(subst), ty)
      case ViewConcat(iv, ty) => ViewConcat(iv.replaced(subst), ty)
      case ViewUnzip(iv, ty) => ViewUnzip(iv.replaced(subst), ty)
      case ViewSplit(n, iv, ty) => ViewSplit(ArithExpr.substitute(n, subst), iv.replaced(subst), ty)
      case ViewJoin(n, iv, ty) => ViewJoin(ArithExpr.substitute(n, subst), iv.replaced(subst), ty)
      case ViewTranspose(iv, ty) => ViewTranspose(iv.replaced(subst), ty)
      case ViewReorder(f, iv, ty) => ViewReorder(f, iv.replaced(subst), ty)
      case ViewAsVector(n, iv, ty) => ViewAsVector(n, iv.replaced(subst), ty)
      case ViewAsScalar(iv, n, ty) => ViewAsScalar(iv.replaced(subst), n, ty)
      case ViewFilter(iv, ids, ty) => ViewFilter(iv.replaced(subst), ids.replaced(subst), ty)
      case ViewTuple(ivs, ty) => ViewTuple(ivs.map(_.replaced(subst)), ty)
      case ViewTupleComponent(i, ivs, ty) => ViewTupleComponent(i, ivs.replaced(subst), ty)
      case ViewSlide(iv, slide, ty) => ViewSlide(iv.replaced(subst), slide, ty)
      case ViewPad(iv, left, right, padFun, ty) => ViewPad(iv.replaced(subst), left, right, padFun, ty)
      case ViewPadConstant(iv, left, right, constant, ty) => ViewPadConstant(iv.replaced(subst), left, right, constant, ty)
      case ViewSize(iv) => ViewSize(iv.replaced(subst))
      case ViewHead(iv, ty) => ViewHead(iv.replaced(subst), ty)
      case ViewTail(iv, ty) => ViewTail(iv.replaced(subst), ty)
      case ViewOffset(offset, iv, ty) => ViewOffset(offset, iv.replaced(subst), ty)
      case _: View2DGeneratorUserFun | _: View3DGeneratorUserFun | _: ViewGenerator | _: ViewGeneratorUserFun |
           _: ViewConstant | NoView =>
        this
      case _ => throw new NotImplementedError()
    }
  }

  def offset(offset: ArithExpr) : View = {
    t match {
      case ArrayTypeWSWC(et,s,c) => ViewOffset(offset, this, ArrayTypeWSWC(et,s-offset,c-offset))
      case _ => throw new IllegalArgumentException("PANIC: access expects an array type, found "+t)
    }
  }


  /**
   * Construct a new view for accessing the current view at position `idx`.
   *
   * @param idx Access position
   * @return The view for position `idx`
   */
  def access(idx: ArithExpr): View = {
    t match {
      case ArrayType(et) => ViewAccess(idx, this, et)
      case _ => throw new IllegalArgumentException("PANIC: access expects an array type, found "+t)
    }
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
      case ArrayTypeWS(elemT, n) => ViewSplit(chunkSize, this,
        ArrayTypeWSWC(ArrayTypeWSWC(elemT, chunkSize), n /^ chunkSize))
      case _ => throw new IllegalArgumentException("PANIC: split expects an array type")
    }
  }

  def transpose(t:Type) : View = {

    this.t match {
      case ArrayTypeWS(ArrayTypeWS(_,_),_) => ViewTranspose(this, t)
      case _ => throw new IllegalArgumentException("PANIC: transpose expects an 2D array type")
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
      case ArrayTypeWS(ArrayTypeWS(elemT, n), m) =>
        ViewJoin(chunkSize, this, ArrayTypeWSWC(elemT, n * m))
      case _ => throw new IllegalArgumentException("PANIC: join expects a 2D array type with size, found "+this.t.toString)
    }
  }

  def transpose(): View = {
    this.t match {
      case ArrayTypeWS(ArrayTypeWS(elemT,n),m) =>
        ViewTranspose(this,ArrayTypeWSWC(ArrayTypeWSWC(elemT,m),n))
      case _ => throw new IllegalArgumentException("PANIC: transpose expects a 2D array type with size, found "+this.t.toString)
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
      case ArrayTypeWSWC(st: ScalarType, s, c) =>
        ViewAsVector(n, this, ArrayTypeWSWC(st.vectorize(n), s /^ n, c /^ n))
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
      case ArrayTypeWSWC(VectorType(st, n), s, c) =>
        ViewAsScalar(this, n, ArrayTypeWSWC(st, s*n, c*n))
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
      ArrayTypeWS(this.t.asInstanceOf[ArrayType].elemT, ids.t.asInstanceOf[ArrayType with Size].size))
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
      case tt: TupleType =>
        val newT = Zip.computeOutType(tt)
        ViewZip(this, newT)
      case other => throw new IllegalArgumentException("Can't zip " + other)
    }

  }

  def concat(): View = {
    t match {
      case tt: TupleType =>
        val newT = Concat.computeOutType(tt)
        ViewConcat(this, newT)
      case other => throw new IllegalArgumentException("Can't concat " + other)
    }

  }

  /**
   * Construct a view for unzipping the current view. The current view has to be an
   * array of tuples.
   */
  def unzip(): View = {
    t match {
      case at@ArrayType(TupleType(ts@_*)) =>
        ViewUnzip(this, TupleType(ts.map(at.replacedElemT): _*))
      case other => throw new IllegalArgumentException("Can't unzip " + other)
    }
  }

  def slide(s: Slide): View = {
    this.t match {
      case ArrayType(_) =>
        ViewSlide(this, s, s.checkType(this.t, setType=false))
      case other => throw new IllegalArgumentException("Can't slide" + other)
    }
  }

  def slice(s: Slice): View = {
    this.t match {
      case ArrayType(_) =>
        ViewSlice(this, s, s.checkType(this.t, setType=false))
      case other => throw new IllegalArgumentException("Can't slide" + other)
    }
  }

  def pad(left: Int, right: Int, boundary: Pad.BoundaryFun): View = {
    this.t match {
      case ArrayTypeWS(elemT, len) =>
        ViewPad(this, left, right, boundary, ArrayTypeWSWC(elemT, len + left + right))
      case other => throw new IllegalArgumentException("Can't pad " + other)
    }
  }

  def padConstant(left: Int, right: Int, constant: Value): View = {
    this.t match {
      case ArrayTypeWS(elemT, len) =>
        ViewPadConstant(this, left, right, constant, ArrayTypeWSWC(elemT, len + left + right))
      case other => throw new IllegalArgumentException("Can't pad constant " + other)
    }
  }

  /**
   * Construct a view for getting the size of an array assuming that it is not
   * statically known
   */
  def size(): View = {
    this match {
      case z: ViewZip => z.iv match {
        case ViewTuple(ivs, tt) =>
          ViewTuple(
            ivs.map(_.size()),
            TupleType(Seq.fill(tt.asInstanceOf[TupleType].elemsT.length)(Int): _*)
          )
        case _ => throw new IllegalView(z)
      }
      case _ => this.t match {
        case at: ArrayType => at match {
          // Sanity check: if the size is statically known, we must not reach
          // this point
          case _: Size =>
            throw new IllegalArgumentException(
              "Trying to generate an access to the size of an array which " +
              "size is known statically."
            )
          case _ => ViewSize(this)
        }
        case ty => throw new IllegalAccess(ty)
      }
    }
  }
}

// The view "generator" MUST have all sizes and capacity in their type
case class ViewGeneratorUserFun(f: UserFun, override val t: ArrayType with Size with Capacity) extends View(t)

case class View2DGeneratorUserFun(f: UserFun, override val t: ArrayType with Size with Capacity) extends View(t)

case class View3DGeneratorUserFun(f: UserFun, override val t: ArrayType with Size with Capacity) extends View(t)

case class ViewGenerator(f: (ArithExpr, ArithExpr) => ExpressionT, override
val t: ArrayType with Size with Capacity) extends View(t)

case class ViewConstant(value: Value, override val t: Type) extends View(t)

/**
 * A view to memory object.
 *
 * @param v the variable representing the memory object/array.
 * @param t Type of the view.
 */
case class ViewMem(v: Var, override val t: Type) extends View(t)

case class ViewMemScalar(i: ArithExpr, override val t: Type) extends View(t)

/**
  * A variant of ViewMem that contain a inner view,
  * in case the type allow inner structure, like tuples
  * */
case class ViewMemWithInnerView(v: Var, iv: View, override val t: Type) extends View(t)

case class ViewOffset(offset : ArithExpr, iv : View, override val t : Type) extends View(t)

/**
 * A view for accessing another view at position `i`.
 *
 * @param i Index to access.
 * @param iv View to access.
 * @param t Type of the view.
 */
case class ViewAccess(i: ArithExpr, iv: View, override val t: Type) extends View(t)
/**
  * wrapping another view in an array of size 1
  *
  * @param iv View to access.
  * @param t Type of the view.
  */
case class ViewArrayWrapper(iv: View, override val t: Type) extends View(t)

/**
 * A view for splitting another view.
 *
 * @param n The size of chunks the input view should be split in.
 * @param iv View to split.
 * @param t Type of the view.
 */
case class ViewSplit(n: ArithExpr, iv: View, override val t: Type) extends View(t)

case class ViewTranspose(iv: View, override val t: Type) extends View(t)

/**
 * A view for joining another view.
 *
 * @param n The chunk size of the dimension that should be joined.
 * @param iv The view to join.
 * @param t Type of the view.
 */
case class ViewJoin(n: ArithExpr, iv: View, override val t: Type) extends View(t)

/**
 * A view for zipping a number of views.
 *
 * @param iv View to zip.
 * @param t Type of the view.
 */
case class ViewZip(iv: View, override val t: Type) extends View(t)

case class ViewConcat(iv: View, override val t: Type) extends View(t)

/**
 * A view for unzipping another view
 *
 * @param iv View to unzip
 * @param t Type of the view
 */
case class ViewUnzip(iv: View, override val t: Type) extends View(t)

/**
 * A view for reordering.
 *
 * @param f Function to use for reordering.
 * @param iv View to reorder.
 * @param t Type of the view.
 */
case class ViewReorder(f: ArithExpr => ArithExpr, iv: View, override val t: Type) extends View(t)

/**
 * A view for vectorisation.
 *
 * @param n Vector width.
 * @param iv View to vectorise.
 * @param t Type of the view.
 */
case class ViewAsVector(n: ArithExpr, iv: View, override val t: Type) extends View(t)

/**
 * A view for undoing vectorisation
 *
 * @param iv View to scalarise
 * @param n Vector width
 * @param t Type of the view
 */
case class ViewAsScalar(iv: View, n: ArithExpr, override val t: Type) extends View(t)

/**
 * A view for filtering.
 *
 * @param iv View to filter.
 * @param ids A view providing the indices.
 * @param t Type of the View
 */
case class ViewFilter(iv: View, ids: View, override val t: Type) extends View(t)

/**
 * A view for exiting a dimension
 *
 * @param iv View to add a dimension to.
 * @param itVar Iteration variable used for the current dimension
 * @param t Type of the view
 */
case class ViewMap(iv: View, itVar: ArithExpr, override val t: Type) extends View(t)

case class ViewMapSeq(iv: View, itVar: ArithExpr, override val t: Type) extends View(t)

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
case class ViewTuple(ivs: Seq[View], override val t: Type) extends View(t)

/**
 *  A view for sliding.
 *
 * @param iv View to Slide.
 * @param slide The slide function to use.
 * @param t Type of the view.
 */
case class ViewSlide(iv: View, slide: Slide, override val t: Type) extends View(t)

/**
  *  A view for slicing.
  *
  * @param iv View to Slice.
  * @param slice The slice function to use.
  * @param t Type of the view.
  */
case class ViewSlice(iv: View, slice: Slice, override val t: Type) extends View(t)

/**
 * Get the head of a view.
 *
 * @param iv The view to get the head of.
 * @param t Type of view
 */
case class ViewHead(iv: View, override val t: Type) extends View(t)

/**
 * A view for getting the tail of a view.
 *
 * @param iv The view to get the tail of.
 * @param t The type of view.
 */
case class ViewTail(iv: View, override val t: Type) extends View(t)

/**
 * A view for padding an array.
 *
 * @param iv The view to pad.
 * @param left The number of elements to add on the left
 * @param right The number of elements to add on the right
 * @param fct The boundary handling function.
 * @param t The type of view.
 */
case class ViewPad(iv: View, left: Int, right: Int, fct: Pad.BoundaryFun,
                   override val t: Type) extends View(t)

/**
  * A view for padding an array.
  *
  * @param iv The view to pad.
  * @param left The number of elements to add on the left
  * @param right The number of elements to add on the right
  * @param constant The constant value
  * @param t The type of view.
  */
case class ViewPadConstant(iv: View, left: Int, right: Int, constant: Value,
                   override val t: Type) extends View(t)

/**
 * A view for fetching the size of an array assuming that it can't be known
 * statically
 *
 * @param iv the view of the array
 */
case class ViewSize(iv: View) extends View(opencl.ir.Int)


case class ViewNull() extends View(opencl.ir.Int)


/**
 * Placeholder for a view that is not yet created.
 */
object NoView extends View()

object UnusedInExprOutputView extends View()

object View {

  @scala.annotation.tailrec
  def getSubViews(sv: View, tupleAccessStack: List[Int] = List(), allViews: Seq[View] = Seq()): Seq[View] = {
    val newAllViews = allViews :+ sv
    sv match {
      case ViewTuple(ivs, _) =>
        val i :: newTAS = tupleAccessStack
        getSubViews(ivs(i), newTAS, newAllViews)

      case ViewTupleComponent(i, iv, _) =>
        val newTAS = i :: tupleAccessStack
        getSubViews(iv, newTAS, newAllViews)

      case ViewAccess(_, iv, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewMap(iv, _, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewSplit(_, iv, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewJoin(_, iv, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewTranspose(iv, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewReorder(_, iv, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewFilter(iv, _, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewZip(iv, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewUnzip(iv, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewAsVector(_, iv, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewAsScalar(iv, _, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewHead(iv, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewTail(iv, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewSlide(iv, _, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewPad(iv, _, _, _, _) => getSubViews(iv, tupleAccessStack, newAllViews)
      case ViewSize(iv) => getSubViews(iv, tupleAccessStack)

      case ViewMem(_, _) => newAllViews
      case ViewConstant(_, _) => newAllViews
      case ViewGenerator(_, _) => newAllViews
      case ViewGeneratorUserFun(_, _) => newAllViews
      case View2DGeneratorUserFun(_, _) => newAllViews
      case View3DGeneratorUserFun(_, _) => newAllViews
      case unknownView => throw new IllegalArgumentException(s"Unknown view: $unknownView")
    }
  }

  /**
   * Create new view representing an array in memory
   *
   * @param t The type of the view.
   * @param v a Var representing the array.
   * @return
   */
  def apply(t: Type, v: Var): View =
    ViewMem(v, t)

  /**
    * Create new view representing a value
    * @return
    */
  def apply(t: Type, v: Value): View = ViewConstant(v, t)


  private[view] def tuple(ivs: View*) = ViewTuple(ivs, TupleType(ivs.map(_.t): _*))

  /**
   * Visit the expression and construct all views for all sub-expressions.
   *
   * @param lambda The starting expression.
   */
  def apply(lambda: Lambda): Unit = {
   lambda.params.foreach((p) => {
      p.view = View(p.t, p.mem.variable)
    })
    View(lambda.body)
  }

  /**
   * Visit the expression and construct all views for all sub-expressions.
   *
   * @param expr The starting expression.
   */
  def apply(expr: Expr): Unit = {
    BuildDepthInfoCL(expr)
    InputView(expr)
    OutputView(expr)
  }

  private[view] def getFullType(outputType: Type, outputAccessInf: List[(Type => ArrayType, ArithExpr)]): Type = {
    outputAccessInf.foldLeft(outputType)((t, inf) => {
      val (arrayTypeConstructor, _) = inf
      arrayTypeConstructor(t)
    })
  }


  private[view] def initialiseNewView(t: Type, outputAccessInf: List[(Type => ArrayType, ArithExpr)], v: Var): View = {
    // Use the lengths and iteration vars to mimic inputs
    val outArray = getFullType(t, outputAccessInf)
    val outView = View(outArray, v)
    outputAccessInf.foldRight(outView)((inf, view) => view.access(inf._2))
  }

}

class ViewPrinter(val replacements: immutable.Map[ArithExpr, ArithExpr], val mainAddressSpace: OpenCLAddressSpace) {

  /**
    * Produces an openCL expression accessing a multidimensional array using
    * a given view
    *
    * @param sv the view
    * @param arrayAccessStack
    *        see http://www.lift-project.org/papers/steuwer17LiftIR.pdf,
    *        section 5.3
    * @param tupleAccessStack
    *        see http://www.lift-project.org/papers/steuwer17LiftIR.pdf,
    *        section 5.3
    * @return an expression accessing the array
    */
  private def emitView(sv: View,
                       arrayAccessStack: List[ArithExpr],
                       tupleAccessStack: List[Int]): ExpressionT = {
    sv match {
      case ViewMem(memVar, ty) =>
        assert(tupleAccessStack.isEmpty)
        GenerateAccess(memVar, ty, arrayAccessStack, tupleAccessStack)

      case ViewOffset(offset, iv, t) =>
        // increment read / write access by offset
        val idx :: indices  = arrayAccessStack
        emitView(iv,(idx + offset):: indices,tupleAccessStack)

      case ViewAccess(i, iv, _) =>
        emitView(iv, i :: arrayAccessStack, tupleAccessStack)

      case ViewArrayWrapper(iv, ty) =>
            val idx :: indices = arrayAccessStack
           // assert(idx == Cst(0))
            emitView(iv,indices,tupleAccessStack)


      case ViewMap(iv, itVar, _) =>
        val idx :: indices = arrayAccessStack
        val newV = iv.replaced(itVar, idx)
        emitView(newV, indices, tupleAccessStack)

      case ViewSplit(chunkSize, iv, _) =>
        val chunkIdx :: elemIdx :: indices = arrayAccessStack
        val newIdx = chunkIdx * chunkSize + elemIdx
        emitView(iv, newIdx :: indices, tupleAccessStack)

      case ViewJoin(chunkSize, iv, _) =>
        val idx :: indices = arrayAccessStack
        val chunkIdx = idx / chunkSize
        val elemIdx = idx % chunkSize
        emitView(iv, chunkIdx :: elemIdx :: indices, tupleAccessStack)

      case ViewTranspose(iv,_) =>
        val idx0 :: idx1 :: indices = arrayAccessStack
        emitView(iv,idx1 :: idx0 :: indices, tupleAccessStack)

      case ViewReorder(reindexFun, iv, _) =>
        val idx :: indices = arrayAccessStack
        emitView(iv, reindexFun(idx) :: indices, tupleAccessStack)

      case ViewFilter(iv, ids, _) =>
        val idx :: indices = arrayAccessStack
         // Assume it's the same address space
         val indirection = ViewPrinter.emit(ids.access(idx), replacements, mainAddressSpace) match {
           case VarRef(indicesVar, _, i) => AccessVar(indicesVar.v, i
             .get.asInstanceOf[ArithExpressionT].content)
           case x => throw new IllegalArgumentException(s"Expected an VarRef, got $x")
         }
         emitView(iv, indirection :: indices, tupleAccessStack)

      case ViewTupleComponent(i, iv, _) =>
        val newTAS = i :: tupleAccessStack
        emitView(iv, arrayAccessStack, newTAS)

      case ViewZip(iv, _) =>
        emitView(iv, arrayAccessStack, tupleAccessStack)

      case ViewUnzip(iv, _) =>
        emitView(iv, arrayAccessStack, tupleAccessStack)

      case ViewConcat(iv, _) =>
        val i :: idx = arrayAccessStack
        val ivs = iv match {
          case vt: ViewTuple => vt.ivs
          case _ => throw new NotImplementedError()
        }

        var offset = i
        def getSubview(): View = {
          for (sv <- ivs) {
            sv.t match {
              case ArrayTypeWSWC(_, s, _) =>
                println("s: "+s+" offset: "+offset)
                if (ArithExpr.isSmaller(offset, s).get)
                  return sv
                else offset = offset - s
            }
          }
          ???
        }

        val subView = getSubview()

        emitView(subView, offset :: idx, tupleAccessStack)

      case ViewTuple(ivs, _) =>
        val i :: newTAS = tupleAccessStack
        emitView(ivs(i), arrayAccessStack, newTAS)

      case ViewAsVector(vecSize, iv, _) =>
        val idx :: indices = arrayAccessStack
        val newIdx = idx * vecSize
        emitView(iv, newIdx :: indices, tupleAccessStack)

      case ViewAsScalar(iv, vecSize, _) =>
        val idx :: indices = arrayAccessStack
        val newIdx = idx /^ vecSize
        emitView(iv, newIdx :: indices, tupleAccessStack)

      case ViewHead(iv, _) =>
        val newAAS = arrayAccessStack.tail
        emitView(iv, newAAS, tupleAccessStack)

      case ViewTail(iv, _) =>
        val idx :: indices = arrayAccessStack
        emitView(iv, (idx + 1) :: indices, tupleAccessStack)

      case ViewSlide(iv, slide, _) =>
        val chunkIdx :: elemIdx :: indices = arrayAccessStack
        val newIdx = chunkIdx * slide.step + elemIdx
        emitView(iv, newIdx :: indices, tupleAccessStack)

      case ViewPad(iv, left, _, padFun, _) =>
        val idx :: indices = arrayAccessStack
        val currentIdx = idx - left
        val length = iv.t.asInstanceOf[ArrayType with Size].size
        val newIdx = if(ArithExpr.mightBeNegative(currentIdx) || ArithExpr.isSmaller(length -1, currentIdx.max).getOrElse(true))
          padFun(currentIdx, length)
        else
          currentIdx
        emitView(iv, newIdx :: indices, tupleAccessStack)

      case ViewPadConstant(iv, left, _, constant, _) =>
        val idx :: indices = arrayAccessStack
        val currentIdx = idx - left
        val originalSize = iv.t match {
          case ArrayTypeWS(_, size) => size
        }
        import GenericAST.BinaryExpressionT.Operator._
        TernaryExpression(
          BinaryExpression(
            BinaryExpression(ArithExpression(currentIdx), <, ArithExpression(0)),
            ||,
            BinaryExpression(ArithExpression(currentIdx), >=, ArithExpression(originalSize))
          ),
          GenericAST.RawCode(constant.value),
          emitView(iv, currentIdx :: indices, tupleAccessStack)
        )

      case ViewConstant(value, _) =>
        GenericAST.RawCode(value.value)

      case ViewSize(iv) =>
        // Sanity check: the size of an array is an integer, to does not make
        //               sense to access it at a certain index…
        assert(arrayAccessStack.isEmpty)
        val newAAS = SizeIndex() :: Nil
        emitView(iv, newAAS, tupleAccessStack)

      case ViewGenerator(f, ArrayTypeWS(_,s)) =>
        assert(arrayAccessStack.length == 1)
        val index :: Nil = arrayAccessStack
        val i = ArithExpr.substitute(index, replacements)
        val l = ArithExpr.substitute(s, replacements)
        f(i, l)

      case ViewGeneratorUserFun(f, ArrayTypeWS(_, m)) =>
        assert(arrayAccessStack.length == 1)
        val i :: Nil = arrayAccessStack
        GenericAST.FunctionCall(
          f.name,
          List(i, m)
              .map(ArithExpr.substitute(_, replacements))
              .map(ArithExpression)
        )

      case View2DGeneratorUserFun(f, ArrayTypeWS(ArrayTypeWS(_, n), m)) =>
        val i :: j :: _ = arrayAccessStack
        GenericAST.FunctionCall(
          f.name,
          List(i, j, m, n)
            .map(ArithExpr.substitute(_, replacements))
            .map(ArithExpression)
        )

      case View3DGeneratorUserFun(f, ArrayTypeWS(ArrayTypeWS(ArrayTypeWS(_, o), n), m)) =>
        val i :: j :: k :: _ = arrayAccessStack
        GenericAST.FunctionCall(
          f.name,
          List(i, j, k, m, n, o)
            .map(ArithExpr.substitute(_, replacements))
            .map(ArithExpression)
        )
    }
  }

  /**
   * The code below is used to turn the list of indices used to access a
   * multi-dimensional array into a single index used to access the raw memory
   * depending on the type of the array.
   */
  object GenerateAccess {
    def apply(mainVar: Var, mainType: Type,
              arrayAccessStack: List[ArithExpr],
              tupleAccessStack: List[Int]): VarRef = {
      val g = new GenerateAccess(mainVar, mainType, tupleAccessStack)
      g.generate(0, mainType, arrayAccessStack, tupleAccessStack)
    }
  }

  private class GenerateAccess private (mainVar: Var, mainType: Type, mainTas: List[Int]) {
    /**
     * Main function computing the index for accessing a nd-array out of a
     * list of indices.
     *
     * In this function, in `getSize` and in `getElementAt` below, `acc` refers
     * to the partial result of the computation: the offset between `mainVar`
     * and the "beginning" of the area of memory storing the next array to be
     * accessed.
     */
    @scala.annotation.tailrec
    private def generate(acc: ArithExpr, ty: Type,
                         arrayAccessStack: List[ArithExpr],
                         tupleAccessStack: List[Int]): VarRef = {
      if (arrayAccessStack.isEmpty) varRef(mainVar, acc)
      else {
        ty match {
          case at: ArrayType =>
            val idx :: indices = arrayAccessStack
            idx match {
              case _: SizeIndex => getSize(acc, at)
              case _ =>
                val newAcc = getElementAt(acc, at, idx, tupleAccessStack)
                generate(newAcc, at.elemT, indices, tupleAccessStack)
            }

          // This tuple comes from a view, project and continue
          case tt: TupleType =>
            val i :: remaining = tupleAccessStack
            generate(acc, tt.proj(i), arrayAccessStack, remaining)

          case _ => throw new IllegalAccess(ty)
        }
      }
    }

    /** Generates an access to the size */
    private def getSize(acc: ArithExpr, at: ArrayType): VarRef = {
      // Sanity check
      if (addressSpace == PrivateMemory)
        throw new IllegalView("An array in private memory must have a size and a capacity in the type")

      // Only cast the pointer if it's necessary
      if (baseType == size_t) varRef(mainVar, acc + at.sizeIndex)
      else {
        val casted = CastedPointer(mainVar, size_t, acc, addressSpace)
        varRef(casted, at.sizeIndex * alignment / size_t.size)
      }
    }

    /** Generates an access at position `idx` in the nd-array */
    private def getElementAt(acc: ArithExpr, at: ArrayType, idx: ArithExpr, tupleAccessStack: List[Int]): ArithExpr = {
      // Sanity check
      if (addressSpace == PrivateMemory && !at.isInstanceOf[ArrayType with Size with Capacity])
        throw new IllegalView("An array in private memory must have a size and a capacity in the type")

      if (at.elemT.hasFixedAllocatedSize) {
        // Just skip the header
        val len = getLengthForArrayAccess(at.elemT, tupleAccessStack)
        acc + at.headerLength * alignment / baseSize + idx * len
      } else {
        // Perform an indirection. Do not cast the pointer if it's not required.
        val elementOffset = if (baseType == size_t)
          AccessVar(mainVar, acc + at.headerLength + idx)
        else {
          val casted = CastedPointer(mainVar, size_t, acc, addressSpace)
          AccessVar(casted, at.headerLength * alignment / size_t.size + idx)
        }
        // The offset read from the headers is in bytes but must be a multiple of `baseSize`
        acc + elementOffset / baseSize
      }
    }

    // ---
    // Some helper functions
    // ---

    private lazy val (baseType, addressSpace) = getBaseTypeAndAddressSpace(mainType, mainTas, mainAddressSpace)

    // Useful shorthands
    private lazy val baseSize = Type.getAllocatedSize(baseType).eval
    private val alignment = Math.max(size_t.size.eval, baseSize)
    private def varRef(v: Var, idx: ArithExpr): VarRef = VarRef(v, arrayIndex
      = Some(ArithExpression(idx)))
    private def align(value: ArithExpr): ArithExpr = ((value + alignment - 1) / alignment) * alignment

    /**
     * Get the number of elements contained in a type once we have projected
     * the tuples that come from a view (and therefore are not backed as structs
     * in memory) on their appropriate component according to `tupleAccessStack`
     */
    private def getLengthForArrayAccess(ty: Type, tupleAccessStack: List[Int]): ArithExpr = {
      ty match {
        case ScalarType(_, _) => 1
        case VectorType(_, len) => len
        case at@ArrayTypeWC(elemT, capacity) =>
          val elemSize = getLengthForArrayAccess(elemT, tupleAccessStack)
          val contentSize = {
            if (baseSize < alignment && at.headerLength != 0) align(capacity * elemSize)
            else capacity * elemSize
          }
          at.headerLength * alignment / baseSize + contentSize
        case tt: TupleType =>
          if (tupleAccessStack.isEmpty) 1
          else getLengthForArrayAccess(
            tt.proj(tupleAccessStack.head),
            tupleAccessStack.tail
          )
        case _ => throw new IllegalArgumentException(ty.toString)
      }
    }

    /**
     * Similarly, get the base type and the address space of the memory we are
     * accessing by projecting the tuples that come from a view in the type and
     * in the address space collections.
     */
    private def getBaseTypeAndAddressSpace(ty: Type, tupleAccessStack: List[Int],
                                           addressSpace: OpenCLAddressSpace): (Type, OpenCLAddressSpace) = {
      if (tupleAccessStack.isEmpty) (Type.getBaseType(ty), addressSpace)
      else (ty, addressSpace) match {
        case (tt: TupleType, AddressSpaceCollection(coll)) =>
          val i :: tas = tupleAccessStack
          getBaseTypeAndAddressSpace(tt.proj(i), tas, coll(i))
        case (ArrayType(elemT), _) =>
          getBaseTypeAndAddressSpace(elemT, tupleAccessStack, addressSpace)
        case _ => throw new IllegalAccess(ty, addressSpace)
      }
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
  def emit(
    view: View,
    replacements: immutable.Map[ArithExpr, ArithExpr] = immutable.Map(),
    addressSpace: OpenCLAddressSpace = UndefAddressSpace
  ): ExpressionT = {
    val vp = new ViewPrinter(replacements, addressSpace)
    assert(!view.t.isInstanceOf[ArrayType])
    vp.emitView(view.replaced(replacements), List(), List())
  }
}
