package ir.view

import lift.arithmetic._
import ir._
import ir.ast._
import opencl.generator.OpenCLAST.{ArithExpression, Expression, VarRef}

import scala.collection.immutable
import opencl.generator.{OpenCLAST, OpenCLPrinter}


private class IllegalAccess(ty: Type)
      extends IllegalArgumentException(s"Cannot compute access for type $ty")

private class IllegalView(v: View)
      extends IllegalArgumentException(s"View $v is ill-formed")

/**
 * An arithmetic expression that performs an access to `array[idx]`
 *
 * @param array Array name
 * @param idx Index to access in the array
 */
case class AccessVar(array: String, idx: ArithExpression,
                     r: Range = RangeUnknown,
                     fixedId: Option[Long] = None) extends ExtensibleVar("", r, fixedId) {
  override def copy(r: Range) = AccessVar(array, idx, r, Some(id))

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr =
    f(AccessVar(array, ArithExpression(idx.content.visitAndRebuild(f)), range.visitAndRebuild(f), Some(id)))
}

/**
 * Hack: a special variable to fetch the size of an array. Should not appear
 * in the generated code.
 *
 * array[SizeIndex()]  ==  array.size
 */
case class SizeIndex() extends ExtensibleVar("SIZE", RangeUnknown, None) {
  override def copy(r: Range) = SizeIndex()
  
  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr = this
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
      case _: ViewMem | _: ViewHead | NoView | _: View2DGeneratorUserFun |
           _: View3DGeneratorUserFun | _: ViewConstant | _: ViewGenerator |
           _: ViewGeneratorUserFun | _: ViewTail | _: ViewSize => this
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
      case ArrayTypeWS(elemT, n) => ViewSplit(chunkSize, this,
        ArrayTypeWSWC(ArrayTypeWSWC(elemT, chunkSize), n /^ chunkSize))
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
      case ArrayTypeWS(ArrayTypeWS(elemT, n), m) =>
        ViewJoin(chunkSize, this, ArrayTypeWSWC(elemT, n * m))
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
      case TupleType(ts@_*) if ts.forall(_.isInstanceOf[ArrayType with Size with Capacity]) =>
        val arrayTs: Seq[ArrayType with Size with Capacity] = ts.map(_.asInstanceOf[ArrayType with Size  with Capacity])
        val newT = ArrayTypeWSWC(TupleType(arrayTs.map(_.elemT):_*), arrayTs.head.size)
        assert (arrayTs.head.size == arrayTs.head.capacity)
        // TODO: handle the case where the capacity is different from size
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
      case ArrayTypeWS(TupleType(ts@_*), len) =>
        ViewUnzip(this, TupleType(ts.map(ArrayTypeWSWC(_, len)): _*))
      case other => throw new IllegalArgumentException("Can't unzip " + other)
    }
  }

  def slide(s: Slide): View = {
    this.t match {
      case ArrayType(_) =>
        ViewSlide(this, s, s.checkType(this.t, setType=false))
      case other => throw new IllegalArgumentException("Can't group " + other)
    }
  }

  def pad(left: Int, right: Int, boundary: Pad.BoundaryFun): View = {
    this.t match {
      case ArrayTypeWS(elemT, len) =>
        ViewPad(this, left, right, boundary, ArrayTypeWSWC(elemT, len + left + right))
      case other => throw new IllegalArgumentException("Can't pad " + other)
    }
  }
  
  /**
   * Construct a view for getting the size of an array assuming that it is not
   * statically known
   */
  def size(): View = {
    this match {
      case z: ViewZip => z.iv match {
        case t: ViewTuple => t.ivs.head.size()
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
 * A view for fetching the size of an array assuming that it can't be known
 * statically
 *
 * @param iv the view of the array
 */
private[view] case class ViewSize(iv: View) extends View(opencl.ir.Int)


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

  private[view] def getFullType(outputType: Type, outputAccessInf: List[(Type => ArrayType, ArithExpr)]): Type = {
    outputAccessInf.foldLeft(outputType)((t, inf) => {
      val (arrayTypeConstructor, _) = inf
      arrayTypeConstructor(t)
    })
  }

  private[view] def initialiseNewView(t: Type, outputAccessInf: List[(Type => ArrayType, ArithExpr)], name: String = ""): View = {
    // Use the lengths and iteration vars to mimic inputs
    val outArray = getFullType(t, outputAccessInf)
    val outView = View(outArray, name)
    outputAccessInf.foldRight(outView)((inf, view) => view.access(inf._2))
  }

}

class ViewPrinter(val replacements: immutable.Map[ArithExpr, ArithExpr]) {
  /**
    * Produces an openCL expression accessing a multi-dimentional array using
    * a given view
    *
    * @param v the array
    * @param sv the view
    * @param arrayAccessStack
    *        see http://www.lift-project.org/papers/steuwer17LiftIR.pdf,
    *        section 5.3
    * @param tupleAccessStack
    *        see http://www.lift-project.org/papers/steuwer17LiftIR.pdf,
    *        section 5.3
    * @return an expression accessing the array
    */
  @scala.annotation.tailrec
  private def emitView(v: Var,
                       sv: View,
                       arrayAccessStack: List[ArithExpr],
                       tupleAccessStack: List[Int]): Expression = {
    sv match {
      case _: ViewMem =>
        assert(tupleAccessStack.isEmpty)
        val index = aggregateAccesses(0, v, sv.t, arrayAccessStack, tupleAccessStack)
        VarRef(v, arrayIndex = ArithExpression(index))

      case access: ViewAccess =>
        emitView(v, access.iv, access.i :: arrayAccessStack, tupleAccessStack)

      case map: ViewMap =>
        val idx :: indices = arrayAccessStack
        val newV = map.iv.replaced(map.itVar, idx)
        emitView(v, newV, indices, tupleAccessStack)

      case split: ViewSplit =>
        val chunkIdx :: elemIdx :: indices = arrayAccessStack
        val newIdx = chunkIdx * split.n + elemIdx
        emitView(v, split.iv, newIdx :: indices, tupleAccessStack)

      case join: ViewJoin =>
        val idx :: indices = arrayAccessStack
        val chunkIdx = idx / join.n
        val elemIdx = idx % join.n
        emitView(v, join.iv, chunkIdx :: elemIdx :: indices, tupleAccessStack)

      case gather: ViewReorder =>
        val idx :: indices = arrayAccessStack
        emitView(v, gather.iv, gather.f(idx) :: indices, tupleAccessStack)

      case filter: ViewFilter =>
        val idx :: indices = arrayAccessStack
        val newIdx = ViewPrinter.emit(v, filter.ids.access(idx), replacements)
        
        val indirection = newIdx match {
          case VarRef(_, _, index) =>
            AccessVar(ViewPrinter.getViewMem(filter.ids).name, index)
          case x => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
        }

        emitView(v, filter.iv, indirection :: indices, tupleAccessStack)

      case component: ViewTupleComponent =>
        val newTAS = component.i :: tupleAccessStack
        emitView(v, component.iv, arrayAccessStack, newTAS)

      case zip: ViewZip =>
        emitView(v, zip.iv, arrayAccessStack, tupleAccessStack)

      case unzip: ViewUnzip =>
        emitView(v, unzip.iv, arrayAccessStack, tupleAccessStack)

      case tuple: ViewTuple =>
        val i :: newTAS = tupleAccessStack
        emitView(v, tuple.ivs(i), arrayAccessStack, newTAS)

      case asVector: ViewAsVector =>
        val idx :: indices = arrayAccessStack
        val newIdx = idx * asVector.n
        emitView(v, asVector.iv, newIdx :: indices, tupleAccessStack)

      case asScalar: ViewAsScalar =>
        val idx :: indices = arrayAccessStack
        val newIdx = idx /^ asScalar.n
        emitView(v, asScalar.iv, newIdx :: indices, tupleAccessStack)

      case head: ViewHead =>
        val newAAS = arrayAccessStack.tail
        emitView(v, head.iv, newAAS, tupleAccessStack)

      case tail: ViewTail =>
        val idx :: indices = arrayAccessStack
        emitView(v, tail.iv, (idx + 1) :: indices, tupleAccessStack)

      case ag: ViewSlide =>
        val chunkIdx :: elemIdx :: indices = arrayAccessStack
        val newIdx = chunkIdx * ag.slide.step + elemIdx
        emitView(v, ag.iv, newIdx :: indices, tupleAccessStack)

      case pad: ViewPad =>
        val idx :: indices = arrayAccessStack
        val currentIdx = idx - pad.left
        val length = pad.iv.t.asInstanceOf[ArrayType with Size].size
        val newIdx = if(ArithExpr.mightBeNegative(currentIdx) || ArithExpr.isSmaller(length -1, currentIdx.max).getOrElse(true))
          pad.fct(currentIdx, length)
        else
          currentIdx
        emitView(v, pad.iv, newIdx :: indices, tupleAccessStack)

      case ViewConstant(value, _) =>
        OpenCLAST.OpenCLExpression(value.value)

      case ViewSize(iv) =>
        // Sanity check: the size of an array is an integer, to does not make
        //               sense to access it at a certain index…
        assert(arrayAccessStack.isEmpty)
        val newAAS = SizeIndex() :: Nil
        emitView(v, iv, newAAS, tupleAccessStack)

      case ViewGenerator(f, ArrayTypeWS(_,s)) =>
        val index = aggregateAccesses(0, v, sv.t, arrayAccessStack, tupleAccessStack)
        val i = ArithExpr.substitute(index, replacements)
        val l = ArithExpr.substitute(s, replacements)
        f(i, l)

      case ViewGeneratorUserFun(f, ArrayTypeWS(_,s)) =>
        val index = aggregateAccesses(0, v, sv.t, arrayAccessStack, tupleAccessStack)
        val i = ArithExpr.substitute(index, replacements)
        val l = ArithExpr.substitute(s, replacements)
        OpenCLAST.FunctionCall(f.name,
          List(OpenCLAST.ArithExpression(i), OpenCLAST.ArithExpression(l)))

      case View2DGeneratorUserFun(f, at) =>
        val i :: j :: _ = arrayAccessStack
        val (m, n) = at match {
          case ArrayTypeWS(ArrayTypeWS(_, n_), m_) => (n_, m_)
        }
        OpenCLAST.FunctionCall(
          f.name,
          List(i, j, m, n)
            .map(ArithExpr.substitute(_, replacements))
            .map(ArithExpression)
        )

      case View3DGeneratorUserFun(f, at) =>
        val i :: j :: k :: _ = arrayAccessStack
        val (m, n, o) = at match {
          case ArrayTypeWS(ArrayTypeWS(ArrayTypeWS(_, o_), n_), m_) =>
            (m_, n_, o_)
        }
        OpenCLAST.FunctionCall(
          f.name,
          List(i, j, k, m, n, o)
            .map(ArithExpr.substitute(_, replacements))
            .map(ArithExpression)
        )
    }
  }
  
  /**
    * Turn the list of indices used to access an multi-dimensional array into
    * an arithmetic expression depending on the type of the array.
    *
    * @param acc the partial result of this aggregation. It corresponds to the
    *            position in memory of the very beginning of the next nested
    *            array we are going the access.
    * @param ty the type of the array
    * @param v a variable representing the array (used for indirections)
    * @param arrayAccessStack the indices used to access the array
    * @param tupleAccessStack the indices used to some tuples all along the way
    * @return the index we have to use to access the flattened memory
    *         representation of the array.
    */
  @scala.annotation.tailrec
  private def aggregateAccesses(acc: ArithExpr,
                                v: Var, ty: Type,
                                arrayAccessStack: List[ArithExpr],
                                tupleAccessStack: List[Int]): ArithExpr = {
    if (arrayAccessStack.isEmpty) acc
    else {
      ty match {
        case at: ArrayType =>
          val idx :: indices = arrayAccessStack
          
          val position = idx match {
            case SizeIndex() =>
              // Special index:
              // We are fetching the size of an array. `getSizeIndex` gives the
              // index in the header where it is stored.
              Cst(at.getSizeIndex)
            case _ =>
              // We are actually accessing the array at position idx.
              val headerOffset = at.getHeaderSize // Skip the header.
              if (at.elemT.hasFixedAllocatedSize) {
                // Regular array
                val length = ViewPrinter.getLengthForArrayAccess(1, at.elemT, tupleAccessStack)
                headerOffset + idx * length
              } else {
                // The elements of this array may have different sizes. We need an
                // indirection: we fetch the actual position of the i-th element
                // from the array's metadata (see issue #107)
                // NB. We make a choice here, see issue #110
                val elementOffset = AccessVar(
                  v.toString, ArithExpression(acc + headerOffset + idx)
                )
                headerOffset + elementOffset
              }
          }
          
          aggregateAccesses(acc + position,
                            v, at.elemT, indices, tupleAccessStack)
        case tt: TupleType =>
          val i :: tas = tupleAccessStack
          aggregateAccesses(acc, v, tt.proj(i), arrayAccessStack, tas)
        case _ =>
          throw new IllegalAccess(ty)
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
  def emit(v: Var,
           view: View,
           replacements: immutable.Map[ArithExpr, ArithExpr] = immutable.Map()
          ): Expression = {
    val vp = new ViewPrinter(replacements)
    assert(!view.t.isInstanceOf[ArrayType])
    vp.emitView(v, view.replaced(replacements), List(), List())
  }
  
  /**
    * Traverse a view, following the eventual tuple accesses, and returns
    * the ViewMem (there must be one) at the bottom of it.
    *
    * @param sv ths view
    * @param tupleAccessStack indices used tu project the tuple views all along
    *                         the way
    * @return a `ViewMem`
    */
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
  
  /**
    * The size in words of a type as we can find it in an array.
    *
    * @param acc partial result
    * @param ty the type in question
    * @param tupleAccessStack list of tuple indices used all along the way to
    *                      choose what component of tuples should be
    *                      considered.
    * @return a size in words as an arithmetic expression
    */
  @scala.annotation.tailrec
  private def getLengthForArrayAccess(acc: ArithExpr,
                                      ty: Type,
                                      tupleAccessStack: List[Int]): ArithExpr = {
    ty match {
      case _: ScalarType => acc
      case vt: VectorType => vt.len * acc
      case tt: TupleType =>
        if (tupleAccessStack.isEmpty)
          // All the tuple indices have been consumed: this array won't be
          // traversed. We stop here.
          acc
        else getLengthForArrayAccess(
          // We project one component of the tuple
          acc,
          tt.proj(tupleAccessStack.head),
          tupleAccessStack.tail
        )
      case at @ ArrayTypeWC(elemT, n) =>
        getLengthForArrayAccess(
          acc * (n + at.getHeaderSize), // Do not forget the header!
          elemT, tupleAccessStack
        )
      case ArrayType(elemT) =>
        // We must know all the allocated sizes in this function
        assert(elemT.hasFixedAllocatedSize)
        // TODO: getLengthForArrayAccess(acc * v[0], elemT, tupleAccessStack)
        throw new NotImplementedError()
      case _ =>
        throw new IllegalAccess(ty)
    }
  }
}


