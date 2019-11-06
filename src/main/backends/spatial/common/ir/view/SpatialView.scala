package backends.spatial.common.ir.view

import backends.spatial.accel.ir.ast.SpatialAccelAST.{AddressorT, VarSlicedRef}
import backends.spatial.common.ir.{AddressSpaceCollection, LiteralMemory, RegMemory, SpatialAddressSpace, UndefAddressSpace}
import core.generator.GenericAST
import core.generator.GenericAST.{ArithExpression, ExpressionT}
import ir.Type.size_t
import ir.ast.{Expr, Lambda}
import ir.{ArrayType, ArrayTypeWC, ArrayTypeWS, Capacity, ScalarType, Size, TupleType, Type, VectorType}
import ir.view.{InputView, OutputView, SizeIndex, View, View2DGeneratorUserFun, View3DGeneratorUserFun, ViewAccess, ViewArrayWrapper, ViewAsScalar, ViewAsVector, ViewConcat, ViewConstant, ViewFilter, ViewGenerator, ViewHead, ViewJoin, ViewMap, ViewMem, ViewOffset, ViewPad, ViewPadConstant, ViewReorder, ViewSize, ViewSlide, ViewSplit, ViewTail, ViewTranspose, ViewTuple, ViewTupleComponent, ViewUnzip, ViewZip}
import lift.arithmetic.{ArithExpr, Cst, RangeAdd, Var}

import scala.collection.{immutable, mutable}

/**
 * SpatialView implements Spatial-specific View printer and access generator.
 * The views themselves are Lift-generic.
 */
object SpatialView {

  /**
   * Visit the expression and construct all views for all sub-expressions.
   *
   * @param lambda The starting expression.
   */
  def apply(lambda: Lambda): Unit = {
    lambda.params.foreach((p) => {
      p.view = _root_.ir.view.View(p.t, p.mem.variable)
    })
    SpatialView(lambda.body)
  }

  /**
   * Visit the expression and construct all views for all sub-expressions.
   *
   * @param expr The starting expression.
   */
  def apply(expr: Expr): Unit = {
    BuildDepthInfoSp(expr)
    InputView(expr)
    OutputView(expr)
  }
}

private[view] class IllegalSpatialAccess(err: String)
  extends IllegalArgumentException() {
  def this(ty: Type) = this(s"Cannot compute access for type $ty")
  def this(ty: Type, as: SpatialAddressSpace) = this(s"Cannot compute access for type $ty in $as")
}

private[view] class IllegalSpatialView(err: String)
  extends IllegalArgumentException(err) {
  def this(v: View) = this(s"View $v is ill-formed")
}

class SpatialViewPrinter(val replacements: immutable.Map[ArithExpr, ArithExpr],
                         val mainAddressSpace: SpatialAddressSpace) {
  private var replacementsOfScalarWithSlicedAccesses = mutable.Map[ArithExpr, Slice]()

  /**
   * Produces a Spatial expression accessing a multidimensional array using a given view
   *
   * @param sv the view
   * @param arrayAccessStack
   *           see http://www.lift-project.org/papers/steuwer17LiftIR.pdf,
   *           section 5.3
   * @param tupleAccessStack
   *           see http://www.lift-project.org/papers/steuwer17LiftIR.pdf,
   *           section 5.3
   * @return an expression accessing the array
   */
  @scala.annotation.tailrec
  private def emitView(sv: View,
                       arrayAccessStack: List[ArrayAddressor],
                       tupleAccessStack: List[Int]): ExpressionT = {
    sv match {
      case ViewMem(memVar, ty) =>
        assert(tupleAccessStack.isEmpty)
        assert(replacementsOfScalarWithSlicedAccesses.isEmpty)
        GenerateAccess(memVar, ty, arrayAccessStack, tupleAccessStack)

      case ViewOffset(offset, iv, t) =>
        // increment read / write access by offset
        val addr :: addressors  = arrayAccessStack
        emitView(iv, (addr + offset) :: addressors,tupleAccessStack)

      case ViewAccess(i, iv, _) =>
        // For strided iterator, add division by step with the expectation that further down
        // the View tree we will encounter Split or Slide that will add multiplication by step
        // and thus cancel the step coefficient.
        // This is to express the strided iteration of Spatial Foreach, Fold and Reduce, where
        // we want to iterate from (0 to N by step) instead of from (0 to N/step) and multiplying
        // the iterator by step inside the loop body.
        // The strided range is introduced in RangesAndCountsSp().
        val newAddressor = i match {
          case _ if replacementsOfScalarWithSlicedAccesses.contains(i) =>
            val slice = replacementsOfScalarWithSlicedAccesses(i)
            replacementsOfScalarWithSlicedAccesses -= i
            slice
          case Var(_, RangeAdd(_, _, step)) if step !== Cst(1) =>
            Index(i /^ step)

          case _ => Index(i)
        }
        emitView(iv, newAddressor :: arrayAccessStack, tupleAccessStack)

      case ViewArrayWrapper(iv, _) =>
        val _ :: addressors = arrayAccessStack
        emitView(iv, addressors, tupleAccessStack)

      case ViewMap(iv, itVar, _) =>
        val (newV, newArrayAccessStack) = arrayAccessStack match {
          case (addr: Index) :: addressors => (iv.replaced(itVar, addr.ae), addressors)
          case (slice: Slice) :: addressors =>
            replacementsOfScalarWithSlicedAccesses += (itVar -> slice)
            (iv, addressors)
          case _ => throw new NotImplementedError()
        }

        emitView(newV, newArrayAccessStack, tupleAccessStack)

      case ViewSplit(chunkSize, iv, _) =>
        val newArrayAccessStack = arrayAccessStack match {
          // fun(ArrayT(ArrayT()), x => ..) o Split(s)
          // No addressors to add to stack since the array is accessed as a whole
          case Nil =>
            arrayAccessStack

          // Map(Map(..)) o Split(s)
          case Index(chunkIdx) :: Index(elemIdx) :: addressors =>
            Index(chunkIdx * chunkSize + elemIdx) :: addressors

          // Map(batchFun) o Split(s)
          case Index(chunkIdx) :: Slice(start, end) :: addressors =>
            assert(end - start == chunkSize)
            Slice(chunkIdx * chunkSize + start, chunkIdx * chunkSize + (end - start)) :: addressors

          // Map(batchFun) o Transpose() o Split(s)
          case Slice(start, end) :: Index(elemIdx) :: addressors =>
            Slice(start + elemIdx, start + elemIdx + chunkSize * (end - start)) :: addressors

          // matrixFun() o Split(s)
          case (outerSlice: Slice) :: (innerSlice: Slice) :: addressors =>
            // Restricting to simple cases until better generic approach is found
            assert(outerSlice.start == Cst(0));
            assert(innerSlice.start == Cst(0));
            assert(chunkSize == innerSlice.end - innerSlice.start)
            Slice(outerSlice.start,
                  (outerSlice.end - outerSlice.start) * (innerSlice.end - innerSlice.start)) :: addressors


          case _ => throw new IllegalArgumentException(f"Unexpected array access stack: $arrayAccessStack")
        }

        emitView(iv, newArrayAccessStack, tupleAccessStack)

      case ViewJoin(chunkSize, iv, _) =>
        val newArrayAccessStack = arrayAccessStack match {
          case (idx: Index) :: addressors =>
            val chunkIdx = idx / chunkSize
            val elemIdx = idx % chunkSize
            chunkIdx :: elemIdx :: addressors

          case (slice: Slice) :: addressors =>
            // Restricting to simple cases until better generic approach is found
            assert(slice.start == Cst(0));
            assert((slice.end - slice.start) % chunkSize == Cst(0))

            // A slice of a memory with a joined view should already cover all data contained
            // within the joined chunks, so there is no need to alter the slice to access all that data.
            // We do have to add one more access dimension since the inner views are expecting it, so
            // we are adding Index(0) here.
            Index(0) :: slice :: addressors

          case _ => throw new IllegalArgumentException(f"Unexpected array access stack: $arrayAccessStack")
        }

        emitView(iv, newArrayAccessStack, tupleAccessStack)

      case ViewTranspose(iv,_) =>
        val addr0 :: addr1 :: indices = arrayAccessStack
        emitView(iv, addr1 :: addr0 :: indices, tupleAccessStack)

      case ViewReorder(reindexFun, iv, _) =>
        val newArrayAccessStack = arrayAccessStack match {
          case (idx: Index) :: addressors =>
            Index(reindexFun(idx.ae)) :: addressors

          case Slice(start, end) :: addressors =>
            val newStart = reindexFun(start)
            Slice(newStart, newStart + (end - start)) :: addressors

          case _ => throw new IllegalArgumentException(f"Unexpected array access stack: $arrayAccessStack")
        }

        emitView(iv, newArrayAccessStack, tupleAccessStack)

      case ViewFilter(iv, ids, _) =>                throw new NotImplementedError()
      case ViewTupleComponent(i, iv, _) =>          emitView(iv, arrayAccessStack, i :: tupleAccessStack)
      case ViewZip(iv, _) =>                        emitView(iv, arrayAccessStack, tupleAccessStack)
      case ViewUnzip(iv, _) =>                      emitView(iv, arrayAccessStack, tupleAccessStack)
      case ViewConcat(iv, _) =>                     throw new NotImplementedError()
      case ViewTuple(ivs, _) =>                     val i :: newTAS = tupleAccessStack
                                                    emitView(ivs(i), arrayAccessStack, newTAS)
      case ViewAsVector(vecSize, iv, _) =>          throw new NotImplementedError()
      case ViewAsScalar(iv, vecSize, _) =>          throw new NotImplementedError()
      case ViewHead(iv, _) =>                       throw new NotImplementedError()
      case ViewTail(iv, _) =>                       throw new NotImplementedError()
      case ViewSlide(iv, slide, _) =>
        val newArrayAccessStack = arrayAccessStack match {
          // fun(ArrayT(ArrayT()), x => ..) o Split(s)
          // No addressors to add to stack since the array is accessed as a whole
          case Nil =>
            arrayAccessStack

          // Map(Map(..)) o Slide(s)
          case Index(chunkIdx) :: Index(elemIdx) :: addressors =>
            Index(chunkIdx * slide.step + elemIdx) :: addressors

          // Map(batchFun) o Slide(s)
          case Index(chunkIdx) :: Slice(start, end) :: addressors =>
            Slice(chunkIdx * slide.step, chunkIdx * slide.step + (end - start)) :: addressors

          // Map(batchFun) o Transpose() o Slide(s)
          case Slice(start, end) :: Index(elemIdx) :: addressors =>
            Slice(start + elemIdx, start + elemIdx + slide.step * (end - start)) :: addressors

          // matrixFun() o Slide(s)
          case (_: Slice) :: (_: Slice) :: addressors =>
            // TODO: confirm whether this is illegal
            throw new IllegalSpatialView("ND-sliced access to 1D array")

          case _ => throw new IllegalArgumentException(f"Unexpected array access stack: $arrayAccessStack")
        }
        emitView(iv, newArrayAccessStack, tupleAccessStack)

      case ViewPad(iv, left, _, padFun, _) =>       throw new NotImplementedError()
      case ViewPadConstant(iv, left, _,
      constant, _) =>                               throw new NotImplementedError()
      case ViewConstant(value, _) =>                GenericAST.RawCode(value.value)
      case ViewSize(iv) =>
        assert(arrayAccessStack.isEmpty)
        val newAAS = Index(SizeIndex()) :: Nil
        emitView(iv, newAAS, tupleAccessStack)

      case ViewGenerator(f, ArrayTypeWS(_,s)) =>    throw new NotImplementedError()
      case View2DGeneratorUserFun(f, ArrayTypeWS(ArrayTypeWS(_, n), m)) =>
        throw new NotImplementedError()
      case View3DGeneratorUserFun(f, ArrayTypeWS(ArrayTypeWS(ArrayTypeWS(_, o), n), m)) =>
        throw new NotImplementedError()
    }
  }



  /**
   * Uses the list of array addressors (indices and slices) expressing an access to
   * a multi-dimensional potentially ragged array to generate the AST node for an array access.
   * Resolves accessing arrays in tuples and performs some sanity checks.
   */
  object GenerateAccess {
    def apply(mainVar: Var, mainType: Type,
              arrayAccessStack: List[ArrayAddressor],
              tupleAccessStack: List[Int]): VarSlicedRef = {
      // Sanity check -- array access stack must have the same number of elements as there are dimensions in the array
      assert(Type.getLengths(mainType).length - 1 == arrayAccessStack.length)
      val g = new GenerateAccess(mainVar, mainType, tupleAccessStack)
      g.generate(List(), mainType, arrayAccessStack, tupleAccessStack)
    }
  }

  private class GenerateAccess private (mainVar: Var, mainType: Type, mainTupleAccessStack: List[Int]) {
    @scala.annotation.tailrec
    private def generate(partialAAStack: List[ArrayAddressor], ty: Type,
                         arrayAccessStack: List[ArrayAddressor],
                         tupleAccessStack: List[Int]): VarSlicedRef = {
      if (arrayAccessStack.isEmpty)
        VarSlicedRef(mainVar, arrayAddressors = Some(partialAAStack.map(_.toTargetAST)))
      else {
        ty match {
          case at: ArrayType =>
            val addr :: addressors = arrayAccessStack

            val newASS = getElementAt(partialAAStack, at, addr, tupleAccessStack)
            generate(newASS, at.elemT, addressors, tupleAccessStack)

          // This tuple comes from a view, project and continue
          case tt: TupleType =>
            val i :: remaining = tupleAccessStack
            generate(partialAAStack, tt.proj(i), arrayAccessStack, remaining)

          case _ => throw new IllegalSpatialAccess(ty)
        }
      }
    }

    /**
     * Generates an access at position `idx` or `slice.start::slice.step::slice.end` in the nd-array
     * This method is simpler than that of the C-like backends in the __root__.ir.view package because
     * we don't need to store the header (capacity + size) and offsets with data for ragged ND-array navigation.
     * Scala-Spatial takes care of indexing in complex structures.
     */
    private def getElementAt(partialAAStack: List[ArrayAddressor], at: ArrayType,
                             addr: ArrayAddressor, tupleAccessStack: List[Int]): List[ArrayAddressor] = {
      // Sanity check
      if (addressSpace == RegMemory && !at.isInstanceOf[ArrayType with Size with Capacity])
        throw new IllegalSpatialView("An array in register memory must have a size and a capacity in the type")
      if (addressSpace == LiteralMemory && !at.isInstanceOf[ArrayType with Size with Capacity])
        throw new IllegalSpatialView("An array in literal memory must have a size and a capacity in the type")

      //        partialAAStack :+ addr
      if (at.elemT.hasFixedAllocatedSize) {
        // Just skip the header
//        val len = getLengthForArrayAccess(at.elemT, tupleAccessStack)
        // No need to multiply indices by the length of the dimension thanks to
        // the support of multidimensional accesses in Spatial Scala
        partialAAStack :+ (addr/* * len*/ +  at.headerLength * alignment / baseSize)
      } else {
        (ArrayAddressor.getNonSlicedAccess(addr +: partialAAStack)) match {
          case None => throw new IllegalSpatialAccess(
            "Cannot produce sliced access to an array with statically unknown size")

          case Some(Nil) => throw new IllegalArgumentException("Expected at least one array addressor. Got none")

          case Some(idx :: partialArrayIdxStack) =>

            // Perform an indirection. Do not cast the pointer if it's not required.
            val elementOffset: AccessVar = {
              if (baseType == size_t)
                AccessVar(mainVar, partialArrayIdxStack :+ (idx + at.headerLength))
              else {
                val casted = CastedPointer(mainVar, size_t, idx, addressSpace)
                AccessVar(casted, partialArrayIdxStack :+ (idx + at.headerLength * alignment / size_t.size))
              }
            }
            // The offset read from the headers is in bytes but must be a multiple of `baseSize`
            partialArrayIdxStack :+ Index(elementOffset / baseSize)
        }
      }
    }

    // ---
    // Helpers
    // ---
    private lazy val (baseType, addressSpace) = getBaseTypeAndAddressSpace(
      mainType, mainTupleAccessStack, mainAddressSpace)
    private lazy val baseSize = Type.getAllocatedSize(baseType).eval
    private val alignment = Math.max(size_t.size.eval, baseSize)
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
                                           addressSpace: SpatialAddressSpace): (Type, SpatialAddressSpace) = {
      if (tupleAccessStack.isEmpty) (Type.getBaseType(ty), addressSpace)
      else (ty, addressSpace) match {
        case (tt: TupleType, AddressSpaceCollection(coll)) =>
          val i :: tas = tupleAccessStack
          getBaseTypeAndAddressSpace(tt.proj(i), tas, coll(i))
        case (ArrayType(elemT), _) =>
          getBaseTypeAndAddressSpace(elemT, tupleAccessStack, addressSpace)
        case _ => throw new IllegalSpatialAccess(ty, addressSpace)
      }
    }
  }
}


/**
 * Helper object for converting views to arithmetic expressions.
 *
 * Incrementally backtracks the links in the views modifying access variables and
 * dimension lengths as necessary.
 */
object SpatialViewPrinter {

  /**
   * Emit the arithmetic expression for accessing an array that corresponds
   * to the view.
   *
   * @param view The view to emit.
   * @return The arithmetic expression.
   */
  def emit(view: View,
           replacements: immutable.Map[ArithExpr, ArithExpr] = immutable.Map(),
           addressSpace: SpatialAddressSpace = UndefAddressSpace): ExpressionT = {
    val vp = new SpatialViewPrinter(replacements, addressSpace)
    // This requirement is relaxed compared to the C-like backends since Spatial supports sliced accesses to arrays:
    //assert(!view.t.isInstanceOf[ArrayType])

    // Instead, we populate the array access stack with slices corresponding to the arrays in the view type
    val arrayAccessStack: List[Slice] = view.t match {
      case at: ArrayType with Size =>
        Type.getLengths(at).dropRight(1).map(Slice.continuous).toList

      case ArrayType(_) => throw new IllegalArgumentException(
        "Cannot emit sliced access to an array without a statically known size")

      case _ => List() // Scalar access
    }

    vp.emitView(view.replaced(replacements), arrayAccessStack, List())
  }
}
