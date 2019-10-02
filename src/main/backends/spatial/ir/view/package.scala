package backends.spatial.ir

import core.generator.GenericAST.{ArithExpression, ExpressionT, VarRef}
import _root_.ir.{ArrayType, ArrayTypeWC, Capacity, ScalarType, Size, TupleType, Type, VectorType}
import _root_.ir.view.{CastedPointer, SizeIndex, View, ViewMem}
import backends.spatial.accel.generator.SpatialAccelAST.{IdxInterval, NDVarSlicedRef}
import ir.Type.size_t
import lift.arithmetic.{ArithExpr, Cst, ExtensibleVar, Range, RangeUnknown, SimplifiedExpr, Var}

import scala.collection.immutable

package object view {

  private case class Interval(start: ArithExpr, stop: ArithExpr)
  type ArrayAddressor = Either[ArithExpr, Interval]

  private class IllegalSpatialAccess(err: String)
    extends IllegalArgumentException() {
    def this(ty: Type) = this(s"Cannot compute access for type $ty")
    def this(ty: Type, as: SpatialAddressSpace) = this(s"Cannot compute access for type $ty in $as")
  }

  private class IllegalSpatialView(err: String)
    extends IllegalArgumentException(err) {
    def this(v: View) = this(s"View $v is ill-formed")
  }

  class ViewPrinter(val replacements: immutable.Map[ArithExpr, ArithExpr],
                    val mainAddressSpace: SpatialAddressSpace) {

    /**
     * Produces a Spatial expression accessing a multi-dimentional array using
     * a given view
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
    private def emitView(sv: View,
                         arrayAccessStack: List[ArrayAddressor],
                         tupleAccessStack: List[Int]): ExpressionT = {
      sv match {
        case ViewMem(memVar, ty) =>
          assert(tupleAccessStack.isEmpty)
          GenerateAccess(memVar, ty, arrayAccessStack, tupleAccessStack)
      }
    }



    /**
     * Uses the list of array addressors (indices and intervals) expressing an access to
     * a multi-dimensional potentially ragged array to generate the AST node for an array access.
     * Resolves accessing arrays in tuples and performs some sanity checks.
     */
    object GenerateAccess {
      def apply(mainVar: Var, mainType: Type,
                arrayAccessStack: List[ArrayAddressor],
                tupleAccessStack: List[Int]): NDVarSlicedRef = {
        val g = new GenerateAccess(mainVar, mainType, tupleAccessStack)
        g.generate(List(), mainType, arrayAccessStack, tupleAccessStack)
      }
    }

    private class GenerateAccess private (mainVar: Var, mainType: Type, mainTupleAccessStack: List[Int]) {
      @scala.annotation.tailrec
      private def generate(arrayAccessStackRebuilt: List[ArrayAddressor], ty: Type,
                           arrayAccessStack: List[ArrayAddressor],
                           tupleAccessStack: List[Int]): NDVarSlicedRef = {
        if (arrayAccessStack.isEmpty)
          NDVarSlicedRef(mainVar, arrayAddressors = Some(arrayAccessStackRebuilt.map {
            case Left(idx)        => ArithExpression(idx)
            case Right(interval)  => IdxInterval(ArithExpression(interval.start), ArithExpression(interval.stop))
          }))
        else {
          ty match {
            case at: ArrayType =>
              val addr :: addressors = arrayAccessStack

              val newASS = getElementAt(arrayAccessStackRebuilt, at, addr, tupleAccessStack)
              generate(newASS, at.elemT, addressors, tupleAccessStack)

            // This tuple comes from a view, project and continue
            case tt: TupleType =>
              val i :: remaining = tupleAccessStack
              generate(arrayAccessStackRebuilt, tt.proj(i), arrayAccessStack, remaining)

            case _ => throw new IllegalSpatialAccess(ty)
          }
        }
      }

      /**
       * Generates an access at position `idx` or `interval.start::interval.stop` in the nd-array
       * This method is simpler than that of the C-like backends in the __root__.ir.view package because
       * we don't need to store the header (capacity + size) and offsets with data for ragged ND-array navigation.
       * Scala-Spatial takes care of indexing in complex structures.
       */
      private def getElementAt(arrayAccessStackRebuilt: List[ArrayAddressor], at: ArrayType,
                               addr: ArrayAddressor, tupleAccessStack: List[Int]): List[ArrayAddressor] = {
        // Sanity check
        if (addressSpace == RegMemory && !at.isInstanceOf[ArrayType with Size with Capacity])
          throw new IllegalSpatialView("An array in private memory must have a size and a capacity in the type")

        arrayAccessStackRebuilt :+ (addr match {
          case Left(idx)      => idx
          case Right(inter)   => Interval(inter.start, inter.stop)
        }).asInstanceOf[ArrayAddressor]
      }

      // ---
      // Helpers
      // ---
      private lazy val addressSpace = getAddressSpace(mainType, mainTupleAccessStack, mainAddressSpace)


      /**
       * Similarly, get the address space of the memory we are
       * accessing by projecting the tuples that come from a view in the type and
       * in the address space collections.
       */
      @scala.annotation.tailrec
      private def getAddressSpace(ty: Type, tupleAccessStack: List[Int],
                                             addressSpace: SpatialAddressSpace): SpatialAddressSpace = {
        if (tupleAccessStack.isEmpty) addressSpace
        else (ty, addressSpace) match {
          case (tt: TupleType, AddressSpaceCollection(coll)) =>
            val i :: tas = tupleAccessStack
            getAddressSpace(tt.proj(i), tas, coll(i))
          case (ArrayType(elemT), _)  => getAddressSpace(elemT, tupleAccessStack, addressSpace)
          case _                      => throw new IllegalSpatialAccess(ty, addressSpace)
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
    def emit(
              view: View,
              replacements: immutable.Map[ArithExpr, ArithExpr] = immutable.Map(),
              addressSpace: SpatialAddressSpace = UndefAddressSpace
            ): ExpressionT = {
      val vp = new ViewPrinter(replacements, addressSpace)
      assert(!view.t.isInstanceOf[ArrayType])
      vp.emitView(view.replaced(replacements), List(), List())
    }
  }
}
