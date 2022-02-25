package ir.ast

import ir.interpreter.Interpreter.ValueMap
import ir.{ArrayTypeWSWC, Type, TypeException}
import lift.arithmetic.ArithExpr

/**
 * Unslide takes a slided array and reverses sliding, producing a view where overlapping elements occur only once.
 */
case class Unslide(size: ArithExpr, step: ArithExpr) extends Pattern(arity = 1) {

  ArithExpr.isSmaller(step, size) match {
    case Some(false) if step != size =>
      throw new IllegalArgumentException("Unslide requires that step <= size: if there are elements between sliding " +
        "windows, unsliding would eliminate them")
    case Some(_) =>
    case None =>
      println("WARNING (Unslide): Cannot verify that step <= size")
  }

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWSWC(ArrayTypeWSWC(et, innerSize, innerCapacity), outerSize, outerCapacity)
        if outerSize == outerCapacity &&
          innerSize == innerCapacity &&
          innerSize == size =>

//        val unslidedSizeWhenInnerSizeEqualSlideSize = outerSize * step + (size - step)
//        val skippedSizeWhenInnerSizeSmallerThanSlideSize = (1 - innerSize / size) * // Only enable for innerSize < size
//          (outerSize * (size - innerSize - (size - step)) + (size - step))
//
//        val unslidedSize = unslidedSizeWhenInnerSizeEqualSlideSize - skippedSizeWhenInnerSizeSmallerThanSlideSize
        val unslidedSize = outerSize * step + (size - step)

        ArrayTypeWSWC(et, unslidedSize)

      case _ => throw new TypeException(argType, "ArrayType(ArrayType(.., size))", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Array[Any] = {
    throw new NotImplementedError()
  }

  override def toString: String = "Unslide(" + size + "," + step + ")"
}
