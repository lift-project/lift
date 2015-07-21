package ir.ast

import apart.arithmetic.ArithExpr
import ir.{ArrayType, Type, UndefType}
import scala.language.implicitConversions

/**
 * Abstract base class for all patterns (i.e., primitives defined in our
 * language)
 *
 * @param params The parameters of the function declaration.
 */
abstract class Pattern(override val params: Array[Param])
  extends FunDecl(params) {

  def this(arity: Int) = this(Array.fill(arity)(Param(UndefType)))
}

/**
 * Trait for all patterns which have a nested lambda (e.g., Map or Reduce)
 */
trait FPattern {
  def f: Lambda
}

class IndexFunction(val f: (ArithExpr, Type) => ArithExpr)

object IndexFunction {
  implicit def apply(f: (ArithExpr, Type) => ArithExpr): IndexFunction = new IndexFunction(f)

  // predefined reorder functions ...
  val transposeFunction = (outerSize: ArithExpr, innerSize: ArithExpr) => (i: ArithExpr, t: Type) => {
    val col = (i % innerSize) * outerSize
    val row = i / innerSize

    row + col
  }

  val transpose = (i: ArithExpr, t: Type) => {
    val outerType = t match { case at: ArrayType => at }
    val innerType = outerType.elemT match { case at: ArrayType => at }

    transposeFunction(outerType.len, innerType.len)(i, t)
  }

  val reverse = (i: ArithExpr, t: Type) => {
    val n = Type.getLength(t)

    n - 1 - i
  }

  val reorderStride = (s:ArithExpr) => (i: ArithExpr, t:Type) => {
    val n = Type.getLength(t) /^ s
    (i / n) + s * (i % n)
  }
}
