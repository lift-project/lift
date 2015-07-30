package ir.ast

import apart.arithmetic.ArithExpr
import ir.{ArrayType, Type, UndefType}
import scala.language.implicitConversions

/**
 * Abstract base class for all patterns (i.e., primitives defined in our
 * language)
 */
abstract class Pattern(override val arity: Int) extends FunDecl(arity)

object Pattern {
  def unapply(l: Lambda): Option[(Pattern)] = l match {
    case Lambda(_, FunCall(x, _)) if x.isInstanceOf[Pattern] => Some(x.asInstanceOf[Pattern])
    case _ => None
  }
}

/**
 * Trait for all patterns which have a nested lambda (e.g., Map or Reduce)
 */
trait FPattern {
  def f: Lambda
  def copy(f: Lambda): Pattern
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