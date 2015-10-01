package ir

import apart.arithmetic.ArithExpr
import scala.language.implicitConversions

package object ast {

  class IndexFunction(val f: (ArithExpr, Type) => ArithExpr)

  implicit def apply(f: (ArithExpr, Type) => ArithExpr): IndexFunction = new IndexFunction(f)

  // predefined reorder functions ...
  val transposeFunction = (outerSize: ArithExpr, innerSize: ArithExpr) => (i: ArithExpr, t: Type) => {
    val col = (i % innerSize) * outerSize
    val row = i / innerSize

    row + col
  }

  val transpose = (i: ArithExpr, t: Type) => {
    t match {
      case ArrayType(ArrayType(_, n), m) =>
        transposeFunction(m, n)(i, t)
      case _ => throw new IllegalArgumentException
    }
  }

  val reverse = (i: ArithExpr, t: Type) => {
    val n = Type.getLength(t)

    n - 1 - i
  }

  val reorderStride = (s:ArithExpr) => (i: ArithExpr, t:Type) => {
    val n = Type.getLength(t) /^ s
    (i / n) + s * (i % n)
  }

  case class ReorderWithStride(s: ArithExpr) extends IndexFunction(reorderStride(s)) {

    def canEqual(other: Any): Boolean = other.isInstanceOf[ReorderWithStride]

    override def equals(other: Any): Boolean = other match {
      case that: ReorderWithStride =>
        (that canEqual this) &&
          s == that.s
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(s)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }
}
