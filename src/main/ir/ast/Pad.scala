package ir.ast

import apart.arithmetic.ArithExpr
import ir.{TypeException, ArrayType, Type, UndefType}

case class Pad(offset: Int, boundary: (ArithExpr, ArithExpr) => ArithExpr)
  extends Pattern(arity = 1) with isGenerable
{
  var paramType: Type = UndefType

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t, n) =>
        paramType = ArrayType(t, n + 2*offset)
        paramType

      case _ => throw new TypeException(argType, "ArrayType")
    }
  }
}

object Pad {
  type BoundaryFct = (ArithExpr, ArithExpr) => ArithExpr

  object Boundary {
    val Wrap: BoundaryFct = (idx: ArithExpr, len: ArithExpr) => (idx % len + len) % len

    val Clamp: BoundaryFct = (idx: ArithExpr, len: ArithExpr) => ArithExpr.Math.Clamp(idx, 0, len-1)

    val Mirror: BoundaryFct = (idx: ArithExpr, len: ArithExpr) => {
      val id = ((idx lt 0) ?? (-1-idx) !! idx) % (2*len)
      (id ge len) ?? (len+len-id-1) !! id
    }

    val Bounce: BoundaryFct = (idx: ArithExpr, len: ArithExpr) => {
      throw new NotImplementedError("Not Implemented")
    }
  }
}

object Pad2D {
  def apply(offset: Int, boundary: (ArithExpr, ArithExpr) => ArithExpr): Lambda = {
    Transpose() o Pad(offset, boundary) o Transpose() o Pad(offset, boundary)
  }
}
