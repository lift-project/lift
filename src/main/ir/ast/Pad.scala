package ir.ast

import apart.arithmetic.ArithExpr
import ir.{TypeException, ArrayType, Type}

case class Pad(paddingSize: Int, boundary: (ArithExpr, ArithExpr) => ArithExpr)
  extends Pattern(arity = 1) with isGenerable
{
  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t, n) => ArrayType(t, n + 2*paddingSize)
      case _ => throw new TypeException(argType, "ArrayType")
    }
  }
}

object Pad {
  type BoundaryFun = (ArithExpr, ArithExpr) => ArithExpr

  object Boundary {
    val Wrap: BoundaryFun = (idx: ArithExpr, len: ArithExpr) => (idx % len + len) % len
    val Clamp: BoundaryFun = (idx: ArithExpr, len: ArithExpr) => ArithExpr.Math.Clamp(idx, 0, len-1)
    val Mirror: BoundaryFun = (idx: ArithExpr, len: ArithExpr) => {
      val id = ((idx lt 0) ?? (-1-idx) !! idx) % (2*len)
      (id ge len) ?? (len+len-id-1) !! id
    }

    val Bounce: BoundaryFun = (idx: ArithExpr, len: ArithExpr) => {
      throw new NotImplementedError("Not Implemented")
    }
  }
}

object Pad2D {
  import ir.ast.Pad.BoundaryFun
  def apply(paddingSize: Int, boundary: BoundaryFun): Lambda = {
    Transpose() o Pad(paddingSize, boundary) o Transpose() o Pad(paddingSize, boundary)
  }
}
