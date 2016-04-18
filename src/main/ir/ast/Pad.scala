package ir.ast

import apart.arithmetic.ArithExpr
import ir.{TypeException, ArrayType, Type}

case class Pad(paddingSize: Int, boundary: Pad.BoundaryFun)
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

  abstract class BoundaryFun {
    def apply(idx: ArithExpr, len: ArithExpr): ArithExpr // move this into ReindexingFun eventually ...
  }

  object Boundary {
    //class Constant(val left: ArithExpr, val right: ArithExpr) extends BoundaryFun

    abstract class ReindexingFun extends BoundaryFun //{
    //  def apply(idx: ArithExpr,  len: ArithExpr): ArithExpr
    //}

    object Wrap extends ReindexingFun {
      override def apply(idx: ArithExpr, len: ArithExpr) = {
        (idx % len + len) % len
      }
    }

    object Clamp extends ReindexingFun {
      override def apply(idx: ArithExpr, len: ArithExpr) = {
        ArithExpr.Math.Clamp(idx, 0, len-1)
      }
    }

    object Mirror extends ReindexingFun {
      override def apply(idx: ArithExpr, len: ArithExpr) = {
        val id = ((idx lt 0) ?? (-1-idx) !! idx) % (2*len)
        (id ge len) ?? (len+len-id-1) !! id
      }
    }

    // Pad size is not allowed to exceed input size
    object MirrorUnsafe extends ReindexingFun {
      override def apply(idx: ArithExpr, len: ArithExpr) = {
        //(idx ge len) ?? (2*len-idx-1) !! ((idx lt 0) ?? (-1-idx) !! idx)
        val leftBoundaryCheck = ((idx lt 0) ?? (-1-idx) !! idx)
        //(leftBoundaryCheck ge len) ?? (2*len-idx-1) !! leftBoundaryCheck
        (leftBoundaryCheck le len-1) ?? leftBoundaryCheck !! (2*len-idx-1)
      }
    }
  }
}

object Pad2D {
  def apply(paddingSize: Int, boundary: Pad.BoundaryFun): Lambda = {
    Transpose() o Pad(paddingSize, boundary) o Transpose() o Pad(paddingSize, boundary)
  }
}
