package ir.ast

import apart.arithmetic.ArithExpr
import ir.interpreter.Interpreter._
import ir.{TypeException, ArrayType, Type, UndefType}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

case class Pad(left: Int, right: Int, boundary: Pad.BoundaryFun)
  extends Pattern(arity = 1) with isGenerable
{
  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t, n) => ArrayType(t, n + left + right)
      case _ => throw new TypeException(argType, "ArrayType")
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case a: Vector[_] => throw new NotImplementedException()
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
        //ArithExpr.Math.Clamp(idx, 0, len-1)
        (idx ge 0) ?? ((idx lt len) ?? idx !! len-1) !! 0
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
        //(leftBoundaryCheck ge len) ?? (2*len-idx-1) !! leftBoundaryCheck

        //val leftBoundaryCheck = ((idx lt 0) ?? (-1-idx) !! idx)
        //(leftBoundaryCheck lt len) ?? leftBoundaryCheck !! (2*len-idx-1)

        //(idx lt 0) ?? (-1-idx) !! ((idx lt len) ?? idx !! 2*len-idx-1)
        (idx ge 0) ?? ((idx lt len) ?? idx !! 2*len-idx-1) !! (-1-idx)
      }
    }
  }
}

object Pad2D {
  // todo rename top bottom left right check if naming is correct
  def apply(top: Int, bottom: Int, left: Int, right: Int, boundary: Pad.BoundaryFun): Lambda = {
    Transpose() o Pad(left, right, boundary) o Transpose() o Pad(top, bottom, boundary)
  }

  // Symmetric 2D padding
  def apply(left: Int, right: Int, boundary: Pad.BoundaryFun): Lambda = {
    Transpose() o Pad(left, right, boundary) o Transpose() o Pad(left, right, boundary)
  }
}
