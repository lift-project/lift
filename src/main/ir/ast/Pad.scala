package ir.ast

import lift.arithmetic.{ArithExpr, SimplifiedExpr}
import ir.interpreter.Interpreter._
import ir._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

case class Pad(left: ArithExpr, right: ArithExpr, boundary: Pad.BoundaryFun)
  extends Pattern(arity = 1) {

  override def toString: String = "Pad(" + left + "," + right + "," + boundary + ")"

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
        //todo @bastian include comment again, issue with pad2d(a,b, 0,0)
      case ArrayTypeWSWC(et,s,c)/*if (left > 0 || right > 0)*/ => ArrayTypeWSWC(et, s + left + right, c + left + right)
      case _ => throw new TypeException(argType, "ArrayType", this)
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

    object Identity extends ReindexingFun {
      override def toString: String = "Pad.Boundary.Identity"
      override def apply(idx: ArithExpr, len: ArithExpr): ArithExpr with SimplifiedExpr = idx
    }

    object Wrap extends ReindexingFun {
      override def toString: String = "Pad.Boundary.Wrap"
      override def apply(idx: ArithExpr, len: ArithExpr) = {
        (idx % len + len) % len
      }
    }

    // Pad size is not allowed to exceed input size
    object WrapUnsafe extends ReindexingFun {
      override def toString: String = "Pad.Boundary.WrapUnsafe"
      override def apply(idx: ArithExpr, len: ArithExpr) = {
        (idx + len) % len
      }
    }

    object Clamp extends ReindexingFun {
      override def toString: String = "Pad.Boundary.Clamp"
      override def apply(idx: ArithExpr, len: ArithExpr) = {
        //ArithExpr.Math.Clamp(idx, 0, len-1)
        (idx ge 0) ?? ((idx lt len) ?? idx !! len-1) !! 0
      }
    }

    object Mirror extends ReindexingFun {
      override def toString: String = "Pad.Boundary.Mirror"
      override def apply(idx: ArithExpr, len: ArithExpr) = {
        val id = ((idx lt 0) ?? (-1-idx) !! idx) % (2*len)
        (id ge len) ?? (len+len-id-1) !! id
      }
    }

    // Pad size is not allowed to exceed input size
    object MirrorUnsafe extends ReindexingFun {
      override def toString: String = "Pad.Boundary.MirrorUnsafe"
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
  def apply(top: Int, bottom: Int, left: Int, right: Int, boundary: Pad.BoundaryFun): Lambda = {
    Map(Pad(left, right, boundary)) o Pad(top, bottom, boundary)
    // other possible implementation using transpose
    //Transpose() o Pad(left, right, boundary) o Transpose() o Pad(top, bottom, boundary)
  }

  // Symmetric 2D padding
  def apply(left: Int, right: Int, boundary: Pad.BoundaryFun): Lambda = {
    PadND(2)(left,right,boundary)
    //Map(Pad(left, right, boundary)) o Pad(left, right, boundary)
    // other possible implementation using transpose
    //Transpose() o Pad(left, right, boundary) o Transpose() o Pad(left, right, boundary)
  }
}

object Pad3D {
  def apply(x: Int, y: Int, z: Int, b: Pad.BoundaryFun): Lambda = {
    Map(Map(Pad(x, x, b)) o Pad(y, y, b)) o Pad(z, z, b)
  }
}

object PadND {
  def apply(dim: Int)(left: Int, right: Int, b: Pad.BoundaryFun): Lambda = {
    GenerateIR.applyInEveryDimUntilDim(Pad(left,right,b), dim)
  }
}

