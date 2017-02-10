package ir.ast

import ir.interpreter.Interpreter._
import lift.arithmetic.ArithExpr
import ir.{ArrayType, Type, TypeException}
import rewriting.utils.NumberExpression
import rewriting.{Rewrite, Rules, SimplifyAndFuse}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * Slide pattern.
 * Create sliding windows of input
 */
case class Slide(size: ArithExpr, step: ArithExpr) extends Pattern(arity = 1) with isGenerable {
  Slide.cnt += 1
  val id = Slide.cnt

  override def toString: String = "Slide(" + size + "," + step + ")"

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t, n) =>
        // todo check that the sliding window always ends at the last element of the input
        //if (((n - (size - step)) % step) != Cst(0)) throw new TypeException(argType, "slide args not as")
        val innerLength = size
        val outerLength = (n - (size - step)) / step
        ArrayType(ArrayType(t, innerLength), outerLength)
      case _ => throw new TypeException(argType, "ArrayType")
    }
  }

  /**
   * Define equality operator based on ID to be able to insert [[Slide]] instances
   * into a set properly.
   *
   * @param other Another object.
   * @return True if the other object is a [[Slide]] instance with the same ID, false otherwise.
   */
  override def equals(other: Any): Boolean = other match {
    case s: Slide => s.id == id
    case _ => false
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case a: Vector[_] => throw new NotImplementedException()
    }
  }

  /**
   * Define hash based on the ID to identify unique instances in associative containers.
   * @return The hashCode of the id.
   */
  override def hashCode = id.hashCode()
}

object Slide {
  var cnt: Int = -1
}

object Slide2D {
  /** Symmetrical sliding */
  def apply(size: Int, step: Int): Lambda = {
    SlideND(2)(size,step)
    //Map(Transpose()) o Slide(size, step) o Map(Slide(size, step))
    // other possible implementation
    // Map(Map(Transpose()) o Slide(size, step) o Transpose()) o Slide(size, step)
  }

  /** Asymmetrical sliding */
  def apply(sizeRow: Int, stepRow: Int,
            sizeCol: Int, stepCol: Int): Lambda = {
    Map(Transpose()) o Slide(sizeRow, stepRow) o Map(Slide(sizeCol, stepCol))
    // other possible implementation
//    Map(Map(Transpose()) o Slide(sizeCol, stepCol) o Transpose()) o Slide(sizeRow, stepRow)
  }
}

object Slide3D {
  /** Symmetrical sliding */
  def apply(size: Int, step: Int): Lambda = {
    SlideND(3)(size,step)

    //Slide3DTest(size, step)

    //Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Map(Transpose()))) o
    //Slide(size,step) o Map(Slide(size,step)) o Map(Map(Slide(size,step)))
    /*
    Map(Map(Transpose()) o Transpose()) o
    Slide(size, step) o
    Map(Map(Transpose()) o Slide(size, step) o Map(Slide(size, step)))
    */
    /* other possible implementation
    Map(Map(Transpose()) o Transpose() o
      Map(Map(Transpose() o Map(Slide(size, step))
        ) o Slide(size, step))) o Slide(size, step)
    */
  }

  def apply(sizeX: Int, stepX: Int,
            sizeY: Int, stepY: Int,
            sizeZ: Int, stepZ: Int): Lambda = {
    Map(Map(Transpose()) o Transpose()) o
    Slide(sizeZ, stepZ) o
    Map(Map(Transpose()) o Slide(sizeY, stepY) o Map(Slide(sizeX, stepX)))
  }
}

object SlideND {

  // 0 -> f, 1 -> Map(f), 2 -> Map(Map(f)), ...
  def wrapInMaps(f: => Lambda)(count: Int): Lambda = {
    if(count < 1) f
    else Map(wrapInMaps(f)(count-1))
  }

  // creates Map(...(Map(f)...) o ... o Map(f) o f
  def applyInEveryDimUntilDim(f: => Lambda)(dim: Int): Lambda = {
    if(dim <= 1) f
    else applyInEveryDimUntilDim(Map(f))(dim-1) o f
    //else Map(applyInEveryDimUntilDim(f)(dim-1)) o f <- fused version
  }

  // creates f o Map(f) o ... o Map(...(Map(f)...)
  def applyInEveryDimUntilDimReverse(f: => Lambda)(dim: Int): Lambda = {
    if(dim <= 1) f
    else f o applyInEveryDimUntilDimReverse(Map(f))(dim-1)
    //else f o Map(applyInEveryDimUntilDimReverse(f)(dim-1)) <- fused version
  }

  // [a][A][b][C][c][C]... => [a][b][c]...[A][B][C]...
  def interleaveDimensions(count: Int, i: Int): Lambda = {
    val howManyMaps = -2 * (count - 1 - i) - 1
    if(count == 2) wrapInMaps(Transpose())(howManyMaps)
    else {
      applyInEveryDimUntilDim(wrapInMaps(Transpose())(howManyMaps))(count - 1) o interleaveDimensions(count - 1, i)
    }
  }

  def apply(dim: Int)(size: Int, step: Int): Lambda = {
    if(dim==1) Slide(size,step)
    else {
      interleaveDimensions(dim, dim) o applyInEveryDimUntilDimReverse(Slide(size, step))(dim)
    }
  }
}
