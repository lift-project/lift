package ir.ast

import ir.interpreter.Interpreter._
import lift.arithmetic.ArithExpr
import ir.{ArrayType, Type, TypeException}
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
    Map(Transpose()) o Slide(size, step) o Map(Slide(size, step))
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
    Map(Map(Transpose()) o Transpose()) o
    Slide(size, step) o
    Map(Map(Transpose()) o Slide(size, step) o Map(Slide(size, step)))
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
