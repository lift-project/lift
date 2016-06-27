package ir.ast

import ir.interpreter.Interpreter._

import apart.arithmetic.Cst
import ir.{TypeException, ArrayType, Type}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * Slide pattern.
 * Create sliding windows of input
 */
case class Slide(size: Int, step: Int) extends Pattern(arity = 1) with isGenerable {
  Slide.cnt += 1
  val id = Slide.cnt

  override def toString: String = "Slide(" + size + "," + step + ")"

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t, n) =>
        //check that the following holds true!
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
    Map(
      Map(
        Transpose()
      ) o Slide(size, step) o Transpose()
    ) o Slide(size, step)
  }

  /** Asymmetrical sliding */
  def apply(size1: Int, step1: Int,
            size2: Int, step2: Int): Lambda = {
    Map(
      Map(
        Transpose()
      ) o Slide(size2, step2) o Transpose()
    ) o Slide(size1, step1)
  }
}
