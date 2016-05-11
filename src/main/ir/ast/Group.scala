package ir.ast

import ir.interpreter.Interpreter._
import ir.{TypeException, ArrayType, UndefType, Type}
import sun.reflect.generics.reflectiveObjects.NotImplementedException


/**
 * Group pattern.
 * Slice volume in overlapping tiles without boundary handling.
 *
 */
case class Group(leftHalo: Int, center: Int, rightHalo: Int) extends Pattern(arity = 1) with isGenerable {
  Group.cnt += 1
  val id = Group.cnt

  override def toString: String = "Group(" + leftHalo + "," + center + "," + rightHalo + ")"

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t, n) =>
        val innerLength = leftHalo + center + rightHalo
        val outerLength = (n - (leftHalo + rightHalo)) / center // do proper type checking
        ArrayType(ArrayType(t, innerLength), outerLength)
      case _ => throw new TypeException(argType, "ArrayType")
    }
  }

  /**
   * Define equality operator based on ID to be able to insert [[Group]] instances
   * into a set properly.
   * @param other Another object.
   * @return True if the other object is a [[Group]] instance with the same ID, false otherwise.
   */
  override def equals(other: Any): Boolean = other match {
    case g: Group => g.id == id
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

object Group {
  var cnt: Int = -1
}

object Group2D {
  /** Symmetrical grouping */
  def apply(leftHalo: Int, center: Int, rightHalo: Int): Lambda = {
    Map(
      Map(
        Transpose()
      ) o Group(leftHalo, center, rightHalo) o Transpose()
    ) o Group(leftHalo, center, rightHalo)
  }

  /** Asymmetrical grouping */
  def apply(l1: Int, c1: Int, r1: Int,
            l2: Int, c2: Int, r2: Int): Lambda = {
    Map(
      Map(
        Transpose()
      ) o Group(l2, c2, r2) o Transpose()
    ) o Group(l1, c1, r1)
  }
}
