package ir.ast

import ir.{TypeException, ArrayType, Type}

/**
 * Group pattern.
 * Slice volume in overlapping tiles without boundary handling.
 *
 * @param relIndices Array of relative indices.
 */
case class Group(relIndices: Array[Int]) extends Pattern(arity = 1) with isGenerable {
  Group.cnt += 1
  val id = Group.cnt

  val offset = Math.abs(Math.min(0, relIndices.min))

  // internally relIndices will be used shiftet to all positives
  val posIndices = relIndices.map(_+offset)

  override def toString: String = "Group([" + relIndices.mkString(",") + "])"

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t, n) =>
        ArrayType(ArrayType(t, relIndices.length), n - (Math.abs(Math.min(0, relIndices.min)) + Math.max(0, relIndices.max)))
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
  def apply(neighbors: Array[Int]): Lambda = {
    Map(
      Map(
        Transpose()
      ) o Group(neighbors) o Transpose()
    ) o Group(neighbors)
  }

  /** Asymmetrical grouping */
  def apply(relRows: Array[Int],
            relColumns: Array[Int]): Lambda = {
    Map(
      Map(
        Transpose()
      ) o Group(relColumns) o Transpose()
    ) o Group(relRows)
  }
}
