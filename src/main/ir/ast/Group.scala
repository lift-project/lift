package ir.ast

import ir.{TypeException, ArrayType, UndefType, Type}


/**
 * Group pattern. Slice volume in overlapping tiles.
 * Code for this pattern can be generated.
 *
 * The Group pattern has the following high-level semantics:
 *   `Group([i,,1,,, ..., i,,n,,])( [x,,1,,, ..., x,,m,,] ) =
 *     [ [x,,1,,, ..., x,,n,,], ..., [x,,m-nx2,,, ..., x,,m,,] ]`
 *
 * The split pattern has the following type:
 *   `Group([i],,n,,) : [a],,m,, -> [ [a],,n,, ],,m-2 x \u2016 i \u2016,,`
 *
 * @param relIndices Array of relative indices.
 */
case class Group(relIndices: Array[Int]) extends Pattern(arity = 1) with isGenerable {
  Group.cnt += 1
  val id = Group.cnt

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
  def apply(relColumns: Array[Int],
            relRows: Array[Int]): Lambda = {
    Map(
      Map(
        Transpose()
      ) o Group(relColumns) o Transpose()
    ) o Group(relRows)
  }
}
