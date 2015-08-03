package ir.ast

import apart.arithmetic.ArithExpr
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
case class Group(relIndices: Array[Int],
                 negOutOfBoundsF: (ArithExpr, ArithExpr) => ArithExpr,
                 posOutOfBoundsF: (ArithExpr, ArithExpr) => ArithExpr) extends Pattern(arity = 1) with isGenerable {
  Group.cnt += 1
  val id = Group.cnt

  var paramType: Type = UndefType

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t, n) =>
        paramType = ArrayType(ArrayType(t, relIndices.length), n)
        paramType

      case _ => throw new TypeException(argType, "ArrayType")
    }
  }

}

object Group {
  var cnt: Int = -1

  // Predefined out-of-boundary cases
  val edgeNeg: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => 0
  val edgePos: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => len - 1
  val reflectNeg: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => -1 - idx
  val reflectPos: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => len - idx
  val wrapNeg: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => len + idx
  val wrapPos: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => idx - 1
}

object Group2D {
  def apply(relColumns: Array[Int],
            relRows: Array[Int],
            negOOB: (ArithExpr, ArithExpr) => ArithExpr,
            posOOB: (ArithExpr, ArithExpr) => ArithExpr): Lambda = {
    Map(
      Map(
        Transpose()
      ) o Group(relColumns, negOOB, posOOB) o Transpose()
    ) o Group(relRows, negOOB, posOOB)
  }
}
