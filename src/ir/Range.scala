package ir

sealed abstract class Range

class RangeUnkownException(msg: String) extends Exception(msg)

object Range {
  def getMin(r: Range) = {
    r match {
      case RangeAdd(s, _, _) => s
      case RangeMul(s, _, _) => s
      case RangeUnkown => throw new RangeUnkownException("")
    }
  }

  def getMax(r: Range) = {
    r match {
      case RangeAdd(_, s, _) => s
      case RangeMul(_, s, _) => s
      case RangeUnkown => throw new RangeUnkownException("")
    }
  }
}

case class RangeAdd(val start: Expr, val stop: Expr, step: Expr) extends Range
case class RangeMul(val start: Expr, val stop: Expr, mul: Expr) extends Range

object ContinousRange {
  def apply(start: Expr, stop: Expr) = {
    RangeAdd(start, stop, Cst(1))
  }
}

case object RangeUnkown extends Range