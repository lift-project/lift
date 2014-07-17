package ir

sealed abstract class Range {
  // default impl
  def *(e: Expr): Range = this
  def min : Expr = ?
  def max : Expr = ?
}

class RangeUnkownException(msg: String) extends Exception(msg)


case class RangeAdd(val start: Expr, val stop: Expr, step: Expr) extends Range {
  override def *(e: Expr): Range = {
    RangeAdd(ExprSimplifier.simplify(start * e), ExprSimplifier.simplify(stop * e), step)
  }
  override def min = start
  override def max = stop
}
case class RangeMul(val start: Expr, val stop: Expr, mul: Expr) extends Range {
  override def *(e: Expr): Range = {
    RangeMul(ExprSimplifier.simplify(start * e), ExprSimplifier.simplify(stop * e), mul)
  }
  override def min = start
  override def max = stop
}

object ContinousRange {
  def apply(start: Expr, stop: Expr) = {
    RangeAdd(start, stop, Cst(1))
  }
}

case object RangeUnkown extends Range