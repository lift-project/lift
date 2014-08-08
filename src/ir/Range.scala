package ir

sealed abstract class Range {
  // default impl
  def *(e: ArithExpr): Range = this
  def min : ArithExpr = ?
  def max : ArithExpr = ?
}

class RangeUnkownException(msg: String) extends Exception(msg)


case class RangeAdd(val start: ArithExpr, val stop: ArithExpr, step: ArithExpr) extends Range {
  override def *(e: ArithExpr): Range = {
    RangeAdd(ExprSimplifier.simplify(start * e), ExprSimplifier.simplify(stop * e), step)
  }
  override def min = start
  override def max = stop
}
case class RangeMul(val start: ArithExpr, val stop: ArithExpr, mul: ArithExpr) extends Range {
  override def *(e: ArithExpr): Range = {
    RangeMul(ExprSimplifier.simplify(start * e), ExprSimplifier.simplify(stop * e), mul)
  }
  override def min = start
  override def max = stop
}

object ContinousRange {
  def apply(start: ArithExpr, stop: ArithExpr) = {
    RangeAdd(start, stop, Cst(1))
  }
}

case object RangeUnkown extends Range