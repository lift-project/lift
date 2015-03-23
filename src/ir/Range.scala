package ir

sealed abstract class Range {
  // default impl
  def *(e: ArithExpr): Range = this
  def min : ArithExpr = ?
  def max : ArithExpr = ?
}

class RangeUnknownException(msg: String) extends Exception(msg)

case class StartFromRange(start: ArithExpr) extends Range {
  override def *(e: ArithExpr): Range = {
    StartFromRange(ExprSimplifier.simplify(start * e))
  }
  override def min = start
  override def max = ?
}

case class GoesToRange(end: ArithExpr) extends Range {
  override def *(e: ArithExpr): Range = {
    GoesToRange(ExprSimplifier.simplify(end * e))
  }
  override def min = ?
  override def max = end
}

case class RangeAdd(start: ArithExpr, stop: ArithExpr, step: ArithExpr) extends Range {
  override def *(e: ArithExpr): Range = {
    RangeAdd(ExprSimplifier.simplify(start * e), ExprSimplifier.simplify(stop * e), step)
  }
  override def min = start
  override def max = stop
}

case class RangeMul(start: ArithExpr, stop: ArithExpr, mul: ArithExpr) extends Range {
  override def *(e: ArithExpr): Range = {
    RangeMul(ExprSimplifier.simplify(start * e), ExprSimplifier.simplify(stop * e), mul)
  }
  override def min = start
  override def max = stop
}

object ContinuousRange {
  def apply(start: ArithExpr, stop: ArithExpr) = {
    RangeAdd(start, stop, Cst(1))
  }
}

case object RangeUnknown extends Range