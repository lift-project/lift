package test

sealed abstract class Range

case class RangeAdd(val start: Expr, val stop: Expr, step: Expr) extends Range
case class RangeMul(val start: Expr, val stop: Expr, mul: Expr) extends Range

case object RangeUnkown extends Range