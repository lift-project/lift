package exploration.constraint_solvers

import lift.arithmetic.{ArithExpr, Var}

import scala.language.implicitConversions

trait RelationalExpr

// First class citizen: can be nested and composed
trait FirstClassCitizen extends RelationalExpr {
  def and(others: RelationalExpr with FirstClassCitizen*): VarArityLogisticExpr = And((this +: others): _*)
  def or(others: RelationalExpr with FirstClassCitizen*): VarArityLogisticExpr = Or((this +: others): _*)
  def xor(others: RelationalExpr with FirstClassCitizen*): VarArityLogisticExpr = Xor((this +: others): _*)
}

// IfThen has to be the topmost expression
trait SecondClassCitizen extends RelationalExpr

case class Eq(left: ArithExpr, right: ArithExpr) extends RelationalExpr with FirstClassCitizen
case class Neq(left: ArithExpr, right: ArithExpr) extends RelationalExpr with FirstClassCitizen
case class Ge(left: ArithExpr, right: ArithExpr) extends RelationalExpr with FirstClassCitizen
case class Le(left: ArithExpr, right: ArithExpr) extends RelationalExpr with FirstClassCitizen

trait LogisticExpr extends RelationalExpr with FirstClassCitizen

trait VarArityLogisticExpr extends LogisticExpr {
  val exprs: Seq[RelationalExpr with FirstClassCitizen]
}

case class Not(expr: RelationalExpr with FirstClassCitizen) extends LogisticExpr with FirstClassCitizen

case class And private (exprs: RelationalExpr with FirstClassCitizen*) extends VarArityLogisticExpr {
  assert(exprs.size >= 2)
  override def and(others: RelationalExpr with FirstClassCitizen*): VarArityLogisticExpr =
    And((exprs ++ others): _*)
}

case class Or private (exprs: RelationalExpr with FirstClassCitizen*) extends VarArityLogisticExpr {
  assert(exprs.size >= 2)
  override def or(others: RelationalExpr with FirstClassCitizen*): VarArityLogisticExpr =
    Or((exprs ++ others): _*)
}

case class Xor private (exprs: RelationalExpr with FirstClassCitizen*) extends VarArityLogisticExpr {
  assert(exprs.size >= 2)
  override def xor(others: RelationalExpr with FirstClassCitizen*): VarArityLogisticExpr =
    Xor((exprs ++ others): _*)
}

case class Count(value: ArithExpr, vars: Seq[Var], lb: Int, ub: Int) extends RelationalExpr with FirstClassCitizen

case class IfThen(ifExpr: RelationalExpr with FirstClassCitizen,
                  thenExpr: RelationalExpr with FirstClassCitizen) extends SecondClassCitizen
