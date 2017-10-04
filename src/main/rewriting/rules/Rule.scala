package rewriting.rules

import ir.ast.Expr

case class Rule(desc: String, rewrite: PartialFunction[Expr, Expr]) {

  def isDefinedAt(expr: Expr): Boolean = rewrite.isDefinedAt(expr)

  override def toString: String = desc
}
