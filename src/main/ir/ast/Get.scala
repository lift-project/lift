package ir.ast

case class Get(n: Int) extends Pattern(arity = 1) with isGenerable

object Get {
  def apply(e: Expr, n: Int): Expr = Get(n)(e)
}