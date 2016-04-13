package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast._

case class LSearch(override val f: Lambda) extends AbstractSearch(f, "LSearch") with isGenerable{
  override def copy(f: Lambda): Pattern = LSearch(f)
}

object LSearch {
  def apply(f: Lambda, init: Expr) : Lambda1 = fun((x) => LSearch(f)(init, x))
}