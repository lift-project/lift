package opencl.ir.pattern

import lift.arithmetic.Var
import ir.ast._

case class LSearch(override val f: Lambda) extends AbstractSearch(f, "LSearch") {
  override def copy(f: Lambda): Pattern = LSearch(f)
}

object LSearch {
  def apply(f: Lambda, init: Expr) : Lambda1 = fun((x) => LSearch(f)(init, x))
}
