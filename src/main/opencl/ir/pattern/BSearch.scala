package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast._

case class BSearch(override val f: Lambda) extends AbstractSearch(f, "BSearch") with isGenerable{
  override def copy(f: Lambda): Pattern = BSearch(f)
}

object BSearch {
  def apply(f: Lambda, init: Expr) : Lambda1 = fun((x) => BSearch(f)(init, x))
}