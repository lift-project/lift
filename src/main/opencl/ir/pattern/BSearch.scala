package opencl.ir.pattern

import ir.ast._

case class BSearch(override val f: Lambda) extends AbstractSearch(f, "BSearch") {
  override def copy(f: Lambda): Pattern = BSearch(f)
}

object BSearch {
  def apply(f: Lambda, init: Expr) : Lambda1 = fun((x) => BSearch(f)(init, x))
}
