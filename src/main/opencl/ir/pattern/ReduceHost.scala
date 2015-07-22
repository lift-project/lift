package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast._


case class ReduceHost(override val f: Lambda2)
extends AbstractReduce(f, Var("i")) with isGenerable {
  override def copy(f: Lambda): Pattern = ReduceHost(f)
}

object ReduceHost {
  def apply(f: Lambda2, init: Expr): Lambda1 = fun((x) => ReduceHost(f)(init, x))
}
