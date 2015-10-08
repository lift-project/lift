package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast._

case class ReduceSeq(override val f: Lambda)
  extends AbstractReduce(f, Var("i")) with isGenerable {
  assert(f.body.isConcrete)

  override def copy(f: Lambda): Pattern = ReduceSeq(f)
}

object ReduceSeq {
  def apply(f: Lambda2, init: Expr): Lambda1 = fun((x) => ReduceSeq(f)(init, x))
}
