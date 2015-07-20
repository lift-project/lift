package opencl.ir.pattern

import arithmetic.Var
import ir.ast._

case class ReduceSeq(override val f: Lambda2)
extends AbstractReduce(f, Var("i")) with isGenerable

object ReduceSeq {
  def apply(f: Lambda2, init: Expr): Lambda1 = fun((x) => ReduceSeq(f)(init, x))
}
