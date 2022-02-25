package opencl.ir.pattern

import ir._
import ir.ast._
import ir.interpreter.Interpreter._

class Barrier(val local: Boolean, val global: Boolean) extends Pattern(arity = 1) {
  assert(local || global)

  override def checkType(argType: Type, setType: Boolean): Type = argType

  override def eval(valueMap: ValueMap, args: Any*): Any = args.head

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = Barrier(local, global)

  override def toString: String = s"Barrier(local=$local, global=$global)"
}

object Barrier {
  def apply(local: Boolean, global: Boolean): Barrier = new Barrier(local, global)

  def unapply(barrier: Barrier): Option[(Boolean, Boolean)] = Some((barrier.local, barrier.global))
}