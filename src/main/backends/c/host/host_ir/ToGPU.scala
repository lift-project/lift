package backends.c.host.host_ir

import ir.Type
import ir.ast.{Expr, FunCall, FunDecl, IRNode}

object ToGPU {

  def apply(arg: Expr): FunCall = FunCall(ToGPU(), arg)

}

case class ToGPU(cpu_timer: Boolean = false, gpu_timer: Boolean = false) extends FunDecl(arity = 1) with Measurable {
  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = argType
}