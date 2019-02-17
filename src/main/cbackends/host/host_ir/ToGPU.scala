package cbackends.host.host_ir

import ir.Type
import ir.ast.{Expr, FunCall, FunDecl, IRNode}

object ToGPU {

  def apply(arg: Expr): FunCall = FunCall(ToGPU(), arg)

}

case class ToGPU() extends FunDecl(arity = 1) {
  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = argType
}