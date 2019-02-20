package cbackends.host.host_ir

import ir.Type
import ir.ast.{Expr, FunCall, FunDecl, IRNode}

object ToHost {

  def apply(arg: Expr): FunCall = FunCall(ToHost(), arg)

}

case class ToHost(cpu_timer: Boolean = false, gpu_timer: Boolean = false) extends FunDecl(arity = 1) with Measurable {


  override def checkType(argType: Type, setType: Boolean): Type = argType

  //type MyType = this.type
  override def _visitAndRebuild(pre: IRNode => IRNode,
                                post: IRNode => IRNode): IRNode =
    this

}

