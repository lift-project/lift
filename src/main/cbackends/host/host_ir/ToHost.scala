package cbackends.host.host_ir

import ir.Type
import ir.ast.{Expr, FunCall, FunDecl, IRNode}

object ToHost {

  def apply(arg: Expr): FunCall = FunCall(ToHost(), arg)

}

case class ToHost() extends FunDecl(arity = 1) {


  override def checkType(argType: Type, setType: Boolean): Type = argType

  //type MyType = this.type
  override def _visitAndRebuild(pre: IRNode => IRNode,
                                post: IRNode => IRNode): IRNode =
    this

}

