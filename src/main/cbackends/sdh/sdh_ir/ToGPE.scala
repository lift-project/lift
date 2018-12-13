package cbackends.sdh.sdh_ir


import ir.Type
import ir.ast.{Expr, FunCall, FunDecl, IRNode, Pattern}
import ir.interpreter.Interpreter.ValueMap

object ToGPE {
  def apply(arg: Expr): FunCall = FunCall(ToGPE(), arg)
}

case class ToGPE() extends Pattern(arity = 1) {

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = argType

  override def eval(valueMap: ValueMap, args: Any*): Any = this

}

