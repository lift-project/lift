package backends.c.sdh.sdh_ir


import ir.Type
import ir.ast.{Expr, FunCall, IRNode, Pattern}
import ir.interpreter.Interpreter.ValueMap

object ToLCP {
  def apply(arg: Expr): FunCall = FunCall(ToLCP(), arg)
}

case class ToLCP() extends Pattern(arity = 1) {

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = argType

  override def eval(valueMap: ValueMap, args: Any*): Any = this

}
