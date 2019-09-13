package backends.c.sdh.sdh_ir

import ir.Type
import ir.ast.{Expr, FunCall, IRNode, Pattern}
import ir.interpreter.Interpreter.ValueMap

object MapGPESync {
  def apply(arg: Expr): FunCall = FunCall(MapGPESync(), arg)
}

//TODO: can define a trait for this num_hw_elements for both MapGPE and MapGPESync
case class MapGPESync(val num_hw_elements: Int = 4) extends Pattern(arity = 1) {

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = argType

  override def eval(valueMap: ValueMap, args: Any*): Any = this

}

