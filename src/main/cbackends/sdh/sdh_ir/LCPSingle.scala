package cbackends.sdh.sdh_ir

import ir.{Type, TypeChecker}
import ir.ast.{FPattern, IRNode, Lambda, Pattern}
import ir.interpreter.Interpreter.ValueMap


case class LCPSingle(f: Lambda
                   ) extends Pattern(f.arity) with FPattern  {

  override def checkType(argType: Type, setType: Boolean): Type = {
    TypeChecker.checkAndSetTypeForParams(f.params, argType, this)
    TypeChecker.check(f.body, setType)
  }

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)

  def copy(lambda: Lambda) = TMKernel(lambda)

  override def eval(valueMap: ValueMap, args: Any*): Any = this

}
