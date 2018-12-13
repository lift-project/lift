package cbackends.sdh.sdh_ir

import core.generator.GenericAST.CVarWithType
import ir.{Type, TypeChecker}
import ir.ast.{FPattern, IRNode, Lambda, Pattern}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{PosVar, Var}

case class TMKernel(f: Lambda, var loopVar: Var = PosVar("i"), var signature_parameters: Vector[CVarWithType] = Vector.empty[CVarWithType]
                   ) extends Pattern(f.arity) with FPattern  {

  override def checkType(argType: Type, setType: Boolean): Type = {
    TypeChecker.checkAndSetTypeForParams(f.params, argType, this)
    TypeChecker.check(f.body, setType)
  }

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)

  def copy(lambda: Lambda) = TMKernel(lambda)

  override def eval(valueMap: ValueMap, args: Any*): Any = this

}

