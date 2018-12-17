package cbackends.host

import cbackends.common.CBackendsCompilerTrait
import core.generator.GenericAST
import core.generator.GenericAST.{Block, CVarWithType}
import ir.ast.Lambda
import lift.arithmetic.ArithExpr

object HostCompiler extends CBackendsCompilerTrait {

  override def lowerIR2CAST(lambda: Lambda, memoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr)]): GenericAST.Block = {

    Block()
  }

}
