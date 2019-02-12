package cbackends.global

import cbackends.common.CBackendsCompilerTrait
import cbackends.common.common_cast.CbackendCAST.SourceFile
import cbackends.host.lowering.LowerIR2HostCAST
import core.generator.GenericAST
import core.generator.GenericAST.{Block, CVarWithType}
import ir.ast.Lambda
import lift.arithmetic.ArithExpr

object GlobalCompiler extends CBackendsCompilerTrait{


  override def lowerIR2CAST(lambda: Lambda,
                            memoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr)],
                            path: String,
                            files: List[String]
                           ): List[SourceFile] = {

    assert(files.length == 1, "There should be exactly one file name passed")

    //List(new SourceFile(path, files(0), LowerIR2HostCAST(lambda, memoryDeclaredInSignature) ) )
  }

}
