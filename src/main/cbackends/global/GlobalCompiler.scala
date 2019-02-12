package cbackends.global

import cbackends.common.CBackendsCompilerTrait
import cbackends.common.common_cast.CbackendCAST.SourceFile
import cbackends.host.HostCompiler
import cbackends.host.lowering.LowerIR2HostCAST
import core.generator.GenericAST
import core.generator.GenericAST.{Block, CVarWithType}
import ir.ast.Lambda
import lift.arithmetic.ArithExpr

object GlobalCompiler{

  def ! (lambda: Lambda, path: String, files: List[String]): Unit = {


    assert(files.length == 1, "There should be exactly one file name passed")

    //val all_cpufunc_outline_targets = outline_target_analysis(lambda)
    //val final_cpufundefs = all_cpu_outline_targets.map(LowerIR2HostCAST).map(OutlineTransformation)

    //castPrinter(List(  new SourceFile(path, file(0), final_cpufundefs :: HostCompiler ! ( empty_cpu_func(lambda) ) )) )

  }


}
