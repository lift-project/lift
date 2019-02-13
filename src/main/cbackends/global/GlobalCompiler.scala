package cbackends.global

import cbackends.common.CBackendsCompilerTrait
import cbackends.common.common_cast.CbackendCAST.SourceFile
import cbackends.global.transformation.cast_transformation.cpu_outline_transformation.OutlineTargetAnalysis
import cbackends.global.transformation.funcall2closure.FunCall2Closure
import cbackends.host.HostCompiler
import cbackends.host.lowering.LowerIR2HostCAST
import core.generator.GenericAST
import core.generator.GenericAST.{Block, CVarWithType}
import ir.ast.{FunCall, Lambda}
import lift.arithmetic.ArithExpr

object GlobalCompiler{

  def ! (lambda: Lambda, path: String, files: List[String]): Unit = {


    assert(files.length == 1, "There should be exactly one file name passed")

    val all_cpufunc_outline_targets = OutlineTargetAnalysis(lambda)
    val final_cpufundefs = all_cpufunc_outline_targets.map(FunCall2Closure.apply _)   //map( HostCompiler.!! _ ) //.map(OutlineTransformation)

    //castPrinter(List(  new SourceFile(path, file(0), final_cpufundefs :: HostCompiler ! ( empty_cpu_func(lambda) ) )) )

    println("hello")

  }


}
