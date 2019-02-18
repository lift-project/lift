package cbackends.global

import cbackends.common.CBackendsCompilerTrait
import cbackends.common.common_cast.CbackendCAST.SourceFile
import cbackends.global.lowering.GenerateOclGlobalFacility
import cbackends.global.transformation.cast_transformation.cpu_outline_transformation.{CPUOutlineTargetAnalysis, OclOutlineTargetAnalysis}
import cbackends.global.transformation.empty_kernel_structure.EmptyKernelStructure
import cbackends.global.transformation.funcall2closure.FunCall2Closure
import cbackends.global.transformation.unique_user_func.UniqueUserFunc
import cbackends.host.HostCompiler
import cbackends.host.lowering.LowerIR2HostCAST
import core.generator.GenericAST
import core.generator.GenericAST.{Block, CVarWithType}
import ir.ast.{FunCall, Lambda}
import lift.arithmetic.ArithExpr

object GlobalCompiler{

  def ! (lambda: Lambda, path: String, files: List[String]): Unit = {


    assert(files.length == 1, "There should be exactly one file name passed")


    HostCompiler.typeCheck(lambda)

    val all_cpufunc_outline_targets = CPUOutlineTargetAnalysis(lambda)

    val all_oclfunc_outline_targets = OclOutlineTargetAnalysis(lambda)

    val emptified_lambda = EmptyKernelStructure(lambda)

    //-----------------------------------------------------------------//
    //DO ALL TRANSFORMATION ABOVE, FROM HERE, NO TRANSFORMATION ALLOWED
    //-----------------------------------------------------------------//

    val cpufundefs = all_cpufunc_outline_targets.map( HostCompiler.!! _ )
    val final_cpufundefs = UniqueUserFunc(cpufundefs)

    //TODO: need a way to specify the file name, but can do later at CAST printer, here only CAST returned, good!
    val oclfundefs = all_oclfunc_outline_targets.map( opencl.executor.Compile.!! _ )
    val final_oclfundefs = UniqueUserFunc(oclfundefs)

    val (global_val_decl_cast, global_val_init_cast) = GenerateOclGlobalFacility(emptified_lambda)
    val top_cast = HostCompiler !! emptified_lambda

    HostCompiler.castPrinter(List(  new SourceFile(path, files(0), Block(Vector(LowerIR2HostCAST.boilerplate_code), global = true) :+ global_val_decl_cast :+ global_val_init_cast :+ ( Block( final_cpufundefs.toVector, global = true) :: top_cast  )) ) )

    println("hello")

  }


}
