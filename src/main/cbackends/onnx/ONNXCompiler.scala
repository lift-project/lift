package cbackends.onnx

import cbackends.common.CBackendsCompilerTrait
import cbackends.common.common_cast.CbackendCAST.SourceFile
import cbackends.global.GlobalCompiler
import cbackends.host.HostCompiler
import core.generator.GenericAST.{Block, CVarWithType}
import ir.ast.Lambda
import lift.arithmetic.ArithExpr
import opencl.ir.OpenCLAddressSpace
import rewriting.LowerONNXIR

object ONNXCompiler extends CBackendsCompilerTrait{

  override def lowerIR2CAST(lambda: Lambda,
                            memoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)],
                            path: String,
                            files: List[String],
                            func_name: String = "execute"
                           ): List[SourceFile] = {

    List(new SourceFile(path, files(0), Block() ) )

  }

  //compile a lambda
  override def !(onnxIR: Lambda, path: String, files: List[String], func_name: String): Unit = {

    assert(files.length == 2)

    val lift_host_ir = LowerONNXIR(lambda = onnxIR, loweringRules = rewriting.onnxLoweringRules.toList)

    GlobalCompiler ! (Lambda(onnxIR.params, lift_host_ir.head._1), path, List(files(0)) )

    /*
    println("1.traditional compiler process")

    typeCheck(lambda)
    memorySpaceInference(lambda)
    loopVarInference(lambda)
    memoryAlloc(lambda)
    val finalMemoryAllocated = finalMemoryAllocationAnalysis(lambda)
    inputView(lambda)
    outputView(lambda)

    val listOfSourceFiles = lowerIR2CAST(lambda, finalMemoryAllocated, path, files)

    castPrinter(listOfSourceFiles) */

    println("n.compiler done")

  }

}
