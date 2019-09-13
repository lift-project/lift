package backends.c.onnx

import backends.c.common.CCompiler
import backends.c.common.common_cast.CbackendCAST.SourceFile
import backends.c.global.GlobalCompiler
import backends.c.host.HostCompiler
import core.generator.GenericAST.{Block, CVarWithType}
import ir.ast.Lambda
import lift.arithmetic.ArithExpr
import opencl.ir.OpenCLAddressSpace
import rewriting.LowerONNXIR

object ONNXCompiler extends CCompiler{

  override def lowerIR2CAST(lambda: Lambda,
                            memoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)],
                            path: String,
                            files: List[String],
                            func_name: String = "execute"
                           ): List[SourceFile] = {

    List(new SourceFile(path, files(0), Block() ) )

  }

  //compile a lambda
  override def compile(onnxIR: Lambda, path: String, files: List[String], func_name: String): Unit = {

    assert(files.length == 2)

    val lift_host_ir = LowerONNXIR(lambda = onnxIR, loweringRules = rewriting.onnxLoweringRules.toList)

    GlobalCompiler apply (Lambda(onnxIR.params, lift_host_ir.head._1), path, List(files(0)) )

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
