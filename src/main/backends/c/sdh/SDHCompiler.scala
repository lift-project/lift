package backends.c.sdh

import backends.c.common.CCompiler
import backends.c.common.common_cast.CbackendCAST.SourceFile
import backends.c.sdh.lowering.{LowerIR2KernelCAST, LowerIR2SchedCAST}
import backends.c.sdh.sdh_ir.{MapTM, MapTile}
import core.generator.GenericAST.CVarWithType
import ir.ast.{FunCall, IRNode, Lambda}
import lift.arithmetic.ArithExpr
import opencl.ir.OpenCLAddressSpace

object SDHCompiler extends CCompiler{


  override def loopVarInference(lambda: Lambda): Unit = {

    println("4. overrided loop var inference by sdh called")

    val sdh_loopvar_inference = backends.c.common.loop_var_inference.LoopVarInference andThen backends.c.sdh.loop_var_inference.LoopVarInference
    sdh_loopvar_inference(lambda)

  }

  override def memoryAlloc(lambda: Lambda): Unit = {

    backends.c.sdh.memory_management.MemoryAllocator(lambda)
  }

  /*
  override def finalMemoryAllocationAnalysis(lambda: Lambda): Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)] = {

    backends.c.sdh.memory_management.FinalMemoryAllocationAnalysis(lambda)

  }
  */

  override def inputView(lambda: Lambda): Unit = {

    println("7. overrided input view by sdh called")

    backends.c.sdh.view.InputView(lambda)


  }

  override def outputView(lambda: Lambda): Unit = {

    println("8. overrided output view by sdh called")

    backends.c.sdh.view.OutputView(lambda)

  }


  override def lowerIR2CAST(lambda: Lambda,
                            memoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)],
                            path: String,
                            files: List[String],
                            func_name: String
                           ): List[SourceFile] = {

    val tuple = LowerIR2SchedCAST(lambda, memoryDeclaredInSignature, func_name)
    val CAST = tuple._1
    val all_signature_cvars = tuple._2

    //kernel code generator
    //find kernel
    var kernel_lambda : Lambda = null
    var kernel_lambda_count = 0
    lambda visitBy {
      case FunCall(m:MapTile, _) =>
        kernel_lambda = Lambda.FunDefToLambda(m);
        //kernel_lambda = Lambda.FunDefToLambda(f);
        kernel_lambda_count += 1
      case _ =>
    }
    assert( kernel_lambda != null )
    assert( kernel_lambda_count == 1 )

    val CAST_kernel = LowerIR2KernelCAST(kernel_lambda, all_signature_cvars, func_name)

    assert(files.length == 2)
    List(new SourceFile(path, files(0), CAST ), new SourceFile(path, files(1), CAST_kernel) )


  }

}
