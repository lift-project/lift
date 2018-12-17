package cbackends.sdh

import cbackends.common.CBackendsCompilerTrait
import cbackends.common.loop_var_inference.LoopVarInference
import core.generator.GenericAST
import core.generator.GenericAST.{Block, CVarWithType}
import ir.ast.Lambda
import lift.arithmetic.ArithExpr

object SDHCompiler extends CBackendsCompilerTrait{


  override def loopVarInference(lambda: Lambda): Unit = {

    println("4. overrided loop var inference by sdh called")

    val sdh_loopvar_inference = cbackends.common.loop_var_inference.LoopVarInference andThen cbackends.sdh.loop_var_inference.LoopVarInference
    sdh_loopvar_inference(lambda)

  }

  override def inputView(lambda: Lambda): Unit = {

    println("7. overrided input view by sdh called")

    cbackends.sdh.view.InputView(lambda)


  }

  override def outputView(lambda: Lambda): Unit = {

    println("8. overrided output view by sdh called")

    cbackends.sdh.view.OutputView(lambda)

  }

  override def lowerIR2CAST(lambda: Lambda, memoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr)]): GenericAST.Block = {

    //lowerIR2CASTSched()
    //lowerIR2CASTWorker()

    Block()

  }

}
