package cbackends.sdh

import cbackends.common.CBackendsCompilerTrait
import cbackends.common.loop_var_inference.LoopVarInference
import ir.ast.Lambda

object SDHCompiler extends CBackendsCompilerTrait{


  override def loopVarInference(lambda: Lambda): Unit = {

    println("4. overrided loop var inference by sdh called")

    val sdh_loopvar_inference = cbackends.common.loop_var_inference.LoopVarInference andThen cbackends.sdh.loop_var_inference.LoopVarInference
    sdh_loopvar_inference(lambda)

  }

  override def inputView(lambda: Lambda): Unit = {

    println("6. overrided input view by sdh called")

    cbackends.sdh.view.InputView(lambda)


  }

}
