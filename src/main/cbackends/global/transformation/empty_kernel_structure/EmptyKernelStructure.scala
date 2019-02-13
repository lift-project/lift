package cbackends.global.transformation.empty_kernel_structure

import cbackends.global.global_ir.CPUFunCall
import cbackends.host.host_ir.{CPUFunCall, CPUFunc}
import ir.ast.{FunCall, Lambda}

object EmptyKernelStructure {

  def apply(lambda: Lambda) : Lambda = {
    val lowered = lambda visitAndRebuild  {
      case FunCall(_:CPUFunc, arg) => FunCall(CPUFunCall("name"), arg)
      case x => x
    }

    lowered.asInstanceOf[Lambda]
  }

}
