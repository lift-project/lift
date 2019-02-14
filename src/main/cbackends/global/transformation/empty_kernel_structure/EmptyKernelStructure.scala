package cbackends.global.transformation.empty_kernel_structure

import cbackends.host.host_ir.CPUFunCall
import cbackends.host.host_ir.{CPUFunCall, CPUFunc}
import ir.ast.{FunCall, Lambda}

object EmptyKernelStructure {

  def apply(lambda: Lambda) : Lambda = {
    val lowered = lambda visitAndRebuild  {
      case FunCall(cf:CPUFunc, arg) => FunCall(CPUFunCall(cf.funcName), arg)
      case x => x
    }

    lowered.asInstanceOf[Lambda]
  }

}
