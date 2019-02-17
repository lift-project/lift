package cbackends.global.transformation.empty_kernel_structure

import cbackends.host.host_ir._
import ir.ast.{FunCall, Lambda}

object EmptyKernelStructure {

  def apply(lambda: Lambda) : Lambda = {
    val lowered = lambda visitAndRebuild  (pre = {
      case fc@FunCall(cf:CPUFunc, arg) =>
        val new_funcall = FunCall(CPUFunCall(cf.funcName, cf.f.params), arg )
        new_funcall.t = fc.t
        new_funcall
      case fc@FunCall(cf:CPUFunc2, args@_*) =>
        val new_funcall = FunCall(CPUFunCall2(cf.funcName, cf.f.params), args:_*)
        new_funcall.t = fc.t
        new_funcall
      case fc@FunCall(cf:OclFunc, args@_*) =>
        val new_funcall = FunCall(OclFunCall(cf.funcName, cf.f.params), args:_*)
        new_funcall.t = fc.t
        new_funcall
      case x => x
    } )

    lowered.asInstanceOf[Lambda]
  }

}
