package cbackends.global.transformation.host_ir_mapper

import cbackends.host.host_ir._
import ir.ast.{FunCall, Lambda}

object HostIRMapper {


  def apply(lambda: Lambda) : Lambda = {
    val lowered = lambda visitAndRebuild  (pre = {
      /*case fc@FunCall(cf:CPUFunc, arg) =>
        val new_funcall = FunCall(CPUFunCall(cf.funcName, cf.f.params), arg )
        new_funcall.t = fc.t
        new_funcall*/
      case fc@FunCall(cf:CPUFunc, args@_*) =>
        val new_funcall = FunCall(CPUFunContainer(cf), args:_*)
        new_funcall.t = fc.t
        new_funcall
      case fc@FunCall(cf:OclFunc, args@_*) =>
        val new_funcall = FunCall(OclFunContainer(cf), args:_*)
        new_funcall.t = fc.t
        new_funcall.gid = fc.gid
        new_funcall
      case x => x
    } )

    lowered.asInstanceOf[Lambda]
  }

}
