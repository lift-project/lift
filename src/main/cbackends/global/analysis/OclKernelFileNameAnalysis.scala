package cbackends.global.analysis

import cbackends.host.host_ir.{OclFunContainer, ToGPU, ToHost}
import ir.ast.{Expr, FunCall, Lambda, Param, Value}

object OclKernelFileNameAnalysis {

  def extract(res: List[String], expr: Expr) : List[String] = {
    expr match {
      case fc@FunCall(_: OclFunContainer, args@_*) =>
        val new_res = (res /: args)(extract _)
        new_res :+ ("kernel_"+fc.gid+".cl")
      case FunCall(_:ToHost|_:ToGPU, arg) =>
        extract(res, arg)
      case _:Param|_:Value =>
        res
      case _ =>
        assert(false, "Some other patterns appear in host expression but not implemented, please implement."); List[String]()

    }
  }

  def apply(lambda: Lambda) : List[String] = {
    extract(List[String](), lambda.body)
  }

}
