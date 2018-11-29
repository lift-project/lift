package host.memory_management

import ir.ast.{AbstractPartRed, FunCall, IRNode, UserFun}

/*So far not used, as type inference is doing great to make this object obsolete*/
object PropagateMemForReduce {

  def propagate(node: IRNode): Unit = {
    node match {

      case fc@FunCall(rd: AbstractPartRed, args@_*) =>
        rd.f.body.mem = fc.mem
        propagate(rd.f.body)

      case fc@FunCall(uf: UserFun, args@_*) =>


      case _ =>
    }

  }

  def apply(fc: FunCall): Unit = {

    propagate(fc)

  }

}
