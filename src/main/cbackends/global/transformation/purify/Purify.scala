package cbackends.global.transformation.purify

import ir.UnallocatedMemory
import ir.ast.{AbstractMap, AbstractReduce, Expr, FPattern, FunCall, Lambda, Param, Value}
import ir.view.NoView
import opencl.ir.UndefAddressSpace

object Purify {

  private def purifyOne(expr: Expr) : Unit = {

    expr.addressSpace = UndefAddressSpace
    expr.mem = UnallocatedMemory
    expr.view = NoView
    expr.outputView = NoView
  }

  def purify(expr: Expr) : Unit = {
    expr match {
      case fc@FunCall(fp:FPattern, args@_*) =>
        args.foreach( purifyOne )

        fp.f.params.foreach( purifyOne )
        purify(fp.f.body)

        /*
        fp.f match {
          case m:AbstractMap => m.loopVar =
          case m:AbstractReduce => m.loopVar =
        }*/

        purifyOne(fc)
      case fc@FunCall(_, args@_*) =>
        args.foreach( purifyOne )
        purifyOne(fc)
      case v:Value => purifyOne(v)
      case p:Param => purifyOne(p)
    }
  }

  def apply(lambda: Lambda) : Lambda = {

    lambda.params.foreach( purifyOne )

    purify(lambda.body)

    lambda
  }

}
