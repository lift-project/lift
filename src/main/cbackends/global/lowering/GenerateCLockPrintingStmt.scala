package cbackends.global.lowering

import cbackends.host.host_ir.{CPUFunCall, OclFunCall, ToGPU, ToHost}
import core.generator.GenericAST.Block
import ir.ast.{Expr, FunCall, Lambda, Param, Value}

object GenerateCLockPrintingStmt {

  def generate(expr: Expr, block: Block) : Block = {
    expr match {
      case fc@FunCall(_: ToHost | _: ToGPU, arg) => Block()
      case fc@FunCall(c: CPUFunCall, args@_*) =>Block()
      case fc@FunCall(o: OclFunCall, args@_*) =>
        Block()
      case _: Param | _: Value =>
        block
      case _ => assert(false, "Some other patterns appear in host expression but not implemented, please implement."); Block()
    }
  }

  def apply(lambda: Lambda) : Block = {

    generate(lambda.body, Block(global = true))


  }

}
