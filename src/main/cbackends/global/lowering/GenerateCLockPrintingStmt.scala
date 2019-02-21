package cbackends.global.lowering

import cbackends.host.host_ir._
import core.generator.GenericAST.{Block, RawCode}
import ir.ast.{Expr, FunCall, Lambda, Param, Value}

object GenerateCLockPrintingStmt {

  def generate(expr: Expr, block: Block) : Block = {
    expr match {
      case fc@FunCall(_: ToHost | _: ToGPU, arg) =>

        val measurable = fc.f.asInstanceOf[Measurable]

        /*
        (measurable.cpu_timer, measurable.gpu_timer) match {
          case (true, true) =>
          case (true, false) =>
          case (false, true) =>
          case (false, false) =>
        }*/
        Block()

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
