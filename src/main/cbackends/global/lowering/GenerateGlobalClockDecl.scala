package cbackends.global.lowering

import cbackends.host.host_ir._
import core.generator.GenericAST.{Block, CVarWithType, ClassOrStructType, RawCode, VarDeclPure}
import ir.ast.{Expr, FunCall, Iterate, Lambda, Param, Value}

object GenerateGlobalClockDecl {


  def generate(expr: Expr, block: Block) : Block = {
    expr match {
      case fc @ FunCall(_:ToHost|_:ToGPU, arg) =>

        val measurable = fc.f.asInstanceOf[Measurable]

        val cvar_cpu_start = CVarWithType("cpu_clock_start_"+fc.gid , ClassOrStructType("std::chrono::milliseconds"))
        val cvar_cpu_end = CVarWithType("cpu_clock_end_"+fc.gid , ClassOrStructType("std::chrono::milliseconds"))

        val cvar_gpu_event = CVarWithType("event_"+fc.gid, ClassOrStructType("cl::Event"))

        generate(arg, Block(Vector(
          (if(measurable.cpu_timer) VarDeclPure(cvar_cpu_start, cvar_cpu_start.t) else RawCode("") ),
          (if(measurable.cpu_timer) VarDeclPure(cvar_cpu_end, cvar_cpu_end.t) else RawCode("") ),
          (if(measurable.gpu_timer) VarDeclPure(cvar_gpu_event, cvar_gpu_event.t) else RawCode("") )
        ), global = true) :++ block )

      case fc @ FunCall(c:OpaqueCPUFunc, args@_*) =>

        val measurable = c.cpuFun.asInstanceOf[CPUMeasurable]

        val cvar_cpu_start = CVarWithType("cpu_clock_start_"+fc.gid , ClassOrStructType("std::chrono::milliseconds"))
        val cvar_cpu_end = CVarWithType("cpu_clock_end_"+fc.gid , ClassOrStructType("std::chrono::milliseconds"))

        val block_for_this_call = Block(Vector(
          (if(measurable.cpu_timer) VarDeclPure(cvar_cpu_start, cvar_cpu_start.t) else RawCode("") ),
          (if(measurable.cpu_timer) VarDeclPure(cvar_cpu_end, cvar_cpu_end.t) else RawCode("") )
        ), global = true)

        val all_arg_blocks =  args.map(
        generate(_, Block(global = true) ) ).toList

        ( ( block_for_this_call /: all_arg_blocks) ( _ :++ _) ) :++ block

      case fc @ FunCall(o:OpaqueOclFunc, args@_*) =>

        val measurable = o.oclFun.asInstanceOf[Measurable]

        val cvar_cpu_start = CVarWithType("cpu_clock_start_"+fc.gid , ClassOrStructType("std::chrono::milliseconds"))
        val cvar_cpu_end = CVarWithType("cpu_clock_end_"+fc.gid , ClassOrStructType("std::chrono::milliseconds"))

        val cvar_gpu_event = CVarWithType("event_"+fc.gid, ClassOrStructType("cl::Event"))

        val block_for_this_call = Block(Vector(
          (if(measurable.cpu_timer) VarDeclPure(cvar_cpu_start, cvar_cpu_start.t) else RawCode("") ),
          (if(measurable.cpu_timer) VarDeclPure(cvar_cpu_end, cvar_cpu_end.t) else RawCode("") ),
          (if(measurable.gpu_timer) VarDeclPure(cvar_gpu_event, cvar_gpu_event.t) else RawCode("") )
        ), global = true)

        val all_arg_blocks =  args.map(
          generate(_, Block(global = true) ) ).toList

        ( ( block_for_this_call /: all_arg_blocks ) ( _ :++ _) ) :++ block

      case fc@FunCall(i:Iterate, arg) =>
        val arg_block = generate(arg, Block(global = true))
        val body_block = generate(i.f.body, Block(global = true))

        arg_block :++ body_block :++ block


      case _:Param|_:Value =>
        block
      case _ => assert(false, "Some other patterns appear in host expression but not implemented, please implement."); Block()

    }
  }

  def apply(lambda: Lambda) : Block = {

    generate(lambda.body, Block(global = true))


  }

}
