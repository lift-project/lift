package cbackends.global.lowering

import cbackends.host.host_ir._
import core.generator.GenericAST.{AssignmentExpression, Block, CVarWithType, ClassOrStructType, DoubleType, ExpressionStatement, FunctionCall, RawCode, StringConstant, VarDeclPure, VarRefPure}
import ir.ast.{Expr, FunCall, Iterate, Lambda, Param, Value}

object GenerateCLockPrintingStmt {


  val print_stmt_prefix = "std::cout"
  val print_stmt_suffix = "std::endl"
  val sep = "<<"
  val double_quote = '"'
  val comma = double_quote+", "+double_quote
  val empty_value = "-1"

  def generate(expr: Expr, block: Block) : Block = {
    expr match {
      case fc@FunCall(_: ToHost | _: ToGPU, arg) =>

        val measurable = fc.f.asInstanceOf[Measurable]

        //1) func name
        val func_name = (fc.f match {
          case _:ToGPU => "ToGPU_"
          case _:ToHost => "ToGPU_"
        }) + fc.gid


        //2) cpu time
        val cpu_start_cvar = CVarWithType("cpu_clock_start_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
        val cpu_end_cvar = CVarWithType("cpu_clock_end_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
        val res_cpu = CVarWithType("cpu_time_ms", DoubleType())

        val cpu_time_init = VarDeclPure(res_cpu, res_cpu.t, Some(FunctionCall("cpu_time_in_ms", List(VarRefPure(cpu_start_cvar), VarRefPure(cpu_end_cvar))) )  )

        //3) gpu time
        val gpu_event_cvar = CVarWithType("event_"+fc.gid, ClassOrStructType("cl::Event"))
        val res_gpu = CVarWithType("gpu_time_ms", DoubleType())

        val gpu_time_init = VarDeclPure(res_gpu, res_gpu.t, Some(FunctionCall("gpu_time_in_ms", List(VarRefPure(gpu_event_cvar))) ) )

        //4) difference percentage
        val res_diff_percent = CVarWithType("diff_pc", DoubleType())
        val diff_percent_init = VarDeclPure(res_diff_percent, res_diff_percent.t, Some(FunctionCall("diff_percent", List(VarRefPure(res_cpu), VarRefPure(res_gpu))) ) )

        val print_for_this_call = (measurable.cpu_timer, measurable.gpu_timer) match {
          case (true, true) =>
            Block(Vector( Block(Vector(
              cpu_time_init,
              gpu_time_init,
              diff_percent_init,
              ExpressionStatement(StringConstant(
                print_stmt_prefix + sep +
                  double_quote+ func_name + double_quote + sep + comma + sep +
                  res_cpu.name + sep + comma + sep +
                  res_gpu.name + sep + comma + sep +
                  res_diff_percent.name + sep +
                  print_stmt_suffix)) )) ),
              global = true
            )
          case (true, false) =>
              Block(Vector( Block(Vector(
                cpu_time_init,
                ExpressionStatement(StringConstant(
                print_stmt_prefix + sep +
                  double_quote+ func_name + double_quote + sep + comma + sep +
                  res_cpu.name + sep + comma + sep +
                  empty_value + sep + comma + sep +
                  empty_value + sep +
                  print_stmt_suffix)) ) )),
              global = true
            )
          case (false, true) =>
            Block(Vector( Block(Vector(
              gpu_time_init,
              ExpressionStatement(StringConstant(
                print_stmt_prefix + sep +
                  double_quote+ func_name + double_quote + sep + comma + sep +
                  empty_value + sep + comma + sep +
                  res_gpu.name + sep + comma + sep +
                  empty_value + sep +
                  print_stmt_suffix)) ) )),
              global = true
            )
          case (false, false) =>
            Block(global = true)
        }

      generate(arg, print_for_this_call :++ block)

      case fc@FunCall(c: CPUFunContainer, args@_*) =>
        val arg_blocks = args.map(generate(_, Block(global = true))).toList

        val measurable = c.cpuFun.asInstanceOf[CPUMeasurable]

        //1) func name
        val func_name = "CPUFunCall_" + fc.gid

        //2) cpu time
        val cpu_start_cvar = CVarWithType("cpu_clock_start_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
        val cpu_end_cvar = CVarWithType("cpu_clock_end_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
        val res_cpu = CVarWithType("cpu_time_ms", DoubleType())

        val cpu_time_init = VarDeclPure(res_cpu, res_cpu.t, Some(FunctionCall("cpu_time_in_ms", List(VarRefPure(cpu_start_cvar), VarRefPure(cpu_end_cvar))) )  )

        val print_for_this_call = measurable.cpu_timer match {
          case true =>
            Block(Vector( Block(Vector(
              cpu_time_init,
              ExpressionStatement(StringConstant(
                print_stmt_prefix + sep +
                  double_quote+ func_name + double_quote + sep + comma + sep +
                  res_cpu.name + sep + comma + sep +
                  empty_value + sep + comma + sep +
                  empty_value + sep +
                  print_stmt_suffix)) ) ) ) ,
              global = true
            )
          case false =>
            Block(global = true)
        }

        ( (block :++ print_for_this_call) /: arg_blocks) (_ :++ _)

      case fc@FunCall(o: OclFunContainer, args@_*) =>
        val arg_blocks = args.map(generate(_, Block(global = true))).toList

        val measurable = o.oclFun.asInstanceOf[Measurable]


        //1) func name
        val func_name = "OclFunCall_" + fc.gid


        //2) cpu time
        val cpu_start_cvar = CVarWithType("cpu_clock_start_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
        val cpu_end_cvar = CVarWithType("cpu_clock_end_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
        val res_cpu = CVarWithType("cpu_time_ms", DoubleType())

        val cpu_time_init = VarDeclPure(res_cpu, res_cpu.t, Some(FunctionCall("cpu_time_in_ms", List(VarRefPure(cpu_start_cvar), VarRefPure(cpu_end_cvar))) )  )

        //3) gpu time
        val gpu_event_cvar = CVarWithType("event_"+fc.gid, ClassOrStructType("cl::Event"))
        val res_gpu = CVarWithType("gpu_time_ms", DoubleType())

        val gpu_time_init = VarDeclPure(res_gpu, res_gpu.t, Some(FunctionCall("gpu_time_in_ms", List(VarRefPure(gpu_event_cvar))) ) )

        //4) difference percentage
        val res_diff_percent = CVarWithType("diff_pc", DoubleType())
        val diff_percent_init = VarDeclPure(res_diff_percent, res_diff_percent.t, Some(FunctionCall("diff_percent", List(VarRefPure(res_cpu), VarRefPure(res_gpu))) ) )

        val print_for_this_call = (measurable.cpu_timer, measurable.gpu_timer) match {
          case (true, true) =>
            Block(Vector( Block(Vector(
              cpu_time_init,
              gpu_time_init,
              diff_percent_init,
              ExpressionStatement(StringConstant(
                print_stmt_prefix + sep +
                  double_quote+ func_name + double_quote + sep + comma + sep +
                  res_cpu.name + sep + comma + sep +
                  res_gpu.name + sep + comma + sep +
                  res_diff_percent.name + sep +
                  print_stmt_suffix)) ) ) ),
              global = true
            )
          case (true, false) =>
            Block(Vector( Block(Vector(
              cpu_time_init,
              ExpressionStatement(StringConstant(
                print_stmt_prefix + sep +
                  double_quote+ func_name + double_quote + sep + comma + sep +
                  res_cpu.name + sep + comma + sep +
                  empty_value + sep + comma + sep +
                  empty_value + sep +
                  print_stmt_suffix)) ) ) ),
              global = true
            )
          case (false, true) =>
            Block(Vector( Block(Vector(
              gpu_time_init,
              ExpressionStatement(StringConstant(
                print_stmt_prefix + sep +
                  double_quote+ func_name + double_quote + sep + comma + sep +
                  empty_value + sep + comma + sep +
                  res_gpu.name + sep + comma + sep +
                  empty_value + sep +
                  print_stmt_suffix)) ) ) ),
              global = true
            )
          case (false, false) =>
            Block(global = true)
        }

        ( (block :++ print_for_this_call ) /: arg_blocks) (_ :++ _)


      case fc@FunCall(i:Iterate, arg) =>
        val arg_block = generate(arg, Block(global = true))
        val body_block = generate(i.f.body, Block(global = true))

        arg_block :++ body_block :++ block


      case _: Param | _: Value =>
        block

      case _ => assert(false, "Some other patterns appear in host expression but not implemented, please implement."); Block()
    }
  }

  def apply(lambda: Lambda) : Block = {

    generate(lambda.body, Block(global = true))


  }

}
