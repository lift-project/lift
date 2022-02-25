package cbackends.global.lowering

import java.util.function.BinaryOperator

import cbackends.common.OpenCLVersion
import cbackends.host.host_ir._
import core.generator.GenericAST
import core.generator.GenericAST.{AssignmentExpression, AstNode, BinaryExpression, BinaryExpressionT, Block, BlockMember, CVarWithType, ClassOrStructType, FunctionCall, FunctionPure, IfThenElseIm, IntConstant, MethodInvocation, ParamDeclPure, RawCode, RefType, StringConstant, StringType, TupleExpr, VarDeclPure, VoidType}
import ir.ast.{Expr, FunCall, Iterate, Lambda, Param, Value}

object GenerateOclGlobalFacility {

  def generate(expr: Expr, path: String, global_decl_cast: Block, global_init_cast: Block): (Block, Block) = {

    expr match {

      case fc@FunCall(_: OpaqueOclFunc, args@_*) =>

        //val globals_for_args = args.map( generate(_, path, global_decl_cast, global_init_cast) ).toList
        val globals_for_args = args.map( generate(_, path, Block(global = true), Block(global = true)) ).toList
        val (global_decl_for_args,global_init_for_args) = ( (Block(), Block()) /: globals_for_args) {
          (tuple1: Tuple2[Block, Block], tuple2: Tuple2[Block,Block]) => (tuple1._1 :++ tuple2._1, tuple1._2 :++ tuple2._2)
        }


        //construct global declaration
        val kernel_string_cvar = CVarWithType("kernel_string_" + fc.gid, ClassOrStructType("std::string"))
        val kernel_string_decl = VarDeclPure(kernel_string_cvar, kernel_string_cvar.t)
        val kernel_source_cvar = CVarWithType("kernel_source_" + fc.gid, ClassOrStructType("cl::Program::Sources"))
        val kernel_source_decl = VarDeclPure(kernel_source_cvar, kernel_source_cvar.t)
        val kernel_program_cvar = CVarWithType("kernel_program_" + fc.gid, ClassOrStructType("cl::Program"))
        val kernel_program_decl = VarDeclPure(kernel_program_cvar, kernel_program_cvar.t)
        val kernel_cvar = CVarWithType("kernel_" + fc.gid, ClassOrStructType("cl::Kernel"))
        val kernel_cvar_decl = VarDeclPure(kernel_cvar, kernel_cvar.t)

        val global_decl_for_this_call = Block(Vector(
          kernel_string_decl, kernel_source_decl, kernel_program_decl, kernel_cvar_decl
        ))

        //construct global init
        val kernel_string_init = AssignmentExpression(kernel_string_cvar, FunctionCall("readFile", List(StringConstant( "(pwd + " + '"' + "/kernel_" + fc.gid + ".cl" + '"' + ").c_str()"))))
        val kernel_source_init = AssignmentExpression(kernel_source_cvar, FunctionCall("cl::Program::Sources", List(IntConstant(1),
          TupleExpr( List(
            MethodInvocation(kernel_string_cvar, "c_str", List()),
            MethodInvocation(kernel_string_cvar, "length", List())
          ))
        )))
        val kernel_program_init = AssignmentExpression(kernel_program_cvar, FunctionCall("cl::Program", List( StringConstant("context") , kernel_source_cvar)))
        val kernel_build_statement = IfThenElseIm(
          BinaryExpression(
            MethodInvocation(kernel_program_cvar, "build", List(StringConstant("{ device }"))),
            BinaryExpressionT.Operator.!=,
            StringConstant("CL_SUCCESS")
          ),
          Block(Vector[AstNode with BlockMember](
            StringConstant("std::cerr << \"kernel build error\" << std::endl")) ++ (
            if (OpenCLVersion.is200) Vector[AstNode with BlockMember](
              StringConstant("char* log"),
              StringConstant("size_t logsize"),
              FunctionCall("assert", List(
                BinaryExpression(
                  FunctionCall("clGetProgramBuildInfo", List(
                    MethodInvocation(kernel_program_cvar, "get", List()),
                    StringConstant("device.get()"), StringConstant("CL_PROGRAM_BUILD_LOG"),
                    StringConstant("0"), StringConstant("NULL"), StringConstant("&logsize"))),
                  BinaryExpressionT.Operator.==,
                  StringConstant("CL_SUCCESS"))
              )),
              StringConstant("log = (char*)malloc(sizeof(char) * logsize)"),
              FunctionCall("assert", List(
                BinaryExpression(
                  FunctionCall("clGetProgramBuildInfo", List(
                    MethodInvocation(kernel_program_cvar, "get", List()),
                    StringConstant("device.get()"), StringConstant("CL_PROGRAM_BUILD_LOG"),
                    StringConstant("logsize"), StringConstant("log"), StringConstant("NULL"))),
                  BinaryExpressionT.Operator.==,
                  StringConstant("CL_SUCCESS")))))
            else Vector[AstNode with BlockMember](
              StringConstant("std::string log"),
              FunctionCall("assert", List(
                BinaryExpression(
                  MethodInvocation(kernel_program_cvar, "getBuildInfo",
                    List(StringConstant("device"), StringConstant("CL_PROGRAM_BUILD_LOG"), StringConstant("&log"))),
                  BinaryExpressionT.Operator.==,
                  StringConstant("CL_SUCCESS"))))
            )) ++ Vector[AstNode with BlockMember](
            StringConstant("std::cerr << log << std::endl"),
            StringConstant("exit(1)"))),
          Block()
        )
        val kernel_init = AssignmentExpression(
          kernel_cvar, FunctionCall("cl::Kernel", List(kernel_program_cvar, StringConstant('"'+"KERNEL"+'"')))
        )

        val global_init_for_this_call = Block(Vector(
          kernel_string_init, kernel_source_init, kernel_program_init, kernel_build_statement, kernel_init
        ))

        (global_decl_cast :++ global_decl_for_args :++ global_decl_for_this_call, global_init_cast :++ global_init_for_args :++ global_init_for_this_call)

      case FunCall(_:ToHost|_:ToGPU, arg) =>
        generate(arg, path, global_decl_cast, global_init_cast)
      case FunCall(i:Iterate, arg) =>
        val (arg_decl, arg_init) = generate(arg, path, Block(global = true), Block(global = true))
        val (body_decl, body_init) = generate(i.f.body, path, Block(global = true), Block(global = true))
        (arg_decl :++ body_decl :++ global_decl_cast, arg_init :++ body_init :++ global_init_cast)

      case FunCall(_:OpaqueCPUFunc, args@_*) => {
        val globals_for_args = args.map(generate(_, path, Block(global = true), Block(global = true))).toList
        /*val (global_decl_for_args, global_init_for_args) = ((Block(global = true), Block(global = true)) /: globals_for_args) {
          (tuple1: Tuple2[Block, Block], tuple2: Tuple2[Block, Block]) => (tuple1._1 :++ tuple2._1, tuple1._2 :++ tuple2._2)
        }*/
        //(global_decl_cast :++ global_decl_for_args , global_init_cast :++ global_init_for_args )
         ((global_decl_cast, global_init_cast) /: globals_for_args) {
          (tuple1, tuple2) => (tuple1._1 :++ tuple2._1, tuple1._2 :++ tuple2._2)
        }
        //(global_decl_cast :++ global_decl_for_args , global_init_cast :++ global_init_for_args )
      }

      case _:Param|_:Value =>
        (Block(global=true), Block(global=true))
      case _ => assert(false, "Some other patterns appear in host expression but not implemented, please implement."); (Block(), Block())
    }
  }


  def apply(lambda: Lambda, path: String) : (Block, Block) = {


    val (global_decl_cast, global_init_cast) = generate(lambda.body, path, Block(global = true), Block(global=true))

    val global_init_boilerplates = RawCode(
      """
        |	cl::Platform::get(&allPlatforms);
        | if (allPlatforms.size() == 0) {
        | std::cerr << " No platforms found. Check OpenCL installation!" << std::endl;
        | exit(1);
        | }
        |
        | platform = allPlatforms[platformId];
        |
        | platform.getDevices(CL_DEVICE_TYPE_ALL, &allDevices);
        | if (allDevices.size() == 0) {
        | std::cerr << " No devices found. Check OpenCL installation!" << std::endl;
        | exit(1);
        | }
        |
        | device = allDevices[deviceId];
        |
        | std::cerr << "Using platform: " << platform.getInfo<CL_PLATFORM_NAME>() << std::endl;
        | std::cerr << "Using device: " << device.getInfo<CL_DEVICE_NAME>() << std::endl;
        |
        | // create context
        | cl::Context tmp_context({ device });
        | context = std::move(tmp_context);
        |
        | // create queue
        | cl::CommandQueue tmp_queue(context, device, CL_QUEUE_PROFILING_ENABLE);
        | lift_queue = std::move(tmp_queue);
      """.stripMargin )

    val global_init_function = Block(Vector(FunctionPure("lift_init", VoidType(), List(ParamDeclPure("pwd", RefType(StringType()) ,true) ), global_init_boilerplates +: global_init_cast) ), global = true)

    val global_decl_boilerplates = RawCode(
      """
        |int platformId = 0;
        |int deviceId = 0;
        |
        | std::vector<cl::Platform> allPlatforms;
        | cl::Platform platform;
        | std::vector<cl::Device> allDevices;
        | cl::Device device;
        | cl::Context context;
        | cl::CommandQueue lift_queue;
        |
        | size_t lift_global_0 = 1, lift_global_1 = 1, lift_global_2 =1;
        |
      """.stripMargin)
    val global_decl_final = global_decl_boilerplates +: global_decl_cast

    (global_decl_final, global_init_function)

  }

}
