package cbackends.host.lowering

import cbackends.common.common_ir.{CPUMainMemoryAddressSpace, Concat}
import cbackends.common.utils.type_lowering.TypeLowering
import cbackends.host.host_ir._
import core.generator.GenericAST.{ArithExpression, AssignmentExpression, AstNode, BinaryExpression, BinaryExpressionT, Block, BlockMember, CVarWithType, ClassOrStructType, Comment, EmptyNode, ExpressionStatement, FloatType, ForLoopIm, FunctionCall, FunctionPure, IfThenElifIm, IfThenElseIm, IntConstant, IntegerType, MethodInvocation, MutableBlock, ObjectDecl, ParamDeclPure, PrimitiveTypeT, RawCode, RefType, StringConstant, TypeDef, TypeDefHost, UnaryExpression, VarDeclPure, VarRef, VarRefPure, VoidType}
import ir.{TupleType, Type}
import ir.ast.{Iterate, Zip}
import opencl.generator.NDRange
import opencl.ir.pattern.{MapGlb, MapWrg, ScanSeq}
import opencl.ir.{GlobalMemory, OpenCLAddressSpace}

import scala.collection.mutable.ArrayBuffer
//import host_obsolete.ir_host.MapHSeq
//import host_obsolete.view.ViewPrinter
import ir.ast.{AbstractMap, AbstractPartRed, FunCall, IRNode, Join, Lambda, Slide, Split, Transpose, TransposeW, UserFun, Value}
import lift.arithmetic.{ArithExpr, Cst}
import opencl.generator.OpenCLAST.OclCode
import opencl.ir.pattern.{MapSeq, ReduceSeq}
import cbackends.common.view.ViewPrinter

import scala.collection.mutable

object LowerIR2HostCAST {

  val boilerplate_code = ExpressionStatement(RawCode(
    """
      |#include <bits/stdc++.h>
      |
      |using namespace std;
      |
    """.stripMargin), true )

  val ocl_boilerplate_code = ExpressionStatement(RawCode(
    """
      |#include <iostream>
      |#include <CL/cl2.hpp>
      |#include <fstream>
      |
      |std::string readFile(const char *filename){
      |
      |  std::ifstream in(filename, std::ios::in);
      |
      |  if (in.fail())
      |  {
      |  std::cerr << "Error reading file " << filename << std::endl;
      |  exit(1); }
      |
      |  std::string contents;
      |  in.seekg(0, std::ios::end);
      |  contents.resize(in.tellg());
      |  in.seekg(0, std::ios::beg);
      |  in.read(&contents[0], contents.size());
      |  in.close();
      |  return contents;
      |  }
      |
    """.stripMargin), true)

  val cpu_clock = StringConstant("std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch())")

  val sync = RawCode("assert(lift_queue.finish() == CL_SUCCESS)")

  def generate(node:IRNode): Block = {
    //lots of pattern matching code
    node match {
      case lambda@Lambda(_,_,_) =>
        generate(lambda.body)
      case fc@FunCall(lambda@Lambda(_,_,_), _) =>
        generate(lambda.body)
      case fc@FunCall(_:ir.ast.Map, _) =>
        generateNothing(fc)
      case fc@FunCall(_:AbstractMap, _) =>
        generateAbstractMap(fc)
      case fc@FunCall(_:ReduceSeq, _*) =>
        generateReduceSeq(fc)
      case fc@FunCall(Split(_), _ ) =>
        generateNothing(fc)
      case fc@FunCall(Join(), _) =>
        generateNothing(fc)
      case fc@FunCall(Slide(_,_), _ ) =>
        generateNothing(fc)
      case fc@FunCall(_:Concat, _* ) =>
        generateNothing(fc)
      case fc@FunCall(Transpose(), _) =>
        generateNothing(fc)
      case fc@FunCall(TransposeW(), _) =>
        generateNothing(fc)
      case fc@FunCall(_:UserFun,_*) =>
        generateUserFun(fc)
      /*case fc@FunCall(_:CPUFunCall,_) =>
        generateCPUFunCall(fc)*/
      case fc@FunCall(_:CPUFunCall, _*) =>
        generateCPUFunCall(fc)
      case fc@FunCall(_:OclFunCall, _*) =>
        generateOclFunCall(fc)
      case fc@FunCall(_:ToHost, _*) =>
        generateDataTransfer(fc)
      case fc@FunCall(_:ToGPU, _*) =>
        generateDataTransfer(fc)
      case fc@FunCall(_:Iterate, _) =>
        generateIterate(fc)
      case fc@FunCall(_:ScanSeq, _*) =>
        generateScanSeq(fc)
      case fc@FunCall(_:Zip, _*) =>
        generateNothing(fc)
      case _ =>
        Block()
    }

  }

  private def generateScanSeq(fc: FunCall) : Block = {

    val arg_block = generate(fc.args(1))

    val scan = fc.f.asInstanceOf[ScanSeq]
    val stop = scan.loopVar.range.max

    val indexVar =  CVarWithType(scan.loopVar.toString, IntegerType() )
    val init = VarDeclPure( indexVar, indexVar.t, Some(IntConstant(0)) )
    val cond = BinaryExpression(VarRefPure(indexVar), BinaryExpressionT.Operator.<=, ArithExpression(stop) )
    val increment = UnaryExpression("++", (indexVar) )


    val comment = fc.f match {
      case _:ScanSeq => Comment("For each element scanned sequentially")
      case _ => assert(false, "Not implemented"); Comment("Not reachable")
    }

    val assignment = {
      generate(scan.f.body).content(0) match {
        case ExpressionStatement(x,_) => x
        case y => assert(false,"Not implemented");null
      } }.asInstanceOf[AssignmentExpression]

    val userfuncall = assignment.value.asInstanceOf[FunctionCall]
    val init_value = userfuncall.args(0)

    val accumulator = CVarWithType("scan_acc_"+fc.gid, TypeLowering.GetElementTypeFromPointer(TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(fc.t)) )  )
    //val init_assignment = AssignmentExpression(VarRefPure(accumulator), init_value)
    val init_assignment = VarDeclPure(accumulator, accumulator.t, Some(init_value) )

    val input_array = assignment.value.asInstanceOf[FunctionCall].args(1)
    val acc_assignment = AssignmentExpression(VarRefPure(accumulator), FunctionCall(userfuncall.name, List(VarRefPure(accumulator), input_array ) ) )
    val array_elem_update =AssignmentExpression(assignment.to, VarRefPure(accumulator))

    arg_block :+ comment :+ init_assignment :+ ForLoopIm( init, cond, increment, Block(Vector(acc_assignment, array_elem_update)) )

/*
    val tuple_args = funcall.args.tail
    val inloop_assignment = AssignmentExpression(assignment.to, FunctionCall(funcall.name, assignment.to :: tuple_args))
    val inloop_assignment_block = Block(Vector(ExpressionStatement(inloop_assignment)))

    arg_block :+ comment :+ init_assignment :+ ForLoopIm( init, cond, increment, inloop_assignment_block )
    */

    //Block()

  }

  private def generateIterate(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val i = fc.f.asInstanceOf[Iterate]
    val i_cvar = CVarWithType(i.loopVar.toString, IntegerType())


    val kernel_type = i.f match {
      case l:Lambda => l.body match {
        case fc2:FunCall => fc2.f match {
          case _:OclFunCall => "GPU"
          case _:CPUFunCall => "CPU"
          case _ => assert(false, "Not implemented"); "Invalid"
        }
        case _ => assert(false, "Not implemented"); "Invalid"
      }
      case _ => assert(false, "Not implemented"); "Invalid"
    }

    val body = generate(i.f.body)

    val cond_1 = BinaryExpression(VarRefPure(i_cvar), BinaryExpressionT.Operator.==, IntConstant(0))
    val cond_2 = BinaryExpression(VarRefPure(i_cvar), BinaryExpressionT.Operator.==, ArithExpression(i.loopVar.range.max) )
    val cond_3 = BinaryExpression(BinaryExpression(VarRefPure(i_cvar), BinaryExpressionT.Operator.%,  IntConstant(2)),
      BinaryExpressionT.Operator.==, IntConstant(1) )
    val cond_4 = BinaryExpression(BinaryExpression(VarRefPure(i_cvar), BinaryExpressionT.Operator.%,  IntConstant(2)),
      BinaryExpressionT.Operator.==, IntConstant(0) )

    val init = VarDeclPure( i_cvar, i_cvar.t, Some(IntConstant(0)) )
    val cond = BinaryExpression(VarRefPure(i_cvar), BinaryExpressionT.Operator.<=, ArithExpression(i.loopVar.range.max) )
    val increment = UnaryExpression("++", (i_cvar) )

    val cast_for_this_call = kernel_type match {
      case "CPU" =>

        //assume that Iterate always contain either CPUFunCall or OclFunCall,
        //thus you can extract the function call
        var funcall_tmp : FunctionCall = null
        body visitBy {
          case ExpressionStatement(fc@FunctionCall(_,_,_),_) => funcall_tmp = fc
          case _ =>
        }
        assert(funcall_tmp != null, "Can not find funcall in Iterate")
        val funcall = funcall_tmp
        val in :: out :: size :: Nil = funcall.args

        //assume Iterate is a unary function, thus you know the funcall only have one input and one output

        val buffer1_cvar = CVarWithType("cpu_buffer1", TypeLowering.Array2Pointer( TypeLowering.IRType2CastType( fc.args.head.t), true ) )
        val buffer2_cvar = CVarWithType("cpu_buffer2", TypeLowering.Array2Pointer( TypeLowering.IRType2CastType( fc.args.head.t), true ) )
        val buffer1_decl = VarDeclPure(buffer1_cvar, buffer1_cvar.t, Some(FunctionCall("reinterpret_cast", List(
              FunctionCall("malloc", List(BinaryExpression(ArithExpression(Type.getElementCount(fc.args.head.t)), BinaryExpressionT.Operator.*,
                FunctionCall("sizeof", List(TypeLowering.GetElementTypeFromPointer(buffer1_cvar.t)))
              )))), List(buffer1_cvar.t)) ) )
        val buffer2_decl = VarDeclPure(buffer2_cvar, buffer2_cvar.t, Some(FunctionCall("reinterpret_cast", List(
              FunctionCall("malloc", List(BinaryExpression(ArithExpression(Type.getElementCount(fc.args.head.t)), BinaryExpressionT.Operator.*,
                FunctionCall("sizeof", List(TypeLowering.GetElementTypeFromPointer(buffer2_cvar.t)))
              )))), List(buffer2_cvar.t)) ) )

        val funcall_1 = Block(Vector(funcall.copy( args = List(in, VarRefPure(buffer1_cvar), size) ) ) )
        val funcall_2 = Block(Vector(
          IfThenElseIm(
            cond_4,
            Block( Vector(funcall.copy( args = List(VarRefPure(buffer1_cvar), out, size) ) ) ),
            Block( Vector(funcall.copy( args = List(VarRefPure(buffer2_cvar), out, size) ) ) )
          )
        ) )
        val funcall_3 = Block(Vector( funcall.copy( args = List(VarRefPure(buffer1_cvar), VarRefPure(buffer2_cvar), size) ) ) )
        val funcall_4 = Block(Vector( funcall.copy( args = List(VarRefPure(buffer2_cvar), VarRefPure(buffer1_cvar), size) ) ) )


        val if_else = Block(Vector(IfThenElifIm(List(cond_1, cond_2, cond_3, cond_4), List(funcall_1, funcall_2, funcall_3, funcall_4), Block() ) ) )

        val forloop = ForLoopIm(init,cond,increment,if_else)

        Block(Vector(buffer1_decl, buffer2_decl, forloop), global = true)

      case "GPU" =>

        var funcall_tmp : MethodInvocation = null
        body visitBy {
          case ExpressionStatement(fc@MethodInvocation(_,"enqueueNDRangeKernel",_,_),_) => funcall_tmp = fc
          case _ =>
        }
        assert(funcall_tmp != null, "Can not find funcall in Iterate")
        val funcall = funcall_tmp

        val arg_list : ArrayBuffer[MethodInvocation] = ArrayBuffer.empty[MethodInvocation]
        body visitBy {
          case ExpressionStatement(fc@MethodInvocation(_,"setArg",_,_),_) => arg_list += fc
          case _ =>
        }
        val args = arg_list.toList
        val args_sorted = args.sortWith {
          (arg1, arg2) => {
            val MethodInvocation(_, _, IntConstant(order1) :: _ , _) = arg1
            val MethodInvocation(_, _, IntConstant(order2) :: _ , _) = arg2
            order1 < order2
          }
        }
        val in_setarg :: out_setarg :: size_setarg :: Nil = args_sorted
        val MethodInvocation(_,_, _ :: in :: Nil, _) = in_setarg
        val MethodInvocation(_,_, _ :: out :: Nil, _) = out_setarg
        val MethodInvocation(_,_, _ :: size :: Nil, _) = size_setarg

        val buffer1_cvar = CVarWithType("gpu_buffer1", ClassOrStructType("cl::Buffer"))
        val buffer2_cvar = CVarWithType("gpu_buffer2", ClassOrStructType("cl::Buffer"))
        val buffer1_decl = ObjectDecl(buffer1_cvar, buffer1_cvar.t,
          List(
            StringConstant("context"),
            StringConstant("CL_MEM_READ_WRITE"),
            BinaryExpression(ArithExpression(Type.getElementCount(fc.args.head.t)), BinaryExpressionT.Operator.*,
              FunctionCall("sizeof", List(TypeLowering.GetElementTypeFromPointer(  TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(fc.args.head.t))  ))) )
          )
        )
        val buffer2_decl = ObjectDecl(buffer2_cvar, buffer2_cvar.t,
          List(
            StringConstant("context"),
            StringConstant("CL_MEM_READ_WRITE"),
            BinaryExpression(ArithExpression(Type.getElementCount(fc.args.head.t)), BinaryExpressionT.Operator.*,
              FunctionCall("sizeof", List(TypeLowering.GetElementTypeFromPointer(TypeLowering.GetElementTypeFromPointer(  TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(fc.args.head.t))  )))) )
          )
        )

        val setArg1_buffer1 = out_setarg.copy(args = List(IntConstant(1), VarRefPure(buffer1_cvar)))
        val funcall_1 = Block(Vector(in_setarg, setArg1_buffer1, size_setarg, funcall) )
        val setArg0_buffer1 = out_setarg.copy(args = List(IntConstant(0), VarRefPure(buffer1_cvar)))
        val setArg0_buffer2 = out_setarg.copy(args = List(IntConstant(0), VarRefPure(buffer2_cvar)))
        val last_iter_in_arg =
          IfThenElseIm(
            cond_4,
            Block(Vector(setArg0_buffer1)),
            Block(Vector(setArg0_buffer2))
          )
        val funcall_2 = Block(Vector( last_iter_in_arg, out_setarg, size_setarg, funcall) )
        val setArg1_buffer2 = out_setarg.copy(args = List(IntConstant(1), VarRefPure(buffer2_cvar)))
        val funcall_3 = Block(Vector(setArg0_buffer1, setArg1_buffer2, size_setarg, funcall) )
        val funcall_4 = Block(Vector(setArg0_buffer2, setArg1_buffer1, size_setarg, funcall) )

        val if_else = Block(Vector(IfThenElifIm(List(cond_1, cond_2, cond_3, cond_4), List(funcall_1, funcall_2, funcall_3, funcall_4), Block() ) ) )

        val forloop = ForLoopIm(init,cond,increment,if_else)

        Block(Vector(buffer1_decl, buffer2_decl, forloop), global = true)

    }

    //Block()

    arg_block :++ cast_for_this_call


  }

  private def generateDataTransfer(fc: FunCall) : Block = {
    //parameter sequnence convention: first input pointers, then output pointers, then sizes

    val arg_block = generate(fc.args.head)

    val measurable = fc.f.asInstanceOf[Measurable]

    val eventCVar = CVarWithType("event_"+fc.gid, ClassOrStructType("cl::Event"))
    //val eventDecl = VarDeclPure( eventCVar, eventCVar.t  )

    val in_arg = fc.args.head
    val in = CVarWithType(in_arg.mem.variable.toString, TypeLowering.IRType2CastType(in_arg.t))
    val out = CVarWithType(fc.mem.variable.toString, TypeLowering.IRType2CastType(fc.t))

    val enqueue_cast =
      fc.f match {
        case _:ToGPU => ExpressionStatement(MethodInvocation(
          StringConstant("lift_queue"),
          "enqueueWriteBuffer",
          List(
            VarRefPure(out),
            StringConstant("CL_TRUE"),
            IntConstant(0),
            BinaryExpression(ArithExpression(Type.getElementCount(in_arg.t)), BinaryExpressionT.Operator.*,
              FunctionCall("sizeof", List(TypeLowering.GetElementTypeFromArray(in.t))) ),
            VarRefPure(in),
            StringConstant("NULL"),
            if(measurable.gpu_timer) UnaryExpression("&", VarRefPure(eventCVar)) else StringConstant("NULL")
          )
        ) )
        case _:ToHost => ExpressionStatement(MethodInvocation(
          StringConstant("lift_queue"),
          "enqueueReadBuffer",
          List(
            VarRefPure(in),
            StringConstant("CL_TRUE"),
            IntConstant(0),
            BinaryExpression(ArithExpression(Type.getElementCount(in_arg.t)), BinaryExpressionT.Operator.*,
              FunctionCall("sizeof", List(TypeLowering.GetElementTypeFromArray(in.t))) ),
            VarRefPure(out),
            StringConstant("NULL"),
            if(measurable.gpu_timer) UnaryExpression("&", VarRefPure(eventCVar)) else StringConstant("NULL")
          )
        ) )
      }


    val cpu_start_clock_cvar = CVarWithType("cpu_clock_start_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
    val cpu_start_clock = AssignmentExpression(cpu_start_clock_cvar, cpu_clock )
    val cpu_end_clock_cvar = CVarWithType("cpu_clock_end_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
    val cpu_end_clock = AssignmentExpression(cpu_end_clock_cvar, cpu_clock )

    /*val block_for_this_call = measurable.gpu_timer match {

      case true => Block(Vector(eventDecl, enqueue_cast), global = true)
      case false => Block(Vector(enqueue_cast), global = true)

    }*/
    val block_for_this_call = Block(Vector(
      (if(measurable.cpu_timer) cpu_start_clock else RawCode("") ) ,
      enqueue_cast,
      //(if(measurable.cpu_timer || measurable.gpu_timer) sync else RawCode("")),
      sync,
      (if(measurable.cpu_timer) cpu_end_clock else RawCode(""))
      ), global = true)

    arg_block :++ block_for_this_call


  }



  private def generateOclFunCall(fc: FunCall) : Block = {
    // parameter sequence convention: first input pointers, then output pointers,
    // then intermediate global buffers, then sizes

    val arg_blocks = fc.args.map(generate(_) )

    val cfc = fc.f.asInstanceOf[OclFunCall]
    val measurable = cfc.oclFun.asInstanceOf[Measurable]


    //(1) set arg

    val input_args = fc.args.map( arg => CVarWithType(arg.mem.variable.toString, TypeLowering.IRType2CastType(arg.t) ) ).toList
    // Ordering of the intermediate buffers below mirrors that in OpenCL generator

    val output_arg = CVarWithType(fc.mem.variable.toString, TypeLowering.IRType2CastType(fc.t))

    val intermediate_global_buffer_args = cfc.intermediateGlobalMem.
      map(typedMem => CVarWithType(typedMem.mem.variable.toString, TypeLowering.IRType2CastType(typedMem.t)))

    val sizes = cfc.oclFun.f.params.flatMap(p =>
      ArithExpr.collectVars(p.mem.size)).map(p =>
      CVarWithType(p.toString, IntegerType())).distinct
    val all_args = ((input_args :+ output_arg) ++ intermediate_global_buffer_args) ++ sizes
    val arg_id = (0 until all_args.length).toList

    //  rebuild kernel cvar
    val kernel_cvar = CVarWithType("kernel_" + fc.gid, ClassOrStructType("cl::Kernel"))

    val set_all_args : List[ AstNode with BlockMember ] = (all_args zip arg_id).map{
      case (cvar:CVarWithType, id:Int) =>
        ExpressionStatement(MethodInvocation(kernel_cvar, "setArg", List(IntConstant(id), cvar)))
    }

    val local_thread_setting : NDRange = cfc.oclFun.ndranges._1
    val global_thread_setting : NDRange = cfc.oclFun.ndranges._2

    //(2) enqueue kernel
    val eventCVar = CVarWithType("event_"+fc.gid, ClassOrStructType("cl::Event"))
    //val eventDecl = VarDeclPure( eventCVar, eventCVar.t  )
    val enqueue_cast = ExpressionStatement(MethodInvocation(
      StringConstant("lift_queue"),
      "enqueueNDRangeKernel",
      List(
        kernel_cvar,
        StringConstant("cl::NullRange"),
        StringConstant("cl::NDRange("+global_thread_setting.toString+")"), //global
        StringConstant(
          if (local_thread_setting != null) "cl::NDRange("+local_thread_setting.toString+")"
          else "cl::NullRange"), //local
        StringConstant("NULL"),
        if(measurable.gpu_timer) UnaryExpression("&", VarRefPure(eventCVar)) else StringConstant("NULL")
      )
    ) )

    /*
    val block_for_this_call = measurable.gpu_timer match {
      case true => set_all_args :+ eventDecl :+ enqueue_cast
      case false => set_all_args :+ enqueue_cast
    }*/

    val cpu_start_clock_cvar = CVarWithType("cpu_clock_start_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
    //val cpu_start_clock = VarDeclPure(cpu_start_clock_cvar, cpu_start_clock_cvar.t, Some(cpu_clock) )
    val cpu_start_clock = ExpressionStatement(AssignmentExpression(cpu_start_clock_cvar, cpu_clock ) )
    val cpu_end_clock_cvar = CVarWithType("cpu_clock_end_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
    //val cpu_end_clock = VarDeclPure(cpu_end_clock_cvar, cpu_end_clock_cvar.t, Some(cpu_clock) )
    val cpu_end_clock = ExpressionStatement(AssignmentExpression(cpu_end_clock_cvar, cpu_clock ) )

    val block_for_this_call = (set_all_args :+ (if(measurable.cpu_timer) cpu_start_clock else RawCode("") ) ) :+
      enqueue_cast :+
      ExpressionStatement(if(measurable.cpu_timer || measurable.gpu_timer) sync else RawCode("")) :+
      (if(measurable.cpu_timer) cpu_end_clock else RawCode(""))


    Block(arg_blocks.toVector, global = true) :++ Block( block_for_this_call.asInstanceOf[List[AstNode with BlockMember]].toVector, global = true)


  }

  private def generateCPUFunCall(fc: FunCall) : Block = {
    //parameter sequence convention: first input pointers, then output pointers, then sizes

    val arg_blocks = fc.args.map( generate(_) )

    val cfc = fc.f.asInstanceOf[CPUFunCall]
    val measurable = cfc.cpuFun.asInstanceOf[CPUMeasurable]

    val input_args = fc.args.map( arg => CVarWithType(arg.mem.variable.toString, TypeLowering.IRType2CastType(arg.t) ) ).toList

    val output_arg = CVarWithType(fc.mem.variable.toString, TypeLowering.IRType2CastType(fc.t))

    val intermediate_global_buffer_args = cfc.intermediateGlobalMem.
      map(typedMem => CVarWithType(typedMem.mem.variable.toString, TypeLowering.IRType2CastType(typedMem.t))).toList

    val sizes = cfc.cpuFun.f.params.flatMap(p =>
      ArithExpr.collectVars(p.mem.size)).map(p =>
      CVarWithType(p.toString, IntegerType())).distinct

    val fc_cast = FunctionCall(cfc.cpuFun.funcName,
      input_args ::: output_arg :: (intermediate_global_buffer_args ::: sizes.toList ) )


    val cpu_start_clock_cvar = CVarWithType("cpu_clock_start_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
    val cpu_start_clock = AssignmentExpression(cpu_start_clock_cvar, cpu_clock )
    val cpu_end_clock_cvar = CVarWithType("cpu_clock_end_"+fc.gid, ClassOrStructType("std::chrono::milliseconds"))
    val cpu_end_clock = AssignmentExpression(cpu_end_clock_cvar, cpu_clock )

    Block(arg_blocks.toVector, global = true) :++ Block( Vector(
      (if(measurable.cpu_timer) cpu_start_clock else RawCode("") ),
      fc_cast,
      (if(measurable.cpu_timer) cpu_end_clock else RawCode("") )
    ), global = true)


  }

  /*
  private def generateCPUFunCall(fc: FunCall) : Block = {
    //parameter sequnence convention: first input pointers, then output pointers, then sizes

    val arg_block = generate(fc.args.head)

    val cfc = fc.f.asInstanceOf[CPUFunCall]

    val input_arg = CVarWithType(fc.args.head.mem.variable.toString, TypeLowering.IRType2CastType(fc.args.head.t) )
    val output_arg = CVarWithType(fc.mem.variable.toString, TypeLowering.IRType2CastType(fc.t))
    val sizes = cfc.params.flatMap(p => ArithExpr.collectVars(p.mem.size)).map(p => CVarWithType(p.toString, IntegerType())).distinct

    val fc_block = FunctionCall(cfc.funcName, input_arg :: (output_arg :: sizes.toList ) )

    arg_block :+ fc_block


  }
  */

  private def generateUserFun(fc: FunCall) : Block = {

    //val argBlock = generate(fc.args(0))
    val mutableArgBlock = MutableBlock()
    fc.args.foreach(mutableArgBlock :++ generate(_))
    val argBlock = mutableArgBlock.toBlock

    //should emit a global function decl
    val uf = fc.f.asInstanceOf[UserFun]

    val arg_list : List[AstNode] = fc.args.map(a => a match {
      case v:Value => StringConstant(v.value)
      case _ =>
        ViewPrinter(a.view)
    }).toList

    println("only for break point")

    val out_offset = ViewPrinter(fc.outputView)

    println("only for break point")

    val userfunc_apply = AssignmentExpression( out_offset , FunctionCall(uf.name, arg_list) )

    argBlock :+ userfunc_apply

  }

  def generateAbstractMap(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val m = fc.f.asInstanceOf[AbstractMap]
    val stop = m.loopVar.range.max

    val indexVar =  CVarWithType(m.loopVar.toString, IntegerType() )
    val init = VarDeclPure( indexVar, indexVar.t, Some(IntConstant(0)) )
    val cond = BinaryExpression(VarRefPure(indexVar), BinaryExpressionT.Operator.<=, ArithExpression(stop) )
    val increment = UnaryExpression("++", (indexVar) )


    val comment = fc.f match {
      case _:MapSeq => Comment("For each element processed sequentially")
      case _:ir.ast.Map => Comment("For each element processed lazily and sequentially")
      //case _:MapHSeq => Comment("For each element processed sequentially")
      case _ => assert(false, "Not implemented"); Comment("Not reachable")
    }


    arg_block :+ comment :+ ForLoopIm( init, cond, increment, generate(m.f.body) )

  }

  def generateReduceSeq(fc: FunCall) : Block = {

    val arg_block = generate(fc.args(1))

    val rd = fc.f.asInstanceOf[ReduceSeq]
    val stop = rd.loopVar.range.max

    val indexVar =  CVarWithType(rd.loopVar.toString, IntegerType() )
    val init = VarDeclPure( indexVar, indexVar.t, Some(IntConstant(0)) )
    val cond = BinaryExpression(VarRefPure(indexVar), BinaryExpressionT.Operator.<=, ArithExpression(stop) )
    val increment = UnaryExpression("++", (indexVar) )

    val comment = fc.f match {
      case _:ReduceSeq => Comment("For each element reduced sequentially")
      case _ => assert(false, "Not implemented"); Comment("Not reachable")
    }

    val assignment = {
      generate(rd.f.body).content(0) match {
        case ExpressionStatement(x,_) => x
        case y => assert(false,"Not implemented");null
      } }.asInstanceOf[AssignmentExpression]

    val funcall = assignment.value.asInstanceOf[FunctionCall]
    val init_value = funcall.args(0)

    val init_assignment = AssignmentExpression(assignment.to, init_value)

    val tuple_args = funcall.args.tail
    val inloop_assignment = AssignmentExpression(assignment.to, FunctionCall(funcall.name, assignment.to :: tuple_args))
    val inloop_assignment_block = Block(Vector(ExpressionStatement(inloop_assignment)))

    arg_block :+ comment :+ init_assignment :+ ForLoopIm( init, cond, increment, inloop_assignment_block )

    /*
    val indexVar =  CVarWithType(rd.loopVar.toString, IntegerType() )
    val init = VarDeclPure( indexVar, indexVar.t, Some(IntConstant(1)) )
    val cond = BinaryExpression(VarRefPure(indexVar), BinaryExpressionT.Operator.<=, ArithExpression(stop) )
    val increment = UnaryExpression("++", (indexVar) )

    val comment = fc.f match {
      case _:ReduceSeq => Comment("For each element reduced sequentially")
      case _ => assert(false, "Not implemented"); Comment("Not reachable")
    }

    val test = generate(rd.f.body)
    val test1 = test.content(0)
    val assignment = {
      generate(rd.f.body).content(0) match {
        case ExpressionStatement(x,_) => x
        case y => assert(false,"Not implemented");null
      } }.asInstanceOf[AssignmentExpression]

    val funcall = assignment.value.asInstanceOf[FunctionCall]
    val init_value = funcall.args(0)
    val tuple_args = funcall.args.tail
    val tuple_args_for_first_iter = tuple_args.map({case VarRef(v,suffix,_) => VarRef(v,suffix, Some(ArithExpression(Cst(0))))})
    val init_assignment = AssignmentExpression(assignment.to, FunctionCall(funcall.name, init_value :: tuple_args_for_first_iter) )
    val inloop_assignment = AssignmentExpression(assignment.to, FunctionCall(funcall.name, assignment.to :: tuple_args))
    val inloop_assignment_block = Block(Vector(ExpressionStatement(inloop_assignment)))

    arg_block :+ comment :+ init_assignment :+ ForLoopIm( init, cond, increment, inloop_assignment_block )
    */
  }


  def generateNothing(fc: FunCall) : Block = {

    Block(fc.args.map( generate(_) ).toVector )

  }

  def apply(lambda: Lambda, hostMemoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)], func_name: String) : Block = {

    val userfun_decl_code = generateUserFunDecl(lambda)

    val tuple_decl_code = generateTupleDecl(lambda)

    val ins_cvars = lambda.params.map(p => CVarWithType(p.mem.variable.toString,TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(p.t), flatType = true ) ))
    //val out_cvar = CVarWithType(lambda.body.mem.variable.toString, TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(lambda.body.t), flatType = true) )
    val out_cvar_in_execute = CVarWithType(lambda.body.mem.variable.toString, RefType(TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(lambda.body.t), flatType = true) ) )
    val sizes_cvars = lambda.params.flatMap(p => ArithExpr.collectVars(p.mem.size)).map(p => CVarWithType(p.toString, IntegerType())).distinct

    val memory_alloc_code = generateMemAlloc(hostMemoryDeclaredInSignature, out_cvar_in_execute)

    //val all_signature_cvars = ( (ins_cvars :+ out_cvar ) ++ sizes_cvars ).toList
    val all_signature_cvars_for_execute = ( (ins_cvars :+ out_cvar_in_execute ) ++ sizes_cvars ).toList

    val param_list = all_signature_cvars_for_execute.map(cv => ParamDeclPure(cv.name, cv.t))

    val core_body_code = generate(lambda)

    //( Block(Vector(boilerplate_code, userfun_decl_code, FunctionPure("execute",VoidType(), param_list, memory_alloc_code  :++ core_body_code ) ), global = true ), all_signature_cvars )
    Block(Vector(
      boilerplate_code,
      RawCode("namespace lift {"),
      tuple_decl_code :++ userfun_decl_code,
      FunctionPure(func_name,VoidType(), param_list, memory_alloc_code  :++ core_body_code ), RawCode("}")
    ), global = true )

  }


  def apply_no_header(lambda: Lambda,
                      hostMemoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)],
                      generatePostExecuteHook: Boolean = false) : Block = {

    val userfun_decl_code = generateUserFunDecl(lambda)

    val tuple_decl_code = generateTupleDecl(lambda)

    val ins_cvars = lambda.params.map(p => CVarWithType(p.mem.variable.toString,TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(p.t), flatType = true ) ))
    //val out_cvar = CVarWithType(lambda.body.mem.variable.toString, TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(lambda.body.t), flatType = true) )
    val out_cvar_in_execute = CVarWithType(lambda.body.mem.variable.toString, RefType(TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(lambda.body.t), flatType = true) ) )
    val sizes_cvars = lambda.params.flatMap(p => ArithExpr.collectVars(p.mem.size)).map(p => CVarWithType(p.toString, IntegerType())).distinct

    val memory_alloc_code = generateMemAlloc(hostMemoryDeclaredInSignature, out_cvar_in_execute)

    //val all_signature_cvars = ( (ins_cvars :+ out_cvar ) ++ sizes_cvars ).toList
    val all_signature_cvars_for_execute = ( (ins_cvars :+ out_cvar_in_execute ) ++ sizes_cvars ).toList

    val param_list = all_signature_cvars_for_execute.map(cv => ParamDeclPure(cv.name, cv.t))

    val core_body_code = generate(lambda) :+ (if(generatePostExecuteHook) FunctionCall("post_execute", List()) else RawCode("") )

    //( Block(Vector(boilerplate_code, userfun_decl_code, FunctionPure("execute",VoidType(), param_list, memory_alloc_code  :++ core_body_code ) ), global = true ), all_signature_cvars )
    Block(Vector( RawCode("namespace lift {"), tuple_decl_code :++ userfun_decl_code, FunctionPure(lambda.funcName,VoidType(), param_list, memory_alloc_code  :++ core_body_code ), RawCode("}")  ), global = true )




  }


  def generateMemAlloc(hostMemoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)], out_cvar_in_execute: CVarWithType) : Block = {


    val memory_alloc_vector =
      hostMemoryDeclaredInSignature.map(
        record => {

          if(record._2._3 == CPUMainMemoryAddressSpace  ) {
            val rhs = FunctionCall("reinterpret_cast", List(
              FunctionCall("malloc", List(BinaryExpression(ArithExpression(record._2._2), BinaryExpressionT.Operator.*,
                FunctionCall("sizeof", List(TypeLowering.GetElementTypeFromPointer(record._2._1.t)))
              )))),
              List(record._2._1.t))
            val out_name = out_cvar_in_execute.name
            record._2._1.name match {
              case `out_name` => ExpressionStatement(AssignmentExpression(VarRefPure(record._2._1), rhs))
              case _ => val cvar = record._2._1
                VarDeclPure(cvar, cvar.t, Some(rhs))
            }
          } else if (record._2._3 == GlobalMemory ) {

            val cvar = CVarWithType(record._2._1.name, ClassOrStructType("cl::Buffer"))
            ObjectDecl(cvar, cvar.t,
              List(
                StringConstant("context"),
                StringConstant("CL_MEM_READ_WRITE"),
                BinaryExpression(ArithExpression(record._2._2), BinaryExpressionT.Operator.*,
                  FunctionCall("sizeof", List(TypeLowering.GetElementTypeFromPointer(record._2._1.t))) )
              )
            )


          } else {
            assert(false, "New mem address not implemented in final memory alloc CAST generation")
            ExpressionStatement(RawCode("dummy"))
          }

                  } ).toVector

    /*
    val empty_node_filtered = memory_alloc_vector.filter({case EmptyNode() => false; case _ => true})
    empty_node_filtered.length match {
      case 0 => Block()
      case _ => Comment("Allocate memory for output pointers") +: Block(empty_node_filtered, global = true)
    }*/
    Comment("Allocate memory for output pointers") +: Block(memory_alloc_vector, global = true)

  }


  private def generateUserFunDecl(lambda: Lambda) : Block = {

    val all_userfunc = mutable.Set.empty[UserFun]
    val all_userfunc_names = mutable.Set.empty[String]

    lambda visitBy {
      case uf:UserFun =>
        all_userfunc_names.contains(uf.name) match {
          case false =>
            all_userfunc += uf
            all_userfunc_names += uf.name
            ()
          case true => ()
        }
      case _ => ()
      }

    val all_user_decl = all_userfunc.map(createFunctionDefinition)
    val all_user_decl_with_incl_guard: Vector[Block] = all_user_decl.toVector.map(
      //Block(Vector(RawCode(pre1 = "#ifndef GRANDPARENT_H", pre2 = "#define GRANDPARENT_H", code = " "), _, RawCode("#endif") ), global = true)
       func => Block( Vector(RawCode(pre1 = s"#ifndef ${func.name.toUpperCase}_H", pre2 = s"#define ${func.name.toUpperCase}_H"), func, RawCode(post1 = "#endif", post2 = " ")) ,  global = true)

    )

    Block(all_user_decl_with_incl_guard, global = true)


  }

  private def createFunctionDefinition(uf: UserFun): FunctionPure = {

    FunctionPure(
      name = uf.name,
      ret = TypeLowering.IRType2CastType(uf.outT),
      params = (uf.inTs, uf.paramNames).
        zipped.map((t, n) => ParamDeclPure(n, TypeLowering.IRType2CastType(t))).toList,
      body = Block( Vector( OclCode(uf.body) ), global = true))
  }

  private def generateTupleDecl(lambda: Lambda) : Block = {

    val mutable_result = mutable.Set.empty[TupleType]
    lambda visitBy {
      case FunCall(uf: UserFun, _*) =>
        mutable_result ++= uf.tupleTypes
      case _ =>
    }
    val result = mutable_result.toSet

    val all_tuple_decl = result.map(TypeDefHost(_)).toVector
    val all_tuple_decl_with_incl_guard = all_tuple_decl.map(
      td => Block( Vector(RawCode(pre1 = s"#ifndef ${Type.name(td.t).toUpperCase}_H", pre2 = s"#define ${Type.name(td.t).toUpperCase}_H"), td, RawCode(post1 = "#endif", post2 = " ")) , global = true)
    )

    Block(all_tuple_decl_with_incl_guard, global = true)


    /*
    val all_user_decl_with_incl_guard: Vector[Block] = all_user_decl.toVector.map(
      //Block(Vector(RawCode(pre1 = "#ifndef GRANDPARENT_H", pre2 = "#define GRANDPARENT_H", code = " "), _, RawCode("#endif") ), global = true)
      func => Block( Vector(RawCode(pre1 = s"#ifndef ${func.name.toUpperCase}_H", pre2 = s"#define ${func.name.toUpperCase}_H"), func, RawCode(post1 = "#endif")) ,  global = true)

    )*/

  }



}
