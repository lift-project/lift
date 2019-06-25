package cbackends.sdh.lowering


import core.generator.GenericAST._
import ir.ArrayTypeWSWC
import ir.ast.{AbstractPartRed, Expr, FunCall, Get, IDGenerator, IRNode, Join, Lambda, Split, UserFun, Value}
import opencl.generator.OpenCLAST.OclCode

import scala.collection.mutable
import lift.arithmetic.ArithExpr
import opencl.ir.pattern.MapSeq
import cbackends.sdh.sdh_ir._
import cbackends.sdh.lowering.LowerIR2SchedCAST.wait_for_branch_predictor_cycle2
import cbackends.common.utils.type_lowering.TypeLowering
import cbackends.common.view.{CollectAllLoopVars, ViewPrinter}
import cbackends.host.lowering.LowerIR2HostCAST.generateAbstractReduce
import ir.printer.DotPrinter

object LowerIR2KernelCAST {

  val boilerplate_code = RawCode(
    """
      |#include <stdio.h>
      |#include <stdlib.h>
      |#include <sys/types.h>
      |#include <sys/stat.h>
      |#include <unistd.h>
      |#include <fcntl.h>
      |#include <sys/mman.h>
      |#include <stdint.h>
      |#include <string.h>
      |#include <bits/stdc++.h>
      |
      |using namespace std;
      |
      |#include "util.hpp"
      |
    """.stripMargin)


  private def generate(node:IRNode): Block = {
    //lots of pattern matching code
    node match {
      case lambda@Lambda(_,_,_) =>
        generate(lambda.body)
      case fc@FunCall(_:MapTM, _) =>
        generateMapTM(fc)
      case fc@FunCall(_:MapTile, _) =>
        generateMapTile(fc)
      case fc@FunCall(_:MapGPE, _) =>
        generateMapGPE(fc)
      case fc@FunCall(_:MapGPESync, _) =>
        generateSync(fc)
      case fc@FunCall(_:TMKernel, _) =>
        generateTMKernel(fc)
      case fc@FunCall(_:MapSeq, _) =>
        generateMapSeq(fc)
      case fc@FunCall(_:AbstractPartRed, _*) =>
        generateAbstractReduce(fc)
      case fc@FunCall(_:UserFun,_*) =>
        generateUserFun(fc)
      case fc@FunCall(_:Get, _) =>
        generateNothing(fc)
      case fc@FunCall(Split(_), _ ) =>
        generateNothing(fc)
      case fc@FunCall(Join(), _) =>
        generateNothing(fc)
      case _ =>
        Block()
    }

  }

  def generateNothing(fc: FunCall) : Block = {

    generate(fc.args.head)

  }

  def generateTMKernel(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)
    val tm = fc.f.asInstanceOf[TMKernel]
    val inner_code = generate(tm.f.body)

    arg_block :++ inner_code

  }

  def generatePopParameterAndTMKernelCode(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val tm = fc.f.asInstanceOf[TMKernel]

    //push argument to queue

    //(1) collect loop variables in the view system
    if (tm.signature_parameters.isEmpty) {
      val input_view_chain = tm.f.params.head.view
      val output_view_chain = tm.f.body.outputView
      val input_view_loop_variables = CollectAllLoopVars(input_view_chain)
      val output_view_loop_variables = CollectAllLoopVars(output_view_chain)
      val all_view_loop_variables = input_view_loop_variables ++ output_view_loop_variables

      tm.signature_parameters = all_view_loop_variables.toVector
    }
    assert(!tm.signature_parameters.isEmpty)

    //(2) generate the declarations of those kernel parameters
    val param_decls = tm.signature_parameters.map( p => VarDeclPure(p, p.t, init = Some(
      FunctionCall("reinterpret_cast", List(FunctionCall("GPEQ_POP", List())), List(p.t) )
    )) )
    val param_decl_code = Block(param_decls, global = true)

    //(3) generate inner code
    val inner_code = generate(tm.f.body)

    arg_block :++ param_decl_code  :++ inner_code

  }

  def generateSync(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val body = FunctionCall("LCPQ_PUSH", List(IntConstant(1)))

    arg_block :+ Comment("Sync to LCP") :+ body

  }


  private def generateMapGPE(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val m = fc.f.asInstanceOf[MapGPE]
    val stop = m.loopVar.range.max + 1

    val indexVar1 =  CVarWithType("v_gpe_batch_" + IDGenerator.get_id(), IntegerType() )
    val init1 = VarDeclPure( indexVar1, indexVar1.t, Some(IntConstant(0)) )
    val cond1 = BinaryExpression(VarRefPure(indexVar1), BinaryExpressionT.Operator.<=, ArithExpression(stop/m.num_hw_elements) )
    val increment1 = UnaryExpression("++", (indexVar1) )

    val body_block = generate(m.f)
    //val body_block_no_brackets = body_block.copy(global = true)

    val gpe_id_cvar = CVarWithType(m.loopVar.toString, IntegerType())
    val pop_gpe_id = VarDeclPure(gpe_id_cvar, gpe_id_cvar.t, init = Some( FunctionCall("GPEQ_POP", List()) ))

    val push_finish_signal = FunctionCall("LCPQ_PUSH", List(IntConstant(1)))

    val cond = BinaryExpression(VarRefPure(gpe_id_cvar), BinaryExpressionT.Operator.<, ArithExpression(stop))
    //val body_and_pop_guard = IfThenElseIm(cond, Block(Vector(body_block, push_finish_signal)), Block())
    val body_and_pop_guard = IfThenElseIm(cond, Block(Vector(body_block)), Block())

    arg_block :+ Block(Vector(ForLoopIm(init1, cond1, increment1, Block(Vector(wait_for_branch_predictor_cycle2, pop_gpe_id, body_and_pop_guard, push_finish_signal))   )))
    //arg_block :+ Block(Vector(ForLoopIm(init1, cond1, increment1, Block(Vector(pop_gpe_id, body_block , push_finish_signal))   )))

    //(arg_block :+ Comment("For each GPE. TODO: check if you can get this by API call instead of push and pop") :+ pop_gpe_id) :++ body_block_no_brackets
  }

  private def generateMapTM(fc:FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val m = fc.f.asInstanceOf[MapTM]
    val indexVar =  CVarWithType(m.loopVar.toString, IntegerType())

    val stop = m.loopVar.range.max + 1

    val init = VarDeclPure( indexVar, indexVar.t, Some(IntConstant(0)) )
    val cond = BinaryExpression(VarRefPure(indexVar), BinaryExpressionT.Operator.<, ArithExpression(stop) )
    val increment = UnaryExpression("++", (indexVar) )

    arg_block :+ Comment("For each transmuter") :+ ForLoopIm(init, cond, increment, generate(m.f.body))

  }

  def generateMapTile(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)


    val m = fc.f.asInstanceOf[AbstractSDHMap]
    val stop = m.loopVar.range.max + 1

    val indexVar1 =  CVarWithType("v_tile_batch_" + IDGenerator.get_id(), IntegerType() )
    val init1 = VarDeclPure( indexVar1, indexVar1.t, Some(IntConstant(0)) )
    val cond1 = BinaryExpression(VarRefPure(indexVar1), BinaryExpressionT.Operator.<=, ArithExpression(stop/m.num_hw_elements) )
    val increment1 = UnaryExpression("++", (indexVar1) )

    val new_tile_id_cvar = CVarWithType("v_virtual_tile_id_" + IDGenerator.get_id(), IntegerType())
    val old_tile_id = FunctionCall("GPE_TILE_ID", List())
    val new_tile_id_cast = BinaryExpression(old_tile_id, BinaryExpressionT.Operator.+,
      BinaryExpression(VarRefPure(indexVar1), BinaryExpressionT.Operator.*, ArithExpression(m.num_hw_elements) )
    )
    val new_tile_id_assignment = VarDeclPure( new_tile_id_cvar, new_tile_id_cvar.t, Some(new_tile_id_cast) )
    val loop_var_from_view_system_cvar = CVarWithType(m.loopVar.toString, IntegerType())
    val assign_loop_var_from_view_system_the_new_tile_id = VarDeclPure( loop_var_from_view_system_cvar, loop_var_from_view_system_cvar.t, Some(VarRefPure(new_tile_id_cvar)))
    val body =  generate(m.f.body)
    val cond = BinaryExpression(VarRefPure(new_tile_id_cvar), BinaryExpressionT.Operator.<, ArithExpression(stop))
    val body_guard = Block(Vector(new_tile_id_assignment, assign_loop_var_from_view_system_the_new_tile_id, IfThenElseIm(cond, body, Block()) ) )
    val forloop = Block(Vector(ForLoopIm(init1, cond1, increment1, body_guard) ))

    arg_block :++ forloop

  }

  /*
  private def generateMapTile(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val m = fc.f.asInstanceOf[MapTile]
    val indexVar =  CVarWithType(m.loopVar.toString, IntegerType())
    arg_block :+ Comment("For each tile") :+ VarDeclPure( indexVar , indexVar.t, Some(FunctionCall("GPE_TILE_ID", List())) ) :++ generate(m.f.body)
  }*/

  private def generateMapSeq(fc: FunCall) : Block = {

    val argBlock = generate(fc.args.head)

    val mapseq = fc.f.asInstanceOf[MapSeq]

    val size = getArrayWSWCSize(fc.args.head)

    val indexVar = mapseq.loopVar
    val start = Some(ArithExpression(indexVar.range.min))
    val stop = ArithExpression(size)
    val indexCVar = CVarWithType(indexVar.toString, IntegerType())
    val init = VarDeclPure( indexCVar, indexCVar.t, start )
    val increment = AssignmentExpression(VarRefPure(indexCVar), BinaryExpression(ArithExpression(indexVar), BinaryExpressionT.Operator.+ ,IntConstant(1) ) )
    val cond = BinaryExpression(ArithExpression(indexVar), BinaryExpressionT.Operator.<, stop)
    val forloop = ForLoopIm(init, cond, increment, generate(mapseq.f))


    argBlock :+ Comment("For each element processed sequentially") :+ Block(Vector(forloop), global = true)


  }

  private def generateUserFun(fc: FunCall) : Block = {

    //val argBlock = generate(fc.args(0))
    val mutableArgBlock = MutableBlock()
    fc.args.foreach(mutableArgBlock :++ generate(_))
    val argBlock = mutableArgBlock.toBlock

    //should emit a global function decl
    val uf = fc.f.asInstanceOf[UserFun]

    val arg_list : List[AstNode] = fc.args.map(a => a match {
      case v:Value => StringConstant(v.value)
      case _ => ViewPrinter(a.view)
    }).toList
    val out_offset = ViewPrinter(fc.outputView)

    val userfunc_apply = AssignmentExpression( out_offset , FunctionCall(uf.name, arg_list) )

    argBlock :+ userfunc_apply

  }


  private def createFunctionDefinition(uf: UserFun): FunctionPure = {

    FunctionPure(
      name = uf.name,
      ret = TypeLowering.IRType2CastType(uf.outT),
      params = (uf.inTs, uf.paramNames).
        zipped.map((t, n) => ParamDeclPure(n, TypeLowering.IRType2CastType(t))).toList,
      body = Block( Vector( OclCode(uf.body) ), global = true))
  }

  private def generateUserFunDecl(lambda: Lambda) : Block = {

    val all_userfunc = mutable.Set.empty[UserFun]

    lambda visitBy {
        case uf:UserFun => all_userfunc += uf; ()
        case _ => ()
      }

    val all_user_decl = all_userfunc.map(createFunctionDefinition).toVector

    Block(all_user_decl, global = true)


  }

  def generatePopTopLevelParameters(all_signature_cvars: List[CVarWithType]) : Block = {

    Block(
      Comment("Pop input, output pointers and sizes") +:
        all_signature_cvars.toVector.map(
          p => VarDeclPure(p, p.t, init = Some(
            p.t match {
              case _:PointerType => FunctionCall("reinterpret_cast", List(FunctionCall("GPEQ_POP", List())), List(p.t) )
              case _ => FunctionCall("GPEQ_POP", List())
            }
          ))
        )
      , global=true )
  }

  def apply(lambda: Lambda, all_signature_cvars: List[CVarWithType]) : Block = {

    //DotPrinter("worker",lambda)
    //DotPrinter.withNumbering("/home/lu/Downloads","worker_num",lambda)

    val userfun_decl_code = generateUserFunDecl(lambda)

    val pop_top_level_parameters = generatePopTopLevelParameters(all_signature_cvars)

    val core_body_code = generate(lambda)

    Block( Vector(boilerplate_code, userfun_decl_code, FunctionPure("main", IntegerType(), List(), pop_top_level_parameters :++ core_body_code ) ), global = true)

  }



  def getArrayWSWCSize(expr:Expr): ArithExpr = {

    expr.t match {
      case ArrayTypeWSWC(_, _, s) => s
      case _ => throw new Exception("Only wswc supported")
    }
  }


}
