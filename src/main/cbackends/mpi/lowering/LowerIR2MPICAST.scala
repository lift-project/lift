package cbackends.mpi.lowering

import cbackends.common.utils.type_lowering.TypeLowering
import cbackends.mpi.mpi_ir.BcastMPI
import core.generator.GenericAST.{ArithExpression, Block, CVarWithType, ExpressionStatement, FunctionCall, FunctionPure, IntConstant, IntegerType, ParamDeclPure, RawCode, RefType, VoidType}
import ir.Type
import ir.ast.{FunCall, IRNode, Lambda}
import lift.arithmetic.ArithExpr
import opencl.ir.OpenCLAddressSpace

object LowerIR2MPICAST {


  val boilerplate_code = ExpressionStatement(RawCode(
  """
  |#include <bits/stdc++.h>
  |#include <mpi.h>
  |using namespace std;
  |""".stripMargin), true )


  def generate(node:IRNode): Block = {
    //lots of pattern matching code
    node match {

      case lambda@Lambda(_,_,_) =>
        generate(lambda.body)

      case fc@FunCall(_:BcastMPI, _) =>
        generateBcastMPI(fc)

      case _ =>
        Block()
    }

  }

  def generateBcastMPI(fc: FunCall) : Block = {


    val arg_block = generate(fc.args.head)

    val out_cvar = CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(fc.t), flatType = true) )
    val size = Type.getElementCount(fc.t)

    Block(Vector(FunctionCall("MPI_Bcast", List( out_cvar , ArithExpression(size), RawCode("MPI_Float"), IntConstant(0) , RawCode("MPI_COMM_WORLD") ) ) ) )

  }

  def apply(lambda: Lambda, hostMemoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)]) : Block = {


    val core_body_code = generate(lambda)

    val ins_cvars = lambda.params.map(p => CVarWithType(p.mem.variable.toString,TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(p.t), flatType = true ) ))
    //val out_cvar = CVarWithType(lambda.body.mem.variable.toString, TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(lambda.body.t), flatType = true) )
    val out_cvar_in_execute = CVarWithType(lambda.body.mem.variable.toString, RefType(TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(lambda.body.t), flatType = true) ) )
    val sizes_cvars = lambda.params.flatMap(p => ArithExpr.collectVars(p.mem.size)).map(p => CVarWithType(p.toString, IntegerType())).distinct

    val all_signature_cvars_for_execute = ( (ins_cvars :+ out_cvar_in_execute ) ++ sizes_cvars ).toList

    val param_list = all_signature_cvars_for_execute.map(cv => ParamDeclPure(cv.name, cv.t))


    Block(Vector(boilerplate_code,  FunctionPure("execute",VoidType(), param_list,  core_body_code ) ), global = true )
  }

}
