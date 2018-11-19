package host.lowering

import core.generator.GenericAST.{ArithExpression, AssignmentExpression, BinaryExpression, BinaryExpressionT, Block, CVarWithType, Comment, ExpressionStatement, ForLoopIm, FunctionCall, FunctionPure, IntConstant, IntegerType, ParamDeclPure, RawCode, RefType, StringConstant, UnaryExpression, VarDeclPure, VarRefPure, VoidType}
import host.ir_host.MapHSeq
import ir.ast.{AbstractMap, FunCall, IRNode, Join, Lambda, Split}
import lift.arithmetic.ArithExpr
import opencl.ir.pattern.MapSeq

object LowerIR2HostCAST {

  val boilerplate_code = RawCode(
    """
      |#include <bits/stdc++.h>
      |
      |using namespace std;
      |
    """.stripMargin)

  def generate(node:IRNode): Block = {
    //lots of pattern matching code
    node match {
      case lambda@Lambda(_,_) =>
        generate(lambda.body)
      case fc@FunCall(lambda@Lambda(_,_), _) =>
        generate(lambda.body)
      case fc@FunCall(_:AbstractMap, _) =>
        generateAbstractMap(fc)
      case fc@FunCall(Split(_), _ ) =>
        generateNothing(fc)
      case fc@FunCall(Join(), _) =>
        generateNothing(fc)
      case _ =>
        Block()
    }

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
      case _:MapHSeq => Comment("For each element processed sequentially")
      case _ => assert(false, "Not implemented"); Comment("Not reachable")
    }


    arg_block :+ comment :+ ForLoopIm( init, cond, increment, generate(m.f.body) )

  }


  def generateNothing(fc: FunCall) : Block = {

    generate(fc.args.head)

  }

  def apply(lambda: Lambda, hostMemoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr)]) : (Block, List[CVarWithType]) = {

    val memory_alloc_code = generateMemAlloc(hostMemoryDeclaredInSignature)

    val ins_cvars = lambda.params.map(p => CVarWithType(p.mem.variable.toString,Util.Array2Pointer(Util.IRType2CastType(p.t), flatType = true ) ))
    val out_cvar = CVarWithType(lambda.body.mem.variable.toString, Util.Array2Pointer(Util.IRType2CastType(lambda.body.t), flatType = true) )
    val out_cvar_in_execute = CVarWithType(lambda.body.mem.variable.toString, RefType(Util.Array2Pointer(Util.IRType2CastType(lambda.body.t), flatType = true) ) )
    val sizes_cvars = lambda.params.flatMap(p => ArithExpr.collectVars(p.mem.size)).map(p => CVarWithType(p.toString, IntegerType())).distinct

    val all_signature_cvars = ( (ins_cvars :+ out_cvar ) ++ sizes_cvars ).toList
    val all_signature_cvars_for_execute = ( (ins_cvars :+ out_cvar_in_execute ) ++ sizes_cvars ).toList

    val param_list = all_signature_cvars_for_execute.map(cv => ParamDeclPure(cv.name, cv.t))

    val core_body_code = generate(lambda)

    ( Block(Vector(boilerplate_code, FunctionPure("execute",VoidType(), param_list, memory_alloc_code  :++ core_body_code ) ), global = true ), all_signature_cvars )




  }

  def generateMemAlloc(hostMemoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr)]) : Block = {


    val memory_alloc_vector = hostMemoryDeclaredInSignature.map(record =>

      ExpressionStatement(AssignmentExpression(VarRefPure(record._2._1),
        FunctionCall("reinterpret_cast", List(
          FunctionCall("trans_alloc", List(BinaryExpression(ArithExpression(record._2._2), BinaryExpressionT.Operator.*,
            FunctionCall("sizeof", List(Util.GetElementTypeFromPointer(record._2._1.t)))
          )))),
          List(record._2._1.t))
      ) )
    ).toVector

    Comment("Allocate memory for output pointers") +: Block(memory_alloc_vector, global = true)

  }



}
