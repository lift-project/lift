package host.lowering

import core.generator.GenericAST.{ArithExpression, BinaryExpression, BinaryExpressionT, Block, CVarWithType, ExpressionStatement, FunctionCall, IntConstant, IntegerType, RawCode, StringConstant}
import ir.ast.{AbstractMap, FunCall, IRNode, Join, Lambda, Split}

object LowerIR2CAST {

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
        generateMap(fc)
      case fc@FunCall(Split(_), _ ) =>
        generateNothing(fc)
      case fc@FunCall(Join(), _) =>
        generateNothing(fc)
      case _ =>
        Block()
    }

  }

  def generateMap(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val m = fc.f.asInstanceOf[AbstractMap]
    val stop = m.loopVar.range.max

    val indexVar =  CVarWithType(m.loopVar.toString, IntegerType() )
    val init = VarDeclPure( indexVar, indexVar.t, Some(IntConstant(0)) )
    val cond = BinaryExpression(VarRefPure(indexVar), BinaryExpressionT.Operator.<=, ArithExpression(stop) )
    val increment = UnaryExpression("++", (indexVar) )

    //For MapGPE, needs to push gpe_id, as the for loop for gpe_id does not exist in worker code
    val push_gpe_loop_var = m match {
      case _:MapGPE => Block(Vector(ExpressionStatement(FunctionCall("GPEQ_PUSH", List(VarRefPure(indexVar), VarRefPure(indexVar))))) )
      case _ => Block()
    }
    //For MapTM, needs to sync all LCPs
    val sync_all_lcps = m match {
      case _:MapTM => Block(Vector(FunctionCall("barrier_wait", List(StringConstant("NUM_TILES"))) ) )
      case _ => Block()
    }

    val comment = fc.f match {
      case _:MapTM => Comment("For each transmuter chip")
      //case _:MapTile => Comment("For each tile")
      case _:MapGPE => Comment("For each GPE")
      case _:MapSeq => Comment("For each element processed sequentially")
      case _ => assert(false, "Not implemented"); Comment("Not reachable")
    }


    arg_block :+ comment :+ ForLoopIm(init, cond, increment, push_gpe_loop_var :+ generate(m.f.body) :+ sync_all_lcps)

  }


  def generateNothing(fc: FunCall) : Block = {

    generate(fc.args.head)

  }


}
