package cbackends.sdh.lowering


import cbackends.common.utils.type_lowering.TypeLowering
import core.generator.GenericAST.{ArithExpression, AssignmentExpression, BinaryExpression, BinaryExpressionT, Block, CVarWithType, Comment, ExpressionStatement, ForLoopIm, FunctionCall, FunctionPure, IfThenElseIm, IntConstant, IntegerType, ParamDeclPure, PointerType, RawCode, RefType, StringConstant, Uint32_t, UnaryExpression, VarDeclPure, VarRefPure, VoidType}
import ir.ast.{AbstractMap, FunCall, IDGenerator, IRNode, Join, Lambda, Split}
import lift.arithmetic.ArithExpr
import opencl.ir.pattern.MapSeq
import cbackends.sdh.sdh_ir._
import cbackends.common.view.CollectAllLoopVars
import ir.printer.DotPrinter
import opencl.ir.OpenCLAddressSpace


object LowerIR2SchedCAST {

  val num_of_tiles = 1
  val tile_size = 4

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
      |#define VEC_START_ADDR SHARED_SPACE_START_ADDR
      |
      |void* trans_alloc(unsigned int size){
      |    static unsigned int current_pos = 0;
      |    void *memory_allocated = reinterpret_cast<void*>(VEC_START_ADDR + current_pos);
      |    current_pos += size;
      |    return memory_allocated;
      |}
      |
      |char *syncSpmStartAddr = (char *)SYNC_SPM_START_ADDR;
      |
      |void barrier_wait(unsigned n) {
      |    // Create pointers to a global cv, mutex and a barrier.
      |    pthread_cond_t *condPtr = (pthread_cond_t *)(syncSpmStartAddr);
      |    pthread_barrier_t *barrierPtr = (pthread_barrier_t *)(condPtr + 1);
      |    pthread_mutex_t *mutexPtr = (pthread_mutex_t *)(barrierPtr + 1);
      |    // Start is used by LCP[0] with the condition variable to signal other cores to "go".
      |    bool *start = (bool *)(mutexPtr + 1);
      |    // TODO_SDH_10_31_18: creating a new barrier object for every instance: not a very clean/scalable approach.
      |    syncSpmStartAddr += sizeof(pthread_cond_t) + sizeof(pthread_barrier_t) + sizeof(pthread_mutex_t) + sizeof(uint32_t);
      |    if(LCP_TILE_ID() == 0) {
      |       // Initialize the barrier with the number of participants.
      |       pthread_barrier_init(barrierPtr, nullptr, n);
      |       // Signal "go" and broadcast to all cores waiting.
      |       STORE_BYTE(start, 1);
      |       // LCP_PRINTF("--> Signaling all cores to start -->\n");
      |       pthread_cond_broadcast(condPtr);
      |    } else {
      |       // Need to grab a lock before sleeping with a cv.
      |       pthread_mutex_lock(mutexPtr);
      |       while(*start == 0) {
      |           // Release the lock and sleep until signaled.
      |           pthread_cond_wait(condPtr, mutexPtr);
      |       }
      |    // Unlock and wait on barrier until GPEs are done.
      |    pthread_mutex_unlock(mutexPtr);
      |    }
      |
      |    pthread_barrier_wait(barrierPtr);
      |}
      |""".stripMargin)


  val wait_for_branch_predictor_cycle = RawCode(
    """
      |                __asm__ __volatile__ (
      |                "dmb\n\t"
      |                )""".stripMargin)

  val wait_for_branch_predictor_cycle2 = RawCode(
    """
      |                    __asm__ __volatile__ (
      |                    "dmb\n\t"
      |                    )""".stripMargin)

  def generate(node:IRNode): Block = {
    //lots of pattern matching code
    node match {
      case lambda@Lambda(_,_,_) =>
        generate(lambda.body)
      case FunCall(lambda@Lambda(_,_,_), _) =>
        generate(lambda.body)
      case fc@FunCall(ToGPE(), _) =>
        generateCacheFlush(fc)
      case fc@FunCall(ToLCP(), _) =>
        generateCacheFlush(fc)
        //TODO: absolte, delete when all test cases passes
      case fc@FunCall(MapGPESync(_), _) =>
        generateSync(fc)
      case fc@FunCall(_:MapTile, _) =>
        generateMapTile(fc)
      case fc@FunCall(_:MapGPE, _) =>
        generateMapGPE(fc)
      case fc@FunCall(_:AbstractSDHMap, _) =>
        generateSDHMap(fc)
      case fc@FunCall(_:MapSeq, _) =>
        generateAbstractMap(fc)
      case fc@FunCall(_:TMKernel, _) =>
        generateNothing(fc)
      case fc@FunCall(Split(_), _ ) =>
        generateNothing(fc)
      case fc@FunCall(Join(), _) =>
        generateNothing(fc)
      case _ =>
        Block()
    }

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
    val old_tile_id = FunctionCall("LCP_TILE_ID", List())
    val new_tile_id_cast = BinaryExpression(old_tile_id, BinaryExpressionT.Operator.+,
      BinaryExpression(VarRefPure(indexVar1), BinaryExpressionT.Operator.*, ArithExpression(m.num_hw_elements) )
    )
    val new_tile_id_assignment = VarDeclPure( new_tile_id_cvar, new_tile_id_cvar.t, Some(new_tile_id_cast) )
    val body =  generate(m.f.body)
    val cond = BinaryExpression(VarRefPure(new_tile_id_cvar), BinaryExpressionT.Operator.<, ArithExpression(stop))
    val body_guard = Block(Vector(new_tile_id_assignment, IfThenElseIm(cond, body, Block()) ) )
    val forloop = Block(Vector(ForLoopIm(init1, cond1, increment1, body_guard) ))

    arg_block :++ forloop

  }

  def generateMapGPE(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val m = fc.f.asInstanceOf[MapGPE]
    val stop = m.loopVar.range.max + 1

    //two levels of loops should be generated
    // 1) for loop for each batch of size 4
    val indexVar1 =  CVarWithType("v_gpe_batch_" + IDGenerator.get_id(), IntegerType() )
    val init1 = VarDeclPure( indexVar1, indexVar1.t, Some(IntConstant(0)) )
    val cond1 = BinaryExpression(VarRefPure(indexVar1), BinaryExpressionT.Operator.<=, ArithExpression(stop/m.num_hw_elements) )
    val increment1 = UnaryExpression("++", (indexVar1) )
    // 2a) for loop for push each virtual id
    val indexVar2a =  CVarWithType("v_gpe_" + IDGenerator.get_id(), IntegerType() )
    val init2a = VarDeclPure( indexVar2a, indexVar2a.t, Some(IntConstant(0)) )
    val cond2a = BinaryExpression(VarRefPure(indexVar2a), BinaryExpressionT.Operator.<, ArithExpression(m.num_hw_elements) )
    val increment2a = UnaryExpression("++", (indexVar2a) )
    // 2a) for loop for push each virtual id
    val indexVar2b =  CVarWithType("v_gpe_" + IDGenerator.get_id(), IntegerType() )
    val init2b = VarDeclPure( indexVar2b, indexVar2b.t, Some(IntConstant(0)) )
    val cond2b = BinaryExpression(VarRefPure(indexVar2b), BinaryExpressionT.Operator.<, ArithExpression(m.num_hw_elements) )
    val increment2b = UnaryExpression("++", (indexVar2b) )

    val virtual_id_a = BinaryExpression(VarRefPure(indexVar2a), BinaryExpressionT.Operator.+,
        BinaryExpression(ArithExpression(m.num_hw_elements),BinaryExpressionT.Operator.*,VarRefPure(indexVar1)) )
    val push_virtual_thread_id = Block(Vector(ExpressionStatement(FunctionCall("GPEQ_PUSH",
      List(VarRefPure(indexVar2a), virtual_id_a
      )))) )
    //val cond_a = BinaryExpression(virtual_id_a, BinaryExpressionT.Operator.<, ArithExpression(stop))
    //val push_guard = Block(Vector(IfThenElseIm( cond_a, push_virtual_thread_id, Block()) ))

    //val virtual_id_b = BinaryExpression(VarRefPure(indexVar2b), BinaryExpressionT.Operator.+,
    //  BinaryExpression(ArithExpression(m.num_hw_elements),BinaryExpressionT.Operator.*,VarRefPure(indexVar1)) )
    //val cond_b = BinaryExpression(virtual_id_b, BinaryExpressionT.Operator.<, ArithExpression(stop))
    val pop_finish_signal = Block(Vector(wait_for_branch_predictor_cycle, FunctionCall("LCPQ_POP", List(VarRefPure(indexVar2b))) ) )
    //val pop_guard = Block(Vector(IfThenElseIm(cond_b, pop_finish_signal, Block())))

    val innerloopA = ForLoopIm(init2a, cond2a, increment2a, push_virtual_thread_id)
    val innerloopB = ForLoopIm(init2b, cond2b, increment2b, pop_finish_signal)

    arg_block :++ Block(Vector(ForLoopIm(init1, cond1, increment1, Block(Vector(innerloopA, generate(m.f.body), innerloopB)) ) ) )

  }


  def generateSDHMap(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val m = fc.f.asInstanceOf[AbstractSDHMap]
    val stop = m.loopVar.range.max + 1

    val indexVar =  CVarWithType(m.loopVar.toString, IntegerType() )
    val init = VarDeclPure( indexVar, indexVar.t, Some(IntConstant(0)) )
    val cond = BinaryExpression(VarRefPure(indexVar), BinaryExpressionT.Operator.<, ArithExpression(stop) )
    val increment = UnaryExpression("++", (indexVar) )

    //For MapGPE, needs to push gpe_id, as the for loop for gpe_id does not exist in worker code
    val push_gpe_loop_var = m match {
      case _:MapGPE => Block(Vector(ExpressionStatement(FunctionCall("GPEQ_PUSH", List(VarRefPure(indexVar), VarRefPure(indexVar))))) )
      case _ => Block()
    }
    //For MapTM, needs to sync all LCPs
    val sync_all_lcps = m match {
      //case _:MapTM => Block(Vector(FunctionCall("barrier_wait", List(StringConstant("NUM_TILES"))) ) )
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
      case _ => assert(false, "Not implemented"); Comment("Not reachable")
    }


    arg_block :+ comment :+ ForLoopIm( init, cond, increment, generate(m.f.body) )

  }


  def generatePushParameterCode(fc: FunCall) : Block = {

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

    //(2) generate push CAST
    val gpe_id_cvar = CVarWithType(tm.loopVar.toString, IntegerType())
    val push_code = tm.signature_parameters.map(v => ExpressionStatement(FunctionCall("GPEQ_PUSH", List(VarRefPure(gpe_id_cvar), VarRefPure(v)))) ).toVector


    arg_block :+ Block(push_code, global = true)

  }

  def generateSync(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val m = fc.f.asInstanceOf[MapGPESync]

    val indexVar =  CVarWithType(s"i_${fc.gid}", IntegerType())
    val init = VarDeclPure( indexVar, indexVar.t, Some(IntConstant(0)) )
    val cond = BinaryExpression(VarRefPure(indexVar), BinaryExpressionT.Operator.<, ArithExpression(m.num_hw_elements) )
    val increment = UnaryExpression("++", (indexVar) )
    val body = FunctionCall("LCPQ_POP", List(VarRefPure(indexVar)))

    arg_block :+ Comment("Sync all GPEs") :+ ForLoopIm(init, cond, increment, Block(Vector(body)))

  }


  def generateCacheFlush(fc: FunCall) : Block = {

    val arg_block = generate(fc.args.head)

    val flush_cast = ExpressionStatement(FunctionCall("CACHE_FLUSH", List()))

    val comment = fc.f match {
      case ToGPE() => Comment("ToGPE")
      case ToLCP() => Comment("ToLCP")
      case _ => assert(false, "not implemented"); Comment("Not reachable")
    }

    arg_block :+ comment :+ flush_cast

  }

  def generateNothing(fc: FunCall) : Block = {

    generate(fc.args.head)

  }

  def generateMemAlloc(hostMemoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)]) : Block = {


    val memory_alloc_vector = hostMemoryDeclaredInSignature.map(record =>

      ExpressionStatement(AssignmentExpression(VarRefPure(record._2._1),
        FunctionCall("reinterpret_cast", List(
          FunctionCall("trans_alloc", List(BinaryExpression(ArithExpression(record._2._2), BinaryExpressionT.Operator.*,
            FunctionCall("sizeof", List(TypeLowering.GetElementTypeFromPointer(record._2._1.t)))
          )))),
          List(record._2._1.t))
      ) )
    ).toVector

    Comment("Allocate memory for output pointers") +: Block(memory_alloc_vector, global = true)

  }

  def apply(lambda: Lambda, hostMemoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)]) : (Block, List[CVarWithType]) = {

    //DotPrinter("whole",lambda)
    //DotPrinter.withNumbering("/home/lu/Downloads","sched_num",lambda)

    val memory_alloc_code = generateMemAlloc(hostMemoryDeclaredInSignature)

    val core_body_code = generate(lambda)

    //compute the Vector[CVarWithType] for ins, out, and sizes
    val ins_cvars = lambda.params.map(p => CVarWithType(p.mem.variable.toString,TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(p.t), flatType = true ) ))
    val out_cvar = CVarWithType(lambda.body.mem.variable.toString, TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(lambda.body.t), flatType = true) )
    val out_cvar_in_execute = CVarWithType(lambda.body.mem.variable.toString, RefType(TypeLowering.Array2Pointer(TypeLowering.IRType2CastType(lambda.body.t), flatType = true) ) )
    //TODO: here you could use a Const to wrap IntegerType
    //TODO: here you could have better naming for sizes, so that the semantics of the sizes are clear!
    val sizes_cvars = lambda.params.flatMap(p => ArithExpr.collectVars(p.mem.size)).map(p => CVarWithType(p.toString, IntegerType())).distinct
    val all_signature_cvars = ( (ins_cvars :+ out_cvar ) ++ sizes_cvars ).toList
    val all_signature_cvars_for_execute = ( (ins_cvars :+ out_cvar_in_execute ) ++ sizes_cvars ).toList

    val param_list = all_signature_cvars_for_execute.map(cv => ParamDeclPure(cv.name, cv.t))

    //val tile_loop_cvar = CVarWithType(s"tile_loop_cvar_${lambda.gid}", IntegerType())
    val gpe_loop_cvar = CVarWithType(s"gpe_loop_cvar_${lambda.gid}", IntegerType())
    val push_top_level_parameters = all_signature_cvars.map(v =>
      ExpressionStatement(FunctionCall("GPEQ_PUSH",
        v.t match {
          //here t is guaranteed to be a flat type
          case p:PointerType => List(VarRefPure(gpe_loop_cvar), FunctionCall("reinterpret_cast", List(VarRefPure(v)), List(Uint32_t()))    )
          case _ => List(VarRefPure(gpe_loop_cvar), VarRefPure(v))
        }
      ))).toVector
    //the double nested loop counts are only related to hardware configuration, not related to workload size
    val push_forloop = ForLoopIm(
        VarDeclPure( gpe_loop_cvar, gpe_loop_cvar.t, Some(IntConstant(0) ) ),
        BinaryExpression(VarRefPure(gpe_loop_cvar), BinaryExpressionT.Operator.<, IntConstant(tile_size)),
        UnaryExpression("++", VarRefPure(gpe_loop_cvar)),
        Block(push_top_level_parameters)
      )

    val push_forloop_comment = Comment("Push all pointers and sizes to GPEs")

    ( Block(Vector(boilerplate_code, FunctionPure("execute",VoidType(), param_list, (memory_alloc_code :+ push_forloop_comment :+ push_forloop ) :++ core_body_code ) ), global = true ), all_signature_cvars )

  }

}
