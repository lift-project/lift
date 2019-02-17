package cbackends.host.lowering

import cbackends.common.common_ir.CPUMainMemoryAddressSpace
import cbackends.common.utils.type_lowering.TypeLowering
import cbackends.host.host_ir.{CPUFunCall, CPUFunCall2, OclFunCall}
import core.generator.GenericAST.{ArithExpression, AssignmentExpression, AstNode, BinaryExpression, BinaryExpressionT, Block, BlockMember, CVarWithType, ClassOrStructType, Comment, EmptyNode, ExpressionStatement, FloatType, ForLoopIm, FunctionCall, FunctionPure, IntConstant, IntegerType, MutableBlock, ObjectDecl, ParamDeclPure, PrimitiveTypeT, RawCode, RefType, StringConstant, UnaryExpression, VarDeclPure, VarRef, VarRefPure, VoidType}
import opencl.ir.{GlobalMemory, OpenCLAddressSpace}
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
      case fc@FunCall(_:AbstractPartRed, _*) =>
        generateAbstractReduce(fc)
      case fc@FunCall(Split(_), _ ) =>
        generateNothing(fc)
      case fc@FunCall(Join(), _) =>
        generateNothing(fc)
      case fc@FunCall(Slide(_,_), _ ) =>
        generateNothing(fc)
      case fc@FunCall(Transpose(), _) =>
        generateNothing(fc)
      case fc@FunCall(TransposeW(), _) =>
        generateNothing(fc)
      case fc@FunCall(_:UserFun,_*) =>
        generateUserFun(fc)
      case fc@FunCall(_:CPUFunCall,_) =>
        generateCPUFunCall(fc)
      case fc@FunCall(_:CPUFunCall2, _*) =>
        generateCPUFunCall2(fc)
      case fc@FunCall(_:OclFunCall, _*) =>
        generateOclFunCall(fc)
      case _ =>
        Block()
    }

  }


  private def generateOclFunCall(fc: FunCall) : Block = {
    //parameter sequnence convention: first input pointers, then output pointers, then sizes

    val arg_blocks = fc.args.map(generate(_) )

    val cfc = fc.f.asInstanceOf[CPUFunCall2]

    val input_args = fc.args.map( arg => CVarWithType(arg.mem.variable.toString, TypeLowering.IRType2CastType(arg.t) ) ).toList
    val output_arg = CVarWithType(fc.mem.variable.toString, TypeLowering.IRType2CastType(fc.t))
    val sizes = cfc.params.flatMap(p => ArithExpr.collectVars(p.mem.size)).map(p => CVarWithType(p.toString, IntegerType())).distinct

    val fc_cast = FunctionCall(cfc.funcName, input_args ::: (output_arg :: sizes.toList ) )

    Block(arg_blocks.toVector, global = true) :++ Block( Vector(fc_cast), global = true)


  }

  private def generateCPUFunCall2(fc: FunCall) : Block = {
    //parameter sequnence convention: first input pointers, then output pointers, then sizes

    val arg_blocks = fc.args.map(generate(_) )

    val cfc = fc.f.asInstanceOf[CPUFunCall2]

    val input_args = fc.args.map( arg => CVarWithType(arg.mem.variable.toString, TypeLowering.IRType2CastType(arg.t) ) ).toList
    val output_arg = CVarWithType(fc.mem.variable.toString, TypeLowering.IRType2CastType(fc.t))
    val sizes = cfc.params.flatMap(p => ArithExpr.collectVars(p.mem.size)).map(p => CVarWithType(p.toString, IntegerType())).distinct

    val fc_cast = FunctionCall(cfc.funcName, input_args ::: (output_arg :: sizes.toList ) )

    Block(arg_blocks.toVector, global = true) :++ Block( Vector(fc_cast), global = true)


  }

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

  def generateAbstractReduce(fc: FunCall) : Block = {

    val arg_block = generate(fc.args(1))

    val rd = fc.f.asInstanceOf[AbstractPartRed]
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

    generate(fc.args.head)

  }

  def apply(lambda: Lambda, hostMemoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)]) : Block = {

    val userfun_decl_code = generateUserFunDecl(lambda)

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
    Block(Vector(boilerplate_code, userfun_decl_code, FunctionPure("execute",VoidType(), param_list, memory_alloc_code  :++ core_body_code ) ), global = true )




  }


  def apply_no_header(lambda: Lambda, hostMemoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)]) : Block = {

    val userfun_decl_code = generateUserFunDecl(lambda)

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
    Block(Vector( userfun_decl_code, FunctionPure(lambda.funcName,VoidType(), param_list, memory_alloc_code  :++ core_body_code ) ), global = true )




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

    val all_user_decl = all_userfunc.map(createFunctionDefinition).toVector

    Block(all_user_decl, global = true)


  }

  private def createFunctionDefinition(uf: UserFun): FunctionPure = {

    FunctionPure(
      name = uf.name,
      ret = TypeLowering.IRType2CastType(uf.outT),
      params = (uf.inTs, uf.paramNames).
        zipped.map((t, n) => ParamDeclPure(n, TypeLowering.IRType2CastType(t))).toList,
      body = Block( Vector( OclCode(uf.body) ), global = true))
  }



}
