package cbackends.common

import cbackends.common.loop_var_inference.LoopVarInference
import cbackends.common.memory_management.{FinalMemoryAllocationAnalysis, InferHostMemoryAddressSpace, MemoryAllocator}
import core.generator.GenericAST.{Block, CVarWithType}
import cbackends.common.view.{InputView, OutputView}
import ir.{TypeChecker, UndefType}
import ir.ast.{Expr, Lambda}
import lift.arithmetic.ArithExpr

trait CBackendsCompilerTrait {

  //no need to override typeCheck,
  //as every backend-specific one are also IRNode,
  //thus amenable to type check
  final def typeCheck(lambda:Lambda) : Unit = {

    println("2. common type check")

    lambda.params.foreach(p => assert(p.t != UndefType))
    TypeChecker(lambda)

    lambda visitBy {
      case e: Expr => assert(e.t != UndefType)
      case _ =>
    }

  }
  def memorySpaceInference(lambda:Lambda) : Unit = {

    println("3. common infer memory space")

    InferHostMemoryAddressSpace(lambda)

  }

  def loopVarInference(lambda:Lambda) : Unit = {

    println("4. common infer loop var in trait")

    LoopVarInference(lambda)

  }

  def memoryAlloc(lambda:Lambda) : Unit = {

    println("5. common memory alloc in trait")

    MemoryAllocator(lambda)

  }

  def finalMemoryAllocationAnalysis(lambda: Lambda) : Map[String, (CVarWithType, ArithExpr) ] = {

    println("6. common final memory allocation analaysis in trait")

    FinalMemoryAllocationAnalysis(lambda)

  }

  def inputView(lambda:Lambda) : Unit = {

    println("7. common input view in trait")

    InputView(lambda)

  }

  def outputView(lambda:Lambda) : Unit = {

    println("8. common output view in trait")

    OutputView(lambda)

  }

  def lowerIR2CAST(lambda: Lambda, memoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr)]) : Block /*
  def castPrinter(mb: Block, path: String, file: String) : Unit */

  //compile a lambda
  def !(lambda: Lambda, path: String, file: String): Unit = {

    println("1.compiler called")

    typeCheck(lambda)
    memorySpaceInference(lambda)
    loopVarInference(lambda)
    memoryAlloc(lambda)
    val finalMemoryAllocated = finalMemoryAllocationAnalysis(lambda)
    inputView(lambda)
    outputView(lambda)

    println("n.compiler done")

  }

}
