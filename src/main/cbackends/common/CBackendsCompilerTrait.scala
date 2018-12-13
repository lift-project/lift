package cbackends.common

import cbackends.common.loop_var_inference.LoopVarInference
import cbackends.common.memory_management.InferHostMemoryAddressSpace
import core.generator.GenericAST.{Block, CVarWithType}
import ir.{TypeChecker, UndefType}
import ir.ast.{Expr, Lambda}

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

  }
  /*
  def inputView(lambda:Lambda) : Unit
  def outputView(lambda:Lambda) : Unit
  def lowerIR2CAST() : (Block, List[CVarWithType])
  def castPrinter(mb: Block, path: String, file: String) : Unit */

  //compile a lambda
  def !(lambda: Lambda, path: String, file: String): Unit = {

    println("1.compiler called")

    typeCheck(lambda)
    memorySpaceInference(lambda)
    loopVarInference(lambda)
    memoryAlloc(lambda)

    println("n.compiler done")

  }

}
