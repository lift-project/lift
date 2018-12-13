package cbackends.common

import core.generator.GenericAST.{Block, CVarWithType}
import ir.ast.Lambda

trait CBackendsCompilerTrait {

  def typeCheck(lambda:Lambda) : Unit
  /* def memorySpaceInference(lambda:Lambda) : Unit
  def loopVarInference(lambda:Lambda) : Unit
  def memoryAlloc(lambda:Lambda) : Unit
  def inputView(lambda:Lambda) : Unit
  def outputView(lambda:Lambda) : Unit
  def lowerIR2CAST() : (Block, List[CVarWithType])
  def castPrinter(mb: Block, path: String, file: String) : Unit */

  //compile a lambda
  def !(lambda: Lambda, path: String, file: String): Unit = {

    println("1.compiler called")

    typeCheck(lambda)

    println("n.compiler done")

  }

}
