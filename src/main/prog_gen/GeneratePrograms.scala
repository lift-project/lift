package prog_gen

import com.typesafe.scalalogging.Logger
import ir.TypeChecker
import ir.ast.Lambda
import lift.arithmetic.{ArithExpr, Cst}
import opencl.executor.Eval

object GeneratePrograms {

  private val logger = Logger(this.getClass)

  private val splitFactors = Seq[ArithExpr](64, 128)
  private val inputSizes = Seq[Cst](Cst(512), Cst(1024), Cst(2048))

  def main(args: Array[String]): Unit = {

    val generator = new ProgramGenerator
    val programs = generator.generatePrograms()

    logger.info(s"${programs.length} programs generated.")

    val concretePrograms = programs.flatMap(substituteSplitFactors)

    logger.info(s"$concretePrograms programs with split factors assigned.")


    concretePrograms.foreach(lambda => {
      val vars = lambda.getVarsInParams()

      val sizes = inputSizes.combinations(vars.length)

      sizes.foreach(size => {
        val substitutions = (vars,size).zipped.toMap

        val inputs = InputGenerator(substitutions)(lambda)

      })
    })



  }


  private def substituteSplitFactors(lambda: Lambda): Seq[Lambda] = {

    val nodes = rewriting.utils.Utils.findTunableNodes(lambda)
    val toReplace = nodes.map(rewriting.utils.Utils.extractArithExpr)
    val vars = lambda.getVarsInParams()

    val factory = Eval.getMethod(rewriting.utils.Utils.dumpLambdaToMethod(lambda))

    val replacementCombinations = splitFactors.combinations(toReplace.length)

    replacementCombinations
      .map(combinations => factory(vars ++ combinations))
      .filter(l => try { TypeChecker(l); true } catch { case _: Throwable => false} )
      .toSeq
  }

}
