package prog_gen

import com.typesafe.scalalogging.Logger
import ir.{Type, TypeChecker}
import ir.ast.Lambda
import lift.arithmetic.{ArithExpr, Cst}
import opencl.executor.{Eval, Compile}
import rewriting.InferNDRange
import rewriting.utils.Utils

object GeneratePrograms {

  private val logger = Logger(this.getClass)

  private val splitFactors = Seq[ArithExpr](64, 128)
  private val inputSizes = Seq[Cst](Cst(512), Cst(1024), Cst(2048))

  def main(args: Array[String]): Unit = {

    val generator = new ProgramGenerator
    val programs = generator.generatePrograms()

    logger.info(s"${programs.length} programs generated.")

    val concretePrograms = programs.flatMap(substituteSplitFactors)

    logger.info(s"${concretePrograms.length} programs with split factors assigned.")

    val allInputCombinations = concretePrograms.map(lambda => {
      val vars = lambda.getVarsInParams()

      val sizes = inputSizes.combinations(vars.length)

      val types = lambda.params.map(_.t)

      sizes.map(s => {

        val substitutions = (vars, s).zipped.toSeq.toMap[ArithExpr, ArithExpr]
        types.map(Type.substitute(_, substitutions))

      })

    })

    concretePrograms.foreach(lambda => {
      val vars = lambda.getVarsInParams()

      val sizes = inputSizes.combinations(vars.length)

      val (local, global) = InferNDRange(lambda)

      val code = Compile(lambda, local, global)

      val lambdaString = Utils.dumpLambdaToString(lambda)

      val hash = Utils.Sha256Hash(lambdaString)

      sizes.foreach(size => {
        val substitutions = (vars,size).zipped.toMap[ArithExpr, Cst]

        // TODO: Share inputs and generate/save only once
        val inputs = InputGenerator(substitutions)(lambda)

        val localSubst = InferNDRange.substituteInNDRange(local, substitutions)
        val globalSubst = InferNDRange.substituteInNDRange(global, substitutions)

        // TODO: Run sequential for output

        // TODO: Dump everything to files

      })
    })

  }


  private def substituteSplitFactors(lambda: Lambda): Seq[Lambda] = {

    val nodes = Utils.findTunableNodes(lambda)
    val toReplace = nodes.map(Utils.extractArithExpr)
    val vars = lambda.getVarsInParams()

    val factory = Eval.getMethod(Utils.dumpLambdaToMethod(lambda))

    val replacementCombinations = splitFactors.combinations(toReplace.length)

    replacementCombinations
      .map(combinations => factory(vars ++ combinations))
      .filter(l => try { TypeChecker(l); true } catch { case _: Throwable => false} )
      .toSeq
  }

}
