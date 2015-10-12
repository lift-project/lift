package exploration

import apart.arithmetic.Var
import exploration.utils._
import ir.ast._
import ir.{ArrayType, TypeChecker}
import opencl.ir._

object HighLevelRewrite {

  def main(args: Array[String]) = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val startingExpression = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, N), K),
      (A, B) => {
        Map(fun(aRow =>
          Map(fun(bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
          )) o Transpose() $ B
        )) $ A
      })

    val startingExpressionATransposed = fun(
      ArrayType(ArrayType(Float, M), K),
      ArrayType(ArrayType(Float, N), K),
      (A, B) => {
        Map(fun(aRow =>
          Map(fun(bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
          )) o Transpose() $ B
        )) o Transpose() $ A
      })

    val dumpThese = rewriteExpression(startingExpressionATransposed)

    println(dumpThese.length + " expressions to dump")

    val lambdas = dumpThese.map(_._1)
    printMinAndMaxDepth(lambdas)

    dumpLambdasToFiles(lambdas)
  }

  def rewriteExpression(startingExpression: Lambda2, maxDepth: Int = 5): Seq[(Lambda, Seq[Rule])] = {
    val newLambdas = (new HighLevelRewrite)(startingExpression, maxDepth)

    val filtered = filterExpressions(newLambdas)
    
    filtered
  }

  def filterExpressions(newLambdas: Seq[(Lambda, Seq[Rule])]): Seq[(Lambda, Seq[Rule])] = {
    val distinctLambdas = newLambdas.map(_._2).distinct

    println(newLambdas.length + " resulting expressions.")
    println(distinctLambdas.length + " distinct sequences of rules (possibly different locations)")

    val oneKernel = newLambdas.filter(pair => hasOneMapOnFirstLevels(pair._1))

    println(oneKernel.length + " expressions with one kernel")

    val filterDepth = oneKernel.filter(pair =>

      (pair._2.head == MacroRules.tileMapMap
        && pair._2.tail.diff(List(MacroRules.apply2DRegisterBlocking,
        MacroRules.apply2DRegisterBlocking,
        MacroRules.finishTiling,
        MacroRules.finishTiling)).isEmpty
        && NumberExpression.byDepth(pair._1).values.max <= 8)
        ||
        (pair._2.head == MacroRules.tileMapMap
          && pair._2.tail.diff(List(MacroRules.apply1DRegisterBlocking,
          MacroRules.apply1DRegisterBlocking,
          MacroRules.finishTiling,
          MacroRules.finishTiling)).isEmpty
          && NumberExpression.byDepth(pair._1).values.max <= 7)
        ||
        NumberExpression.byDepth(pair._1).values.max <= 6)

    filterDepth
  }

  private def dumpLambdasToFiles(lambdas: Seq[Lambda]): Unit = {

    lambdas.zipWithIndex.par.foreach(pair => {
      val lambda = pair._1
      val id = pair._2

      println(s"Processing $id/${lambdas.length - 1}")

      try {

        val appliedRules: Lambda = finishRewriting(lambda)

        val stringRep = Utils.dumpLambdaToString(appliedRules)

        val sha256 = Utils.Sha256Hash(stringRep)
        val folder = "lambdas/" + sha256.charAt(0) + "/" + sha256.charAt(1)

        Utils.dumpToFile(stringRep, sha256, folder)
      } catch {
        case t: Throwable =>
          println(s"No $id failed with ${t.toString.replaceAll("\n", " ")}.")
      }
    })

  }

  def finishRewriting(lambda: Lambda): Lambda = {
    val partRedToReduce = Rewrite.applyRulesUntilCannot(lambda, Seq(Rules.partialReduceToReduce))
    val lowerNext = SimplifyAndFuse(partRedToReduce)
    val appliedRules = applyAlwaysRules(lowerNext)
    appliedRules
  }

  // If the lambda does not have one map, then needs 2 kernels
  private def hasOneMapOnFirstLevels(lambda: Lambda): Boolean = {
    val body = lambda.body
    val body1 = MacroRules.getMapBody(MacroRules.getMapAtDepth(body, 0))

    val mapsOnLevelOne = Utils.countMapsAtCurrentLevel(body)
    val mapsOnLevelTwo = Utils.countMapsAtCurrentLevel(body1)

    mapsOnLevelOne == 1 && mapsOnLevelTwo == 1
  }

  private def applyAlwaysRules(lambda: Lambda): Lambda = {
    val alwaysApply = Seq(MacroRules.moveTransposeInsideTiling)

    Rewrite.applyRulesUntilCannot(lambda, alwaysApply)
  }

  private def printMinAndMaxDepth(lambda: Seq[Lambda]): Unit = {
    val res = lambda.map(NumberExpression.byDepth(_).values.max)
    println(s"with a minimum depth of ${res.min} of and maximum depth of ${res.max}")
  }

}

class HighLevelRewrite {

  private val highLevelRules =
    Seq(
      MacroRules.apply2DRegisterBlocking,
      MacroRules.apply1DRegisterBlocking,
      MacroRules.tileMapMap,
      MacroRules.finishTiling
    )

  private var failures = 0

  def apply(lambda: Lambda, levels: Int): Seq[(Lambda, Seq[Rule])] = {
    val rewritten = rewrite(lambda, levels)
    println(failures + " rule application failures.")
    rewritten
  }

  private def rewrite(lambda: Lambda, levels: Int,
                      rulesSoFar: Seq[Rule] = Seq()
                     ): Seq[(Lambda, Seq[Rule])] = {

    TypeChecker.check(lambda.body)

    var rewritten = Seq[(Lambda, Seq[Rule])]()

    val rulesToTry = filterRules(rulesSoFar)

    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, rulesToTry)
    allRulesAt.foreach(ruleAt => {
      try {
        val applied = Rewrite.applyRuleAt(lambda, ruleAt._2, ruleAt._1)

        TypeChecker(applied)

        rewritten = rewritten :+ (applied, rulesSoFar :+ ruleAt._1)

      } catch {
        case t: Throwable =>
          // println(s"Applying ${ruleAt._1} to\n$lambda\nafter ${rulesSoFar.mkString(", ")},\nfailed with\n$t.\n")
          failures += 1
      }
    })

    if (levels == 1 || rulesToTry.isEmpty) {
      rewritten
    } else {
      rewritten ++ rewritten.flatMap(pair => rewrite(pair._1, levels-1, pair._2))
    }
  }

  def filterRules(rulesApplied: Seq[Rule]): Seq[Rule] = {
    val distinctRulesApplied = rulesApplied.distinct
    val numberOfTimesEachRule = distinctRulesApplied.map(r1 => rulesApplied.count(r2 => r1 == r2))

    // Filter out some rules
    var dontTryThese = (distinctRulesApplied, numberOfTimesEachRule)
      .zipped
      .filter((_, times) => times >= 2)
      ._1

    if (distinctRulesApplied.contains(MacroRules.apply1DRegisterBlocking)
      || distinctRulesApplied.contains(MacroRules.apply2DRegisterBlocking)
      || distinctRulesApplied.contains(MacroRules.tileMapMap))
      dontTryThese = MacroRules.tileMapMap +: dontTryThese

    if (distinctRulesApplied.contains(MacroRules.apply1DRegisterBlocking))
      dontTryThese = MacroRules.apply2DRegisterBlocking +: MacroRules.tileMapMap +: dontTryThese

    if (distinctRulesApplied.contains(MacroRules.apply2DRegisterBlocking))
      dontTryThese = MacroRules.apply1DRegisterBlocking +: dontTryThese

    val rulesToTry = highLevelRules diff dontTryThese
    rulesToTry
  }
}
