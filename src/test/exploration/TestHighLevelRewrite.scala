package exploration

import apart.arithmetic.Var
import ir.{TypeChecker, ArrayType}
import ir.ast._
import opencl.ir._

object TestHighLevelRewrite {

  val highLevelRules =
    Seq(
      MacroRules.apply2DRegisterBlocking,
      MacroRules.apply1DRegisterBlocking,
      MacroRules.tileMapMap,
      MacroRules.finishTiling
    )

  def main(args: Array[String]) = {
    val N = 4096//Var("N")
    val M = 4096//Var("M")
    val K = 4096//Var("K")

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

    val newLambdas = rewrite(startingExpressionATransposed, Seq(), 5)

    val distinctLambdas = newLambdas.map(_._2).distinct

    println(failures + " rule application failures.")
    println(newLambdas.length + " resulting expressions.")
    println(distinctLambdas.length + " distinct sequences of rules (possibly different locations)")

    println(
      newLambdas.count(pair =>
        pair._2 ==
          List(
            MacroRules.tileMapMap,
            MacroRules.finishTiling,
            MacroRules.finishTiling
          )
      ) + " expressions with the basic tiled sequence"
    )

    val oneDBlockingSequence = newLambdas.filter(pair =>
      pair._2 ==
        List(
          MacroRules.tileMapMap,
          MacroRules.finishTiling,
          MacroRules.apply1DRegisterBlocking,
          MacroRules.apply1DRegisterBlocking,
          MacroRules.finishTiling
        )
//        ||
//        pair._2 ==
//          List(
//            MacroRules.tileMapMap,
//            MacroRules.apply1DRegisterBlocking,
//            MacroRules.finishTiling,
//            MacroRules.finishTiling,
//            MacroRules.apply1DRegisterBlocking
//          )
      )

    println(
      oneDBlockingSequence.length + " expressions with a possible tiled 1D blocking sequence,"
    )

    printMinAndMaxDepth(oneDBlockingSequence.map(_._1))

    val twoDBlockingSequence = newLambdas.filter(pair =>
      pair._2 ==
        List(
          MacroRules.tileMapMap,
          MacroRules.finishTiling,
          MacroRules.apply2DRegisterBlocking,
          MacroRules.apply2DRegisterBlocking,
          MacroRules.finishTiling
        ) && NumberExpression.byDepth(pair._1).values.max <= 8
          && hasOneMapOnFirstLevels(pair._1)
//        ||
//        pair._2 ==
//          List(
//            MacroRules.tileMapMap,
//            MacroRules.apply2DRegisterBlocking,
//            MacroRules.finishTiling,
//            MacroRules.finishTiling,
//            MacroRules.apply2DRegisterBlocking
//          )
    )

    println(
      twoDBlockingSequence.length + " expressions with a possible tiled 2D blocking sequence,"
    )

    val lambdas = twoDBlockingSequence.map(_._1)
    printMinAndMaxDepth(lambdas)

    val lowerSome = lower(lambdas, 1)

    println(lowerSome)
    println("testing " + lowerSome.length + " expressions")

    TestLowLevelRewrite.lowlevelexecutor(lowerSome)
  }

  def lower(lambdas: Seq[Lambda], numRandom: Int): List[Lambda] = {
    var loweredLambdas = List[Lambda]()

    var toGo = numRandom
    val numLambda = lambdas.length

    while (toGo > 0) {

      val currentLambda = lambdas(2/*util.Random.nextInt(numLambda)*/)

      try {
        val appliedRules = applyAlwaysRules(currentLambda)
        val lowerNext = Lower.lowerNoAddressSpaces(appliedRules)
        loweredLambdas = lowerNext :: loweredLambdas
      } catch {
        case t: Throwable =>
          println(s"Lowering\n$currentLambda failed with\n$t.")
      }

      toGo -= 1

      println(s"${numRandom - toGo}/$numRandom expressions lowered.")
    }

    loweredLambdas
  }

  def hasOneMapOnFirstLevels(lambda: Lambda): Boolean = {
    val mapsOnLevelOne = Utils.visitFunCallChainWithState(0)(lambda.body, (expr, count) => {
      expr match {
        case FunCall(m: AbstractMap, _) => count + 1
        case _ => count
      }
    })

    val mapsOnLevelTwo = Utils.visitFunCallChainWithState(0)(MacroRules.getMapBody(MacroRules.getMapAtDepth(lambda.body, 0)), (expr, count) => {
      expr match {
        case FunCall(m: AbstractMap, _) => count + 1
        case _ => count
      }
    })

    mapsOnLevelOne == 1 && mapsOnLevelTwo == 1
  }

  def applyAlwaysRules(lambda: Lambda): Lambda = {
    val alwaysApply = MacroRules.moveTransposeInsideTiling

    val locations = Rewrite.listAllPossibleRewrites(lambda, alwaysApply)
    if (locations.nonEmpty) {
      val ruleAt = locations.head
      applyAlwaysRules(Rewrite.applyRuleAt(lambda, ruleAt._2, ruleAt._1))
    } else {
      lambda
    }
  }

  def printMinAndMaxDepth(lambda: Seq[Lambda]): Unit = {
    val res = lambda.map(NumberExpression.byDepth(_).values.max)
    println(s"with a minimum depth of ${res.min} of and maximum depth of ${res.max}")
  }

  var failures = 0
  var depthReached = 0

  def rewrite(lambda: Lambda, rulesSoFar: Seq[Rule], levels: Int): Seq[(Lambda, Seq[Rule])] = {
    TypeChecker.check(lambda.body)

    var rewritten = Seq[(Lambda, Seq[Rule])]()

    val distinctRulesApplied = rulesSoFar.distinct
    val numberOfTimesEachRule = distinctRulesApplied.map(r1 => rulesSoFar.count(r2 => r1 == r2))

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

    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, rulesToTry)
    allRulesAt.foreach(ruleAt => {
      try{
        val applied = Rewrite.applyRuleAt(lambda, ruleAt._2, ruleAt._1)

        TypeChecker(applied)

        rewritten = rewritten :+ (applied, rulesSoFar :+ ruleAt._1)

      } catch {
        case t: Throwable =>
//          println(s"Applying ${ruleAt._1} to\n$lambda\nafter ${rulesSoFar.mkString(", ")},\nfailed with\n$t.\n")
        failures += 1
      }
    })

    if (levels == 1 || rulesToTry.isEmpty) {
      depthReached += 1
      rewritten
    } else {
      rewritten ++ rewritten.flatMap(pair => rewrite(pair._1, pair._2, levels-1))
    }
  }

}
