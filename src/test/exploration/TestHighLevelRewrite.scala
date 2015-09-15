package exploration

import java.nio.file.{Files, Paths}
import java.security.MessageDigest

import apart.arithmetic.Var
import ir.ast._
import ir.{ArrayType, TypeChecker}
import opencl.ir._

import scala.io.Source
import scala.sys.process._

object TestHighLevelRewrite {

  val highLevelRules =
    Seq(
      MacroRules.apply2DRegisterBlocking,
      MacroRules.apply1DRegisterBlocking,
      MacroRules.tileMapMap,
      MacroRules.finishTiling
    )

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

    val newLambdas = rewrite(startingExpressionATransposed, Seq(), 5)

    val distinctLambdas = newLambdas.map(_._2).distinct

    println(failures + " rule application failures.")
    println(newLambdas.length + " resulting expressions.")
    println(distinctLambdas.length + " distinct sequences of rules (possibly different locations)")

    val oneKernel = newLambdas.filter(pair => hasOneMapOnFirstLevels(pair._1))

    println(oneKernel.length + " expressions with one kernel")

    val dumpThese = oneKernel.filter(pair =>

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


    println(dumpThese.length + " expressions to dump")

    val lambdas = dumpThese.map(_._1)
    printMinAndMaxDepth(lambdas)

    dumpLambasToFiles(lambdas)

  }

  def dumpLambdaToString(lambda: Lambda): String = {

    val fullString =  dumpLambdaToStringWithoutDecls(lambda)

    val withIndex: List[(String, Int)] = findVariables(fullString)

    val decls = withIndex.map(pair =>
      "val " + getNewName(pair) + " = Var(\"" + getIdentifier(pair) + "\")\n"
    ).mkString("")

    decls + "\n" + replaceVariableDeclarations(fullString, withIndex)
  }

  def replaceVariableDeclarations(fullString: String, withIndex: List[(String, Int)]): String = {
    withIndex.foldLeft(fullString)((currentString, toReplace) =>
      currentString.replaceAll(toReplace._1, getNewName(toReplace)))
  }

  def dumpLambdaToMethod(lambda: Lambda): String = {
    val fullString =  dumpLambdaToStringWithoutDecls(lambda)

    val variables = findVariables(fullString)

    val replacedVariableNames = replaceVariableDeclarations(fullString, variables)

    val seqName = "variables"

    val declarations = variables.map(pair => {
      "val " + getNewName(pair) + " = " + seqName +"(" + pair._2 + ")"
    }).mkString("\n")

    val method =
      s"""($seqName: Seq[ArithExpr]) => {
        |$declarations
        |
        |$replacedVariableNames
        |}
      """.stripMargin

    method
  }

  def findVariables(fullString: String): List[(String, Int)] = {
    val variable = """v_\p{Alnum}*(_id)?_\d+""".r

    val vars = variable.findAllIn(fullString).map(_.toString).toList.distinct

    val withIndex = vars.zipWithIndex
    withIndex
  }

  def getIdentifier(toReplace: (String, Int)): String = {
    toReplace._1.substring(toReplace._1.indexOf("_") + 1, toReplace._1.lastIndexOf("_"))
  }

  def getNewName(toReplace: (String, Int)): String = {
    "v_" + getIdentifier(toReplace) + "_" + toReplace._2
  }

  def dumpLambdaToStringWithoutDecls(lambda: Lambda): String = {
    val types = lambda.params.map(p => ScalaPrinter(p.t)).mkString(", ")
    val expr = ScalaPrinter(lambda)
    val fullString = expr.substring(0, 4) + types + "," + expr.substring(4)

    val param = """p_\d+""".r

    val params = param.findAllMatchIn(fullString).map(_.toString()).toList.distinct

    params.zipWithIndex.foldRight(fullString)((toReplace, currentString) =>
      currentString.replaceAll(toReplace._1, "p_" + toReplace._2))

  }

  def dumpLambasToFiles(lambdas: Seq[Lambda]): Unit = {

    lambdas.zipWithIndex.par.foreach(pair => {
      val lambda = pair._1
      val id = pair._2

      println(s"Processing $id/${lambdas.length - 1}")

      try {

        val partRedToReduce = Rewrite.applyRulesUntilCannot(lambda, Seq(Rules.partialReduceToReduce))
        val lowerNext = SimplifyAndFuse(partRedToReduce)
        val appliedRules = applyAlwaysRules(lowerNext)

        val stringRep = dumpLambdaToString(appliedRules)

        val sha256 = Sha256Hash(stringRep)
        val folder = "lambdas/" + sha256.charAt(0) + "/" + sha256.charAt(1)

        dumpToFile(stringRep, sha256, folder)
      } catch {
        case t: Throwable =>
          println(s"No $id failed with ${t.toString.replaceAll("\n", " ")}.")
      }
    })

  }

  def Sha256Hash(value: String): String = {
    val md = MessageDigest.getInstance("SHA-256")
    md.update(value.getBytes("UTF-8"))
    val digest = md.digest()
    String.format("%064x", new java.math.BigInteger(1, digest))
  }

  def dumpToFile(content: String, filename2: String, path: String): Unit = {
    var filename = filename2

    ("mkdir -p " + path).!

    if (Files.exists(Paths.get(path + "/" + filename))) {
      val warningString = "Warning! Clash at " + filename + ".\n"

      val clashingContent = Source.fromFile(path + "/" + filename).getLines().mkString("\n")

      if (clashingContent != content) {
        println(warningString + "Content is different, adding System.currentTimeMillis().")
        filename = filename + "_" + System.currentTimeMillis()
      } else {
        println(warningString + "Content is the same, skipping.")
      }

    }

    scala.tools.nsc.io.File(path + "/" + filename).writeAll(content)
  }

  def lower(lambdas: Seq[Lambda], numRandom: Int): List[Lambda] = {
    var loweredLambdas = List[Lambda]()

    var toGo = numRandom
    val numLambda = lambdas.length

    while (toGo > 0) {

      val currentLambda = lambdas(1/*util.Random.nextInt(numLambda)*/)

      try {
        val lowerNext = Lower.lowerNoAddressSpaces(currentLambda)
        val appliedRules = applyAlwaysRules(lowerNext)
        loweredLambdas = appliedRules :: loweredLambdas
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
