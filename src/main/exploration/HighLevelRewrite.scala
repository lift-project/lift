package exploration

import java.io.{File, FileWriter}
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger
import ir.TypeChecker
import ir.ast._
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._
import rewriting._
import rewriting.utils._

object HighLevelRewrite {

  private val logger = Logger(this.getClass)

  private val processed = new AtomicInteger(0)

  private val parser = new ArgotParser("HighLevelRewrite")

  parser.flag[Boolean](List("h", "help"),
    "Show this message.") {
    (sValue, opt) =>
      parser.usage()
      sValue
  }

  private val input = parser.parameter[String]("input",
    "Input file containing the lambda to use for rewriting",
    optional = false) {
    (s, opt) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage("Input file \"" + s + "\" does not exist")
      s
  }

  private val output = parser.option[String](List("o", "output"), "name.",
    "Store the created lambdas into this folder."
    ) {
    (s, opt) =>
      val file = new File(s)
      if (file.exists)
        parser.usage("Output location \"" + s + "\" already exists")
      s
  }

  private val explorationDepth = parser.option[Int](List("d", "explorationDepth"), "depth",
    "How deep to explore.")

  private val depthFilter = parser.option[Int](List("depth"), "depth", "Cutoff depth for filtering.")

  private val distanceFilter = parser.option[Int](List("distance"), "distance",
    "Cutoff distance for filtering.")

  private val vectorWidth = parser.option[Int](List("vector-width", "vw"), "vector width",
    "The vector width to use for vectorising rewrites. Default: 4")

  private val sequential = parser.flag[Boolean](List("s", "seq", "sequential"),
    "Don't execute in parallel.")

  def main(args: Array[String]) = {

    try {
      parser.parse(args)

      val filename = input.value.get
      val lambda = ParameterRewrite.readLambdaFromFile(filename)

      val dumpThese = rewriteExpression(lambda)

      println(dumpThese.length + " expressions to dump")

      val lambdas = dumpThese.map(_._1)
      printMinAndMaxDepth(lambdas)

      val folderName = output.value.getOrElse(filename.split("/").last)

      dumpLambdasToFiles(lambdas :+ lambda, folderName)
    } catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def rewriteExpression(startingExpression: Lambda): Seq[(Lambda, Seq[Rule])] = {
    val maxDepth = explorationDepth.value.getOrElse(5)
    val newLambdas =
      (new HighLevelRewrite(vectorWidth.value.getOrElse(4)))(startingExpression, maxDepth)

    val filtered = filterExpressions(newLambdas)

    filtered
  }

  def filterExpressions(newLambdas: Seq[(Lambda, Seq[Rule])]): Seq[(Lambda, Seq[Rule])] = {
    val distinctLambdas = newLambdas.map(_._2).distinct

    println(newLambdas.length + " resulting expressions.")
    println(distinctLambdas.length +
      " distinct sequences of rules (possibly different locations)")

    val oneKernel = newLambdas.filter(pair => hasOneMapOnFirstLevel(pair._1))

    println(oneKernel.length + " expressions with one kernel")

    val filterDepth = oneKernel.filter(filterByDepth)

    filterDepth
  }

   def filterByDistance(lambda: Lambda): Boolean = {
    val numberMap = NumberExpression.depthFirst(lambda)

    val userFunCalls = Expr.visitWithState(List[Expr]())(lambda.body, (expr, state) => {
      expr match {
        case FunCall(uf: UserFun, _*) if !uf.name.contains("id") => expr :: state
        case FunCall(uf: VectorizeUserFun, _*) if !uf.userFun.name.contains("id")
          => expr :: state
        case _ => state
      }
    })

    if (userFunCalls.length == 1)
      return true

    // TODO: A more reasonable default. Will filter out gemv.
    val cutoff = distanceFilter.value.getOrElse(9)

    val ids = userFunCalls.map(numberMap(_)).sorted

    ids.sliding(2).map(w => (w.head - w(1)).abs <= cutoff).forall(i => i)
  }

   def filterByDepth(pair: (Lambda, Seq[Rule])): Boolean = {
    val cutoff = depthFilter.value.getOrElse(6)
    (pair._2.head == MacroRules.tileMapMap
      && pair._2.tail.diff(List(MacroRules.apply2DRegisterBlocking,
      MacroRules.apply2DRegisterBlocking,
      MacroRules.finishTiling,
      MacroRules.finishTiling)).isEmpty
      && NumberExpression.byDepth(pair._1).values.max <= cutoff+2) ||
      (pair._2.head == MacroRules.tileMapMap
        && pair._2.tail.diff(List(MacroRules.apply1DRegisterBlocking,
        MacroRules.apply1DRegisterBlocking,
        MacroRules.finishTiling,
        MacroRules.finishTiling)).isEmpty
        && NumberExpression.byDepth(pair._1).values.max <= cutoff+1) ||
      NumberExpression.byDepth(pair._1).values.max <= cutoff
  }

  private def dumpLambdasToFiles(lambdas: Seq[Lambda], topLevelFolder: String): Unit = {
    val x = if (sequential.value.isDefined) lambdas else lambdas.par

    x.foreach(lambda => {
      val id = processed.getAndIncrement()

      print(s"\rProcessing $id/${lambdas.length - 1}")

      try {

        val appliedRules: Lambda = finishRewriting(lambda)

        if (filterByDistance(appliedRules)) {

          val stringRep = Utils.dumpLambdaToString(appliedRules)

          val sha256 = Utils.Sha256Hash(stringRep)
          val folder = topLevelFolder + "/" + sha256.charAt(0) + "/" + sha256.charAt(1)

          if (Utils.dumpToFile(stringRep, sha256, folder)) {
            // Add to index if it was unique
            synchronized {
              val idxFile = new FileWriter(topLevelFolder + "/index", true)
              idxFile.write(folder + "/" + sha256 + "\n")
              idxFile.close()
            }
          }

        }
      } catch {
        case t: Throwable =>
          logger.warn(s"Dumping $lambda failed.", t)
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
  private def hasOneMapOnFirstLevel(lambda: Lambda): Boolean =
    Utils.countMapsAtCurrentLevel(lambda.body) == 1

  def applyAlwaysRules(lambda: Lambda): Lambda = {
    val alwaysApply = Seq(MacroRules.moveTransposeInsideTiling)

    Rewrite.applyRulesUntilCannot(lambda, alwaysApply)
  }

  private def printMinAndMaxDepth(lambda: Seq[Lambda]): Unit = {
    val res = lambda.map(NumberExpression.byDepth(_).values.max)
    println(s"with a minimum depth of ${res.min} of and maximum depth of ${res.max}")
  }

}

class HighLevelRewrite(val vectorWidth: Int) {

  private val logger = Logger(this.getClass)

  private val vecRed = MacroRules.vectorizeReduce(vectorWidth)
  private val vecZip = Rules.vectorizeMapZip(vectorWidth)

  private val highLevelRules =
    Seq(
      MacroRules.apply2DRegisterBlocking,
      MacroRules.apply1DRegisterBlocking,
      MacroRules.tileMapMap,
      MacroRules.finishTiling,
      MacroRules.partialReduceWithReorder,
      vecRed,
      vecZip
    )

  private var failures = 0

  def apply(lambda: Lambda, levels: Int): Seq[(Lambda, Seq[Rule])] = {
    val rewritten = rewrite(lambda, levels)
    logger.warn(failures + " rule application failures.")
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

        rewritten = rewritten :+(applied, rulesSoFar :+ ruleAt._1)

      } catch {
        case _: Throwable =>
          logger.warn(s"Applying ${ruleAt._1} to\n$lambda\nafter ${rulesSoFar.mkString(", ")} failed.")
          failures += 1
      }
    })

    if (levels == 1 || rulesToTry.isEmpty) {
      rewritten
    } else {
      rewritten ++ rewritten.flatMap(pair => rewrite(pair._1, levels - 1, pair._2))
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

    if (distinctRulesApplied.contains(vecZip)
          || (distinctRulesApplied.contains(MacroRules.tileMapMap)
            && !distinctRulesApplied.contains(MacroRules.finishTiling)))
      dontTryThese = vecZip +: dontTryThese

    if (distinctRulesApplied.contains(MacroRules.tileMapMap)
        || distinctRulesApplied.contains(MacroRules.apply1DRegisterBlocking)
        || distinctRulesApplied.contains(MacroRules.apply2DRegisterBlocking)
        || distinctRulesApplied.contains(vecRed))
      dontTryThese = vecRed +: dontTryThese

    val rulesToTry = highLevelRules diff dontTryThese
    rulesToTry
  }
}
