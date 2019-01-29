package exploration

import java.io.{File, FileWriter}
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger
import ir._
import ir.ast._
import ir.view._
import opencl.ir._
import lift.arithmetic.{?, ArithExpr}
import opencl.generator.{NDRange, RangesAndCounts}
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._
import rewriting._
import rewriting.rules._
import rewriting.macrorules.{MacroRules, ReuseRules, SlideTiling}
import rewriting.utils._
import HighLevelRewriteSettings._
import ParseSettings.strictHighLevelReads

object HighLevelRewrite {

  private val logger = Logger(this.getClass)

  private val processed = new AtomicInteger(0)

  private val parser = new ArgotParser("HighLevelRewrite")

  parser.flag[Boolean](List("h", "help"),
    "Show this message.") {
    (sValue, _) =>
      parser.usage()
      sValue
  }

  private val input = parser.parameter[String]("input",
    "Input file containing the lambda to use for rewriting",
    optional = false) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage("Input file \"" + s + "\" does not exist")
      s
  }

  private val output = parser.option[String](List("o", "output"), "name.",
    "Store the created lambdas into this folder."
  ) {
    (s, _) =>
      val file = new File(s)
      if (file.exists)
        parser.usage("Output location \"" + s + "\" already exists")
      s
  }

  private val settingsFile = parser.option[String](List("f", "file"), "name",
    "The settings file to use."
  ) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage(s"Settings file $file doesn't exist.")
      s
  }

  protected[exploration] val explorationDepth = parser.option[Int](List("d", keyExplorationDepth), "exploration depth",
    s"How deep to explore (default: $defaultExplorationDepth)")

  protected[exploration] val depthFilter = parser.option[Int](List(keyDepthFilter), "depth filter",
    s"Cutoff depth for filtering (default: $defaultDepthFilter")

  protected[exploration] val distanceFilter = parser.option[Int](List(keyDistanceFilter), "distance filter",
    s"Number of Split/Join/Scatter/Gather/asVector/asScalar allowed between user functions (default: $defaultDistanceFilter")

  protected[exploration] val ruleRepetition = parser.option[Int](List(keyRuleRepetition), "rule repetition",
    s"How often the same rule can be applied (default: $defaultRuleRepetition)")

  protected[exploration] val ruleCollection = parser.option[String](List(keyRuleCollection), "rule collection",
    s"Which collection of rules are used for rewriting (default: $defaultRuleCollection)")

  protected[exploration] val vectorWidth = parser.option[Int](List("vw", keyVectorWidth), "vector width",
    s"The vector width to use for vectorising rewrites (default: $defaultVectorWidth)")

  protected[exploration] val sequential = parser.flag[Boolean](List("s", "seq", keySequential),
    s"Don't execute in parallel (default: $defaultSequential")

  protected[exploration] val onlyLower = parser.flag[Boolean](List(keyOnlyLower),
    s"Do not perform high-level rewriting - only print lambda to enable next rewriting stages (default: $defaultOnlyLower)")

  private var settings = HighLevelRewriteSettings.createDefault

  def main(args: Array[String]): Unit = {

    try {
      parser.parse(args)
      settings = ParseSettings(settingsFile.value).highLevelRewriteSettings

      logger.info(s"Settings:\n$settings")
      logger.info(s"Arguments: ${args.mkString(" ")}")
      logger.info(s"Defaults:")
      logger.info(defaultParameters.mkString("\n"))


      val fullFilename = input.value.get
      // remove file ending if provided
      val filename = if (fullFilename.contains("."))
        fullFilename.substring(0, fullFilename.lastIndexOf('.'))
      else
        fullFilename

      val lambda = ParameterRewrite.readLambdaFromFile(fullFilename)

      val dumpThese = if (settings.onlyLower)
        Seq((lambda, Seq()))
      else
        rewriteExpression(lambda)

      println(dumpThese.length + " expressions to dump")

      val lambdas = dumpThese.map(_._1)
      printMinAndMaxDepth(lambdas)

      val folderName = output.value.getOrElse(filename.split("/").last)

      dumpLambdasToFiles(dumpThese, folderName)
    } catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def rewriteExpression(startingExpression: Lambda): Seq[(Lambda, Seq[Rule])] = {
    val newLambdas =
      (new HighLevelRewrite(
        settings.vectorWidth,
        settings.ruleRepetition,
        settings.explorationDepth,
        settings.ruleCollection
      )
        ) (startingExpression)

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
    import View.getSubViews
    val copiedLambda = Lambda.copyStructure(lambda)

    val userFuns = Expr.visitWithState(Seq[FunCall]())(copiedLambda.body, {
      case (call@FunCall(_: UserFun, _*), seq) => seq :+ call
      case (call@FunCall(_: VectorizeUserFun, _*), seq) => seq :+ call
      case (_, seq) => seq
    })

    TypeChecker(copiedLambda)
    InferOpenCLAddressSpace(copiedLambda)
    RangesAndCounts(copiedLambda, NDRange(?, ?, ?), NDRange(?, ?, ?), collection.Map())
    OpenCLMemoryAllocator(copiedLambda)
    View(copiedLambda)

    val memVars = userFuns.map(_.mem.variable)

    val varsWithDataFlow = userFuns.map(_.args.filter(x => getSubViews(x.view).exists({
      case ViewMem(v, _) => memVars.contains(v)
      case _ => false
    }))).filter(_.nonEmpty).flatten

    val numberOfPatterns = varsWithDataFlow.map(x => getSubViews(x.view).count(patternsToCount.isDefinedAt)) :+ 0

    val maxNumberOfPatterns = numberOfPatterns.max

    maxNumberOfPatterns <= settings.distance
  }

  private val patternsToCount: PartialFunction[View, Unit] = {
    case _: ViewSplit =>
    case _: ViewJoin =>
    case _: ViewReorder =>
    case _: ViewAsVector =>
    case _: ViewAsScalar =>
  }

  def filterByDepth(pair: (Lambda, Seq[Rule])): Boolean =
    filterByDepth(pair._1, pair._2)

  def filterByDepth(lambda: Lambda, ruleSeq: Seq[Rule] = Seq()): Boolean = {
    val cutoff = settings.depth
    val depth = getLambdaDepth(lambda)

    val isTiling = ruleSeq.nonEmpty && ruleSeq.head == ReuseRules.tileMapMap
    val has2finishTiling = isTiling && ruleSeq.length == 5 &&
      ruleSeq.count(_ == ReuseRules.finishTiling) == 2

    val is1DBlocking = has2finishTiling &&
      ruleSeq.count(_ == ReuseRules.apply1DRegisterBlocking) == 2
    val is2DBlocking = has2finishTiling &&
      ruleSeq.count(_ == ReuseRules.apply2DRegisterBlocking) == 2

    isTiling && has2finishTiling &&
      (is2DBlocking && depth <= cutoff + 2 || is1DBlocking && depth <= cutoff + 1) ||
      depth <= cutoff
  }

  private[exploration] def getLambdaDepth(lambda: Lambda) =
    NumberExpression.byDepth(lambda).values.max

  private def dumpLambdasToFiles(lambdas: Seq[(Lambda, Seq[Rule])], topLevelFolder: String): Unit = {
    val x = if (settings.sequential) lambdas else lambdas.par

    x.foreach(lambda => {
      val id = processed.getAndIncrement() + 1

      print(s"\rProcessing $id/${lambdas.length}")

      try {

        val appliedRules = finishRewriting(lambda._1)

        if (filterByDistance(appliedRules)) {

          val stringRep = DumpToFile.dumpLambdaToString(appliedRules)

          val sha256 = DumpToFile.Sha256Hash(stringRep)
          val folder = topLevelFolder + "/" + sha256.charAt(0) + "/" + sha256.charAt(1)

          if (DumpToFile.dumpToFile(stringRep, sha256, folder)) {
            // Add to index if it was unique
            synchronized {
              val idxFile = new FileWriter(topLevelFolder + "/index", true)
              idxFile.write(folder + "/" + sha256 + "\n")
              idxFile.close()
            }

            val rules = lambda._2.mkString(",")
            DumpToFile.dumpToFile(rules, sha256 + "_rules", folder)
          }

        } else
          logger.warn("lambda has been filtered out - distance too long")
      } catch {
        case t: Throwable =>
          logger.warn(s"Dumping $lambda failed.", t)
      }
    })

    println()
  }

  def finishRewriting(lambda: Lambda): Lambda = {
    val partRedToReduce =
      Rewrite.applyRuleUntilCannot(lambda, ReduceRules.partialReduceToReduce)

    val simplified = SimplifyAndFuse(partRedToReduce)
    applyAlwaysRules(simplified)
  }

  // If the lambda does not have one map, then needs 2 kernels
  private def hasOneMapOnFirstLevel(lambda: Lambda): Boolean =
    Utils.countMapsAtCurrentLevel(lambda.body) == 1

  def applyAlwaysRules(lambda: Lambda): Lambda = {
    val alwaysApply = Seq(ReuseRules.moveTransposeInsideTiling)

    Rewrite.applyRulesUntilCannot(lambda, alwaysApply)
  }

  private def printMinAndMaxDepth(lambda: Seq[Lambda]): Unit = {
    if (lambda.size == 1)
      println("No rules were applicable...")

    val res = lambda.map(getLambdaDepth)
    println(s"with a minimum depth of ${res.min} of and maximum depth of ${res.max}")
  }

}

class HighLevelRewrite(
                        val vectorWidth: ArithExpr = HighLevelRewriteSettings.defaultVectorWidth,
                        val repetitions: Int = HighLevelRewriteSettings.defaultRuleRepetition,
                        val explorationDepth: Int = HighLevelRewriteSettings.defaultExplorationDepth,
                        val ruleCollection: String = HighLevelRewriteSettings.defaultRuleCollection) {

  private[exploration] val vecRed = MacroRules.vectorizeReduce(vectorWidth)
  private[exploration] val vecZip = OpenCLRules.vectorizeMapZip(vectorWidth)

  object RuleCollection {

    private val convolution1DRules = Seq(SlideTiling.tileStencils)
    private val convolution2DRules = Seq(
      SlideTiling.tile2DStencils,
      SlideTiling.tile2DStencilsZip,
      SlideTiling.tile2DStencilsZip6
    )
    private val defaultRules = Seq(
      ReuseRules.apply2DRegisterBlocking,
      ReuseRules.apply2DRegisterBlockingNoReorder,
      ReuseRules.apply1DRegisterBlocking,
      ReuseRules.tileMapMap,
      ReuseRules.finishTiling,
      MacroRules.partialReduceWithReorder,
      SlideTiling.tileStencils,
      vecRed,
      vecZip,
      MacroRules.interchange,
      ReuseRules.introduceReuseFromMap,
      Rules.splitJoin
    )

    private val ruleCollectionMap = scala.collection.Map(
      "convolution1D" -> convolution1DRules,
      "convolution2D" -> convolution2DRules,
      "default" -> defaultRules)

    def apply(collection: String): Seq[Rule] =
      ruleCollectionMap.getOrElse(collection, defaultRules)
  }

  private val logger = Logger(this.getClass)

  private val highLevelRules = RuleCollection(ruleCollection)

  private var failures = 0

  def apply(lambda: Lambda): Seq[(Lambda, Seq[Rule])] = {
    val prepared = SimplifyAndFuse.withoutPreventingFurtherOptimisation(lambda)
    logger.info(s"Enabled rules:\n\t${highLevelRules.mkString(",\t\n ")}")
    val rewritten = rewrite(prepared, explorationDepth)
    logger.warn(failures + " rule application failures.")
    rewritten :+ (lambda, Seq()) :+ (prepared, Seq())
  }

  private def rewrite(lambda: Lambda,
                      explorationLayer: Int,
                      rulesSoFar: Seq[Rule] = Seq()
                     ): Seq[(Lambda, Seq[Rule])] = {

    TypeChecker.check(lambda.body)

    var rewritten = Seq[(Lambda, Seq[Rule])]()

    val rulesToTry = filterRules(rulesSoFar, lambda)

    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, rulesToTry)

    allRulesAt.foreach(ruleAt => {
      try {
        val applied = Rewrite.applyRuleAt(lambda, ruleAt.expr, ruleAt.rule)

        TypeChecker(applied)

        rewritten = rewritten :+ (applied, rulesSoFar :+ ruleAt.rule)

      } catch {
        case _: Throwable =>
          logger.warn(s"Applying ${ruleAt.rule} to\n$lambda\nafter ${rulesSoFar.mkString(", ")} failed.")
          failures += 1
      }
    })

    if (explorationLayer == 1 || rulesToTry.isEmpty) {
      rewritten
    } else {
      rewritten ++ rewritten.flatMap(pair => rewrite(pair._1, explorationLayer - 1, pair._2))
    }
  }

  def filterRules(rulesApplied: Seq[Rule], lambda: Lambda): Seq[Rule] = {
    val distinctRulesApplied = rulesApplied.distinct
    val numberOfTimesEachRule = distinctRulesApplied.map(r1 => rulesApplied.count(r2 => r1 == r2))

    // Filter out some rules
    var dontTryThese = (distinctRulesApplied, numberOfTimesEachRule)
      .zipped
      .filter((_, times) => times >= repetitions)
      ._1

    if (HighLevelRewrite.getLambdaDepth(lambda) > 2)
      dontTryThese = Rules.splitJoin +: dontTryThese

    if (!distinctRulesApplied.contains(ReuseRules.tileMapMap))
      dontTryThese = ReuseRules.finishTiling +: dontTryThese

    if (distinctRulesApplied.contains(ReuseRules.apply2DRegisterBlocking))
      dontTryThese = ReuseRules.introduceReuseFromMap +: MacroRules.interchange +: ReuseRules.apply2DRegisterBlockingNoReorder +: dontTryThese

    if (distinctRulesApplied.contains(ReuseRules.apply2DRegisterBlockingNoReorder))
      dontTryThese = ReuseRules.introduceReuseFromMap +: MacroRules.interchange +: ReuseRules.apply2DRegisterBlocking +: dontTryThese

    if (distinctRulesApplied.contains(ReuseRules.apply1DRegisterBlocking)
      || distinctRulesApplied.contains(ReuseRules.apply2DRegisterBlocking)
      || distinctRulesApplied.contains(ReuseRules.apply2DRegisterBlockingNoReorder)
      || distinctRulesApplied.contains(ReuseRules.tileMapMap))
      dontTryThese = ReuseRules.introduceReuseFromMap +: MacroRules.interchange +: ReuseRules.tileMapMap +: dontTryThese

    if (distinctRulesApplied.contains(ReuseRules.apply1DRegisterBlocking))
      dontTryThese = ReuseRules.introduceReuseFromMap +: MacroRules.interchange +: ReuseRules.apply2DRegisterBlocking +: ReuseRules.apply2DRegisterBlockingNoReorder +: ReuseRules.tileMapMap +: dontTryThese

    if (distinctRulesApplied.contains(ReuseRules.apply2DRegisterBlocking)
      || distinctRulesApplied.contains(ReuseRules.apply2DRegisterBlockingNoReorder))
      dontTryThese = ReuseRules.introduceReuseFromMap +: MacroRules.interchange +: ReuseRules.apply1DRegisterBlocking +: dontTryThese

    if (distinctRulesApplied.contains(vecZip)
      || (distinctRulesApplied.contains(ReuseRules.tileMapMap)
      && !distinctRulesApplied.contains(ReuseRules.finishTiling)))
      dontTryThese = vecZip +: dontTryThese

    if (distinctRulesApplied.contains(ReuseRules.tileMapMap)
      || distinctRulesApplied.contains(ReuseRules.apply1DRegisterBlocking)
      || distinctRulesApplied.contains(ReuseRules.apply2DRegisterBlocking)
      || distinctRulesApplied.contains(ReuseRules.apply2DRegisterBlockingNoReorder)
      || distinctRulesApplied.contains(vecRed))
      dontTryThese = ReuseRules.introduceReuseFromMap +: MacroRules.interchange +: vecRed +: dontTryThese

    if (distinctRulesApplied.contains(ReuseRules.introduceReuseFromMap)
      || distinctRulesApplied.contains(MacroRules.interchange))
      dontTryThese = ReuseRules.finishTiling +: ReuseRules.tileMapMap +: ReuseRules.apply1DRegisterBlocking +: ReuseRules.apply2DRegisterBlocking +: dontTryThese

    if (distinctRulesApplied.contains(MacroRules.interchange))
      dontTryThese = MacroRules.interchange +: dontTryThese

    if (distinctRulesApplied.contains(MacroRules.partialReduceWithReorder))
      dontTryThese = ReuseRules.introduceReuseFromMap +: MacroRules.interchange +: dontTryThese

    val rulesToTry = highLevelRules diff dontTryThese
    rulesToTry
  }
}
