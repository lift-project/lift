package benchmarks.conv

import java.math.BigInteger
import java.security.MessageDigest
import com.typesafe.scalalogging.Logger
import exploration.constraint_solvers.ConstraintSolver.SolutionNotFound
import exploration.{ParameterConstraintInference, ParameterRewrite, ParameterSpace}
import ir.ast.Lambda
import lift.arithmetic.{Cst, Var}
import benchmarks.conv.passes.ConvAutoRewritePassSpaceFactory
import LayerSearch.SearchSpace.SearchSpace
import LayerSearch.{ExplorationState, SearchInvariantState, SearchSpace, TunePoint}
import benchmarks.conv.ConvExploration.{nRandomParMappingsExplored, nValidParMappingsExplored}
import benchmarks.conv.layer_configs.LayerConfig
import benchmarks.conv.passes.{ConvAutoRewritePassSearchSpace, ConvAutoRewritePassSearchSpaceExhausted, ConvAutoRewritePassSpaceFactory, ParallelizationAndFusion, Vectorization}
import benchmarks.conv.tuning.ConvTuneParamSpaceFactory
import rewriting.RewriteParamValues.PrintFormat
import benchmarks.conv.passes.ConvAutoRewritePassSpaceFactory.orderedPasses
import benchmarks.conv.passes.ParallelizationAndFusion.Code
import benchmarks.conv.util.BenchmarkStages
import benchmarks.conv.passes.ConvAutoRewritePassSpaceFactory
import benchmarks.conv.util.BenchmarkStages.MD5Hash
import rewriting.passes.RewritingPass.{RewritePassParams, globalNumbering}
import rewriting.utils.{DumpToFile, NumberExpression}
import utils.ShellUtils

import java.io.File
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Random

case class LayerSearch[
  T <: ConvTuneParamSpaceFactory](layerConfigIdx: Int,
                                  layerConfig: LayerConfig,
                                  lastTunePointIdx: Int,
                                  private var skippingPoints: Boolean,
                                  rewindMode: Boolean,
                                  nextRPassSpaceInitSeed: mutable.Map[ConvAutoRewritePassSpaceFactory, Int],
                                  tuneParamSpaceFactory: T,
                                  b: BenchmarkStages[T],
                                  var nextTuneSpaceInitSeed: Int,
                                  var allSearchSpacesExhausted: Boolean = false
                                 )(implicit val runConfig: RunConfig, val jsonSettings: ConvExplorationJSONSettings) {
  private val logger = Logger(this.getClass)

  /**
   * Applies to whole batch if singleTunePointIdxToCheck is empty; otherwise, applies to a single point
   * None = run as usual
   * Some(Left(reason)) = skip everything for now
   * Some(Right(reason)) = skip only script generation, but run the solver to reproduce results at a later point
   */
  def skipCompletelyOrJustScriptGenWithReason(tunePointBatchIdx: Int,
                                              singleTunePointIdxToCheck: Option[Int] = None
                                             )(implicit runConfig: RunConfig): Option[Either[String, String]] = {
    if (allSearchSpacesExhausted)
      Some(Left("all layer's tune param search spaces have been already exhausted"))

    else if (skippingPoints &&
      (tunePointBatchIdx < runConfig.partialBatchIdx.get ||
        (tunePointBatchIdx == runConfig.partialBatchIdx.get && layerConfigIdx < runConfig.continueFromLayer.get) ||
        (tunePointBatchIdx == runConfig.partialBatchIdx.get && layerConfigIdx == runConfig.continueFromLayer.get &&
          singleTunePointIdxToCheck.isDefined &&
          singleTunePointIdxToCheck.get < runConfig.continueFromTunePointForTheContinueFromLayer.get))) {

      val reason = s"continueFromLayer (${runConfig.continueFromLayer.get}) continueFromTunePoint " +
        s"(${runConfig.continueFromTunePointForTheContinueFromLayer}) hasn't been reached"

      if (rewindMode) Some(Right(reason))
      else Some(Left(reason))
    } else {
      skippingPoints = false
      None
    }
  }

  implicit val search: SearchInvariantState = SearchInvariantState(layerConfigIdx)

  /**
   * Obtains a random combination of all (rewrite point | rewrite passes | tune) param values.
   * The combination is obtained in four stages, one for each collection of parameters:
   * rewrite point param values -> rewrite passes param values -> tune param values -> compilation attempt.
   * If a subcombination (e.g. tune param values) cannot be found, the function invalidates the
   * subcombination of the preceding stage (e.g. rewrite passes param values), and tries another
   * again with another subcombination of the preceding stage
   */
  def getAllParamValuesAndCompile(highLevelLambdaBuilder: HighLevelRewriteParamValues => Lambda,
                                  tunePointIdx: Int,
                                  firstSpaceToExplore: SearchSpace): TunePoint[T] = {

    @tailrec
    def advance(state: Either[ExplorationState, ExplorationState]): TunePoint[T] = {
      state match {
        case Right(compilationSuccessful: CompilationSuccessful) => compilationSuccessful.tunePoint
        case Right(state) => advance(state.next)
        case Left(state) => advance(state.next)
      }
    }

    advance(Right(new RewrittenLambdaBuilding(firstSpaceToExplore, layerConfigIdx, layerConfig, tunePointIdx,
      highLevelLambdaBuilder, tuneParamSpaceFactory, b)))
  }

  /**
   * Returns either one of the cached auto rewrite pass search spaces or creates a new one.
   * Rewrites a lambda, but does not return the rewritten lambda since it is saved inside rewritePass
   */
  def getAutoRewritePassSpace(rewritePassFactory: ConvAutoRewritePassSpaceFactory,
                              rewritePassesSpaces: mutable.ListMap[ConvAutoRewritePassSpaceFactory, ConvAutoRewritePassSearchSpace],
                              lambdaToRewrite: Lambda)
                             (implicit runConfig: RunConfig,
                              jsonSettings: ConvExplorationJSONSettings
                             ): Option[ConvAutoRewritePassSearchSpace] = {

    (rewritePassFactory, rewritePassesSpaces.get(rewritePassFactory)) match {
      case (ParallelizationAndFusion, Some(space)) =>
        logger.info("Retrieving the cached rewrite pass search space (Parallelization and fusion). TODO: make sure that it corresponds to the chosen RP values. This is a temporary solution!")
        Some(space)
      case _ =>
        logger.info("Initializing an auto rewrite pass search space for a given choice of rewrite param values")
        rewritePassFactory(layerConfigIdx, lambdaToRewrite, nextRPassSpaceInitSeed(rewritePassFactory)) match {
          case None =>
            logger.info("Could not initialize an auto rewrite pass search space")
            None
          case Some(space) =>
            logger.info("An auto rewrite pass search space initialized.")

            nextRPassSpaceInitSeed(rewritePassFactory) = new Random(
              nextRPassSpaceInitSeed(rewritePassFactory) + 1).nextInt()

            Some(space)
        }
    }
  }

  def isUniqueLambda(lambdaAsString: String): Boolean = {
    val lambdaHash = new BigInteger(1,
      MessageDigest.getInstance("MD5").digest(lambdaAsString.getBytes)
    ).toString(16)
    if (search.visitedLambdaHashes.contains(lambdaHash)) {
      false
    } else {
      search.visitedLambdaHashes += lambdaHash
      true
    }
  }

  def tunePointsWithinBatchToRun(tunePointBatchIdx: Int)(implicit runConfig: RunConfig): Range = {
    val firstTunePointIdxInBatch = tunePointBatchIdx * runConfig.tuneBatchSize
    val untilTunePointIdxInBatch = Math.min((tunePointBatchIdx + 1) * runConfig.tuneBatchSize, lastTunePointIdx + 1)

    firstTunePointIdxInBatch until untilTunePointIdxInBatch
  }


  class RewrittenLambdaBuilding(val firstSpaceToExplore: SearchSpace,
                                val layerConfigIdx: Int,
                                val layerConfig: LayerConfig,
                                val tunePointIdx: Int,
                                val highLevelLambdaBuilder: HighLevelRewriteParamValues => Lambda,
                                val tuneParamSpaceFactory: T,
                                val b: BenchmarkStages[T]
                               )(implicit val runConfig: RunConfig,
                                 val jsonSettings: ConvExplorationJSONSettings,
                                 val search: SearchInvariantState) extends ExplorationState {

    override val searchSpaceEnum: SearchSpace = SearchSpace.RewrittenLambdaBuild

    override val logger: Logger = Logger(this.getClass)

    val tunePointLambdaDir: String = runConfig.lambdaDir(layerConfigIdx, tunePointIdx)

    override val prev: ExplorationState = this

    override def next: Either[ExplorationState, ParFuseSpaceBuilding] = {

      val f = highLevelLambdaBuilder(null)
      val lambdaToParFuse = tuneParamSpaceFactory.replaceGenericParamsWithPredefined(f)

      Right(new ParFuseSpaceBuilding(this, lambdaToParFuse))
    }
  }

  class ParFuseSpaceBuilding(override val prev: RewrittenLambdaBuilding,
                             val lambdaToParFuse: Lambda)
    extends RewrittenLambdaBuilding(
      prev.firstSpaceToExplore, prev.layerConfigIdx, prev.layerConfig, prev.tunePointIdx,
      prev.highLevelLambdaBuilder, prev.tuneParamSpaceFactory, prev.b)
      with RPassSpaceBuilder {

    override val searchSpaceEnum: SearchSpace = SearchSpace.RewrittenLambdaBuild

    override val logger: Logger = Logger(this.getClass)

    override def next: Either[ExplorationState, ParFuseParamValuesObtaining] = {

      build(logger, layerConfigIdx, lambdaToParFuse, "parFuse", ParallelizationAndFusion,
        () => search.currentParFuseSearchInitSeed,
        newSeed => search.currentParFuseSearchInitSeed = newSeed,
        space => new ParFuseParamValuesObtaining(
          prev = this,
          parFuseSpace = space,
          parMappingsFromCsvToUse = runConfig.parMappingsCsv match {
            case None => None
            case Some(csvPath) =>
              b.readParMappingsFromCsv(
                new File(csvPath),
                runConfig.netConfig.name,
                layerConfigIdx,
                space.rewritingPass) match {
                case Nil =>
                  throw new IllegalStateException(s"No par mappings extracted from $csvPath")
                case nonEmptyList => Some(nonEmptyList)
              }
          }
        ))
    }
  }

  class ParFuseParamValuesObtaining(override val prev: ParFuseSpaceBuilding,
                                    val parFuseSpace: ConvAutoRewritePassSearchSpace,
                                    val parMappingsFromCsvToUse: Option[List[(MD5Hash, RewritePassParams#RewriteParamValuesT)]])
    extends ParFuseSpaceBuilding(prev.prev, prev.lambdaToParFuse)
      with RPassParamValueObtainer {

    override val searchSpaceEnum: SearchSpace = SearchSpace.ParFuseParams

    override val logger: Logger = Logger(this.getClass)

    override def next: Either[ExplorationState, TuneSpaceBuilding] = {

      parMappingsFromCsvToUse match {
        case None =>
          obtain(logger, "parFuse", tunePointIdx, parFuseSpace,
            (parFuseParamValues, parFusedLambda) =>
              new TuneSpaceBuilding(this, parFuseParamValues, parFusedLambda))

        case Some(mappings) =>

          val nextParMappingIdx =
            if (search.foundAPointForCurrentParMappingFromCsv.getOrElse(true) ||
              runConfig.skipUncompilableCsvParMappings)
              (search.lastParMappingUsedFromCsvIdx + 1) % mappings.length
            else search.lastParMappingUsedFromCsvIdx

          val (parMappingHash, parFuseParamValues) = mappings(nextParMappingIdx)

          logger.info(s"Picking parFuse mapping #$nextParMappingIdx (MD5: $parMappingHash) from the CSV set")

          search.lastParMappingUsedFromCsvIdx = nextParMappingIdx
          search.foundAPointForCurrentParMappingFromCsv = Some(false)

          logger.info(s"$tunePointIdx. Rewrite pass params:\n" +
            parFuseParamValues.toStrings(PrintFormat.RewritePass).reverse.mkString(",\n"))

          val parFusedLambda = parFuseSpace.rewritingPass.rewriteTopDown(parFuseParamValues)

          Right(new TuneSpaceBuilding(this, parFuseParamValues, parFusedLambda))
      }
    }
  }

  class TuneSpaceBuilding(override val prev: ParFuseParamValuesObtaining,
                          val parFuseParamValues: RewritePassParams # RewriteParamValuesT,
                          val parFusedLambda: Lambda)
    extends ParFuseParamValuesObtaining(prev.prev, prev.parFuseSpace, prev.parMappingsFromCsvToUse) {

    override val searchSpaceEnum: SearchSpace = SearchSpace.ParFuseParams

    override val logger: Logger = Logger(this.getClass)

    override def next: Either[ExplorationState, TuneParamValuesObtaining] = {

      logger.info("Inferring constraints from the rewritten lambda")
      val inferredConstraints = ParameterConstraintInference(parFusedLambda)._2

      logger.info("Initializing a tune space for a given choice of auto rewrite pass param values")
      val space = tuneParamSpaceFactory.getTuneParamSpace(
        layerConfigIdx, layerConfig.values,
        parFusedLambda, inferredConstraints, initialSeed = search.currentTuneSearchInitSeed)

      search.currentTuneSearchInitSeed = new Random(search.currentTuneSearchInitSeed + 2).nextInt()

      Right(new TuneParamValuesObtaining(this, space))
    }
  }

  class TuneParamValuesObtaining(override val prev: TuneSpaceBuilding,
                                 val tuneSpace: ParameterSpace)
    extends TuneSpaceBuilding(prev.prev, prev.parFuseParamValues, prev.parFusedLambda) {

    override val searchSpaceEnum: SearchSpace = SearchSpace.TuneParams

    override val logger: Logger = Logger(this.getClass)

    override def next: Either[ExplorationState, VectSpaceBuilding] = {
      try {
        val tuneParamValues_wrongLocalSizes = tuneSpace.getSolver.getNextRandomPoint(
          enforceSolutionUniqueness = runConfig.enforceSolutionUniquenessThroughCstr, parameterTypeName = "tune")

        val tuneParamValues = if (runConfig.chooseLocalSizeDeterministically)
          b.pickLocalSizes(
            tuneParamSpaceFactory, parFusedLambda, layerConfig, tuneParamValues_wrongLocalSizes)
        else tuneParamValues_wrongLocalSizes

        logger.info(s"$tunePointIdx. Tune params:\n" + tuneParamSpaceFactory.tuneParamValuesToString(
          tuneParamValues, tuneParamSpaceFactory.nWrgs(layerConfig.values ++ tuneParamValues)))

        /* Substitute parameters */
        val concreteNonVectorizedLambda = ParameterRewrite(
          parFusedLambda, layerConfig.values ++ tuneParamValues)

        Right(new VectSpaceBuilding(this, tuneParamValues, concreteNonVectorizedLambda))
      } catch {
        case e: SolutionNotFound =>
          logger.info(e.msg + s"\n$tunePointIdx. Will try another combination of param values.")
          Left(restartSearchFrom(firstSpaceToExplore))
      }
    }
  }

class VectSpaceBuilding(override val prev: TuneParamValuesObtaining,
                        val tuneParamValues: ListMap[Var, Cst],
                        val concreteNonVectorizedLambda: Lambda)
    extends TuneParamValuesObtaining(prev.prev, prev.tuneSpace)
      with RPassSpaceBuilder {

    override val searchSpaceEnum: SearchSpace = SearchSpace.TuneParams

    override val logger: Logger = Logger(this.getClass)

    override def next: Either[ExplorationState, VectParamValuesObtaining] = {

      build(logger, layerConfigIdx, concreteNonVectorizedLambda, "vect", Vectorization,
        () => search.currentVectSearchInitSeed,
        newSeed => search.currentVectSearchInitSeed = newSeed,
        space => new VectParamValuesObtaining(this, space))
    }
  }

  class VectParamValuesObtaining(override val prev: VectSpaceBuilding,
                                 val vectSpace: ConvAutoRewritePassSearchSpace)
    extends VectSpaceBuilding(prev.prev, prev.tuneParamValues, prev.concreteNonVectorizedLambda)
      with RPassParamValueObtainer {

    override val searchSpaceEnum: SearchSpace = SearchSpace.VectParams

    override val logger: Logger = Logger(this.getClass)

    override def next: Either[ExplorationState, Compiling] = {

      obtain(logger, "vectorization", tunePointIdx, vectSpace,
        (vectParamValues, concreteVectorizedLambda) =>
          new Compiling(this, vectParamValues, concreteVectorizedLambda) )
    }
  }

  class Compiling(override val prev: VectParamValuesObtaining,
                  val vectParamValues: RewritePassParams # RewriteParamValuesT,
                  val concreteVectorizedLambda: Lambda)
    extends VectParamValuesObtaining(prev.prev, prev.vectSpace) {

    override val searchSpaceEnum: SearchSpace = SearchSpace.VectParams

    override val logger: Logger = Logger(this.getClass)

    override def next: Either[ExplorationState, CompilationSuccessful] = {
      globalNumbering = NumberExpression.depthFirst(parFuseSpace.rewritingPass.lambda.body)
      saveLambda(Right(parFuseSpace.rewritingPass.lambda.body.toString),
        ShellUtils.join(tunePointLambdaDir, "conv_before_parfuse.scala"))
      globalNumbering = NumberExpression.depthFirst(parFusedLambda.body)
      saveLambda(Left(parFuseSpace.rewritingPass.lambda), ShellUtils.join(tunePointLambdaDir, "conv_before_parfuse_compilable.scala"))
      saveLambda(Right(parFusedLambda.toString), ShellUtils.join(tunePointLambdaDir, "conv_parametric_readable.scala"))
      saveLambda(Left(parFusedLambda), ShellUtils.join(tunePointLambdaDir, "conv_parametric_compilable.scala"))

      val concreteVectorizedLambdaAsStr = DumpToFile.dumpLambdaToString(concreteVectorizedLambda)
      saveLambda(Right(concreteVectorizedLambdaAsStr), ShellUtils.join(tunePointLambdaDir, "conv_concrete.scala"))

      // Check the lambda for uniqueness before proceeding to compilation

      val localSizesStr = tuneParamSpaceFactory.localSizes.map(ls =>
        tuneParamValues.get(ls) match {
          case Some(someLs) => someLs.c
          case None => throw new IllegalStateException("Could not obtain local size for the uniqueness check")
        }).mkString(",")

      if (!isUniqueLambda(concreteVectorizedLambdaAsStr ++ localSizesStr)) {

        logger.info(s"$tunePointIdx. Lambda has already been visited. Will try another combination of param values")

        Left(restartSearchFrom(firstSpaceToExplore))

      } else {

        val tp = TunePoint(layerConfigIdx, tunePointIdx, layerConfig.values,
          lambdaToParFuse,
          ListMap(
            ParallelizationAndFusion -> parFuseParamValues,
            Vectorization -> vectParamValues),
          parametricRewrittenLambda = parFusedLambda, concreteVectorizedLambda, tuneParamSpaceFactory, tuneParamValues)

        val compilationSuccessful = b.compileLambda(tp)

        if (compilationSuccessful)
          Right(new CompilationSuccessful(this, tp))
        else {
          logger.info(s"$tunePointIdx. Compilation failed. Will try another combination of param values")

          Left(restartSearchFrom(firstSpaceToExplore))
        }
      }
    }
  }

  class CompilationSuccessful(override val prev: Compiling,
                              val tunePoint: TunePoint[T])
    extends Compiling(prev.prev, prev.vectParamValues, prev.concreteVectorizedLambda) {

    override val searchSpaceEnum: SearchSpace = SearchSpace.VectParams

    override val logger: Logger = Logger(this.getClass)

    search.foundAPointForCurrentParMappingFromCsv = Some(true)

    // NB: do not call next!
  }

  trait RPassSpaceBuilder extends RewrittenLambdaBuilding {
    def build[U <: RewrittenLambdaBuilding](logger: Logger,
                                            layerConfigIdx: Int,
                                            lambdaToRewrite: Lambda,
                                            rpassShortName: String,
                                            rpassFactory: ConvAutoRewritePassSpaceFactory,
                                            seedGetter: () => Int,
                                            seedSetter: Int => Unit,
                                            nextStateFactory: ConvAutoRewritePassSearchSpace => U
                                           )(implicit jsonSettings: ConvExplorationJSONSettings): Either[ExplorationState, U] = {

      logger.info(s"Initializing $rpassShortName search space.")

      rpassFactory(layerConfigIdx, lambdaToRewrite, seedGetter()) match {
        case Some(space) =>

          seedSetter(new Random(seedGetter() + 1).nextInt())
          Right(nextStateFactory(space))

        case None =>

          Left(restartSearchFrom(firstSpaceToExplore))
      }
    }
  }

  trait RPassParamValueObtainer extends RewrittenLambdaBuilding {
    def obtain[U <: ParFuseSpaceBuilding](logger: Logger,
                                          rpassShortName: String,
                                          tunePointIdx: Int,
                                          space: ConvAutoRewritePassSearchSpace,
                                          nextStateFactory: (RewritePassParams # RewriteParamValuesT, Lambda) => U
                                         )(implicit search: SearchInvariantState,
                                           runConfig: RunConfig,
                                           jsonSettings: ConvExplorationJSONSettings): Either[ExplorationState, U] = {

      val rpassParamValues = try {
        if (runConfig.generateParMappingsWithoutSolver && rpassShortName.equals("parFuse"))
          nRandomParMappingsExplored += 1

        space.getNextRandomPoint(
          tunePointIdx, enforceSolutionUniqueness = runConfig.enforceSolutionUniquenessThroughCstr, search.runConfig.compilationTimeout)
      } catch {
        case e: ConvAutoRewritePassSearchSpaceExhausted =>
          logger.info(s"$tunePointIdx. Could not get even one combination of $rpassShortName param values.")

          if (runConfig.generateParMappingsWithoutSolver && rpassShortName.equals("parFuse"))
            logger.info(s"$tunePointIdx. $nValidParMappingsExplored candidates found; $nRandomParMappingsExplored attempts.")

          return Left(restartSearchFrom(firstSpaceToExplore))
      }

      logger.info(s"$tunePointIdx. Rewrite pass params:\n" +
        rpassParamValues._1.toStrings(PrintFormat.RewritePass).reverse.mkString(",\n"))

      if (runConfig.generateParMappingsWithoutSolver && rpassShortName.equals("parFuse")) {
        logger.info(s"$tunePointIdx. Counting parFuse combinations ($nRandomParMappingsExplored attempts): got another one!")

        nValidParMappingsExplored += 1

        logger.info(s"$tunePointIdx. Counting parFuse combinations ($nRandomParMappingsExplored attempts): will try again.")

        Left(restartSearchFrom(firstSpaceToExplore))
      } else {

        val rewrittenLambda = space.rewritingPass.rewriteTopDown(rpassParamValues._1)

        Right(nextStateFactory(rpassParamValues._1, rewrittenLambda))
      }
    }
  }
}

object LayerSearch {

  object SearchSpace extends Enumeration {
    type SearchSpace = Value
    val RewrittenLambdaBuild: LayerSearch.SearchSpace.Value = Value(0)
    val ParFuseParams: LayerSearch.SearchSpace.Value = Value(1)
    val TuneParams: LayerSearch.SearchSpace.Value = Value(2)
    val VectParams: LayerSearch.SearchSpace.Value = Value(3)

    private val enums = Seq(RewrittenLambdaBuild, ParFuseParams, TuneParams, VectParams)

    def getEnum(id: Int): Value = enums.find(_.id == id).get
  }

  case class TunePoint[T <: ConvTuneParamSpaceFactory](layerConfigIdx: Int, tunePointIdx: Int,
                                                       layerConfigValues: ListMap[Var, Cst],

                                                       lambdaToParFuse: Lambda,

                                                       rewritePassesParamValues: ListMap[ConvAutoRewritePassSpaceFactory, RewritePassParams # RewriteParamValuesT],

                                                       parametricRewrittenLambda: Lambda,
                                                       concreteLambda: Lambda,

                                                       t: T,
                                                       tuneValues: ListMap[Var, Cst]) {
    private val logger = Logger(this.getClass)

    val nWrgs: Vector[Long] = t.nWrgs(layerConfigValues ++ tuneValues)

    def log(): Unit = {
      logger.info("-----------------------------------------------------------------------------")
      logger.info("Layer config:\n" + t.l.configParamValuesToString(layerConfigValues))
      rewritePassesParamValues.foreach {
        case (rewritePassSpaceFactory, rewritePassParamValues) =>
          logger.info(s"${rewritePassSpaceFactory.name} rewrite pass params:\n" +
            rewritePassParamValues.toStrings(PrintFormat.RewritePass).reverse.mkString(",\n"))
      }
      logger.info("Tune params:\n" + t.tuneParamValuesToString(tuneValues, nWrgs))
      logger.info("-----------------------------------------------------------------------------")
    }
  }

  def initCaches[T <: ConvTuneParamSpaceFactory]
  (tuneParamSpaceFactory: T, b: BenchmarkStages[T])
  (implicit runConfig: RunConfig,
   jsonSettings: ConvExplorationJSONSettings): Predef.Map[Int, LayerSearch[T]] =

    runConfig.layersToExplore.map(layerConfigIdx => layerConfigIdx ->
      LayerSearch(
        layerConfigIdx, runConfig.netConfig.getLayerConfig(layerConfigIdx),
        lastTunePointIdx =
          if (runConfig.continueFromLayer.isDefined && runConfig.continueFromLayer.get == layerConfigIdx)
            runConfig.continueFromTunePointForTheContinueFromLayer.get + runConfig.totalTunePoints - 1
          else
            runConfig.totalTunePoints - 1,
        skippingPoints = runConfig.continueFromLayer.isDefined,
        rewindMode = runConfig.rewindMode,
        nextRPassSpaceInitSeed = mutable.Map(
          orderedPasses.map(pass =>
            pass -> (runConfig.initialSeed + layerConfigIdx + 1 + 10)): _*),
        tuneParamSpaceFactory,
        b,
        nextTuneSpaceInitSeed = runConfig.initialSeed + layerConfigIdx + 2
      )).toMap

  def batchesToRun(implicit runConfig: RunConfig): Range = {
    val firstTunePointBatch: Int = 0
//      if (runConfig.continueFromTunePointForTheContinueFromLayer.isEmpty) 0
//      else Math.floor(runConfig.continueFromTunePointForTheContinueFromLayer.get / runConfig.tuneBatchSize).toInt

    val untilTunePointBatch = firstTunePointBatch + runConfig.totalTuningBatches

    firstTunePointBatch until untilTunePointBatch
  }

  object ParamValuesForAllPasses {
    def unapply(arg: mutable.ListMap[ConvAutoRewritePassSpaceFactory, RewritePassParams # RewriteParamValuesT]):
    Option[mutable.ListMap[ConvAutoRewritePassSpaceFactory, RewritePassParams # RewriteParamValuesT]] =
      if (orderedPasses.forall( arg.contains ))
        Some(arg)
      else None
  }

  object NoParamValuesForSomeOrAllPasses {
    def unapply(arg: mutable.ListMap[ConvAutoRewritePassSpaceFactory, RewritePassParams # RewriteParamValuesT]):
    Option[mutable.ListMap[ConvAutoRewritePassSpaceFactory, RewritePassParams # RewriteParamValuesT]] =
    if (arg.size != orderedPasses.length)
      Some(arg)
    else None
  }

  case class SearchInvariantState(private val layerConfigIdx: Int)(implicit val runConfig: RunConfig) {
    var currentRPSearchInitSeed: Int = runConfig.initialSeed + layerConfigIdx
    var currentUnslideSearchInitSeed: Int = runConfig.initialSeed + layerConfigIdx + 1 + 10
    var currentParFuseSearchInitSeed: Int = runConfig.initialSeed + layerConfigIdx + 1 + 10
    var currentVectSearchInitSeed: Int = runConfig.initialSeed + layerConfigIdx + 1 + 10
    var currentTuneSearchInitSeed: Int = runConfig.initialSeed + layerConfigIdx + 2

    val visitedLambdaHashes: mutable.Set[String] = mutable.Set()

    var foundAPointForCurrentParMappingFromCsv: Option[Boolean] = None
    var lastParMappingUsedFromCsvIdx: Int = runConfig.jobId.getOrElse(0) - 1
  }

  trait ExplorationState {
    val prev: ExplorationState
    def next: Either[ExplorationState, ExplorationState]
    val logger: Logger

    val searchSpaceEnum: SearchSpace

    def restartSearchFrom(firstSpaceToExplore: SearchSpace): ExplorationState = {
      if (searchSpaceEnum < firstSpaceToExplore) {
        // Start from scratch
        restartSearchFrom(SearchSpace.RewrittenLambdaBuild)
      } else if (searchSpaceEnum == firstSpaceToExplore &&
        (prev.searchSpaceEnum != firstSpaceToExplore || prev == this)) {
        this
      } else prev.restartSearchFrom(firstSpaceToExplore)
    }
  }
}
