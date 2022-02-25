package benchmarks.conv

import com.typesafe.scalalogging.Logger
import ir.ArrayType
import ir.ast.Lambda
import LayerSearch.SearchSpace
import benchmarks.conv.conv_lambdas.DirectConvRewritten
import LayerSearch.SearchSpace
import benchmarks.conv.conv_lambdas.{DirectConvLambdaFactory, DirectConvRewritten}

import scala.language.postfixOps
import scala.util.control._


object ConvExploration {
  def Arr: ArrayType.type = ArrayType

  private val logger = Logger(this.getClass)

  val debugRun: Boolean = true
  var nRandomParMappingsExplored: Int = 0
  var nValidParMappingsExplored: Int = 0
  val useRefactoredParCstrs: Boolean = true

  val convFactory = DirectConvLambdaFactory //GEMMConvLambdaFactory
  val rewrittenConvLambda = DirectConvRewritten.f

  /******* Exploration benchmark *******/
  def main(args: Array[String]): Unit = {

    /** Initialisation **/
    val b = convFactory.b
    implicit val (runConfig, jsonSettings) = b.init(args)


    val conv = convFactory()
    val lambdaToExplore: HighLevelRewriteParamValues => Lambda = _ => rewrittenConvLambda

    /** Setup layer search space exploration caches **/
    val layerSearches: Predef.Map[
      Int,
      LayerSearch[convFactory.T]] = LayerSearch.initCaches(conv.t, b)

    /** Batches **/
    for {tunePointBatchIdx <- LayerSearch.batchesToRun} {

      /** Layers **/
      val loopAcrossLayers = new Breaks; loopAcrossLayers.breakable {

        for {layerConfigIdx <- runConfig.layersToExplore} {

          /** Skip compilation for the batch? **/
          val skipBatchScriptGenerationOrEverythingWithReason = layerSearches(layerConfigIdx).
            skipCompletelyOrJustScriptGenWithReason(tunePointBatchIdx)

          skipBatchScriptGenerationOrEverythingWithReason match {
            case Some(Left(reasonToSkipCompletely)) =>
              logger.info(s"Skipping layer $layerConfigIdx since " + reasonToSkipCompletely)
              loopAcrossLayers.break()

            case Some(Right(reasonToSkipBatchCompilation)) =>
              logger.info(s"Skipping layer $layerConfigIdx batch $tunePointBatchIdx compilation since " + reasonToSkipBatchCompilation)

            case None =>
          }

          /** Tune points **/
          for {tunePointIdx <- layerSearches(layerConfigIdx).tunePointsWithinBatchToRun(tunePointBatchIdx)} {

            val singleTunePointExploration = new Breaks; singleTunePointExploration.breakable {
              /** Skip compilation for the point? **/
              val skipTunePointScriptGenOrEverythingWithReason = layerSearches(layerConfigIdx).
                skipCompletelyOrJustScriptGenWithReason(tunePointBatchIdx, singleTunePointIdxToCheck = Some(tunePointIdx))

              skipTunePointScriptGenOrEverythingWithReason match {
                case Some(Left(reasonToSkipCompletely)) =>
                  logger.info(s"Skipping layer $layerConfigIdx batch $tunePointBatchIdx tune point $tunePointIdx since " + reasonToSkipCompletely)
                  singleTunePointExploration.break

                case _ =>
              }

              logger.info(s"\n\n\n")
              logger.info(s"Exploring layer $layerConfigIdx batch $tunePointBatchIdx tuning point $tunePointIdx")

              b.setupCompilationEnvironment(layerConfigIdx, tunePointIdx)

              /** Get parameter combinations for the point and compile the kernel if possible **/
              val tunePoint = layerSearches(layerConfigIdx).getAllParamValuesAndCompile(
                lambdaToExplore, tunePointIdx,
                firstSpaceToExplore = SearchSpace.getEnum(runConfig.firstSearchSpaceToExplore))

              tunePoint.log()

              /** The kernel is now compiled, generate scripts (optionally) **/
              (skipBatchScriptGenerationOrEverythingWithReason, skipTunePointScriptGenOrEverythingWithReason) match {
                case (Some(Right(_)), _) =>
                  // Already logged
                  singleTunePointExploration.break

                case (_, Some(Right(reasonToSkipTunePointCompilation))) =>
                  logger.info(s"Skipping layer $layerConfigIdx batch $tunePointBatchIdx tuning point $tunePointIdx " +
                    s"script generation since " + reasonToSkipTunePointCompilation)
                  singleTunePointExploration.break

                case _ =>
              }

              runConfig.runMode match {
                case RunConfig.Mode.compileForBoard => b.generateBashScriptToCompileAndRunOnBoard(tunePoint)

                case RunConfig.Mode.compileAndRunOnDesktop => b.runAndValidateOnDesktop(tunePoint)

                case _ => throw new IllegalArgumentException()
              }

            }
          }
        }
      }
    }

    /** Deinitialisation **/
    b.deinit()
  }
}