package benchmarks.conv.gpgpu20.layeroptimiser.ConvStencil3DOpt

import benchmarks.conv.gpgpu20.ConvStencil3D
import benchmarks.conv.gpgpu20.ConvStencil3D.ConvStencil3DTuneParams
import benchmarks.conv.gpgpu20.layeroptimiser.DirectConvManuallyParallelizedConvGenerator
import benchmarks.conv.gpgpu20.paramspaces.ParamConstraints.lessThanOrEqual
import benchmarks.conv.gpgpu20.paramspaces.{ParamConstraint, ParamConstraints, ParameterSearch}
import lift.arithmetic._
import patterns.nn.utils.Utils.slidingOutputSize
import benchmarks.conv.gpgpu20.paramspaces.layertune.AbstractConvStencil3DTuneParamSpace
import benchmarks.conv.gpgpu20.settings.Settings
import com.typesafe.scalalogging.Logger
import ir.ast.Lambda
import rewriting.rules.{NeuralNetRules, OpenCLRules, Rule}

import scala.collection.immutable.ListMap
import scala.util.Random

case class ConvStencil3DTuneParamSpace(name: String,
                                       private val params: ConvStencil3DTuneParams,
                                       override val constraints: ParamConstraints,
                                       settings: Settings)
  extends AbstractConvStencil3DTuneParamSpace(params, constraints)


object ConvStencil3DTuneParamSpace {
  /* Opt space name */
  val name = "ConvStencil3DTuneParamSpace"


  /****** Rewrite rules ******/
  val rewriteRules: ListMap[String, Rule] = ListMap[String, Rule](
    "vectorise4" -> NeuralNetRules.StencilRules.vectorise(Cst(4))/*,
    "reduceSeqUnroll" -> OpenCLRules.reduceSeqUnroll*/)

  val maxRewriteCombinations: Int = Math.pow(rewriteRules.size, 2).toInt

  def pickRewriteRulesRandomly(random: Random): ListMap[String, Rule] = rewriteRules.filter(_ => random.nextBoolean)


  /****** Parameters ******/
  val fuseLambdas: Boolean = true
  val shareKernels: Boolean = true

  val params: ConvStencil3D.ConvStencil3DTuneParams = {


    // full:
    // It makes sense to explore padOptTotal only to the size equal to inputSize + 2 * padFunc, because larger values would not
    // introduce new factors of the final input size
    val padOptTotal = Var("padOptTotal", RangeAdd(0,
      ConvStencil3DConfigSpace.params.inputWidthHeight + 2 * ConvStencil3DConfigSpace.params.padFunc, 1))
//    val padOptTotal = Var("padOptTotal", RangeAdd(108, 108 + 1, 1))

    val tileWidthHeight = Var("tileWidthHeight", RangeAdd(ConvStencil3DConfigSpace.params.kernelWidthHeight,
      ConvStencil3DConfigSpace.params.inputWidthHeight +
        2 * ConvStencil3DConfigSpace.params.padFunc + padOptTotal + 1,
      step = 1))
//    val tileWidthHeight = Var("tileWidthHeight", RangeAdd(8, 8 + 1, step = 1)),

    val nWindowsInTile = slidingOutputSize(tileWidthHeight,
      ConvStencil3DConfigSpace.params.kernelWidthHeight,
      ConvStencil3DConfigSpace.params.kernelStride)

    new ConvStencil3DTuneParams(
      tileWidthHeight = tileWidthHeight,

      nKernelsPerWrg = Var("nKernelsPerWrg", RangeAdd(1, ConvStencil3DConfigSpace.params.kernelChannels + 1, step = 1)),
//      nKernelsPerWrg = Var("nKernelsPerWrg", RangeAdd(8, 8+1, step = 1)),
      seqOpsPerThread = Var("seqOpsPerThread",
        RangeAdd(1, ConvStencil3DConfigSpace.params.kernelWidthHeight *
          ConvStencil3DConfigSpace.params.kernelWidthHeight *
          ConvStencil3DConfigSpace.params.inputChannels + 1,
          step = 1)),
//      seqOpsPerThread = Var("seqOpsPerThread", RangeAdd(384, 384+1, step = 1)),

      nWindowsPerThread = Var("nWindowsPerThread",
        if (shareKernels) RangeAdd(1, nWindowsInTile + 1, step = 1)
        else RangeAdd(1, 1+1, step = 1)),

      padOptTotal = padOptTotal,

      coalesce = false,
      unrollReduce = false)
  }


  val rewriteParams: ConvStencil3D.ConvStencil3DRewriteParams =
    new ConvStencil3D.ConvStencil3DRewriteParams(
      vectorLen = Var("vectorLen", RangeUnknown))


  /****** Parameter validation constraints ******/
  private lazy val tileStride = params.tileWidthHeight -
    (ConvStencil3DConfigSpace.params.kernelWidthHeight - ConvStencil3DConfigSpace.params.kernelStride)

  private lazy val nTilesInRow = slidingOutputSize(
    ConvStencil3DConfigSpace.params.inputWidthHeight + 2 * ConvStencil3DConfigSpace.params.padFunc + params.padOptTotal,
    params.tileWidthHeight,
    tileStride)

  private lazy val nTilesInCol = nTilesInRow
  private lazy val nTilesInInput = nTilesInCol * nTilesInRow
  private lazy val nTilesTotal = ConvStencil3DConfigSpace.params.nInputs * nTilesInInput

  private lazy val nWindowsInTileRow = slidingOutputSize(
    params.tileWidthHeight,
    ConvStencil3DConfigSpace.params.kernelWidthHeight,
    ConvStencil3DConfigSpace.params.kernelStride)
  private lazy val nWindowsInTileCol = nWindowsInTileRow

  private lazy val nWindowsInTile = nWindowsInTileCol * nWindowsInTileRow

  private lazy val nElementsInWindow = ConvStencil3DConfigSpace.params.kernelWidthHeight *
    ConvStencil3DConfigSpace.params.kernelWidthHeight *
    ConvStencil3DConfigSpace.params.inputChannels

  private lazy val nSeqTilesInWindow = nElementsInWindow /^ params.seqOpsPerThread

  private lazy val flatPartReducedXSize = nTilesTotal * ConvStencil3DConfigSpace.params.kernelChannels * nWindowsInTile *
    nSeqTilesInWindow

  private lazy val firstLambdaOutputSize = {
    val allVars: Vector[Var] = ArithExpr.collectVars(flatPartReducedXSize).sortBy(v =>
        if (ConvStencil3DConfigSpace.params.paramVector.contains(v))
          ConvStencil3DConfigSpace.params.paramVector.indexOf(v)
        else
        if (params.paramVector.contains(v))
          params.paramVector.indexOf(v) + ConvStencil3DConfigSpace.params.paramVector.length
        else
          throw new IllegalArgumentException(f"Unknown parameter $v")).toVector

    new ParamConstraint(
      "firstLambdaOutputSizeMustBeLessThan1GB",
      "The first lambda output size should be less than 1 GB (2^30 bytes)",
      allVars,
      lhs = flatPartReducedXSize * Cst(4) /* float size */,
      rhs = Cst(2).pow(Cst(30)),
      predicate = (lhs: ArithExpr, rhs: ArithExpr) => lessThanOrEqual(lhs, rhs)
    )}


  private lazy val tilesMustCoverWholeInput = {

    val tileStride = params.tileWidthHeight - (ConvStencil3DConfigSpace.params.kernelWidthHeight -
      ConvStencil3DConfigSpace.params.kernelStride)

    val paddedInputSize = ConvStencil3DConfigSpace.params.inputWidthHeight +
      2 * ConvStencil3DConfigSpace.params.padFunc + params.padOptTotal

    new ParamConstraint(
      "tileSizeFitness",
      "(Tile size should be such that tiles cover the whole input: " +
        "(paddedInputSize - (tileSize - tileStride)) % tileStride == 0; " +
        "AND tile size should be such that kernel windows cover the whole tile: " +
        "(tileSize - (kernelSize - kernelStride)) % kernelStride == 0; )" +
        "OR tile size should be equal to the input size" +
        "(TODO): OR tile size should be equal to the kernel size",
      Vector(ConvStencil3DConfigSpace.params.inputWidthHeight,
        ConvStencil3DConfigSpace.params.padFunc,
        ConvStencil3DConfigSpace.params.kernelWidthHeight,
        ConvStencil3DConfigSpace.params.kernelStride,
        params.tileWidthHeight,
        params.padOptTotal),
      lhs = {
        val firstMod = (paddedInputSize - (params.tileWidthHeight - tileStride)) % tileStride
        val secondMod = (params.tileWidthHeight - (ConvStencil3DConfigSpace.params.kernelWidthHeight -
          ConvStencil3DConfigSpace.params.kernelStride)) %
          ConvStencil3DConfigSpace.params.kernelStride

        firstMod + secondMod
      },
      rhs = (paddedInputSize - params.tileWidthHeight), // TODO: implement the last OR
      predicate = (lhs: ArithExpr with SimplifiedExpr, rhs: ArithExpr with SimplifiedExpr) => lhs == Cst(0) || rhs == Cst(0))
  }

//  private lazy val optPaddingSizeMustBeCoverableByKernels = {
//
//    val lhs = params.padOptTotal - (ConvStencil3DConfigSpace.params.kernelWidthHeight -
//      ConvStencil3DConfigSpace.params.kernelStride) % ConvStencil3DConfigSpace.params.kernelStride
//
//    new ParamConstraint(
//      "optPaddingSizeMustBeCoverableByKernels",
//      "The optimisational padding size should be fully coverable by kernels. Otherwise, the sliding windows " +
//        "will be shifted by the amount smaller than kernelStride and the results will be wrong. " +
//        "(padOptTotal - (kernelSize - kernelStride)) % kernelStride == 0",
//      ArithExpr.collectVars(lhs).toVector,
//      lhs = lhs,
//      rhs = Cst(0),
//      predicate = (lhs: ArithExpr, rhs: ArithExpr) => lhs == rhs
//    )
//  }


  def hardwareConstraints(settings: Settings,
                          appliedRules: List[Rule]): Vector[ParamConstraint] = {
    // Naums: PrettyPrinter fires StackOverflow when printing large kernels with default stack size
    // Example large kernel: seqOpsPerThread=64, nKernelsPerWrg=128 =>
    // 8320 ops, 15k lines of OpenCL code
    val threadDuration = {
      new ParamConstraint(
        "threadDuration",
        "The total number of sequential operations per thread should not be too long (TBC)",
        Vector(ConvStencil3DTuneParamSpace.params.nKernelsPerWrg, ConvStencil3DTuneParamSpace.params.nWindowsPerThread,
          ConvStencil3DTuneParamSpace.params.seqOpsPerThread),
        lhs = if (appliedRules.contains(OpenCLRules.reduceSeqUnroll))
          // Unrolled loop length (in code lines)
          ConvStencil3DTuneParamSpace.params.nKernelsPerWrg * ConvStencil3DTuneParamSpace.params.nWindowsPerThread *
            ConvStencil3DTuneParamSpace.params.seqOpsPerThread /^
            (if (appliedRules.contains(NeuralNetRules.StencilRules.vectorise(4))) Cst(4) else Cst(1))
        else // Iteration length (in code lines)
          ConvStencil3DTuneParamSpace.params.nKernelsPerWrg * ConvStencil3DTuneParamSpace.params.nWindowsPerThread,
        // Arbitrary limit to shorten kernels:
        rhs = 1000,
        predicate = (lhs: ArithExpr, rhs: ArithExpr) => lessThanOrEqual(lhs, rhs)
      )}

    val localSizesMustComplyToHWSpec =
      (0 until 2).toVector.flatMap(lambdaIdx =>
        (0 until 3).toVector.map(dim => {
          val localSize = Seq(
            DirectConvManuallyParallelizedConvGenerator.ndRangeExpressions(appliedRules)._1,
            DirectConvManuallyParallelizedConvGenerator.ndRangeExpressions(appliedRules)._2)(lambdaIdx)._1(dim)

          new ParamConstraint(
            f"localSize${dim}InLambda${lambdaIdx}MustBeSmallerThanMaxLocalSize",
            f"Local size in dimension $dim of lambda $lambdaIdx must not be greater than max local " +
              f"size ($settings.maxLocalSize)",
            ArithExpr.collectVars(localSize).toVector,
            lhs = localSize,
            rhs = settings.maxLocalSize,
            predicate = (lhs: ArithExpr, rhs: ArithExpr) => lessThanOrEqual(lhs, rhs))
        }))

    val wrgSizesMustComplyToHWSpec =
      (0 until 2).toVector.map(lambdaIdx => {
        val workGroupSize = Seq(
          DirectConvManuallyParallelizedConvGenerator.ndRangeExpressions(appliedRules)._1,
          DirectConvManuallyParallelizedConvGenerator.ndRangeExpressions(appliedRules)._2)(lambdaIdx)._1.reduce(_ * _)

        new ParamConstraint(
          s"wrgSizeMustInLambda${lambdaIdx}MustBeSmallerThanMaxWrgSize",
          f"Workgroup size of lambda $lambdaIdx must not be greater than max work group " +
            f"size ($settings.maxWorkGroupSize)",
          ArithExpr.collectVars(workGroupSize).toVector,
          lhs = workGroupSize,
          rhs = settings.maxWorkGroupSize,
          predicate = (lhs: ArithExpr, rhs: ArithExpr) => lessThanOrEqual(lhs, rhs)
        )
      })

    threadDuration +: (localSizesMustComplyToHWSpec ++ wrgSizesMustComplyToHWSpec) /*++ globalSizesMustNotBeTooLarge*/
  }

  def debuggingConstraints: Vector[ParamConstraint] = {
    val manualTuneParams = collection.immutable.ListMap[Var, Int](
      // Reproduction of kernel_sharing_coalesce_fix L9 point 4636 from generated_c_files_monaco_26.07.2019_18.57.13_vgg_all_layers_kernels_0-4999_kernel_sharing_coalesce_fix
      params.tileWidthHeight -> 5,
      params.nKernelsPerWrg -> 2,
      params.seqOpsPerThread -> 288,
      params.nWindowsPerThread -> 3,
      params.padOptTotal -> 2
    )

    manualTuneParams.map { case (p, v) =>
      new ParamConstraint(
        s"debugConstraint_${p.name}",
        f"${p.name} must equal ${v}",
        Vector(p),
        lhs = p,
        rhs = Cst(v),
        predicate = (lhs: ArithExpr, rhs: ArithExpr) => lhs == rhs
      )
    }.toVector
  }

  lazy val manualConstraints: Vector[ParamConstraint] =
    Vector(firstLambdaOutputSize, tilesMustCoverWholeInput/*, optPaddingSizeMustBeCoverableByKernels*/) ++ debuggingConstraints


  private val logger = Logger(this.getClass)

  /****** Space constructor ******/
  def apply(lambdas: Seq[Lambda],
            appliedRules: List[Rule],
            settings: Settings): ConvStencil3DTuneParamSpace = {

    logger.info("Searching for tuning parameter constraints in lambdas")
    val (_, inferredConstraints0) = ParameterSearch(lambdas(0))
    val (_, inferredConstraints1: Vector[ParamConstraint]) =
      if (ConvStencil3DTuneParamSpace.fuseLambdas) (Vector(), Vector()) else ParameterSearch(lambdas(1))

    val (layerConfigConstraints0, tuneParamConstraints0) = inferredConstraints0.partition(constraint =>
      constraint.params.forall(param => params.paramVector.contains(param)))

    val (layerConfigConstraints1, tuneParamConstraints1) = inferredConstraints1.partition(constraint =>
      constraint.params.forall(param => params.paramVector.contains(param)))

    val useGlobalMaps: Boolean = System.getenv("USE_GLOBAL_MAPS") != null && System.getenv("USE_GLOBAL_MAPS").toLowerCase == "true"

    // All constraints = manual constraints + automatically inferred constraints
    val constraintsVector = ParamConstraints.removeDuplicateConstraints(manualConstraints ++
      /*ParameterSpace.rangeBasedConstraints(params.paramVector) ++*/
      tuneParamConstraints0 ++ tuneParamConstraints1 ++ (
      if (useGlobalMaps) List() else hardwareConstraints(settings, appliedRules)))


    new ConvStencil3DTuneParamSpace(
      name, params,
      ParamConstraints(ConvStencil3DConfigSpace.params.paramVector ++ params.paramVector, constraintsVector),
      settings)
  }
}
