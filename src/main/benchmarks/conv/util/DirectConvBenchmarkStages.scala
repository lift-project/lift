package benchmarks.conv.util

import analysis.{CacheLinesAccessed, ParMapIterations}
import analysis.CacheLinesAccessed.DeviceConfig
import analysis.ParMapIterations.{ParKind, ParMapDescriptor}
import benchmarks.conv.LayerSearch.TunePoint
import cbackends.global.GlobalCompiler
import cbackends.host.host_ir.{OclFunc, ToGPU, ToHost}
import com.typesafe.scalalogging.Logger
import exploration.ParameterRewrite
import ir.ast.{IDGenerator, Lambda, fun}
import ir.{ArrayTypeWSWC, Type, TypeChecker}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.executor.DeviceCapabilityException
import opencl.generator._
import opencl.ir.Float
import rewriting.RewriteParamValues.PrintFormat
import benchmarks.conv.layer_configs.LayerConfig
import benchmarks.conv.tuning.DirectConvTuneParamSpaceFactory
import benchmarks.conv.{ConvExplorationJSONSettings, HighLevelRewriteParamValues, RunConfig, exec, utility_lambdas}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

case class DirectConvBenchmarkStages() extends BenchmarkStages[DirectConvTuneParamSpaceFactory] {
  private val logger = Logger(this.getClass)

  val maliConfig: DeviceConfig = DeviceConfig( // cache size = 524288 bytes = 524 KB = 8192 lines
    nCores = 12,
    nParQuadsPerCore = 3, // 12*3*4 = 144 simultaneous threads*/
    cacheLineSizeInBytes = 64, // 512 bits
    localMemIsGlobal = true
  )

  val maxCacheLinesAccessedHeuristic: Int = 80216064 * 2 // (best kernel (conservative) value * 2)

  @tailrec
  private def getClosestLargerFactor(multiple: Int,
                                     factorsLowerLimit: Int): Int =
    if (multiple % factorsLowerLimit == 0) factorsLowerLimit
    else getClosestLargerFactor(multiple, factorsLowerLimit + 1)

  /**
   * The heuristic for picking local sizes is following:
   * - Set local sizes equal to the number of iterations performed by the corresponding mapLcls.
   * - If the wrg size is more than permitted by the hardware, decrease the local sizes starting from z, then y, then x (so that X remains the largest and we have as many threads for warps as possible.
   * - - Reducing a local size is done by dividing it by one of its factors; the smallest factor that would yield the allowed wrg size. By using the factors of mapLcl iteration count, we avoid synchronizability problems that occur when the number of iterations is divided by non-factor and the threads enter a loop with a potential barrier unequal number of times.
   * - - If there is more than one instance of a MapLcl in the same dimension (e.g. multiple MapLcl(0)), try all possible combinations of MapLcl instances. In other words, pick one of MapLcl(0), one of MapLcl(1) and one of MapLcl(2); use their number of iterations and get resulting wrgSize. Pick the best combination based on which wrgSize is the largest.
   */
  override def pickLocalSizes(tuneParamSpaceFactory: DirectConvTuneParamSpaceFactory,
                              lambda: Lambda,
                              layerConfig: LayerConfig,
                              tuneValues: ListMap[Var, Cst]
                             )(implicit jsonSettings: ConvExplorationJSONSettings): ListMap[Var, Cst] = {
    val t = tuneParamSpaceFactory

    val tuneValuesWithoutLocalSizes = t.localSizes.foldLeft(tuneValues) {
      case (vals, localSize) => vals - localSize
    }

    val concreteLambdaWithoutLocalSizes = ParameterRewrite(
      lambda, layerConfig.values ++ tuneValuesWithoutLocalSizes)

    val inferredParMapIterations = ParMapIterations(concreteLambdaWithoutLocalSizes)
    val allParMapKinds =
      (0 until 3).map(ParMapDescriptor(ParKind.Local, _))
    val allParMapIterations = allParMapKinds.map(mapKind => mapKind ->
      inferredParMapIterations.getOrElse(mapKind, List(Cst(1))).map(_.evalInt)).toMap

    // Each parMap (e.g. MapLcl(0), mapLcl(1)) can occur multiple times with diff numbers of iterations.
    // This produces combinations where for each parMap we pick the number of iters of one of its instances,
    // until we tried all possible combinations
    def collectUniqueParMapCombinations(remainingParMapKinds: Map[ParMapDescriptor, List[Int]]
                                       ): List[Map[ParMapDescriptor, Int]] = {
      if (remainingParMapKinds.isEmpty) List()
      else if (remainingParMapKinds.size == 1)
        List(remainingParMapKinds.head._2.map(iters => remainingParMapKinds.head._1 -> iters).toMap)
      else {
        val nextParMapKind = remainingParMapKinds.head

        val remainingParMapCombinations = collectUniqueParMapCombinations(remainingParMapKinds.tail)

        remainingParMapCombinations.flatMap(combination =>
          nextParMapKind._2.map(iters =>
            ListMap(nextParMapKind._1 -> iters) ++ combination
          )
        )
      }
    }

    val uniqueParMapCombinations: List[Map[ParMapDescriptor, Int]] =
      collectUniqueParMapCombinations(allParMapIterations)

    val achievableLocalSizesPerCombination: List[(Int, Int, Int)] =

      uniqueParMapCombinations.map(uniqueParMapCombination => {

        val idealLocalSizes = List(
          uniqueParMapCombination.map { case (ParMapDescriptor(mapKind, dim), iterations: Int) =>

            // Get such factor of maxIters which divides maxIters into a number lower than maxLocalSize
            // E.g. for maxIters of 770 and maxLocalSize of 384:
            //   getClosestLargerFactor(770, ceil(770.float / 384)) = getClosestLargerFactor(770, 3) = 5
            //   localSize = 770 / 5 = 154
            // We care that maxIters is divided evenly to avoid sync problems.
            val localSize = iterations /
              getClosestLargerFactor(iterations,
                Math.ceil(iterations.toFloat / jsonSettings.maxLocalSize(dim).toFloat).toInt)

            localSize
          case _ => throw new IllegalArgumentException
          }.toSeq: _*)

        if (idealLocalSizes.product <= jsonSettings.maxWorkgroupSize)
          (idealLocalSizes.head, idealLocalSizes(1), idealLocalSizes(2))
        else {
          val List(x, y, z) = idealLocalSizes
          // e.g. maxWS = 384, x = 8, y = 3, xy = 24, maxAllowedZ = 384/24 = 16, z = 18, zUpd = 18 / 2 = 9
          // e.g. maxWS = 384, xy = 390, maxAllowedZ = 1, z = 2, zUpd = 1
          val maxAllowedZ = Math.max(1, jsonSettings.maxWorkgroupSize / (x * y))
          val zUpd = z / getClosestLargerFactor(z, Math.ceil(z.toFloat / maxAllowedZ).toInt)

          if (x * y * zUpd <= jsonSettings.maxWorkgroupSize)
            (x,y,zUpd)
          else {
            val maxAllowedY = Math.max(1, jsonSettings.maxWorkgroupSize / (x * zUpd))
            val yUpd = y / getClosestLargerFactor(y, Math.ceil(y.toFloat / maxAllowedY).toInt)

            if (x * yUpd * zUpd <= jsonSettings.maxWorkgroupSize)
              (x,yUpd,zUpd)
            else {
              val maxAllowedX = Math.max(1, jsonSettings.maxWorkgroupSize / (yUpd * zUpd))
              val xUpd = x / getClosestLargerFactor(x, Math.ceil(x.toFloat / maxAllowedX).toInt)

              if (xUpd * yUpd * zUpd <= jsonSettings.maxWorkgroupSize)
                (xUpd,yUpd,zUpd)
              else throw new IllegalStateException(
                s"Couldn't find localSizes that produce smaller total localSize than ${jsonSettings.maxWorkgroupSize}")
            }
          }
        }
      })

    val largestLocalSizes = achievableLocalSizesPerCombination.sortWith((l, r) => l._1 * l._2 * l._3 >= r._1 * r._2 * r._3).head

    tuneValuesWithoutLocalSizes ++ ListMap(
      t.localSizes(0) -> Cst(largestLocalSizes._1),
      t.localSizes(1) -> Cst(largestLocalSizes._2),
      t.localSizes(2) -> Cst(largestLocalSizes._3))
  }

  def compileLambda(tp: TunePoint[DirectConvTuneParamSpaceFactory])(implicit runConfig: RunConfig,
                                                                    jsonSettings: ConvExplorationJSONSettings): Boolean = {
    val tunePointCompileDir = runConfig.kernelDir(tp.layerConfigIdx, tp.tunePointIdx)

    val localNDRange = NDRange(
      tp.tuneValues(tp.t.localSizes.head),
      tp.tuneValues(tp.t.localSizes(1)),
      tp.tuneValues(tp.t.localSizes(2)))
    val globalNDRange = NDRange(
      localNDRange(0) * tp.nWrgs.head,
      localNDRange(1) * tp.nWrgs(1),
      localNDRange(2) * tp.nWrgs(2))
    val hostLambda = fun(
      tp.concreteLambda.params(0).t,
      tp.concreteLambda.params(1).t,

      (p_x, p_k) => ToHost() $ OclFunc(tp.concreteLambda, (localNDRange, globalNDRange),
        cpu_timer = true, gpu_timer = true).apply(ToGPU() $ p_x, ToGPU() $ p_k)
    )

    logger.info("Compiling the lambda...")

    var cacheLinesAccessed: Long = -1
    try {
      logger.info(s"NOT verifying cache line usage against the heuristic ($maxCacheLinesAccessedHeuristic lines max)...")
//      TypeChecker(tp.concreteLambda) // todo: disable the second set of passes in the compiler..?
//      RangesAndCounts(tp.concreteLambda, localNDRange, globalNDRange, collection.Map())
//      InferOpenCLAddressSpace(tp.concreteLambda)
//      OpenCLMemoryAllocator(tp.concreteLambda)
//      RemoveRedundantMemory(tp.concreteLambda)
//      View(tp.concreteLambda)

      if (false) {
        cacheLinesAccessed = CacheLinesAccessed(tp.concreteLambda,
          localSizes = (localNDRange(0).evalInt, localNDRange(1).evalInt, localNDRange(2).evalInt),
          globalSizes = (globalNDRange(0).evalInt, globalNDRange(1).evalInt, globalNDRange(2).evalInt),
          deviceConfig = maliConfig)

        logger.info(s"Cache lines accessed: $cacheLinesAccessed")

        if (cacheLinesAccessed > maxCacheLinesAccessedHeuristic)
          throw new DeviceCapabilityException(s"The lambda accesses more than ${maxCacheLinesAccessedHeuristic} lines")

        assert(cacheLinesAccessed != -1)
      }
      
      validateMemorySizes(tp.concreteLambda, localNDRange, globalNDRange, Predef.Map())
    } catch {
      case e: DeviceCapabilityException =>
        logger.info("Compilation unsuccessful: " + e.getMessage)
        return false
    }

//    globalNumbering = NumberExpression.depthFirst(hostLambda.body)

    val compilationCompleted = Future {
      try {
        GlobalCompiler ! (hostLambda, tunePointCompileDir, List(jsonSettings.generatedHostCodeFName),
          headerCommentCL = Some(s"Layer ${tp.layerConfigIdx}, tune point ${tp.tunePointIdx}\n" +
            tp.t.l.configParamValuesToString(tp.layerConfigValues) + "\n" +
            tp.rewritePassesParamValues.map(kv =>
              kv._1.name + "\n" + kv._2.toStrings(PrintFormat.RewritePass).mkString(",\n")).mkString("\n") + "\n" +
            tp.t.tuneParamValuesToString(tp.tuneValues, tp.nWrgs)/* + "\n" +
          "Cache lines accessed = " + cacheLinesAccessed*/))

        kernelsCompiledAfterIDReset += 1

        if (kernelsCompiledAfterIDReset >= 200) {
          logger.info("Resetting global node IDs to prevent integer overflow")
          IDGenerator.reset_id()
          kernelsCompiledAfterIDReset = 0
        }

        logger.info("Compilation successful")
        true
      } catch {
        // Known, allowed exceptions
        case e: IllegalKernel
          if e.getMessage.startsWith("Kernel contains a barrier that might not be taken by all threads inside") ||
            e.getMessage.startsWith("Kernel requires a barrier that might not be taken by all threads inside") =>
          logger.info("Compilation unsuccessful: " + e.getMessage)
          false
        case e: DeviceCapabilityException =>
          logger.info("Compilation unsuccessful: " + e.getMessage)
          false
        // Unknown exceptions
        case e: Throwable => throw e
      }
    }
    try {
      val compilationSuccessful = Await.result(compilationCompleted, runConfig.compilationTimeout seconds)

      if (compilationSuccessful)
      // Compile second host code for functional verification
        compilePadFullConvAndDepadForFuncVerification(
          tp, tunePointCompileDir, (localNDRange, globalNDRange))

      compilationSuccessful
    } catch {
      case e: TimeoutException =>
        logger.info(s"Could not compile kernel in ${runConfig.compilationTimeout} seconds")
        //noinspection ScalaDeprecation
        exec.lastThread.getOrElse(throw new RuntimeException("Not started")).stop()
        false
    }
  }

  /**
   * Generates hostcode and kernels for padding, convolution and depadding for func verification.
   * If tp.highLevelRewriteParamValues.depthWiseTilingPartReduceOnly is true, convolution is split in two kernels.
   */
  def compilePadFullConvAndDepadForFuncVerification(tp: TunePoint[DirectConvTuneParamSpaceFactory],
                                                    tunePointCompileDir: String,
                                                    partialOrFullConvNDRanges: (NDRange, NDRange)
                                                   )(implicit runConfig: RunConfig,
                                                     jsonSettings: ConvExplorationJSONSettings): Unit = {
    
    val l = tp.layerConfigValues
    val t = tp.tuneValues
    val lt: Map[ArithExpr, ArithExpr] = Map((l.toSeq ++ t.toSeq): _*)

    val dimOrder = List(1, 2, 0)

    def toType(dimSizes: List[ArithExpr]): Type =
      dimOrder.map(i => dimSizes.reverse(i)).foldRight[Type](opencl.ir.Float) {
        case (arrSize, innerType) => ArrayTypeWSWC(innerType, arrSize)
      }

    // in new data layout, inputType = Arr(Arr(Arr(Float, l(tp.t.l.inputWidth)), l(tp.t.l.inputHeight)), l(tp.t.l.inputChannels))
    // in old, inputType = Arr(Arr(Arr(Float, l(tp.t.l.inputChannels)), l(tp.t.l.inputWidth)), l(tp.t.l.inputHeight))
    val inputType = toType(List(l(tp.t.l.inputWidth), l(tp.t.l.inputHeight), l(tp.t.l.inputChannels)))
    val paddedInputType = tp.concreteLambda.params(0).t

    val weightsType = Arr(Arr(Arr(Arr(Float, l(tp.t.l.inputChannels)), l(tp.t.l.kernelWidth)), l(tp.t.l.kernelHeight)), l(tp.t.l.numKernels))

    val paddedOutputType = toType(List(
      ArithExpr.substitute(tp.t.nWindowsAcrossInputInXOptPadded, lt),
      ArithExpr.substitute(tp.t.nWindowsAcrossInputInYOptPadded, lt),
      l(tp.t.l.numKernels)))

    val outputType = toType(List(
      ArithExpr.substitute(tp.t.l.outputWidthDepadded, lt),
      ArithExpr.substitute(tp.t.l.outputHeightDepadded, lt),
      l(tp.t.l.numKernels)))

    val padFunFactory = new utility_lambdas.PadConv(
      inputChannels = l(tp.t.l.inputChannels),
      inputWidth = l(tp.t.l.inputWidth),
      inputHeight = l(tp.t.l.inputHeight),
      padFuncX = l(tp.t.l.padFuncX),
      padFuncY = l(tp.t.l.padFuncY),
      padOptRight = t(tp.t.padOptRight),
      padOptBottom = t(tp.t.padOptBottom),

      originalType = inputType, newType = paddedInputType,
      newDataLayout = false)

    val depadFunFactory = new utility_lambdas.DepadConv(
      inputWidth = l(tp.t.l.inputWidth),
      inputHeight = l(tp.t.l.inputHeight),
      kernelChannels = l(tp.t.l.numKernels),

      depadRight = t(tp.t.padOptRight),
      depadBottom = t(tp.t.padOptBottom),

      originalType = paddedOutputType, newType = outputType,
      newDataLayout = false)

    val hostLambdaIn3or4Acts = fun(
      inputType,
      weightsType,

      (inputData, kernelWeights) =>
        ToHost() o
          OclFunc(depadFunFactory.f, depadFunFactory.depaddingLambdaNDRanges,
            cpu_timer = true, gpu_timer = true) $

          OclFunc(tp.concreteLambda, partialOrFullConvNDRanges,
            cpu_timer = true, gpu_timer = true).apply(

            OclFunc(padFunFactory.f, padFunFactory.paddingLambdaNDRanges,
              cpu_timer = true, gpu_timer = true) $ ToGPU(inputData),

            ToGPU(kernelWeights)))

    val compilationCompleted = Future {
      GlobalCompiler ! (hostLambdaIn3or4Acts, tunePointCompileDir,
        List(jsonSettings.generatedHostCodeFName.replace(".cpp", "_all_kernels.cpp")),
        headerCommentCL = Some(s"Layer ${tp.layerConfigIdx}, tune point ${tp.tunePointIdx}\n" +
          tp.t.l.configParamValuesToString(l) + "\n" +
          tp.rewritePassesParamValues.map(kv =>
            kv._1.name + "\n" + kv._2.toStrings(PrintFormat.RewritePass).mkString(",\n")).mkString("\n") + "\n" +
          tp.t.tuneParamValuesToString(t, tp.nWrgs)))
    }
    try {
      Await.result(compilationCompleted, runConfig.compilationTimeout seconds)
    } catch {
      case e: TimeoutException =>
        logger.info(s"Could not compile four kernels in ${runConfig.compilationTimeout} seconds")
        //noinspection ScalaDeprecation
        exec.lastThread.getOrElse(throw new RuntimeException("Not started")).stop()
    }
  }
}
