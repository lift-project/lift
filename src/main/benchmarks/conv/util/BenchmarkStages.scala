package benchmarks.conv.util

import benchmarks.conv.LayerSearch.TunePoint
import benchmarks.conv.tuning.ConvTuneParamSpaceFactory
import cbackends.common.executor.Executor.native_compile_and_run
import cbackends.common.{OpenCLVersion, WindowsOSInUse}
import com.typesafe.scalalogging.Logger
import ir.ArrayType
import ir.ast.Lambda
import lift.arithmetic.{ArithExpr, Cst, NewFactorizationOfSum, Var}
import opencl.executor.DeviceCapabilityException
import opencl.generator.{IgnoreBarrierFlags, NDRange, PerformBarrierElimination, PerformBarrierInsertion, RangesAndCounts}
import opencl.ir.{CollectTypedOpenCLMemory, GlobalMemory, InferOpenCLAddressSpace, LocalMemory, OpenCLMemoryAllocator, RemoveRedundantMemory}
import benchmarks.conv.LayerSearch.TunePoint
import benchmarks.conv.tuning.ConvTuneParamSpaceFactory
import benchmarks.conv.LayerSearch.TunePoint
import benchmarks.conv.layer_configs.{LayerConfig, NetConfig}
import benchmarks.conv.passes.ParallelizationAndFusion
import benchmarks.conv.{ConvExplorationCmdArgs, ConvExplorationJSONSettings, HighLevelRewriteParamValues, RunConfig, getGitInfo, paramValuesAsCppMacroString}
import benchmarks.conv.tuning.ConvTuneParamSpaceFactory
import benchmarks.conv.util.BenchmarkStages.MD5Hash
import rewriting.RewriteParam
import rewriting.RewriteParamValues.PrintFormat
import rewriting.passes.RewritingPass
import rewriting.passes.RewritingPass.RewritePassParams
import utils.ShellUtils

import java.io.{File, PrintWriter}
import java.nio.file.NoSuchFileException
import java.text.SimpleDateFormat
import java.util.Calendar
import scala.sys.process._
import scala.language.postfixOps
import scala.collection.immutable
import scala.collection.immutable.ListMap

abstract class BenchmarkStages[T <: ConvTuneParamSpaceFactory]() {
  private val logger = Logger(this.getClass)

  def Arr: ArrayType.type = ArrayType

  var kernelsCompiledAfterIDReset: Int = 0

  /* Command line options and JSON config */

  var originalOpenCLis200: Boolean = OpenCLVersion.is200
  val originalNewFactorizationEnabledStatus: Boolean = NewFactorizationOfSum()

  def init(args: Array[String]): (RunConfig, ConvExplorationJSONSettings) = {
    logger.info("*********** Exploration initialization ***********")

    NewFactorizationOfSum.enabled = true

    val (cmdArgs, jvmOptions) = ConvExplorationCmdArgs.parseCommandLine(args)
    implicit val jsonSettings: ConvExplorationJSONSettings =
      ConvExplorationJSONSettings(cmdArgs.getOptionValue("path_to_settings"))

    if (cmdArgs.hasOption("log_git_status"))
      logger.info(getGitInfo)

    logger.info("[Initialization] Cmd options:\n" + cmdArgs.getOptions.map(o => o.getArgName + " = " + o.getValue).mkString("\n"))
    logger.info(s"[Initialization] JSON settings:\n$jsonSettings")

    originalOpenCLis200 = OpenCLVersion.is200
    OpenCLVersion.is200 = cmdArgs.hasOption("compile_for_board")

    val netConfig = NetConfig(cmdArgs.getOptionValue("net_name"))

    val runConfig = RunConfig(cmdArgs, netConfig, jvmOptions, jsonSettings)

    logger.info(f"[Initialization] Run config:\n$runConfig")

    logger.info(f"[Initialization] Switched from barrier elimination to barrier insertion")
    PerformBarrierElimination.set(false)
    IgnoreBarrierFlags.set(true)
    PerformBarrierInsertion.set(true)

    (runConfig, jsonSettings)
  }

  def pickLocalSizes(tuneParamSpaceFactory: T,
                     lambda: Lambda,
                     layerConfig: LayerConfig,
                     tuneValues: ListMap[Var, Cst]
                    )(implicit jsonSettings: ConvExplorationJSONSettings): ListMap[Var, Cst] = tuneValues

  /**
   * @return tunePointCompileDir
   */
  def setupCompilationEnvironment(layerConfigIdx: Int, tunePointIdx: Int)(implicit runConfig: RunConfig,
                                                                          jsonSettings: ConvExplorationJSONSettings): Unit = {
    val tunePointCompileDir = runConfig.kernelDir(layerConfigIdx, tunePointIdx)
    val testHarnessDir = ShellUtils.join(jsonSettings.testSrcDir, jsonSettings.testHarnessDirName)

    ShellUtils.mkDirTree(tunePointCompileDir, overwriteIfExists = /*runConfig.overwriteCompiledFiles*/ true)
    ShellUtils.mkDirTree(runConfig.lambdaDir(layerConfigIdx, tunePointIdx), overwriteIfExists = /*runConfig.overwriteCompiledFiles*/ true)


    // Copy test harness files to the new directory
    if (!runConfig.useCases2019TestHarness) {
      if (WindowsOSInUse())
        ShellUtils.join(testHarnessDir, jsonSettings.testHarnessFilesCopyScriptFName) +
          " " + testHarnessDir + " " + tunePointCompileDir + " " + jsonSettings.testHarnessFName !
      else throw new NotImplementedError()
    }

    //    val clean_script = s"$testHarnessPath/clean.sh"

    //clean all kernel files if any
    //    if (!WindowsOSInUse()) (s"$compilationDir/remove_kernels.sh $compilationDir 2> /dev/null") !
    //    else (s"cmd.exe /S /C del $compilationDir\\kernel_*.cl >nul 2>&1") !
  }

  /**
   * A clone of opencl.executor.validateMemorySizes.
   * Takes device specs from jsonSettings instead of the OpenCL runtime.
   */
  def validateMemorySizes(f: Lambda,
                          localSize: NDRange,
                          globalSize: NDRange,
                          valueMap: immutable.Map[ArithExpr, ArithExpr])
                         (implicit jsonSettings: ConvExplorationJSONSettings): Unit = {
    logger.info("Validating lambda memory sizes...")

    InferOpenCLAddressSpace(f)
    RangesAndCounts(f, localSize, globalSize, valueMap)
    OpenCLMemoryAllocator(f)
    RemoveRedundantMemory(f)
    val memories = CollectTypedOpenCLMemory(f, includePrivate = true)

    //    val memories2: (Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory]) = OpenCLGenerator.getMemories(f)

    val (globalMemories, localMemories, privateMemories) = {
      val (g, lp) = (memories._1 ++ memories._2 ++ memories._3 ++ memories._4).
        partition(_.mem.addressSpace == GlobalMemory)
      val (l, p) = lp.partition(_.mem.addressSpace == LocalMemory)
      (g, l, p)
    }

    val globalSizes = globalMemories.map(mem => ArithExpr.substitute(mem.mem.size, valueMap).evalLong)
    val totalSizeOfGlobal = globalSizes.sum
    val totalSizeOfLocal = localMemories.map(mem => ArithExpr.substitute(mem.mem.size, valueMap).evalLong).sum
    val totalSizeOfPrivate = privateMemories.map(mem => ArithExpr.substitute(mem.mem.size, valueMap).evalLong).sum

    globalSizes.foreach(size => {
      val maxMemAllocSize = jsonSettings.maxMemAllocSize //Executor.getDeviceMaxMemAllocSize
      if (size > maxMemAllocSize)
        throw new DeviceCapabilityException(s"Single global buffer size required ($size) cannot be larger than $maxMemAllocSize")
    })

    val globalMemSize = jsonSettings.globalMemSize //Executor.getDeviceGlobalMemSize
    if (totalSizeOfGlobal > globalMemSize)
      throw new DeviceCapabilityException(s"Global size required ($totalSizeOfGlobal) cannot be larger than $globalMemSize")

    val localMemSize = jsonSettings.localMemSize //Executor.getDeviceLocalMemSize
    if (totalSizeOfLocal > localMemSize)
      throw new DeviceCapabilityException(s"Local size required ($totalSizeOfLocal) cannot be larger than $localMemSize")

    val privateMemSize = jsonSettings.floatRegistersPerThreadInMaxOccupancy * 10
    if (totalSizeOfPrivate > privateMemSize)
      throw new DeviceCapabilityException(s"Private size required ($totalSizeOfPrivate) should not be larger than $privateMemSize.")
  }

  def compileLambda(tp: TunePoint[T])(implicit runConfig: RunConfig,
                                      jsonSettings: ConvExplorationJSONSettings): Boolean



  def generateBashScriptToCompileAndRunOnBoard(tp: TunePoint[T])
                                              (implicit runConfig: RunConfig,
                                               jsonSettings: ConvExplorationJSONSettings): Unit = {

    val scriptFile = new File(ShellUtils.join(runConfig.kernelDir(tp.layerConfigIdx, tp.tunePointIdx), "runOne.sh"))
    val scriptWriter = new PrintWriter(scriptFile)

    val layerConfigAsMacroString = paramValuesAsCppMacroString(tp.t.l.macroNamesForConfigVars, tp.layerConfigValues)
    val layerConfigAsEchoCmds = tp.t.l.configParamValuesToString(tp.layerConfigValues)
      .replace("\r", "").split("\n").map("echo \"" + _ + "\"\n")
    val rewritePassParamValsAsEchoStrings =
      tp.rewritePassesParamValues.flatMap(kv => List(kv._1.name,
        kv._2.toStrings(PrintFormat.RewritePass).mkString(",\n").replace("\"", "\\\"")
      ).mkString("\n").replace("\r", "").split("\n").map("echo \"" + _ + "\"\n")).toList
    val tuneParamsAsEchoString = tp.t.tuneParamValuesToString(tp.tuneValues, tp.nWrgs)
      .replace("\r", "").split("\n").map("echo \"" + _ + "\"\n")

    if (!runConfig.useCases2019TestHarness)
      scriptWriter.write("source init_experiment_env.sh\n")

    val cmd = native_compile_and_run(path = ".", jsonSettings.generatedHostCodeFName,
      extra_native_compile_flags = "-I/home/shunya/armnn-onnx/ComputeLibrary/include -L/usr/lib/aarch64-linux/gnu/ -lmali " +
        layerConfigAsMacroString + " $TRIALS -Wno-ignored-attributes" +
        (if (!runConfig.useCases2019TestHarness) " -D TRIALS=" else ""),
      test_harness_file = jsonSettings.testHarnessFName,
      extra_run_flags = ".",
      delete = false, return_cmds = true)

    scriptWriter.write("echo \"*** Layer config ***\"\n")
    layerConfigAsEchoCmds.foreach(scriptWriter.write)
    scriptWriter.write("echo \"*** Rewrite pass params *** \"\n")
    rewritePassParamValsAsEchoStrings.foreach(scriptWriter.write)
    scriptWriter.write("echo \"*** Tuning params ***\"\n")
    tuneParamsAsEchoString.foreach(scriptWriter.write)
    scriptWriter.write("\n")
    scriptWriter.write("echo \"Generated on " + new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(Calendar.getInstance().getTime) + "\"\n")
    if (!runConfig.useCases2019TestHarness)
      scriptWriter.write("echo \"Current datetime is $(date +'%H:%M:%S %d.%m.%Y')\"\n")
    cmd.split("&&").foreach(subCmd => scriptWriter.write(subCmd.trim() + "\n"))

    if (!runConfig.useCases2019TestHarness)
      scriptWriter.write("source deinit_experiment_env.sh\n")
    scriptWriter.close()

    logger.info("Native compilation & running script generated.")
  }

  def runAndValidateOnDesktop(tp: TunePoint[T])
                             (implicit runConfig: RunConfig,
                              jsonSettings: ConvExplorationJSONSettings): Unit = {
    val layerConfigAsMacroString = paramValuesAsCppMacroString(tp.t.l.macroNamesForConfigVars, tp.layerConfigValues)

    val stdout = native_compile_and_run(runConfig.kernelDir(tp.layerConfigIdx, tp.tunePointIdx), jsonSettings.generatedHostCodeFName,
      extra_native_compile_flags = "-I\"" + System.getenv("LIFT_OPENCL_LIB_PATH_ON_WINDOWS") + "\\include\"" +
        " -L \"" + System.getenv("LIFT_OPENCL_LIB_PATH_ON_WINDOWS") + "\\lib\\x64\" " +
        "-Wno-ignored-attributes " +
        layerConfigAsMacroString + " -D VERIFY_CORRECTNESS -D TRIALS=1",
      test_harness_file = jsonSettings.testHarnessFName,
      extra_run_flags = runConfig.kernelDir(tp.layerConfigIdx, tp.tunePointIdx), delete = false,
      return_cmds = false, formatPathsForWindows = true)

    //      (s"$path/remove_kernels.sh $path") !

    assert(stdout.contains("VERIFICATION: SUCCESS"))
    logger.info("Test verification: SUCCESS")
  }

  def deinit(): Unit = {
    OpenCLVersion.is200 = originalOpenCLis200
    NewFactorizationOfSum.enabled = originalNewFactorizationEnabledStatus
  }

  def readParMappingsFromCsv(filePath: File,
                             netName: String,
                             layerConfigIdx: Int,
                             parFusePass: RewritingPass): List[(MD5Hash, RewritePassParams # RewriteParamValuesT)] = {
    if (!filePath.exists())
      throw new NoSuchFileException(filePath.getPath)
    else {
      val bufferedSource = io.Source.fromFile(filePath)
      val lines = bufferedSource.getLines()
      val header = lines.next().split(",").map(_.trim)

      assert(
        header.zip(List("net_name","net_label","layer_idx","tune_point_idx","parMappingHash","mapTransformWithMapIds"))
          .forall { case (l, r) => l.equals(r) })

      val result = lines.map(_.split(",").map(_.trim)).collect {
        case Array(lineNetName, _, lineLayerIdx, _, parMappingHash, mapTransformWithMapIds)
          if lineNetName.equals(netName) && lineLayerIdx.toInt == layerConfigIdx =>
          // A single full parallel mapping

          val mapParamsAndEncodedValuesUnsorted: Predef.Map[RewriteParam[_], Cst] =
            mapTransformWithMapIds.split(";").map(_.split(":") match {
              case Array(mapId: String, transformationCode: String) =>
                assert(mapId.nonEmpty)
                assert(transformationCode.nonEmpty)

                val mapParam: RewriteParam[_] = parFusePass.rewriteParams.paramMap.find {
                  case (paramName, _) if paramName.startsWith(s"mapTransform.$mapId.") => true
                  case _ => false
                } match {
                  case None => throw new IllegalArgumentException(s"$mapId, $transformationCode,\n${parFusePass.rewriteParams.paramMap}")
                  case Some((_, param)) => param
                }

                val parDomain: ParallelizationAndFusion.Code.Val = transformationCode(0) match {
                  case '0' => ParallelizationAndFusion.Code.mapSeq
                  case '2' => ParallelizationAndFusion.Code.mapLcl // NB: the encoding of transformations was changed in Python scripts (domain codes shifted by 1)
                  case '3' => ParallelizationAndFusion.Code.mapWrg
                  case '4' => ParallelizationAndFusion.Code.mapGlb
                  case _ => throw new IllegalArgumentException(s"$mapId, $transformationCode,\n${parFusePass.rewriteParams.paramMap}")
                }

                val parDim = transformationCode(1).toInt - 48
                assert(0 <= parDim && parDim <= 2)

                val translatedIntegerCode = parDomain.value * 10 + parDim

                mapParam -> Cst(translatedIntegerCode)

              case x => throw new IllegalArgumentException(s"${x.mkString("Array(", ", ", ")")},\n${parFusePass.rewriteParams.paramMap}")
            }).toMap

          assert(parFusePass.rewriteParams.params.forall(mapParamsAndEncodedValuesUnsorted.contains(_)))

          val encodedValuesSorted = parFusePass.rewriteParams.params.map(param => mapParamsAndEncodedValuesUnsorted(param))

          (parMappingHash, parFusePass.rewriteParams.decode(Some(encodedValuesSorted)))
      }.toList

      logger.info(s"Read ${result.length} parallel mappings from ${filePath.getPath}")
      result
    }

  }
}

object BenchmarkStages {
  type MD5Hash = String
}