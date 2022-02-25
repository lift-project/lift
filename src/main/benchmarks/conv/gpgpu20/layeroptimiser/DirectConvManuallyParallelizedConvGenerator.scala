package benchmarks.conv.gpgpu20.layeroptimiser

import benchmarks.conv.gpgpu20
import benchmarks.conv.gpgpu20.{ConvStencil3D, LambdaWithMetaInfo, init}
import benchmarks.conv.gpgpu20.layeroptimiser.ConvStencil3DOpt.{ConvStencil3DConfigSpace, ConvStencil3DTuneParamSpace}
import benchmarks.conv.gpgpu20.pad.{DepadConv, PadConv}
import benchmarks.conv.gpgpu20.paramspaces.layerconfig.LayerConfigSpace
import benchmarks.conv.gpgpu20.resources.VGGLayerConfigs
import benchmarks.conv.gpgpu20.settings.Settings
import cbackends.global.GlobalCompiler
import cbackends.host.host_ir.{OclFunc, ToGPU, ToHost}

import java.io.{File, FileNotFoundException, FileWriter, PrintWriter, StringWriter}
import com.typesafe.scalalogging.Logger
import exploration.ParameterRewrite.substitute
import exploration.{ParameterRewrite, ParameterSpace}
import ir.ast.{Lambda, PadConstant, Value, fun}
import ir.{ArrayType, Type}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.executor.{Execute, Executor}
import opencl.generator.NDRange
import opencl.ir.id
import patterns.nn.utils.Utils.slidingOutputSize
import rewriting.Rewrite.applyRulesUntilCannot
import rewriting.rules.{NeuralNetRules, Rule}
import rewriting.utils.DumpToFile

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.io.Source
import scala.util.Random
import scala.util.control.Breaks._

object DirectConvManuallyParallelizedConvGenerator {

  val netLayerConfigs = VGGLayerConfigs

  val useManualConfigs: Boolean = true

  def AT = ArrayType // alias
  type AT = ArrayType // alias

  private val logger = Logger(this.getClass)


  def layerConfigIdString(layerConfigIdx: Int): String = f"$layerConfigIdx" + (if (useManualConfigs) " (" +
    netLayerConfigs.configNames(layerConfigIdx) + ")")

  def logConfigs(layerConfigIdx: Int,
                 layerConfig: Vector[Cst],
                 tunePointIdx: Int,
                 tuningArgs: Vector[Cst]): Unit = {

    logger.info(f"Layer config ${layerConfigIdString(layerConfigIdx)}:\n" +
      f"${ConvStencil3DConfigSpace.params.paramVector.zip(layerConfig)}")
    logger.info(f"Tuning parameters $tunePointIdx: ${ConvStencil3DTuneParamSpace.params.paramVector.zip(tuningArgs)}")
  }


  /* Convolutional lambda factory*/
  val convLambdaFactory = new ConvStencil3D(
    ConvStencil3DConfigSpace.params,
    ConvStencil3DTuneParamSpace.params,
    ConvStencil3DTuneParamSpace.rewriteParams,
    ConvStencil3DTuneParamSpace.fuseLambdas,
    ConvStencil3DTuneParamSpace.shareKernels)

  var layerConfigList: List[Vector[Cst]] = List()

  /** Layer configs **/
  def getLayerConfig(layerConfigIdx: Int,
                     configSpaceFactory: ConvStencil3DConfigSpace,
                     configPoints: Int, random: Random,
                     useManualConfigs: Boolean): Vector[Cst] = {
    logger.info(f"Picking a layer configuration parameter value combination $layerConfigIdx")

    while (layerConfigList.length <= layerConfigIdx)
      if (useManualConfigs) {
        layerConfigList = layerConfigList :+ netLayerConfigs.configs(layerConfigIdx)
        //      Vector(VGGLayerConfigs.configs(0))
      } else {
        layerConfigList = layerConfigList :+ configSpaceFactory.getNextRandomPoint(random)
      }

    if (useManualConfigs)
      logger.info(f"Getting one of the hardcoded layer configurations (${netLayerConfigs.configNames(layerConfigIdx)})")
    else
      logger.info("Generating a random layer configuration.")

    layerConfigList(layerConfigIdx)
  }


  /** Optimisation parameters **/
  def generateTuneParamValues(tuneSpace: ConvStencil3DTuneParamSpace,
                              layerConfig: Vector[Cst],
                              tunePoints: Int, random: Random): Vector[Vector[Cst]] = {
    logger.info("Generating random tuning parameter value combinations.")
    (0 until tunePoints).map(_ =>
      tuneSpace.getNextRandomPoint(
        random,
        independentValues = ListMap(ConvStencil3DConfigSpace.params.paramVector.zip(layerConfig): _*))).toVector
  }


  /** ONNX files **/

  def ensureONNXDirExists(onnxConfigsRootDir: String): Unit = {
    val path = gpgpu20.utils.joinPaths(onnxConfigsRootDir, "onnx", "generated_files")
    val dir = new File(path)
    if (!dir.exists) new File(path).mkdirs()
    else {
      if (!dir.isDirectory) dir.delete()
      new File(path).mkdirs()
    }
  }


  def createONNXFactory(layerConfig: Vector[Cst],
                        onnxConfigsRootDir: String,
                        configSpace: LayerConfigSpace): String = {
    // Make sure ONNX Python factory template exists
    val onnxFactoryTemplate: File = new File(gpgpu20.utils.joinPaths(
      onnxConfigsRootDir, "onnx", "factories", configSpace.pythonFactoryName))

    if (!onnxFactoryTemplate.exists()) throw new FileNotFoundException("ONNX Python factory does not exist")

    /* Create ONNX Python factory with current combination values */
    val factoryTemplate = Source.fromFile(onnxFactoryTemplate.getAbsolutePath).mkString

    // Replace dependent variables with actual values
    val dependentConfigParamValues = configSpace.reifyDependentParameters(layerConfig)


    /* Replace parameters with actual values in the template of the factory */
    // Independent parameters
    val srcWithIndependentVals = configSpace.layerParams.paramVector.zipWithIndex.
      foldLeft(factoryTemplate)((src, paramAndIndex) => {
        val param = paramAndIndex._1
        val paramIndex = paramAndIndex._2
        src.replace("$" + param.name + "%d", layerConfig(paramIndex).evalInt.toString)
      })

    // Dependent parameters
    configSpace.dependentParameters match {
      case Some(dParams) => dParams.foldLeft(srcWithIndependentVals)((src, param) => {
        src.replace("$" + param._1 + "%d", dependentConfigParamValues.get(param._1).evalInt.toString)
      })
      case None => srcWithIndependentVals
    }
  }


  def generateONNXFileUsingFactory(factorySource: String,
                                   layerConfig: Seq[Cst],
                                   configSpace: LayerConfigSpace,
                                   settings: Settings): Unit = {
    // Create new onnx factory script
    //    val factoryFile = File.createTempFile("onnxFactory", ".py")
    //    factoryFile.deleteOnExit()
    //    val factoryWriter = new FileWriter(factoryFile)
    //
    //    factoryWriter.write(factorySource)
    //
    //    factoryWriter.close()
    //
    //    // Make sure the onnx file does not exist
    //    val onnxFile: File = new File(configSpace.getOnnxConfigPath(layerConfig, settings))
    //
    //    if (onnxFile.exists)
    //      if (settings.allowOverwritingOnnxConfigs) onnxFile.delete()
    //      else throw new FileAlreadyExistsException("Cannot overwrite " + onnxFile.getAbsolutePath)
    //
    //    // Produce the onnx file using the Python factory
    //    val (out, err) = utils.runShell(Seq(settings.pythonExec, factoryFile.getAbsolutePath))
    //    if (!err.isEmpty)
    //      throw new IllegalStateException(f"Python ONNX factory failed.\nSTDERR:\n$err\nSTDOUT:\n$out")
  }


  def generateONNXFiles(settings: Settings,
                        layerConfig: Vector[Cst],
                        configSpace: LayerConfigSpace): Unit = {

    ensureONNXDirExists(settings.onnxConfigsRootDir)

    val factorySource = createONNXFactory(layerConfig, settings.onnxConfigsRootDir, configSpace)
    generateONNXFileUsingFactory(factorySource, layerConfig, configSpace, settings)
  }

  //  val configSpaceParams = ConvStencil3DConfigSpace.params
  //  val tuneSpaceParams = ConvStencil3DTuneParamSpace.params

  def rewriteLambdas(lambdas: List[Lambda], rules: ListMap[String, Rule]): List[Lambda] = {

    logger.info(s"Rewriting lambdas. Applying ${rules.size} rule(s): ${rules.keys.mkString(", ")}")

    lambdas.zipWithIndex.map {
      case (lambda, lambdaIdx) =>

        val (rewrittenBody, appliedRules) = applyRulesUntilCannot(lambda.body, rules.values.toSeq, List())
        val rewrittenLambda = Lambda(lambda.params, rewrittenBody)

        logger.info(s"For parametric lambda ${lambdaIdx + 1}, applied the following rules: " +
          s"${rules.filter(pair => appliedRules.contains(pair._2)).keys.mkString(", ")}")

        if (lambdaIdx == 0 && appliedRules.length < rules.size)
          throw new RuntimeException(
            s"For parametric lambda ${lambdaIdx + 1}, applied ${appliedRules.length} out of ${rules.size} rules")


        rewrittenLambda
    }
  }


  def generatePaddingLambda(lambdaFactory: ConvStencil3D,
                            substitutionTable: Map[Var, Cst],
                            layerConfig: Vector[Cst],
                            tuningArgs: Vector[Cst],
                            appliedRules: ListMap[String, Rule],
                            layerConfigIdx: Int,
                            tunePointIdx: Int,
                            settings: Settings): (Lambda, PadConv) = {
    logger.info("Generating the padding lambda")

    val padFactory = new PadConv(
      ConvStencil3DConfigSpace.params,
      ConvStencil3DTuneParamSpace.params, lambdaFactory,
      originalSize = substitutionTable(ConvStencil3DConfigSpace.params.inputWidthHeight),
      originalType =
        AT(AT(AT(AT(opencl.ir.Float,
          ConvStencil3DConfigSpace.params.inputChannels),
          ConvStencil3DConfigSpace.params.inputWidthHeight),
          ConvStencil3DConfigSpace.params.inputWidthHeight),
          ConvStencil3DConfigSpace.params.nInputs),
      padFunc = substitutionTable(ConvStencil3DConfigSpace.params.padFunc),
      padOptTotal = substitutionTable(ConvStencil3DTuneParamSpace.params.padOptTotal),
      newType = lambdaFactory.originalXType)

    val paddingLambda = ParameterRewrite(padFactory(), substitutionTable)

    val paddingLambdaNDRanges = padFactory.paddingLambdaNDRanges(substitutionTable)

    logger.info("Saving the padding lambda")
    logConfigs(layerConfigIdx, layerConfig, tunePointIdx, tuningArgs)

    logger.info(f"Padding lambda local sizes (0-2): ${paddingLambdaNDRanges._1}; " +
      f"global sizes (0-2): ${paddingLambdaNDRanges._2}")

    val paddingLambdaFileName = settings.lambdaDir + //System.getProperty("user.dir") + "/../generated_files/" +
      f"/ConvStencil3DPaddingLambda_${layerConfigIdx}_${tunePointIdx}.scala"

    DirectConvManuallyParallelizedConvGenerator.saveLambda(
      paddingLambda, paddingLambdaFileName, paddingLambdaNDRanges, substitutionTable, appliedRules)

    //    logger.info("Compiling the padding lambda with NDRange injection from the generated file to OpenCL to " +
    //      "make sure it's valid")
    //
    //    val ((localSizesPad: NDRange, globalSizesPad: NDRange), paddingLambdaFromFile: Lambda) =
    //      Eval.eval(readFromFile(paddingLambdaFileName))
    //
    //    val kernelPad = Compile(paddingLambdaFromFile, localSizesPad, globalSizesPad)
    //    val padFile = scala.tools.nsc.io.File("/tmp/paddingKernel.cl")
    //
    //    padFile.writeAll(kernelPad)

    (paddingLambda, padFactory)
  }

  def generateConcreteConvLambda(lambdaId: Int,
                                 rewrittenParametricLambdas: List[Lambda],
                                 substitutionTable: Map[Var, Cst],
                                 appliedRules: ListMap[String, Rule],
                                 layerConfig: Vector[Cst],
                                 layerConfigIdx: Int,
                                 tuningArgs: Vector[Cst],
                                 tuneArgsIdx: Int,
                                 settings: Settings): Lambda = {
    logger.info(f"Substituting params in lambda $lambdaId")
    logConfigs(layerConfigIdx, layerConfig, tuneArgsIdx, tuningArgs)

    val lambdaNDRanges = DirectConvManuallyParallelizedConvGenerator.calculateLocalAndGlobalSizes(substitutionTable, appliedRules.values.toList)

    val concreteLambda = ParameterRewrite(rewrittenParametricLambdas(lambdaId), substitutionTable)

    logger.info(f"Lambda $lambdaId local sizes (0-2): ${lambdaNDRanges(lambdaId)._1}; " +
      f"global sizes (0-2): ${lambdaNDRanges(lambdaId)._2}")

    val lambdaFileName = settings.lambdaDir + //System.getProperty("user.dir") + "/../generated_files/" +
      f"/ConvStencil3DConcreteLambda${lambdaId}_${layerConfigIdx}_${tuneArgsIdx}.scala"

    DirectConvManuallyParallelizedConvGenerator.saveLambda(
      concreteLambda, lambdaFileName, lambdaNDRanges(lambdaId), substitutionTable, appliedRules)

    //    logger.info(f"Compiling lambda ${lambdaId} with NDRange injection from the generated file to OpenCL make sure it's valid")
    //
    //    val ((localSizes: NDRange, globalSizes: NDRange), lambda: Lambda) = Eval.eval(readFromFile(lambdaFileName))
    //    val kernel = Compile(lambda, localSizes, globalSizes)

    logger.info(f"Generation of concrete lambda ${lambdaId} completed")

    concreteLambda
  }


  def generateDepaddingLambda(substitutionTable: Map[Var, Cst],
                              layerConfigIdx: Int,
                              tuneArgsIdx: Int,
                              appliedRules: ListMap[String, Rule],
                              settings: Settings): (Lambda, DepadConv) = {

    val paddedWidthHeight = ArithExpr.substitute(slidingOutputSize(
      ConvStencil3DConfigSpace.params.inputWidthHeight +
        2 * ConvStencil3DConfigSpace.params.padFunc +
        ConvStencil3DTuneParamSpace.params.padOptTotal,
      ConvStencil3DConfigSpace.params.kernelWidthHeight,
      ConvStencil3DConfigSpace.params.kernelStride), substitutionTable.toMap)

    val paddedResultType = Type.substitute(
      AT(AT(AT(AT(opencl.ir.Float,
        paddedWidthHeight),
        paddedWidthHeight),
        ConvStencil3DConfigSpace.params.kernelChannels),
        ConvStencil3DConfigSpace.params.nInputs),
      substitutionTable.toMap)

    val depaddedWidthHeight = ArithExpr.substitute(slidingOutputSize(
      ConvStencil3DConfigSpace.params.inputWidthHeight + 2 * ConvStencil3DConfigSpace.params.padFunc,
      ConvStencil3DConfigSpace.params.kernelWidthHeight,
      ConvStencil3DConfigSpace.params.kernelStride), substitutionTable.toMap)

    val depaddedResultType = Type.substitute(
      AT(AT(AT(AT(opencl.ir.Float,
        depaddedWidthHeight),
        depaddedWidthHeight),
        ConvStencil3DConfigSpace.params.kernelChannels),
        ConvStencil3DConfigSpace.params.nInputs),
      substitutionTable.toMap)


    val depadSize = paddedWidthHeight - depaddedWidthHeight

    val depadFactory = new DepadConv(
      ConvStencil3DConfigSpace.params,
      ConvStencil3DTuneParamSpace.params,
      convLambdaFactory,
      originalSize = paddedWidthHeight,
      originalType = paddedResultType,
      depadSize = depadSize,
      newType = depaddedResultType)

    val depaddingParametricLambda = depadFactory()

    val depaddingLambda = ParameterRewrite(depaddingParametricLambda, substitutionTable)

    val depaddingLambdaNDRanges = depadFactory.depaddingLambdaNDRanges(substitutionTable)

    logger.info(f"Saving the depadding lambda for layer config ${layerConfigIdString(layerConfigIdx)} tuning point $tuneArgsIdx")

    logger.info(f"Depadding lambda local sizes (0-2): ${depaddingLambdaNDRanges._1}; " +
      f"global sizes (0-2): ${depaddingLambdaNDRanges._2}")

    val depaddingLambdaFileName = settings.lambdaDir + //System.getProperty("user.dir") + "/../generated_files/" +
      f"/ConvStencil3DDepaddingLambda_${layerConfigIdx}_${tuneArgsIdx}.scala"

    DirectConvManuallyParallelizedConvGenerator.saveLambda(
      depaddingLambda, depaddingLambdaFileName, depaddingLambdaNDRanges, substitutionTable, appliedRules)

    //    logger.info("Compiling the depadding lambda with NDRange injection from the generated file to OpenCL " +
    //      "make sure it's valid")
    //
    //    val ((localSizesDepad: NDRange, globalSizesDepad: NDRange), depaddingLambdaFromFile: Lambda) =
    //      Eval.eval(readFromFile(depaddingLambdaFileName))
    //
    //    val kernelDepad = Compile(depaddingLambdaFromFile, localSizesDepad, globalSizesDepad)
    //    val depadFile = scala.tools.nsc.io.File("/tmp/depaddingKernel.cl")
    //
    //    depadFile.writeAll(kernelDepad)

    (depaddingLambda, depadFactory)
  }

  val useGlobalMaps: Boolean = System.getenv("USE_GLOBAL_MAPS") != null && System.getenv("USE_GLOBAL_MAPS").toLowerCase == "true"

  def ndRangeExpressions(appliedRules: List[Rule]):
  ((List[ArithExpr], List[ArithExpr]),
    (List[ArithExpr], List[ArithExpr])) = {
    val vectorLen = if (appliedRules.contains(NeuralNetRules.StencilRules.vectorise(4))) Cst(4) else Cst(1)

    if (useGlobalMaps) {

      val lambda1LocalSizes = List(
        Cst(1), // Dimension 0
        Cst(1), // Dimension 1
        Cst(1) // Dimension 2
      )
      val lambda1GlobalSizes = List(
        convLambdaFactory.nKernelGroups * convLambdaFactory.nChunksInWindow,// /^ vectorLen, // Dimension 0
        convLambdaFactory.nTilesTotal * convLambdaFactory.nWindowGroupsInTile, // Dimension 1
        Cst(1) // Dimension 2
      )
      val lambda2LocalSizes = List(
        Cst(1), // Dimension 0
        Cst(1), // Dimension 1
        Cst(1) // Dimension 2
      )
      val lambda2GlobalSizes = List(
        convLambdaFactory.nWindowsInTile, // Dimension 0
        convLambdaFactory.nKernelGroups, // Dimension 1
        convLambdaFactory.nTilesTotal  // Dimension 2
      )
      ((lambda1LocalSizes, lambda1GlobalSizes), (lambda2LocalSizes, lambda2GlobalSizes))

    } else {

      val lambda1LocalSizes = List(
        convLambdaFactory.nChunksInWindow, // Dimension 0
        convLambdaFactory.nWindowGroupsInTile, // Dimension 1
        Cst(1) // Dimension 2
      )
      val lambda1GlobalSizes = List(
        convLambdaFactory.nKernelGroups * lambda1LocalSizes(0), // Dimension 0
        convLambdaFactory.nTilesTotal * lambda1LocalSizes(1), // Dimension 1
        Cst(1) * lambda1LocalSizes(2) // Dimension 2
      )

      val lambda2LocalSizes = List(
        convLambdaFactory.nWindowsInTile, // Dimension 0
        ConvStencil3DTuneParamSpace.params.nKernelsPerWrg, // Dimension 1
        Cst(1) // Dimension 2
      )
      val lambda2GlobalSizes = List(
        convLambdaFactory.nKernelGroups * lambda2LocalSizes(0), // Dimension 0
        convLambdaFactory.nTilesTotal * lambda2LocalSizes(1), // Dimension 1
        Cst(1) * lambda2LocalSizes(2) // Dimension 2
      )

      ((lambda1LocalSizes, lambda1GlobalSizes), (lambda2LocalSizes, lambda2GlobalSizes))
    }
  }


  def calculateLocalAndGlobalSizes(parameterVals: Map[Var, Cst],
                                   appliedRules: List[Rule]): List[(NDRange, NDRange)] = {
    val _substitute = (a: ArithExpr) => substitute(a, parameterVals)
    val constructRange = (l: List[ArithExpr]) => NDRange(l(0), l(1), l(2))
    List(
      (constructRange(ndRangeExpressions(appliedRules)._1._1.map(_substitute)),
        constructRange(ndRangeExpressions(appliedRules)._1._2.map(_substitute))),
      (constructRange(ndRangeExpressions(appliedRules)._2._1.map(_substitute)),
        constructRange(ndRangeExpressions(appliedRules)._2._2.map(_substitute))))
  }


  def saveLambda(lambda: Lambda, lambdaFileName: String): Unit = {
    logger.info(s"Saving a lambda to $lambdaFileName")
    val file = scala.tools.nsc.io.File(lambdaFileName)

    file.writeAll(DumpToFile.dumpLambdaToString(lambda))

    // Make sure it can still be evaluated
    //    Eval(readFromFile(lambdaFileName))
    logger.info(s"Done saving a lambda")
  }


  def saveLambda(lambda: Lambda, lambdaFileName: String, lambdaNDRanges: (NDRange, NDRange),
                 substitutionTable: Map[Var, Cst],
                 appliedRules: ListMap[String, Rule]): Unit = {
    logger.info(f"Saving a lambda to $lambdaFileName")
    val file = scala.tools.nsc.io.File(lambdaFileName)

    file.writeAll("( /* Timestamp */ \"" + java.time.LocalDateTime.now().toString.replace("T", " ") + "\", \n" +
      f"  (/* Local sizes */ NDRange(${lambdaNDRanges._1}), \n" +
      f"   /* Global sizes */ NDRange(${lambdaNDRanges._2})), \n" +
      "  /* Parameters */ \n\"" +
      substitutionTable.toList.sortBy(_._1.name).map {
        case (variable, value) =>
          (variable.name + " = ").padTo(20, " ").mkString + value.evalInt.toString.reverse.padTo(5, " ").reverse.mkString}.
        grouped(3).map(group => group.mkString(", ")).mkString(",    \\n\" + \n\"") + ")\", \n" +
      "  /* Applied rewrite rules */ List(\"" + appliedRules.keys.mkString("\", \"") + "\"), {\n" +
      DumpToFile.dumpLambdaToString(lambda) + "\n})")
    logger.info(s"Done saving a lambda")
  }


  def batch_code_generate_for_cases_paper(settings: Settings): Unit = {
    logger.info("Initializing")


    //val path = s"$common_path/99.cases_paper"
    val lambda_path = settings.lambdaDir
    val generated_c_path = settings.kernelsDir


    val common_file_name0 = lambda_path  + "ConvStencil3DPaddingLambda_"
    val common_file_name1 = lambda_path + "ConvStencil3DConcreteLambda0_"
    val common_file_name2 = lambda_path  + "ConvStencil3DConcreteLambda1_"
    val common_file_name3 = lambda_path  + "ConvStencil3DDepaddingLambda_"

    val totalTuningPoints = 5000
    val tuningPointBatchSize = 500
    val nLayers = 13
    val fuseLambdas: Boolean = true
    val null_local_ranges: Boolean = false
    val continueFromLayer = 0
    val continueFromTunePoint = 0

    for {tuningPointBatch <- 0 until (totalTuningPoints / tuningPointBatchSize)}
    //      for {layerConfigId <- 0 until nLayers} {
      for {layerConfigId <- List(9)} {
        if (layerConfigId >= continueFromLayer) {
          for {tuningId <- (tuningPointBatch * tuningPointBatchSize) until ((tuningPointBatch + 1) * tuningPointBatchSize)} {//000..200, 200..400, 400..600, 600..800, 800..1000
            if (tuningId >= continueFromTunePoint || layerConfigId > continueFromLayer) {
              logger.info(s"Starting with layerConfigId $layerConfigId and tuningId $tuningId")
              val file0 = common_file_name0 + layerConfigId + "_" + tuningId + ".scala"
              val file1 = common_file_name1 + layerConfigId + "_" + tuningId + ".scala"
              val file2 = common_file_name2 + layerConfigId + "_" + tuningId + ".scala"
              val file3 = common_file_name3 + layerConfigId + "_" + tuningId + ".scala"



              logger.info(s"Reading lambdas from files")
              //ndrange is in the reversed order of c++ enqueneNDRange
              val padLambda = LambdaWithMetaInfo(file0)
              val convLambda0 = LambdaWithMetaInfo(file1)
              val convLambda1 = LambdaWithMetaInfo(if (fuseLambdas) /* dummy */file1 else file2)
              val depadLambda = LambdaWithMetaInfo(file3)
              logger.info(s"Done reading lambdas from files")


              //Compile(padLambda.lambda, padLambda.ndranges._1, padLambda.ndranges._2)

              val whole_fun = fun(
                convLambda0.lambda.params(0).t,
                if (fuseLambdas) convLambda0.lambda.params(1).t else convLambda1.lambda.params(0).t,
                padLambda.lambda.params(0).t,

                if (!fuseLambdas)
                  (p_k, p_b, p_x) => ToHost() $
                    OclFunc(depadLambda.lambda, depadLambda.ndranges, cpu_timer = true, gpu_timer = true).apply(
                      OclFunc(convLambda1.lambda, (if (null_local_ranges) null else convLambda1.ndranges._1, convLambda1.ndranges._2)/*convLambda1.ndranges*/,
                        cpu_timer = true, gpu_timer = true).apply(ToGPU() $ p_b,
                        OclFunc( convLambda0.lambda, (if (null_local_ranges) null else convLambda0.ndranges._1, convLambda0.ndranges._2)/*convLambda0.ndranges*/,
                          cpu_timer = true, gpu_timer = true).apply(ToGPU() $ p_k,
                          OclFunc( padLambda.lambda, padLambda.ndranges, cpu_timer = true, gpu_timer = true) o ToGPU() $ p_x)))
                else
                //              (p_k, p_b, p_x) => ToHost() $
                //                OclFunc(depadLambda.lambda, depadLambda.ndranges, cpu_timer = true, gpu_timer = true).apply(
                //                  OclFunc(
                //                    fun((k, b, x) => convLambda1.lambda(b, convLambda0.lambda(k, x))),
                //                    (if (null_local_ranges) null else convLambda0.ndranges._1, convLambda0.ndranges._2)/*convLambda1.ndranges*/,
                //                    cpu_timer = true, gpu_timer = true).apply(ToGPU() $ p_k, ToGPU() $ p_b,
                //                    OclFunc( padLambda.lambda, padLambda.ndranges, cpu_timer = true, gpu_timer = true) o ToGPU() $ p_x))
                  (p_k, p_b, p_x) => ToHost() $
                    OclFunc(depadLambda.lambda, depadLambda.ndranges, cpu_timer = true, gpu_timer = true).apply(
                      OclFunc(
                        fun((k, b, x) => convLambda0.lambda(k, b, x)),
                        (if (null_local_ranges) null else convLambda0.ndranges._1, convLambda0.ndranges._2)/*convLambda1.ndranges*/,
                        cpu_timer = true, gpu_timer = true).apply(ToGPU() $ p_k, ToGPU() $ p_b,
                        OclFunc( padLambda.lambda, padLambda.ndranges, cpu_timer = true, gpu_timer = true) o ToGPU() $ p_x))
              )


              //("mkdir -p " + s"$path" ) !!

              //("mkdir -p " + s"$generated_c_path" ) !!

              val path_with_config = generated_c_path + layerConfigId + "/" + tuningId
              //              ("mkdir -p " + s"$path_with_config") !!
              val file_with_config = "libhost.cpp"

              logger.info("[Log]: compiling for "+path_with_config)

              try {
                // Read libhost.cpp
                GlobalCompiler ! (whole_fun, path_with_config, List(file_with_config))
                val fr = Source.fromFile(path_with_config + "/" + file_with_config)
                val originalFile = fr.getLines().mkString("\n")
                fr.close()

                // Prepend libhost.cpp contents with meta info
                val fw = new FileWriter(path_with_config + "/" + file_with_config)
                try {
                  fw.write(f"// Layer $layerConfigId, tuning point $tuningId\n" +
                    "// Lambda generation timestamp: " + convLambda0.timestamp + " \n" +
                    "// Kernel generation timestamp: " + java.time.LocalDateTime.now().toString.replace("T", " ") + "\n" +
                    "// ND ranges of conv lambda 0: (" + convLambda0.ndranges._1.toString + "), (" +
                    convLambda0.ndranges._2.toString + ")\n" +
                    "// Applied rules: " + convLambda0.appliedRules.mkString(", ") + "\n" +
                    "/* Parameters: \n" + convLambda0.parameters + " */\n" +
                    originalFile)
                }
                finally fw.close()
              } catch {
                case e: StackOverflowError =>
                  println("[Log]: ERROR: could not compile for " + path_with_config + " due to a StackOverflow")
                  val sw = new StringWriter
                  e.printStackTrace(new PrintWriter(sw))
                  println(sw.toString)
              }
              logger.info(s"Done with layerConfigId $layerConfigId and tuningId $tuningId")
            }
          }
        }
      }


    println("Test done!")

  }


  def main(args: Array[String]): Unit = {
    /** Initialisation **/
    val (cmdArgs, settings) = init(args)
//    Executor.loadAndInit()

    if (cmdArgs.hasOption("compile_lambdas")) {
      batch_code_generate_for_cases_paper(settings)
     return
    }


    /** Generate parametric lambdas **/
    logger.info("Generating initial parametric lambdas.")
    if (useGlobalMaps)
      logger.info("Replacing all MapWrg/MapLcl with MapGlb or MapSeq")
    else
      logger.info("NOT replacing all MapWrg/MapLcl with MapGlb or MapSeq")

    /** Set up randomisers **/
    val randomisers = (0 until netLayerConfigs.nConfigs).toList.map(_ => new scala.util.Random(settings.randomSeed))

    val parametricLambdas: List[Lambda] = convLambdaFactory(id)
    val totalTuningPoints = 5000
    val tuningPointBatchSize = 500


    for {tuningPointBatch <- 0 until (totalTuningPoints / tuningPointBatchSize)} {
      for {
//        layerConfigIdx <- 0 until netLayerConfigs.nConfigs
                  layerConfigIdx <- List(9)
      } {
        /** Randomiser **/
        val random = randomisers(layerConfigIdx)

        var inApplicableRewriteRules: mutable.Set[ListMap[String, Rule]] = mutable.Set.empty

        for {tunePointIdx <- //000..200, 200..400, 400..600, 600..800, 800..1000
             (tuningPointBatch * tuningPointBatchSize) until ((tuningPointBatch + 1) * tuningPointBatchSize)} {
          logger.info(s"\n\n\n")

          var configSpaceFactory: Option[ConvStencil3DConfigSpace] = None
          var layerConfig: Option[Vector[Cst]] = None
          var tuneSpaceFactory: Option[ConvStencil3DTuneParamSpace] = None
          var tuningArgs: Option[Vector[Cst]] = None
          var rewrittenParametricLambdas: Option[List[Lambda]] = None


          /** Rewrite lambdas **/
          logger.info(f"Generating rewritten lambdas")

          var applicableRewriteRules: Option[ListMap[String, Rule]] = None

          // Try to generate such rewrite rule combination for which we can generate a lambda
          // A counterexample is applying asVector(4) on the layer where the sliding window size is 3*3*3=27 (27 % 4 != 0)
          while (applicableRewriteRules.isEmpty && // should check applicableRewriteRules instead
            inApplicableRewriteRules.size < ConvStencil3DTuneParamSpace.maxRewriteCombinations) {

            var candidateRewriteRules: Option[ListMap[String, Rule]] = None
            breakable {
              while (candidateRewriteRules.isEmpty || {
                if (inApplicableRewriteRules.contains(candidateRewriteRules.get)) {
                  logger.info("Skipping the rewrite rule combination that proved to be inapplicable in one of the " +
                    "previous iterations")
                  true
                } else false
              }) {
                logger.info(f"Generating a candidate combination of rewrite rules")
                candidateRewriteRules = Some(ConvStencil3DTuneParamSpace.pickRewriteRulesRandomly(random))
              }

              rewrittenParametricLambdas = Some(rewriteLambdas(parametricLambdas, candidateRewriteRules.get))

              /** patch up to coalesce **/
              val (coalescedBody, appliedCoalescing) = applyRulesUntilCannot(rewrittenParametricLambdas.get(0).body,
                Seq(NeuralNetRules.StencilRules.coalesce(
                  //ConvStencil3DTuneParamSpace.rewriteParams.vectorLen *
                    (ConvStencil3DConfigSpace.params.kernelWidthHeight *
                      ConvStencil3DConfigSpace.params.kernelWidthHeight *
                      ConvStencil3DConfigSpace.params.inputChannels) /^
                    ConvStencil3DTuneParamSpace.params.seqOpsPerThread)), List())
              val coalescedLambda = Lambda(rewrittenParametricLambdas.get(0).params, coalescedBody)
              assert(appliedCoalescing.nonEmpty)
              rewrittenParametricLambdas = Some(List(coalescedLambda) ++
                (if (ConvStencil3DTuneParamSpace.fuseLambdas) List() else List(rewrittenParametricLambdas.get(1))))

              /** Generate config space **/
              logger.info(s"Generating config space factory for layer config ${layerConfigIdString(layerConfigIdx)} " +
                s"tuning point $tunePointIdx")
              configSpaceFactory = Some(ConvStencil3DConfigSpace(rewrittenParametricLambdas.get, settings))

              /** Generate layer config combination **/
              layerConfig = Some(getLayerConfig(layerConfigIdx,
                configSpaceFactory.get, 1, random, useManualConfigs))

              /** Generate tuning search space **/
              logger.info("Generating tune param space factory")

              tuneSpaceFactory = Some(ConvStencil3DTuneParamSpace(
                lambdas = rewrittenParametricLambdas.get,
                appliedRules = candidateRewriteRules.get.toList.unzip._2,
                settings = settings))


              /** Generate tuning parameter combination **/
              tuningArgs = try {

                Some(generateTuneParamValues(tuneSpaceFactory.get, layerConfig.get, tunePoints = 1, random).head)

              } catch {
                case e: ParameterSpace.EmptySpace =>

                  inApplicableRewriteRules += candidateRewriteRules.get
                  logger.warn(e.message)
                  break
              }
              applicableRewriteRules = candidateRewriteRules
            }
          }


          if (applicableRewriteRules.isEmpty) {
            logger.warn("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
            logger.warn(s"Could not find a valid rewrite rule combination for layer configuration " +
              s"${layerConfigIdString(layerConfigIdx)}")
            logger.warn("Skipping generating the rest of the tuning point for this layer configuration and " +
              "proceeding to the next layer configuration")
            logger.warn("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n\n\n")
          } else {

            /** Save layer config space as ONNX files **/
            //          generateONNXFiles(settings, layerConfig.get, configSpaceFactory.get)


            /* Produce rewrite param values */
            val rewriteArgs = List(
              if (applicableRewriteRules.get.contains("vectorise4")) Cst(4) else Cst(1)
            )

            /** Replace parameters (Vars) with corresponding values **/
            val substitutionTable: Map[Var, Cst] =
              (ConvStencil3DConfigSpace.params.paramVector.zip(layerConfig.get) ++
                ConvStencil3DTuneParamSpace.params.paramVector.zip(tuningArgs.get) ++
                ConvStencil3DTuneParamSpace.rewriteParams.paramVector.zip(rewriteArgs)).
                map {
                  case (param, value) => param -> value
                }.toMap


            /** ****** Padding lambda ********/
            val (paddingLambda, padFactory) = generatePaddingLambda(
              convLambdaFactory,
              substitutionTable,
              layerConfig.get,
              tuningArgs.get,
              applicableRewriteRules.get,
              layerConfigIdx,
              tunePointIdx,
              settings)


            /** ****** Lambda 1 ********/
            val concreteLambda0 = generateConcreteConvLambda(0,
              rewrittenParametricLambdas.get,
              substitutionTable,
              applicableRewriteRules.get,
              layerConfig.get,
              layerConfigIdx,
              tuningArgs.get,
              tunePointIdx,
              settings)


            /** ****** Lambda 2 ********/
            val concreteLambda1 = if (ConvStencil3DTuneParamSpace.fuseLambdas) null else
              generateConcreteConvLambda(1,
                rewrittenParametricLambdas.get,
                substitutionTable,
                applicableRewriteRules.get,
                layerConfig.get,
                layerConfigIdx,
                tuningArgs.get,
                tunePointIdx,
                settings)


            /** ****** Depadding lambda ********/
            val (depaddingLambda, depadFactory) = generateDepaddingLambda(
              substitutionTable, layerConfigIdx, tunePointIdx, applicableRewriteRules.get, settings)
          }
        }
      }
    }
  }
}

