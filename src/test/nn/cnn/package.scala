package nn

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get
import java.util.Calendar

import _root_.caffe.caffe.{LayerParameter, V1LayerParameter}
import com.typesafe.scalalogging.Logger
import nn.caffe.proto.Config
import nn.conv.Conv
import org.junit.Assert.assertEquals

import scala.util.parsing.json.JSON

/**
  * Created by nm on 08/02/17.
  */

package object cnn {

  def generateList(config: Map[String, Any], inputConfig: cnn.Experiment.InputConfig,
                   layerSizeConfig: Layer.Experiment.Config.Dimensions): List[Int] = {
    val configEvaluated: Map[String, Int] =
      config.map {
        case (key: String, v: Double) => (key, v.toInt)
        case (key: String, v: String) => (key, {
          v match {
            case "n_batches" => inputConfig.nBatches
            case "n_inputs" => inputConfig.nInputs
            case "image_size" => inputConfig.inputSize
            case "input_channels" => inputConfig.nChannels
            case _ =>
              layerSizeConfig match {
                case c: conv.Experiment.Config.Dimensions =>
                  v match {
                    case "n_kernels" => c.nKernels
                    case "kernel_size" => c.kernelSize
                    case _ => throw new java.lang.IllegalArgumentException()
                  }
                case f: fc.Experiment.Config.Dimensions =>
                  v match {
                    case "n_neurons" => f.nNeurons
                    case _ => throw new java.lang.IllegalArgumentException()
                  }
              }
          }
        }) }
    generateList(configEvaluated)
  }

  def generateListsOfInts(jWorkload: Map[String, Any], blockName: String): List[List[Int]] = {
    val paramBlock = jWorkload(blockName).asInstanceOf[List[Map[String, Double]]]
    //            List(generateList(paramBlock.head), generateList(paramBlock(1))
    List(generateList(paramBlock.head))
  }
  
  def generateListsOfFuns(jOptParams: Map[String, Any], blockName: String): List[
    (cnn.Experiment.InputConfig, Layer.Experiment.Config.Dimensions) => List[Int]] = {
    val paramBlock = jOptParams(blockName).asInstanceOf[List[Map[String, Any]]]
    List(
      (inputConfig, convLayerSizeConfig) =>
        generateList(paramBlock.head, inputConfig, convLayerSizeConfig))
    //              (inputConfig, convLayerSizeConfig) =>
    //                generateList(paramBlock(1), inputConfig, convLayerSizeConfig))
  }

  def generateList(config: Map[String, Any]): List[Int] = {
    val _config: Map[String, Int] = config.map{
      case (k, v: Double) => (k, v.toInt)
      case (k, v: java.lang.Integer) => (k, v.toInt)
      case (k, v: Int) => (k, v)
    }
    val mainList: List[Int] = {
      if (_config.contains("step_multiply")) {
        (0 to Math.floor(
          Math.log(_config("end").toFloat / _config("start")) /
            Math.log(_config("step_multiply"))).toInt).toList.map(
          p => (_config("start") * Math.pow(_config("step_multiply"), p)).toInt)
      } else if (_config.contains("step_add")) {
        (0 to Math.floor((_config("end") - _config("start")).toFloat
          / _config("step_add")).toInt).toList.map(
          x => _config("start") + x * _config("step_add"))
      } else
        null
    }
    if (_config.contains("initial"))
      if (mainList != null)
        List(_config("initial").toInt) ++ mainList
      else
        List(_config("initial").toInt)
    else
      mainList
  }

  def getConfigFromJSON(jsonFilePath: String): ExperimentParams = {
    val logger = Logger(this.getClass)
    val source = scala.io.Source.fromFile(jsonFilePath)
    val jsonString = source.getLines.mkString("\n")
    source.close()
    val jsonMap: Option[Any] = JSON.parseFull(jsonString)
    
    jsonMap match {
      case None => 
        throw new java.lang.IllegalArgumentException()
      case Some(someJ) =>
        val j = someJ.asInstanceOf[Map[String, Any]]

        logger.info("Processing JSON config file \"" + j("name").asInstanceOf[String] + "\"\n" +
          "located in " + jsonFilePath)
        
        val jWorkload = j("workload").asInstanceOf[Map[String, Any]]
        val jOptParams = j("optimisational_parameters").asInstanceOf[Map[String, Map[String, Double]]]

        ExperimentParams(
          experimentName = j("name").asInstanceOf[String],
          kernelOutputSubfolder = j("kernel_output_subfolder").asInstanceOf[String],
          nBatchesRange = generateList(jWorkload("n_batches").asInstanceOf[Map[String, Double]]),
          nInputsRange = generateList(jWorkload("n_inputs").asInstanceOf[Map[String, Double]]),
          inputSizeRange = generateList(jWorkload("image_size").asInstanceOf[Map[String, Double]]),
          inputChannelRange = generateList(jWorkload("input_channels").asInstanceOf[Map[String, Double]]),

          nKernelsRange = generateListsOfInts(jWorkload, "n_kernels"),
          kernelSizeRange = generateListsOfInts(jWorkload, "kernel_size"),
          kernelStrideRange = generateListsOfInts(jWorkload, "kernel_stride"),

          neuronsRange = generateListsOfInts(jWorkload, "n_neurons"),


          inputTileSizeRange = generateListsOfFuns(jOptParams, "input_tile_size"),
          elsPerThreadRange = generateListsOfFuns(jOptParams, "els_per_thread"),
          kernelsPerGroupRange = generateListsOfFuns(jOptParams, "kernels_per_group"),
          vectorLenRange = generateListsOfFuns(jOptParams, "vector_len"),

          multsPerThreadRange = generateListsOfFuns(jOptParams, "mults_per_thread"),
          neuronsPerWrgRange = generateListsOfFuns(jOptParams, "neurons_per_wrg"))
    }
  }

  
  
  def getConfigFromProto(protoFilePath: String): Seq[ExperimentParams] = {
    
    val logger = Logger(this.getClass)
    logger.info("Processing PROTO config file \"" + protoFilePath + "\"")
     
    val config: nn.caffe.proto.Config = nn.caffe.proto.Config(protoFilePath)

    {
      config.version match {
        case Config.Version.V1 => config.layersWithSizesV1.get
        case Config.Version.NEW => config.layersWithSizesVNew.get}
    }.zipWithIndex.filter(layerAndNo => {
      layerAndNo._1._1 match {
        case layerV1: V1LayerParameter =>
          nn.caffe.proto.Config.getType(layerV1) == V1LayerParameter.LayerType.CONVOLUTION
        case layerVNew: LayerParameter =>
          nn.caffe.proto.Config.getType(layerVNew) == V1LayerParameter.LayerType.CONVOLUTION
      }}).map{
      case ((layer, inputSize), i) =>
        val nInputs: Int = {
          config.version match {
            case Config.Version.V1 => config.dataLayerV1.get.dataParam
            case Config.Version.NEW => config.dataLayerVNew.get.dataParam
          }}.get.batchSize.get
        
        val experimentName: String = config.version match {
          case Config.Version.V1 => layer.asInstanceOf[V1LayerParameter].name.get  
          case Config.Version.NEW => layer.asInstanceOf[LayerParameter].name.get
        }
        
        val paddedInputSize: Int = inputSize + 2 * {
          config.version match {
            case Config.Version.V1 => layer.asInstanceOf[V1LayerParameter].convolutionParam.get.pad.head
            case Config.Version.NEW => layer.asInstanceOf[LayerParameter].convolutionParam.get.pad.head
          }}

        val nKernels: Int = {
          config.version match {
            case Config.Version.V1 => layer.asInstanceOf[V1LayerParameter].convolutionParam.get.numOutput.get
            case Config.Version.NEW => layer.asInstanceOf[LayerParameter].convolutionParam.get.numOutput.get
          }}
        
        val kernelSize: Int = {
          config.version match {
            case Config.Version.V1 => layer.asInstanceOf[V1LayerParameter].convolutionParam.get.kernelSize.head
            case Config.Version.NEW => layer.asInstanceOf[LayerParameter].convolutionParam.get.kernelSize.head
          }}
        
        val kernelStride: Int = {
          def getStride(stride: Seq[Int]): Int = if (stride.nonEmpty) stride.head else 1
          config.version match {
            case Config.Version.V1 => getStride(layer.asInstanceOf[V1LayerParameter].convolutionParam.get.stride)
            case Config.Version.NEW => getStride(layer.asInstanceOf[LayerParameter].convolutionParam.get.stride)
          }}
        
        ExperimentParams(
          experimentName = experimentName,
          kernelOutputSubfolder = i.toString,
          nBatchesRange = List(1), // TODO: generalise
          nInputsRange = List(nInputs),
          inputSizeRange = List(paddedInputSize),
          inputChannelRange = List(3), // TODO: generalise

          nKernelsRange = List(List(nKernels)),
          kernelSizeRange = List(List(kernelSize)),
          kernelStrideRange = List(List(kernelStride)),

          neuronsRange = List(List(1)),


          inputTileSizeRange = List(
            (in: cnn.Experiment.InputConfig, c: conv.Experiment.Config.Dimensions) => 
              (c.kernelSize to in.inputSize by 1).toList),
          
          elsPerThreadRange = List(
            (in: cnn.Experiment.InputConfig, c: conv.Experiment.Config.Dimensions) =>
              (1 to (in.nChannels * c.kernelSize * c.kernelSize) by 1).toList),
          
          kernelsPerGroupRange = List(
            (in: cnn.Experiment.InputConfig, c: conv.Experiment.Config.Dimensions) =>
              (1 to c.nKernels by 1).toList),
          
          vectorLenRange = List(
            (_: cnn.Experiment.InputConfig, _: conv.Experiment.Config.Dimensions) =>
              List(1, 2, 4)),

          multsPerThreadRange = List(
            (_: cnn.Experiment.InputConfig, _: fc.Experiment.Config.Dimensions) =>
              List(1)),
          neuronsPerWrgRange = List(
            (_: cnn.Experiment.InputConfig, _: fc.Experiment.Config.Dimensions) =>
              List(1)))
    }
  }

  def configToString(iC: cnn.Experiment.InputConfig): String = {
    f"nBatches=${iC.nBatches}%d, nInputs=${iC.nInputs}%d, imageSize=${iC.inputSize}%d\n"
  }

  object Experiment {

    case class InputConfig(nBatches: Int,
                           nInputs: Int,
                           inputSize: Int,
                           nChannels: Int)

    val cnnDir: String = nn.nnDir + "/cnn"
    val pathToResults: String = System.getenv("LIFT_NN_RESOURCES") + f"/neural_net_outputs/"
    val pathToLiftResults = pathToResults + "/lift_results"

    
    def pathToInputs(iC: cnn.Experiment.InputConfig, cD: conv.Experiment.Config.Dimensions): String =
      System.getenv("LIFT_NN_RESOURCES") + f"/neural_net_inputs/input_lmdb_IN_${iC.nInputs}%d_IC_${iC.nChannels}%d_" +
        f"IS_${iC.inputSize}%d_" +
        f"KC_${cD.nKernels}%d_KSI_${cD.kernelSize}%d_KSTR_${cD.kernelStride}%d"

    
    def pathToParams(iC: cnn.Experiment.InputConfig, cD: conv.Experiment.Config.Dimensions): String =
      System.getenv("LIFT_NN_RESOURCES") + f"/neural_net_params/micro_IN_${iC.nInputs}%d_IC_${iC.nChannels}%d_" +
        f"IS_${iC.inputSize}%d_" +
        f"KC_${cD.nKernels}%d_KSI_${cD.kernelSize}%d_KSTR_${cD.kernelStride}%d"
    
    
    def pathToTargets(iC: cnn.Experiment.InputConfig, cD: conv.Experiment.Config.Dimensions): String = 
      pathToResults + f"/outputs_IN_${iC.nInputs}%d_IC_${iC.nChannels}%d_IS_${iC.inputSize}%d_" +
      f"KC_${cD.nKernels}%d_KSI_${cD.kernelSize}%d_KSTR_${cD.kernelStride}%d.binary"


    def inputsExist(iC: cnn.Experiment.InputConfig, cD: conv.Experiment.Config.Dimensions,
                    experimentName: String): Boolean =
      if (exists(get(pathToInputs(iC, cD) + "/test_images_n" + iC.nInputs + ".binary")))
        true
      else {
        System.out.println(
          f"No inputs provided for $experimentName%s (nInputs=${iC.nInputs}%d, imageSize=${iC.inputSize}%d, " +
            f"nChannels=${iC.nChannels}%d)")
        false
      }
    
    def targetsExist(iC: cnn.Experiment.InputConfig, cD: conv.Experiment.Config.Dimensions,
                    experimentName: String): Boolean =
      if (exists(get(Experiment.pathToTargets(iC, cD))))
        true
      else {
        System.out.println(
          f"No targets provided for $experimentName%s (nInputs=${iC.nInputs}%d, imageSize=${iC.inputSize}%d, " +
            f"nChannels=${iC.nChannels}%d\nnKernels=${cD.nKernels}%s, kernelSize=${cD.kernelSize}%s, " +
            f"kernelStride=${cD.kernelStride}%s)")
        false
      }
    
    
    def isFirstRun(iC: cnn.Experiment.InputConfig) = {
      if (!exists(get(pathToResults))) {
        createDirectory(get(pathToResults))
        true
      } else {
        new File(pathToResults).listFiles.toList.count {
          file => file.getName.endsWith(f"_n${iC.nInputs}%d.csv")
        } == 0 
      }
    } 
    

    def datasetsExist(pathToParams: String): Boolean = exists(get(pathToParams + "/wconv1.binary"))

    
    def verifyOutputs(netOutputs: Any, targetOutputs: Any, precision: Float):
    Option[(List[Int], Float, Float)] = {
      (netOutputs, targetOutputs) match {
        case (n: Array[Float], t: Array[Float]) =>
          for ((netOutput, targetOutput, i) <- (n, t, 0 to t.length).zipped.toList) {
            try {
              assertEquals("", targetOutput, netOutput, precision)
            }
            catch {
              case _: AssertionError =>
                return Some(List(i), targetOutput, netOutput)
            }
          }
          None
        case (n: Array[_], t: Array[_]) =>
          for ((netOutput, targetOutput, i) <- (n, t, t.indices).zipped.toList) {
            verifyOutputs(netOutput, targetOutput, precision) match {
              case Some((ix, unmatchedTarget, wrongResult)) =>
                return Some(List(i) ++ ix, unmatchedTarget, wrongResult)
              case None =>
            }
          }
          None
      }
    }
    
    def apply(benchmark: cnn.ExperimentParams,
              iC: cnn.Experiment.InputConfig,
              cD: conv.Experiment.Config.Dimensions,
              fD: fc.Experiment.Config.Dimensions): List[Experiment] = {
      for {
        convConfig <- benchmark.convConfig(iC, cD)
        fcConfig <- benchmark.fcConfig(iC, fD)
      }
        yield new Experiment(iC, convConfig, fcConfig,
          pathToInputs(iC, cD), pathToParams(iC, cD), pathToTargets(iC, cD))
    }
  }

  def saveKernelToFile(experimentNo: Int, testConfigFilename: String, layer: Layer, openclKernel: String, 
                       twoKernels: Boolean, localSize: Array[Int], globalSize: Array[Int], kernelPath: String): Unit = {
    val logger = Logger(this.getClass)
    
    /* Save the OpenCL code into text file */
    val kernelFile = new File(kernelPath)

    /* Make sure all directories in the path exist */
    kernelFile.getParentFile.mkdirs()
      
    // UTF8 to solve the OpenCL compilation error "source file is not valid UTF-8"
    val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(kernelFile, false),
      StandardCharsets.UTF_8))
    // Insert workgroup dimensions and optimisational parameters
    bw.write("//L0=" + localSize(0).toString + "\n")
    bw.write("//L1=" + localSize(1).toString + "\n")
    bw.write("//L2=" + localSize(2).toString + "\n")
    bw.write("//G0=" + globalSize(0).toString + "\n")
    bw.write("//G1=" + globalSize(1).toString + "\n")
    bw.write("//G2=" + globalSize(2).toString + "\n")
    layer match {
      case cL: Conv => {
        bw.write("//input_tile_size=" + cL.inputTiling.size + "\n")
        bw.write("//kernels_per_group=" + cL.kernelsPerGroup + "\n")
        bw.write("//els_per_thread=" + cL.elsPerThread + "\n")
        bw.write("//coalesce=" + cL.coalesce + "\n")
        bw.write("//unroll_reduce=" + cL.unrollReduce + "\n")
        bw.write("//experiment_no=" + experimentNo + "\n")
        bw.write("//test_config=" + testConfigFilename + "\n")
      }
    }
    bw.write("//Generated on " + 
      new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(Calendar.getInstance().getTime) + "\n")
    // Insert offset handling
    val openclKernelWithOffsets = {
      if (!twoKernels)
        openclKernel.replaceFirst(
          raw"void KERNEL\(const global float\* restrict v__(\d+), " +
            raw"const global float\* restrict v__(\d+), " +
            raw"const global float\* restrict v__(\d+), " +
            raw"global float\* v__(\d+)\)\{ \n" +
            raw"\#ifndef WORKGROUP_GUARD\n" +
            raw"\#define WORKGROUP_GUARD\n" +
            raw"\#endif\n" +
            raw"WORKGROUP_GUARD\n" +
            raw"\{",
          "void KERNEL(const global float* restrict v__$1, const global float* restrict v__$2, " +
            "const global float* restrict v__$3, global float* v__$4, int const offsetX, int const offsetOut){\n" +
            "#ifndef WORKGROUP_GUARD\n" +
            "#define WORKGROUP_GUARD\n" +
            "#endif\n" +
            "WORKGROUP_GUARD\n" +
            "{\n" +
            "  /* Apply offsets */\n" +
            "  v__$3 += offsetX;\n" +
            "  v__$4 += offsetOut;")
      else openclKernel.replaceFirst(
        raw"void KERNEL\(const global float\* restrict v__(\d+), " +
          raw"const global float\* restrict v__(\d+), " +
          raw"global float\* v__(\d+)\)\{ \n" +
          raw"\#ifndef WORKGROUP_GUARD\n" +
          raw"\#define WORKGROUP_GUARD\n" +
          raw"\#endif\n" +
          raw"WORKGROUP_GUARD\n" +
          raw"\{",
        "void KERNEL(const global float* restrict v__$1, const global float* restrict v__$2, " +
          "global float* v__$3, int const offsetX, int const offsetOut){\n" +
          "#ifndef WORKGROUP_GUARD\n" +
          "#define WORKGROUP_GUARD\n" +
          "#endif\n" +
          "WORKGROUP_GUARD\n" +
          "{\n" +
          "  /* Apply offsets */\n" +
          "  v__$2 += offsetX;\n" +
          "  v__$3 += offsetOut;")
    }

    bw.write(openclKernelWithOffsets)
    bw.close()
    logger.info(f"Saved the generated OpenCL kernel into $kernelPath%s")
  }


  case class Experiment(inputConfig: cnn.Experiment.InputConfig,
                        convConfig: List[conv.Experiment.Config],
                        fcConfig: List[fc.Experiment.Config],
                        pathToInputs: String,
                        pathToParams: String,
                        pathToTargets: String) {
    
    def loadData(aCNN: CNN, compileOnly: Boolean) = NetDatasetsCollection(
      pathToParams = pathToParams,
      nInputs = inputConfig.nInputs,
      layers = Array[NetDatasets](
        nn.conv.Experiment.loadDatasets(
          paramsPath = pathToParams,
          inputsPath = pathToInputs + "/test_images_n" + inputConfig.nInputs + ".binary",
          targetOutputsPath = pathToTargets,
          inputShape = aCNN.convLayers(0).inputShape,
          outputShape = aCNN.convLayers(0).outputShape,
          //                  targetFilePrefix = "test_caffe_results_n" + inputConfig.nInputs,
          paramFileInfix = "conv1",
          kernelSliding = aCNN.convLayers(0).kernelSliding,
          generateDummies = compileOnly)))
  }

  case class ExperimentParams(experimentName: String,
                              kernelOutputSubfolder: String,
                              nBatchesRange: List[Int],
                              nInputsRange: List[Int],
                              inputSizeRange: List[Int],
                              inputChannelRange: List[Int],

                              nKernelsRange: List[List[Int]],
                              kernelSizeRange: List[List[Int]],
                              kernelStrideRange: List[List[Int]],

                              neuronsRange: List[List[Int]],

                              inputTileSizeRange: List[
                              (cnn.Experiment.InputConfig, conv.Experiment.Config.Dimensions) => List[Int]],
                              elsPerThreadRange: List[
                              (cnn.Experiment.InputConfig, conv.Experiment.Config.Dimensions) => List[Int]],
                              kernelsPerGroupRange: List[
                              (cnn.Experiment.InputConfig, conv.Experiment.Config.Dimensions) => List[Int]],
                              vectorLenRange: List[
                              (cnn.Experiment.InputConfig, conv.Experiment.Config.Dimensions) => List[Int]],

                              multsPerThreadRange: List[
                              (cnn.Experiment.InputConfig, fc.Experiment.Config.Dimensions) => List[Int]],
                              neuronsPerWrgRange: List[
                              (cnn.Experiment.InputConfig, fc.Experiment.Config.Dimensions) => List[Int]]) {
    def inputConfigs: List[cnn.Experiment.InputConfig] = {
      for {
        nBatches <- nBatchesRange
        inputSize <- inputSizeRange
        inputChannels <- inputChannelRange
        nInputs <- nInputsRange}
      // Wrap input parameters into an object
        yield cnn.Experiment.InputConfig(
          nBatches = nBatches, nInputs = nInputs, inputSize = inputSize, nChannels = inputChannels)
    }
    
    def convDimensions: List[List[conv.Experiment.Config.Dimensions]] = {
      for {
        nKernelsL0 <- nKernelsRange.head
        //      _nKernelsL1 <- e.nKernelsRange(1)
        kernelStrideL0 <- kernelStrideRange.head
        kernelSizeL0 <- kernelSizeRange.head
      //      _kernelSizeL1 <- e.kernelSizeRange(1)
      }
        yield List(conv.Experiment.Config.Dimensions(nKernelsL0, kernelSizeL0, kernelStrideL0))
      //        conv.Experiment.Config.Dimensions(_nKernelsL1, _kernelSizeL1, /*TODO*/1)))
    }
    
    def convConfig(iC: cnn.Experiment.InputConfig, cD: conv.Experiment.Config.Dimensions): 
    List[List[nn.conv.Experiment.Config]] = {
      for {
        inputTileSize <- inputTileSizeRange.head(iC, cD)
        //      _inputTileSizeL1 <- e.inputTileSizeRange(1)(inputConfig, convDimensions(1))
        elsPerThread <- elsPerThreadRange.head(iC, cD)
        //      _elsPerThreadL1 <- e.elsPerThreadRange(1)(inputConfig, convDimensions(1))
        kernelsPerGroup <- kernelsPerGroupRange.head(iC, cD)
        //      _kernelsPerGroupL1 <- e.kernelsPerGroupRange(1)(inputConfig, convDimensions(1))
        vectorLen <- vectorLenRange.head(iC, cD)
      
        coalesce <- List(false, true)
      
        unrollReduce <- List(true) // TODO: explore again without unrolling
      }
      // Wrap conv parameters into an object
        yield List(
          conv.Experiment.Config(
            cD, conv.Experiment.Config.OptimisationalParams(
              inputTileSize = inputTileSize, elsPerThread = elsPerThread,
              kernelsPerGroup = kernelsPerGroup, vectorLen = vectorLen,
              coalesce = coalesce, unrollReduce = unrollReduce)))
    }

    def fcDimensions: List[List[fc.Experiment.Config.Dimensions]] = {
      for {
        nNeuronsL0 <- neuronsRange.head
      //      _nNeuronsL1 <- e.neuronsRange(1)
      }
        yield List(fc.Experiment.Config.Dimensions(nNeuronsL0))
      //        fc.Experiment.Config.Dimensions(_nNeuronsL1)))
    }

    def fcConfig(iC: cnn.Experiment.InputConfig, fD: fc.Experiment.Config.Dimensions):
    List[List[fc.Experiment.Config]] = {
      for {
        _multsPerThreadL0 <- multsPerThreadRange.head(iC, fD)
        //      _multsPerThreadL1 <- e.multsPerThreadRange(1)(inputConfig, fcDimensions(1))
        _neuronsPerWrgL0 <- neuronsPerWrgRange.head(iC, fD)
      //      _neuronsPerWrgL1 <- e.neuronsPerWrgRange(1)(inputConfig, fcDimensions(1))
      }
        yield List(
          fc.Experiment.Config(
            fD, fc.Experiment.Config.OptimisationalParams(
              multsPerThread = _multsPerThreadL0, neuronsPerWrg = _neuronsPerWrgL0)))
    }
  }
}
