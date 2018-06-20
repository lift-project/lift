package nn

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files.exists
import java.nio.file.Paths.get
import java.util.Calendar

import _root_.caffe.caffe.{NetParameter, V1LayerParameter}
import com.typesafe.scalalogging.Logger
import nn.conv.Conv
import org.junit.Assert.assertEquals

import scala.collection.mutable.ListBuffer
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
            case "image_size" => inputConfig.imageSize
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

  def getConfigFromJSON(jsonFilePath: String): ExperimentalParams = {
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

        ExperimentalParams(
          kernelOutputSubfolder = j("kernel_output_subfolder").asInstanceOf[String],
          nBatchesRange = generateList(jWorkload("n_batches").asInstanceOf[Map[String, Double]]),
          nInputsRange = generateList(jWorkload("n_inputs").asInstanceOf[Map[String, Double]]),
          imageSizeRange = generateList(jWorkload("image_size").asInstanceOf[Map[String, Double]]),
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

  
  
  def getConfigFromProto(protoFilePath: String): List[ExperimentalParams] = {
    
    val logger = Logger(this.getClass)
    logger.info("Processing PROTO config file \"" + protoFilePath + "\"")
    
    var configs = new ListBuffer[String]()
    
    val netparam: NetParameter = nn.caffe.proto.config.load(protoFilePath)
    
//    if (netparam.layers.nonEmpty)
    
    def fillMissingParametersV1(layers: Seq[V1LayerParameter]): Seq[(V1LayerParameter, Int)] = {

      val dataLayer = layers.find(layer => layer.`type`.get == V1LayerParameter.LayerType.DATA &&
        (layer.include.isEmpty || layer.include.exists(_.phase match {
          case Some(_root_.caffe.caffe.Phase.TEST) => true
          case None => true
          case _ => false
        }))
      ) match {
        case Some(l) => l
        case None => throw new java.util.NoSuchElementException("Cannot find a data layer in Caffe proto-file")
      }

      val processedLayers = scala.collection.mutable.Map[V1LayerParameter, Int]()

      processedLayers += ((dataLayer, dataLayer.transformParam.get.cropSize.get))
      for (layer <- layers) {
        if (!processedLayers.contains(layer)) {

          def computeImageSize(currentLayer: V1LayerParameter): Int = {
            if (processedLayers.contains(layer))
              processedLayers(layer)
            else {

              def getStride(stride: Seq[Int]): Int = if (stride.nonEmpty) stride.head else 1

              val parent = netparam.layers.find(layer => currentLayer.bottom.contains(layer.name.get)).get
              val currentLayerSize: Int = parent.`type`.get match {
                case V1LayerParameter.LayerType.RELU => computeImageSize(parent)
                case V1LayerParameter.LayerType.LRN => computeImageSize(parent)
                case V1LayerParameter.LayerType.DATA => parent.transformParam.get.cropSize.get
                case V1LayerParameter.LayerType.CONVOLUTION =>
                  (computeImageSize(parent) -
                    (parent.convolutionParam.get.kernelSize.head - getStride(parent.convolutionParam.get.stride)) +
                    parent.convolutionParam.get.pad.head * 2) / getStride(parent.convolutionParam.get.stride)
                case V1LayerParameter.LayerType.POOLING =>
                  (computeImageSize(parent) -
                    (parent.poolingParam.get.kernelSize.head - parent.poolingParam.get.stride.get) +
                    parent.poolingParam.get.pad.getOrElse(0) * 2) / parent.poolingParam.get.stride.get
                case V1LayerParameter.LayerType.INNER_PRODUCT =>
                  parent.innerProductParam.get.numOutput.get
                case V1LayerParameter.LayerType.CONCAT => computeImageSize(parent)
                case V1LayerParameter.LayerType.DROPOUT => computeImageSize(parent)
                case V1LayerParameter.LayerType.SOFTMAX => computeImageSize(parent)
                case V1LayerParameter.LayerType.SOFTMAX_LOSS => computeImageSize(parent)
                case V1LayerParameter.LayerType.ACCURACY => parent.accuracyParam.get.topK.get
                case V1LayerParameter.LayerType.ELTWISE => computeImageSize(parent)
                case _ =>
                  parent.`type`.get.name match {
                    case "BatchNorm" =>
                      // I hope this works. Caffe does not seem to have a proper layer type for BatchNorm
                      computeImageSize(parent)
                    case "Scale" =>
                      // I hope this works. Caffe does not seem to have a proper layer type for Scale
                      computeImageSize(parent)

                    case _ =>
                      throw new java.lang.IllegalArgumentException("Cannot recognize a layer from the proto file (" +
                        parent.`type`.get.name + ")")
                  }
              }
              processedLayers += ((layer, currentLayerSize))

              currentLayerSize
            }
          }

          computeImageSize(layer)
        }
      }
      layers.map(layer => (layer, processedLayers(layer)))
      
  

      //    netparam.layers.zipWithIndex.filter(
      //      _._1 == true
      //    ).map{
      //      case (layer: V1LayerParameter, i: Int) =>
      //        ExperimentalParams(
      //          kernelOutputSubfolder = i.toString(),
      //          nBatchesRange = List(1), // TODO: generalise
      //          nInputsRange = List(dataLayer.dataParam.get.batchSize.get),
      //          imageSizeRange = List(dataLayer.transformParam.get.cropSize.get),
      //          inputChannelRange = List(3), // TODO: generalise
      //
      //          nKernelsRange = List(),
      //          kernelSizeRange = generateListsOfInts(jWorkload, "kernel_size"),
      //          kernelStrideRange = generateListsOfInts(jWorkload, "kernel_stride"),
      //
      //          neuronsRange = generateListsOfInts(jWorkload, "n_neurons"),
      //
      //
      //          inputTileSizeRange = generateListsOfFuns(jOptParams, "input_tile_size"),
      //          elsPerThreadRange = generateListsOfFuns(jOptParams, "els_per_thread"),
      //          kernelsPerGroupRange = generateListsOfFuns(jOptParams, "kernels_per_group"),
      //          vectorLenRange = generateListsOfFuns(jOptParams, "vector_len"),
      //
      //          multsPerThreadRange = generateListsOfFuns(jOptParams, "mults_per_thread"),
      //          neuronsPerWrgRange = generateListsOfFuns(jOptParams, "neurons_per_wrg"))}
    }
    
    val layersWithSizes = fillMissingParametersV1(netparam.layers)
    
    return null
  }

  def configToString(nBatches: Int, nInputs: Int, imageSize: Int, nLayers: Int): String = {
    f"nBatches=$nBatches%d, nInputs=$nInputs%d, imageSize=$imageSize%d, nLayers=$nLayers%d\n"
  }

  object Experiment {

    case class InputConfig(nBatches: Int,
                           nInputs: Int,
                           imageSize: Int,
                           nChannels: Int)

    val cnnDir: String = nn.nnDir + "/cnn"

    def getPathToInputs(nInputs: Int, inputSize: Int, inputChannels: Int, nKernels: Int, kernelSize: Int,
                        kernelStride: Int): String = {
//      {
//        val envPath = System.getenv("LIFT_NN_RESOURCES")
//        if (envPath != null) envPath else cnnDir
//      } + f"/experiment.cnn.inputs.$inputSize%d"
//      f"/home/s1569687/microbenchmark/neural_net_inputs/input_lmdb_IN_$nInputs%d_IC_$inputChannels%d_IS_$inputSize%d_" +
//        f"KC_$nKernels%d_KSI_$kernelSize%d_KSTR_$kernelStride%d"
      System.getenv("LIFT_NN_RESOURCES") + 
        f"/neural_net_inputs/input_lmdb_IN_$nInputs%d_IC_$inputChannels%d_IS_$inputSize%d_" +
        f"KC_$nKernels%d_KSI_$kernelSize%d_KSTR_$kernelStride%d"
    }
    def getPathToParams(nKernels: Int, kernelSize: Int, kernelStride: Int, nInputs: Int, inputSize: Int,
                        inputChannels: Int, nNeurons: Int): String = {
//      {
//        val envPath = System.getenv("LIFT_NN_RESOURCES")
//        if (envPath != null) envPath else cnnDir
//      } + f"/experiment.cnn.$nKernelsL1%d.$kernelSize%d.$inputSize%d" + {
//        if (nNeuronsL1 != 256)
//          f".$nNeuronsL1%d"
//        else
//          ""
//      }
      System.getenv("LIFT_NN_RESOURCES") + 
        f"/neural_net_params/micro_IN_$nInputs%d_IC_$inputChannels%d_IS_$inputSize%d_" +
        f"KC_$nKernels%d_KSI_$kernelSize%d_KSTR_$kernelStride%d"
    }
    def getPathToResults(pathToParams: String): String =
      System.getenv("LIFT_NN_RESOURCES") + f"/neural_net_outputs/"


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
                        fcConfig: List[fc.Experiment.Config])

  case class ExperimentalParams(kernelOutputSubfolder: String,
                                nBatchesRange: List[Int],
                                nInputsRange: List[Int],
                                imageSizeRange: List[Int],
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
                              (cnn.Experiment.InputConfig, fc.Experiment.Config.Dimensions) => List[Int]])

}
