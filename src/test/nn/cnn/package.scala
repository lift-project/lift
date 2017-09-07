package nn

import java.nio.file.Files.exists
import java.nio.file.Paths.get
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

  def getConfigFromJSON(jsonFilePath: String): ExperimentsSet = {
    val source = scala.io.Source.fromFile(jsonFilePath)
    val jsonString = source.getLines.mkString("\n")
    source.close()
    val jsonMap: Option[Any] = JSON.parseFull(jsonString)
    jsonMap match {
      case None => throw new java.lang.IllegalArgumentException()
      case Some(someJ) =>
        val j = someJ.asInstanceOf[Map[String, Any]]
        val jWorkload = j("workload").asInstanceOf[Map[String, Any]]
        val jOptParams = j("optimisational_parameters").asInstanceOf[Map[String, Map[String, Double]]]

        ExperimentsSet(
          nBatchesRange = generateList(jWorkload("n_batches").asInstanceOf[Map[String, Double]]),
          nInputsRange = generateList(jWorkload("n_inputs").asInstanceOf[Map[String, Double]]),
          imageSizeRange = generateList(jWorkload("image_size").asInstanceOf[Map[String, Double]]),
          nChannels = jWorkload("n_channels").asInstanceOf[Double].toInt,

          nKernelsRange = {
            val paramBlock = jWorkload("n_kernels").asInstanceOf[List[Map[String, Double]]]
            List(generateList(paramBlock.head), generateList(paramBlock(1))
            )},
          kernelSizeRange = {
            val paramBlock = jWorkload("kernel_size").asInstanceOf[List[Map[String, Double]]]
            List(generateList(paramBlock.head), generateList(paramBlock(1))
            )},

          neuronsRange = {
            val paramBlock = jWorkload("n_neurons").asInstanceOf[List[Map[String, Double]]]
            List(generateList(paramBlock.head), generateList(paramBlock(1))
            )},


          inputTileSizeRange = {
            val paramBlock = jOptParams("input_tile_size").asInstanceOf[List[Map[String, Any]]]
            List(
              (inputConfig, convLayerSizeConfig) =>
                generateList(paramBlock.head, inputConfig, convLayerSizeConfig),
              (inputConfig, convLayerSizeConfig) =>
                generateList(paramBlock(1), inputConfig, convLayerSizeConfig))
            },
          elsPerThreadRange = {
            val paramBlock = jOptParams("els_per_thread").asInstanceOf[List[Map[String, Any]]]
            List(
              (inputConfig, convLayerSizeConfig) =>
                generateList(paramBlock.head, inputConfig, convLayerSizeConfig),
              (inputConfig, convLayerSizeConfig) =>
                generateList(paramBlock(1), inputConfig, convLayerSizeConfig)
            )},
          kernelsPerGroupRange = {
            val paramBlock = jOptParams("kernels_per_group").asInstanceOf[List[Map[String, Any]]]
            List(
              (inputConfig, convLayerSizeConfig) =>
                generateList(paramBlock.head, inputConfig, convLayerSizeConfig),
              (inputConfig, convLayerSizeConfig) =>
                generateList(paramBlock(1), inputConfig, convLayerSizeConfig)
            )},

          multsPerThreadRange = {
            val paramBlock = jOptParams("mults_per_thread").asInstanceOf[List[Map[String, Any]]]
            List(
              (inputConfig, fcLayerSizeConfig) =>
                generateList(paramBlock.head, inputConfig, fcLayerSizeConfig),
              (inputConfig, fcLayerSizeConfig) =>
                generateList(paramBlock(1), inputConfig, fcLayerSizeConfig)
            )},
          neuronsPerWrgRange = {
            val paramBlock = jOptParams("neurons_per_wrg").asInstanceOf[List[Map[String, Any]]]
            List(
              (inputConfig, fcLayerSizeConfig) =>
                generateList(paramBlock.head, inputConfig, fcLayerSizeConfig),
              (inputConfig, fcLayerSizeConfig) =>
                generateList(paramBlock(1), inputConfig, fcLayerSizeConfig)
            )}
        )
    }
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

    def getPathToInputs(inputSize: Int): String = {
      {
        val envPath = System.getenv("LIFT_NN_RESOURCES")
        if (envPath != null) envPath else cnnDir
      } + f"/experiment.cnn.inputs.$inputSize%d"
    }
    def getPathToParams(nKernelsL1: Int, kernelSize: Int, imageSize: Int, nNeuronsL1: Int): String = {
      {
        val envPath = System.getenv("LIFT_NN_RESOURCES")
        if (envPath != null) envPath else cnnDir
      } + f"/experiment.cnn.$nKernelsL1%d.$kernelSize%d.$imageSize%d" + {
        if (nNeuronsL1 != 256)
          f".$nNeuronsL1%d"
        else
          ""
      }
    }
    def getPathToResults(pathToParams: String): String = pathToParams + "/results_lift/"


    def datasetsExist(pathToParams: String): Boolean = exists(get(pathToParams + "/wconv1.binary"))
  }


  case class Experiment(inputConfig: cnn.Experiment.InputConfig,
                        convConfig: List[conv.Experiment.Config],
                        fcConfig: List[fc.Experiment.Config])

  case class ExperimentsSet(nBatchesRange: List[Int],
                            nInputsRange: List[Int],
                            imageSizeRange: List[Int],
                            nChannels: Int,

                            nKernelsRange: List[List[Int]],
                            kernelSizeRange: List[List[Int]],

                            neuronsRange: List[List[Int]],

                            inputTileSizeRange: List[
                              (cnn.Experiment.InputConfig, conv.Experiment.Config.Dimensions) => List[Int]],
                            elsPerThreadRange: List[
                              (cnn.Experiment.InputConfig, conv.Experiment.Config.Dimensions) => List[Int]],
                            kernelsPerGroupRange: List[
                              (cnn.Experiment.InputConfig, conv.Experiment.Config.Dimensions) => List[Int]],

                            multsPerThreadRange: List[
                              (cnn.Experiment.InputConfig, fc.Experiment.Config.Dimensions) => List[Int]],
                            neuronsPerWrgRange: List[
                              (cnn.Experiment.InputConfig, fc.Experiment.Config.Dimensions) => List[Int]])

}
