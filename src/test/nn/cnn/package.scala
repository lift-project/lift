package nn

import java.nio.file.Files.exists
import java.nio.file.Paths.get
import scala.util.parsing.json.JSON

/**
  * Created by nm on 08/02/17.
  */

package object cnn {

  def getConfigFromJSON(jsonFilePath: String): ExperimentsSet = {
    val source = scala.io.Source.fromFile(jsonFilePath)
    val jsonString = source.getLines.mkString("\n")
    source.close()
    val jsonMap:Option[Any] = JSON.parseFull(jsonString)
    jsonMap match {
      case None => throw new java.lang.IllegalArgumentException()
      case Some(someJ) => {
        def generateList(config: Map[String, Int]): List[Int] =
          generateListGeneric(config, config("start"), config("end"))

        def generateListGeneric(config: Map[String, Int], start: Int, end: Int): List[Int] = {
          List({
            if (config.contains("initial")) config("initial")
          }) ++ List({
            if (config.contains("step_multiply")) {
              List(0 until Math.floor(
                Math.log(end.toFloat / start) /
                  Math.log(config("step_multiply"))).toInt).asInstanceOf[List[Int]].map(
                p => start * Math.pow(config("step_multiply"), p))
            }
            else {
              List(0 until Math.floor((end - start).toFloat
                / config("step_add")).toInt).asInstanceOf[List[Int]].map(
                x => start + x)
            }
          })}.asInstanceOf[List[Int]]

        val j = someJ.asInstanceOf[Map[String, Any]]
        val jWorkload = j("workload").asInstanceOf[Map[String, Any]]
        val jOptParams = j("optimisational_parameters").asInstanceOf[Map[String, Map[String, Int]]]

        ExperimentsSet(
          nBatchesRange = generateList(jWorkload("n_batches").asInstanceOf[Map[String, Int]]),
          nInputsRange = generateList(jWorkload("n_images").asInstanceOf[Map[String, Int]]),
          imageSizeRange = generateList(jWorkload("image_size").asInstanceOf[Map[String, Int]]),
          nChannels = jWorkload("n_channels").asInstanceOf[Int],

          nKernelsL0 = 16,
          nKernelsL1Range = generateList(jWorkload("n_kernels").asInstanceOf[Map[String, Int]]),
          kernelSizeRange = generateList(jWorkload("kernel_size").asInstanceOf[Map[String, Int]]),

          neuronsL1Range = generateList(jWorkload("n_neurons").asInstanceOf[Map[String, Int]]),

          kernelsPerGroupL0 = 4,
          inputTileSizeRange = (kernelSize: Int, imageSize: Int) =>
            generateListGeneric(jOptParams("input_tile_size"),
              start = kernelSize,
              end = imageSize),
          elsPerThreadL1Range = kernelSize =>
            generateListGeneric(jOptParams("els_per_thread"),
              start = 2,
              end = kernelSize),
          kernelsPerGroupL1Range = nKernelsL1 =>
            generateListGeneric(jOptParams("kernels_per_group"),
              start = 2,
              end = nKernelsL1),

          multsPerThreadRange = _ => List(1),
          neuronsPerWrgRange = _ => List(1))
        }
    }
    null.asInstanceOf[ExperimentsSet]
  }

  def configToString(nBatches: Int, nInputs: Int, imageSize: Int, nLayers: Int): String = {
    f"nBatches=$nBatches%d, nInputs=$nInputs%d, imageSize=$imageSize%d, nLayers=$nLayers%d\n"
  }

  object ExperimentsSet {
    val cnnDir: String = nn.nnDir + "/cnn"

    def getPathToInputs(inputSize: Int): String = {
      {
        val envPath = System.getenv("LIFT_NN_RESOURCES")
        if (envPath != null) envPath else cnnDir
      } + f"/experiment.cnn.inputs.$inputSize%d"
    }
    def getPathToParams(nKernelsL0: Int, kernelSize: Int, inputSize: Int, neuronsL1: Int): String = {
      {
        val envPath = System.getenv("LIFT_NN_RESOURCES")
        if (envPath != null) envPath else cnnDir
      } + f"/experiment.cnn.$nKernelsL0%d.$kernelSize%d.$inputSize%d" + {
        if (neuronsL1 != 256)
          f".$neuronsL1%d"
        else
          ""
      }
    }
    def getPathToResults(pathToParams: String): String = pathToParams + "/results_lift/"


    def datasetsExist(pathToParams: String): Boolean = exists(get(pathToParams + "/wconv1.binary"))
  }

  case class Experiment(nBatches: Int,
                        nKernelsL1: Int,
                        kernelSize: Int,
                        inputTileSize: Int,
                        elsPerThreadL1: Int,
                        kernelsPerGroupL1: Int,
                        multsPerThread: Int,
                        neuronsPerWrg: Int,
                        imageSize: Int,
                        neuronsL1: Int,
                        nInputs: Int)

  case class ExperimentsSet(nBatchesRange: List[Int],
                            nInputsRange: List[Int],
                            imageSizeRange: List[Int],
                            nChannels: Int,

                            nKernelsL0: Int,
                            nKernelsL1Range: List[Int],
                            kernelSizeRange: List[Int],

                            neuronsL1Range: List[Int],

                            kernelsPerGroupL0: Int,
                            inputTileSizeRange: (Int, Int) => List[Int],
                            elsPerThreadL1Range: Int => List[Int],
                            kernelsPerGroupL1Range: Int => List[Int],

                            multsPerThreadRange: Int => List[Int],
                            neuronsPerWrgRange: Int => List[Int])

}
