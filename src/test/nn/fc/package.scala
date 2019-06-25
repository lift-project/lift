package nn

import patterns.nn._

/**
  * Created by nm on 08/02/17.
  * Things pertaining to TestMLP tests.
  */
package object fc {
  /* Types and data structures */

  case class Tile(seqEls: Int, /*parEls: Int, */ inputsInGroup: Int, neurons: Int)

  class FCDatasets(in: PaddedArray[Array2D[Float]] = PaddedArray(Array.empty),
                   out: PaddedArray[Array2D[Float]] = PaddedArray(Array.empty),
                   targ: Array2D[Float] = Array.empty,
                   w: PaddedArray[Array2D[Float]] = PaddedArray(Array.empty),
                   b: PaddedArray[Array[Float]] = PaddedArray(Array.empty)) extends NetDatasets {
    val inputs: PaddedArray[Array2D[Float]] = in
    val outputs: PaddedArray[Array2D[Float]] = out
    val targets: Array2D[Float] = targ
    val weights: PaddedArray[Array2D[Float]] = w
    val biases: PaddedArray[Array[Float]] = b
  }



  /* Test values */
  val input_W1 = Array(Array(0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f),
    Array(0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.0f),
    Array(0.2f, 0.3f, 0.4f, 0.5f, 0.0f, 0.1f),
    Array(0.3f, 0.4f, 0.5f, 0.0f, 0.1f, 0.2f))

  val input_b1 = Array(0.1f, 0.1f, 0.1f, 0.1f)
  val input_W2 = Array(Array(0.0f, 0.1f, 0.2f, 0.3f),
    Array(0.1f, 0.2f, 0.3f, 0.0f),
    Array(0.2f, 0.3f, 0.0f, 0.1f),
    Array(0.3f, 0.0f, 0.1f, 0.2f),
    Array(0.0f, 0.1f, 0.2f, 0.3f),
    Array(0.1f, 0.2f, 0.3f, 0.0f),
    Array(0.2f, 0.3f, 0.0f, 0.1f),
    Array(0.3f, 0.0f, 0.1f, 0.2f))
  val input_b2 = Array(0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f)
  val input_Wout = Array(Array(0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f),
    Array(0.1f, 0.2f, 0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f),
    Array(0.2f, 0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f),
    Array(0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.0f, 0.1f, 0.2f),
    Array(0.4f, 0.5f, 0.6f, 0.7f, 0.0f, 0.1f, 0.2f, 0.3f),
    Array(0.5f, 0.6f, 0.7f, 0.0f, 0.1f, 0.2f, 0.3f, 0.4f))
  val input_bout = Array(0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f)
  val input_X = Array(6f, 7f, 8f, 9f, 5f, 4f)
  val input_X2 = Array(3f, 4f, 5f, 0f, 1f, 2f)
  val gold = Array(17.492f, 11.356f, 14.406f, 17.636f, 17.492f, 17.732f)
  val gold2 = Array(6.884f, 4.486f, 5.784f, 7.244f, 6.884f, 6.86f)

  def configToString(layerSize: Int, multsPerThread: Int, neuronsPerWrg: Int): String = {
    f"layerSize=$layerSize%d, multsPerThread=$multsPerThread%d, neuronsPerWrg=$neuronsPerWrg%d\n"
  }


  object Experiment extends nn.Experiment {
    object Config {
      case class Dimensions(nNeurons: Int) extends Layer.Experiment.Config.Dimensions

      case class OptimisationalParams(multsPerThread: Int,
                                      neuronsPerWrg: Int)

    }

    case class Config(dim: fc.Experiment.Config.Dimensions,
                      optParams: fc.Experiment.Config.OptimisationalParams)

    val mlpDir: String = nn.nnDir + "/mlp"

    def getPathToInputs(layerSize: Int): String = mlpDir + f"/experiment.$layerSize%d"

    def getPathToResults(pathToInputs: String): String = pathToInputs + "/results_lift"

    def loadDatasets(paramsPath: String,
                     inputsPath: String = "", inputShape: Shape, targetFilePrefix: String = "",
                     paramFileInfix: String, neuronShape: Shape): FCDatasets = {
      new FCDatasets(
        in = {
          if (inputsPath != "")
            PaddedArray(nn.loadBinary(inputsPath, (inputShape.nBatches * inputShape.nInputs, inputShape.size)))
          else
            PaddedArray(Array.empty)
        },
        targ = {
          if (targetFilePrefix != "")
            if (new java.io.File(paramsPath + "/" + targetFilePrefix + ".binary").exists)
              nn.loadBinary(paramsPath + "/" + targetFilePrefix + ".binary",
                (inputShape.nBatches * inputShape.nInputs, neuronShape.size))
            else Array.empty
          else
            Array.empty
        },
        w = PaddedArray(nn.loadBinary(paramsPath + "/w" + paramFileInfix + ".binary",
          (inputShape.size, neuronShape.size)).transpose),
        b = PaddedArray(nn.loadBinary(paramsPath + "/b" + paramFileInfix + ".binary")))
    }
  }

  /*class Experiment(val multsPerThread: Int = 0,
                   val neuronsPerWrg: Int = 0,
                   val layerSize: Int = 0,
                   val nInputs: Int = 0,
                   val tfX: Array2D[Float],
                   val tfW: Array3D[Float],
                   val tfB: Array2D[Float],
                   val tfResult: Array2D[Float]) {
    val pathToInputs: String = FCExperiment.getPathToInputs(layerSize)
    val pathToResults: String = FCExperiment.getPathToResults(pathToInputs)
    var isAFirstRun: Boolean = false
    var resultsDir: java.io.File = _
  }*/
}
