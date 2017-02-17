package nn

import java.nio.file.Files._
import java.nio.file.Paths._

/**
  * Created by nm on 08/02/17.
  * Things pertaining to TestMLP tests.
  */
package object mlp {
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


  object Experiment {
    def getPathToInputs(layerSize: Int): String = TestUtils.currentDir + f"/experiment.$layerSize%d"
    def getPathToResults(pathToInputs: String): String = pathToInputs + "/results_lift"

    def loadDatasets(layerSize: Int, nInputs: Int, pathToInputs: String):
    (Array[Array[Float]], Array[Array[Array[Float]]], Array[Array[Float]], Array[Array[Float]]) = {

      val hiddenLayers: Array[Int] = Array(layerSize, 32)
      if (!exists(get(pathToInputs + "/W1_n" + nInputs + ".json"))) {
        throw new java.io.FileNotFoundException/*(f"Experiment (layerSize=${hiddenLayers(0)}%d, nInputs=$nInputs%d)" +
          "resources not provided (JSON files with test images, NN weights and biases).")*/
      }

      //      println(f"Loading the data for experiment (multsPerThread=$multsPerThread%d, " +
      //        f"neuronsPerWrg=$neuronsPerWrg%d, " + f"layerSize=$layerSize%d, nInputs=$nInputs%d).")

      var tfW = Array(TestUtils.loadJSON2D(pathToInputs + "/W1_n" + nInputs + ".json"))
      var tfB = Array(TestUtils.loadJSON1D(pathToInputs + "/b1_n" + nInputs + ".json"))
      for (i <- Range(2, hiddenLayers.length + 1)) {
        tfW = tfW :+ TestUtils.loadJSON2D(pathToInputs + "/W" + i.toString + "_n" + nInputs + ".json")
        tfB = tfB :+ TestUtils.loadJSON1D(pathToInputs + "/b" + i.toString + "_n" + nInputs + ".json")
      }
      tfW = tfW :+ TestUtils.loadJSON2D(pathToInputs + "/Wout_n" + nInputs + ".json")
      tfB = tfB :+ TestUtils.loadJSON1D(pathToInputs + "/bout_n" + nInputs + ".json")
      val tfX = TestUtils.loadJSON2D(pathToInputs + "/test_images_n" + nInputs + ".json")
      val tfResult = TestUtils.loadJSON2D(pathToInputs + "/test_tf_results_n" + nInputs + ".json")

      (tfX, tfW, tfB, tfResult)
    }
  }

  class Experiment(val multsPerThread: Int = 0,
                   val neuronsPerWrg: Int = 0,
                   val layerSize: Int = 0,
                   val nInputs: Int = 0,
                   val tfX: Array[Array[Float]],
                   val tfW: Array[Array[Array[Float]]],
                   val tfB: Array[Array[Float]],
                   val tfResult: Array[Array[Float]]) {
    val pathToInputs: String = Experiment.getPathToInputs(layerSize)
    val pathToResults: String = Experiment.getPathToResults(pathToInputs)
    var isAFirstRun: Boolean = false
    var resultsDir: java.io.File = _
  }
}
