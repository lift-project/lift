package nn

import java.nio.file.Files.exists
import java.nio.file.Paths.get

/**
  * Created by nm on 08/02/17.
  */

package object cnn {

  def configToString(nBatches: Int, nInputs: Int, nLayers: Int): String = {
    f"nBatches=$nBatches%d, nInputs=$nInputs%d, nLayers=$nLayers%d\n"
  }

  object Experiment {
    val cnnDir: String = nn.nnDir + "/cnn"

    def getPathToInputs(nKernelsL0: Int, kernelSize: Int, inputSize: Int): String = {
      {
        val envPath = System.getenv("LIFT_NN_RESOURCES")
        if (envPath != null) envPath else cnnDir
      } + f"/experiment.cnn.$nKernelsL0%d.$kernelSize%d.$inputSize%d"
    }
    def getPathToResults(pathToInputs: String): String = pathToInputs + "/results_lift/"


    def datasetsExist(pathToInputs: String): Boolean = exists(get(pathToInputs + "/wconv1.json"))
  }

}
