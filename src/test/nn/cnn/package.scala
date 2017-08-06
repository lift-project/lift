package nn

import java.nio.file.Files.exists
import java.nio.file.Paths.get

/**
  * Created by nm on 08/02/17.
  */

package object cnn {
  /* Types and data structures */
  case class Tile(kernels_per_group: Int = 0,
                  els_per_thread: Int = 0,
                  inputTileSize: Int = 0,
                  inputTileSlideStep: Int = 0,
                  nInputTilesPerDim: Int = 0,
                  n_windows_per_tile_per_dim: Int = 0)

  object Experiment {
    val cnnDir: String = nn.nnDir + "/cnn"


    def configToString(nBatches: Int, nInputs: Int, inputShape: Shape, nLayers: Int,
                       inputTileSize: Int, elsPerThread: Int, nKernels: Int,
                       kernelsPerGroup: Int, kernelShape: Shape, kernelStride: Int): String = {
      f"nBatches=$nBatches%d, nInputs=$nInputs%d, imageShape=${inputShape.size}%d, " +
        f"inputChannels=${inputShape.nChannels}%d, inputTileSize=${inputTileSize}%d\n" +
        { var str: String = ""
          for (layerNo <- 0 until nLayers)
            str +=
              f"elsPerThread($layerNo%d)=${elsPerThread(layerNo)}%d, " +
                f"nKernels($layerNo%d)=${nKernels(layerNo)}%d, " +
                f"kernelsPerGroup($layerNo%d)=${kernelsPerGroup(layerNo)}%d,\n" +
                f"kernelSize($layerNo%d)=${kernelShape(layerNo).s}%d, " +
                f"kernelStride($layerNo%d)=${kernelStride(layerNo)}%d\n"
          str}
    }


    def getPathToInputs(nKernelsL0: Int, kernelSize: Int, inputSize: Int): String = {
      {
        val envPath = System.getenv("LIFT_NN_RESOURCES")
        if (envPath != null) envPath else cnnDir
      } + f"/experiment.cnn.$nKernelsL0%d.$kernelSize%d.$inputSize%d"
    }
    def getPathToResults(pathToInputs: String): String = pathToInputs + "/results_lift/"


    def datasetsExist(pathToInputs: String): Boolean = exists(get(pathToInputs + "/wconv1.json"))


    def loadDatasets(nInputs: Int, pathToInputs: String):
    (PaddedArray[Array5D[Float]], Array5D[Float], Array2D[Float],
      Array5D[Float]) = {
      println(f"Loading datasets for nInputs = $nInputs%d and pathToInputs = " + pathToInputs)

      var tfWconv = Array(nn.loadJSON4D(pathToInputs + "/wconv1.json"))
      var tfBconv = Array(nn.loadJSON1D(pathToInputs + "/bconv1.json"))
      tfWconv = tfWconv :+ nn.loadJSON4D(pathToInputs + "/wconv2.json")
      tfBconv = tfBconv :+ nn.loadJSON1D(pathToInputs + "/bconv2.json")
//      var tfWmlp = Array(nn.loadJSON2D(pathToInputs + "/wmlp1.json"))
//      tfWmlp = tfWmlp :+ nn.loadJSON2D(pathToInputs + "/wout.json")
//      var tfBmlp = Array(nn.loadJSON1D(pathToInputs + "/bmlp1.json"))
//      tfBmlp = tfBmlp :+ nn.loadJSON1D(pathToInputs + "/bout.json")

      val tfX = PaddedArray(nn.loadJSON5D(pathToInputs + "/test_images_n" + nInputs + ".json"))
      //val tfResult = nn.loadJSON2D(pathToInputs + "/test_tf_results_n" + nInputs + ".json")
      val tfResult = nn.loadJSON5D(pathToInputs + "/test_tf_results_n" + nInputs + ".json")

      (tfX, tfWconv, tfBconv, tfResult)
    }
  }

}
