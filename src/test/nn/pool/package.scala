package nn

import java.nio.file.Files.exists
import java.nio.file.Paths.get

/**
  * Created by nm on 08/02/17.
  */

package object pool {
  /* Types and data structures */
  case class Tile(kernels_per_group: Int = 0,
                  els_per_thread: Int = 0,
                  input_tile_size: Int = 0,
                  input_tile_stride: Int = 0,
                  n_input_tiles_per_dim: Int = 0,
                  n_kwindows_per_tile_per_dim: Int = 0)


  def poolToString(aPool: Pool): String = {
    f"nBatches=${aPool.nBatches}%d, nInputs=${aPool.nInputs}%d, imageShape=${aPool.inputShape(0).s}%d\n" +
      { var str: String = ""
        for (layerNo <- 0 until aPool.nLayers)
          str +=
            f"inputTileSize($layerNo%d)=${aPool.inputTileSize(layerNo)}%d, " +
            f"elsPerThread($layerNo%d)=${aPool.elsPerThread(layerNo)}%d, " +
            f"kernelStride($layerNo%d)=${aPool.kernelStride(layerNo)}%d, " +
            f"kernelSize($layerNo%d)=${aPool.kernelShape(layerNo).s}%d, " +
        str + "\n"}
  }


  object Experiment {
    val cnnDir: String = nn.nnDir + "/cnn"


    def getPathToInputs(kernelShape: Shape, imageShape: Shape): String = {
      {
        val envPath = System.getenv("LIFT_NN_RESOURCES")
        if (envPath != null) envPath else cnnDir
      } + f"/experiment.pool.${kernelShape.w}%d.${imageShape.h}%d"
    }
    def getPathToResults(pathToInputs: String): String = pathToInputs + "/results_lift/"


    def datasetsExist(pathToInputs: String): Boolean = exists(get(pathToInputs + "/test_images_n.json"))


    def loadDatasets(nInputs: Int, pathToInputs: String):
    (PaddedArray[Array5D[Float]], Array5D[Float]) = {
      println(f"Loading datasets for nInputs = $nInputs%d and pathToInputs = " + pathToInputs)

      val tfX = PaddedArray(nn.loadJSON5D(pathToInputs + "/test_images_n" + nInputs + ".json"))
      val tfResult = nn.loadJSON5D(pathToInputs + "/test_tf_results_n" + nInputs + ".json")

      (tfX, tfResult)
    }
  }

}
