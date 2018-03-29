//package nn
//
//import java.nio.file.Files.exists
//import java.nio.file.Paths.get
//
///**
//  * Created by nm on 08/02/17.
//  */
//
//package object poolScala {
//  /* Types and data structures */
//  case class Tile(kernels_per_group: Int = 0,
//                  els_per_thread: Int = 0,
//                  input_tile_size: Int = 0,
//                  input_tile_stride: Int = 0,
//                  n_input_tiles_per_dim: Int = 0,
//                  n_kwindows_per_tile_per_dim: Int = 0)
//
//
//  def configToString(nBatches: Int, nInputs: Int, inputShape: Array[Shape], nLayers: Int,
//                     inputTileSize: Array[Int], elsPerThread: Array[Int], kernelShape: Array[Shape],
//                     kernelStride: Array[Int]): String = {
//    f"nBatches=$nBatches%d, nInputs=$nInputs%d, imageShape=${inputShape(0).size}%d, " +
//      f"inputChannels=${inputShape(0).nChannels}%d, inputTileSize=${inputTileSize(0)}%d\n" +
//      { var str: String = ""
//        for (layerNo <- 0 until nLayers)
//          str +=
//            f"elsPerThread($layerNo%d)=${elsPerThread(layerNo)}%d, " +
//            f"kernelSize($layerNo%d)=${kernelShape(layerNo).size}%d, " +
//            f"kernelStride($layerNo%d)=${kernelStride(layerNo)}%d\n"
//        str}
//  }
//
//
//  object Experiment {
//    val cnnDir: String = nn.nnDir + "/cnn"
//
//
//    def getPathToInputs(kernelShape: Shape, imageShape: Shape): String = {
//      {
//        val envPath = System.getenv("LIFT_NN_RESOURCES")
//        if (envPath != null) envPath else cnnDir
//      } + f"/experiment.pool.${kernelShape.size}%d.${imageShape.size}%d"
//    }
//    def getPathToResults(pathToInputs: String): String = pathToInputs + "/results_lift/"
//
//
//    def datasetsExist(pathToInputs: String): Boolean = exists(get(pathToInputs + "/test_images_n8.json"))
//
//
//    def loadDatasets(nInputs: Int, pathToInputs: String):
//    (PaddedArray[Array5D[Float]], Array5D[Float]) = {
//      println(f"Loading datasets for nInputs = $nInputs%d and pathToInputs = " + pathToInputs)
//
//      val tfX = PaddedArray(nn.loadJSON5D(pathToInputs + "/test_images_n" + nInputs + ".json"))
//      val tfResult = nn.loadJSON5D(pathToInputs + "/test_tf_results_n" + nInputs + ".json")
//
//      (tfX, tfResult)
//    }
//  }
//
//}
