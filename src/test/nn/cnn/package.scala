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

  /* Test values */
  val input_X = Array(
    Array(Array(0.0f, 0.0f),   Array(1.0f, 1.0f),   Array(2.0f, 2.0f),   Array(3.0f, 3.0f),
          Array(4.0f, 4.0f),   Array(5.0f, 5.0f),   Array(6.0f, 6.0f),  Array(7.0f, 7.0f)),
    Array(Array(8.0f, 8.0f),   Array(9.0f, 9.0f),   Array(10.0f, 10.0f), Array(11.0f, 11.0f),
          Array(12.0f, 12.0f), Array(13.0f, 13.0f), Array(14.0f, 14.0f), Array(15.0f, 15.0f)),
    Array(Array(16.0f, 16.0f), Array(17.0f, 17.0f), Array(18.0f, 18.0f), Array(19.0f, 19.0f),
          Array(20.0f, 20.0f), Array(21.0f, 21.0f), Array(22.0f, 22.0f), Array(23.0f, 23.0f)),
    Array(Array(24.0f, 24.0f), Array(25.0f, 25.0f), Array(26.0f, 26.0f), Array(27.0f, 27.0f),
          Array(28.0f, 28.0f), Array(29.0f, 29.0f), Array(30.0f, 30.0f), Array(31.0f, 31.0f)),
    Array(Array(32.0f, 32.0f), Array(33.0f, 33.0f), Array(34.0f, 34.0f), Array(35.0f, 35.0f),
          Array(36.0f, 36.0f), Array(37.0f, 37.0f), Array(38.0f, 38.0f), Array(39.0f, 39.0f)),
    Array(Array(40.0f, 40.0f), Array(41.0f, 41.0f), Array(42.0f, 42.0f), Array(43.0f, 43.0f),
          Array(44.0f, 44.0f), Array(45.0f, 45.0f), Array(46.0f, 46.0f), Array(47.0f, 47.0f)),
    Array(Array(48.0f, 48.0f), Array(49.0f, 49.0f), Array(50.0f, 50.0f), Array(51.0f, 51.0f),
          Array(52.0f, 52.0f), Array(53.0f, 53.0f), Array(54.0f, 54.0f), Array(55.0f, 55.0f)),
    Array(Array(56.0f, 56.0f), Array(57.0f, 57.0f), Array(58.0f, 58.0f), Array(59.0f, 59.0f),
          Array(60.0f, 60.0f), Array(61.0f, 61.0f), Array(62.0f, 62.0f), Array(63.0f, 63.0f)))

  val input_b = Array(0.0f, 0.0f, 0.0f)

  val input_K = Array(Array(Array(Array(1.0f, 0.0f, 1.0f), Array(0.0f, 1.0f, 0.0f)),
                            Array(Array(3.0f, 0.0f, 3.0f), Array(0.0f, 3.0f, 0.0f)),
                            Array(Array(5.0f, 0.0f, 5.0f), Array(0.0f, 5.0f, 0.0f))),
                      Array(Array(Array(7.0f, 0.0f, 7.0f), Array(0.0f, 7.0f, 0.0f)),
                            Array(Array(9.0f, 0.0f, 9.0f), Array(0.0f, 9.0f, 0.0f)),
                            Array(Array(11.0f, 0.0f, 11.0f), Array(0.0f, 11.0f, 0.0f))))

  val gold =
    Array(Array(Array(260.0f, 260.0f, 260.0f),    Array(296.0f, 296.0f, 296.0f),    Array(332.0f, 332.0f, 332.0f),
                Array(368.0f, 368.0f, 368.0f),    Array(404.0f, 404.0f, 404.0f),    Array(440.0f, 440.0f, 440.0f)),
          Array(Array(548.0f, 548.0f, 548.0f),    Array(584.0f, 584.0f, 584.0f),    Array(620.0f, 620.0f, 620.0f),
                Array(656.0f, 656.0f, 656.0f),    Array(692.0f, 692.0f, 692.0f),    Array(728.0f, 728.0f, 728.0f)),
          Array(Array(836.0f, 836.0f, 836.0f),    Array(872.0f, 872.0f, 872.0f),    Array(908.0f, 908.0f, 908.0f),
                Array(944.0f, 944.0f, 944.0f),    Array(980.0f, 980.0f, 980.0f),    Array(1016.0f, 1016.0f, 1016.0f)),
          Array(Array(1124.0f, 1124.0f, 1124.0f), Array(1160.0f, 1160.0f, 1160.0f), Array(1196.0f, 1196.0f, 1196.0f),
                Array(1232.0f, 1232.0f, 1232.0f), Array(1268.0f, 1268.0f, 1268.0f), Array(1304.0f, 1304.0f, 1304.0f)),
          Array(Array(1412.0f, 1412.0f, 1412.0f), Array(1448.0f, 1448.0f, 1448.0f), Array(1484.0f, 1484.0f, 1484.0f),
                Array(1520.0f, 1520.0f, 1520.0f), Array(1556.0f, 1556.0f, 1556.0f), Array(1592.0f, 1592.0f, 1592.0f)),
          Array(Array(1700.0f, 1700.0f, 1700.0f), Array(1736.0f, 1736.0f, 1736.0f), Array(1772.0f, 1772.0f, 1772.0f),
                Array(1808.0f, 1808.0f, 1808.0f), Array(1844.0f, 1844.0f, 1844.0f), Array(1880.0f, 1880.0f, 1880.0f)),
          Array(Array(1988.0f, 1988.0f, 1988.0f), Array(2024.0f, 2024.0f, 2024.0f), Array(2060.0f, 2060.0f, 2060.0f),
                Array(2096.0f, 2096.0f, 2096.0f), Array(2132.0f, 2132.0f, 2132.0f), Array(2168.0f, 2168.0f, 2168.0f)))


  object Experiment {
    val cnnDir: String = nn.nnDir + "/cnn"


    def getPathToInputs(nKernelsL0: Int, kernelShape: Shape): String = cnnDir + f"/experiment." +
      f"$nKernelsL0%d.${kernelShape.w}%d.${kernelShape.h}%d"
    def getPathToResults(pathToInputs: String): String = pathToInputs + "/results_lift"


    def loadDatasets(nInputs: Int, pathToInputs: String):
    (PaddedArray[Array5D[Float]], Array5D[Float], Array2D[Float],
      Array5D[Float]) = {

      if (!exists(get(pathToInputs + "/wconv1.json")))
        throw new java.io.FileNotFoundException(f"Experiment (nInputs=$nInputs%d) " +
          "resources not provided (JSON files with test images, NN weights and biases).")

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
