package nn.cnn

import nn.cnn
import opencl.executor.Executor
import org.junit.{AfterClass, BeforeClass, Test}

/**
  * Created by s1569687 on 8/17/17.
  */

object TestCNN_Conv {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init(/*avus*/1, 1)
//    Executor.init(/*artemisa*/0, 0)
    // TODO: reenable MySQL
//    nn.cnn.mysql.CreateTable()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
//    Connector.close()
  }

  def main(args: Array[String]): Unit = {
    // For running from the command line
    (new TestCNN_Conv).TestConv()
  }
}

class TestCNN_Conv {
  val reruns: Int = 1

  //      new TestCNN().Test(
  //        cnn.getConfigFromJSON("/home/s1569687/lift/src/test/nn/cnn/cnn_experiments.json"),
  //        continueFrom = Experiment(
  //          cnn.Experiment.InputConfig(
  //            nBatches = 2,
  //            nInputs = 32,
  //            imageSize = 32,
  //            nChannels = 1),
  //          convConfig = List(
  //            conv.Experiment.Config(
  //              conv.Experiment.Config.Dimensions(nKernels = 16, kernelSize = 20),
  //              conv.Experiment.Config.OptimisationalParams(inputTileSize = 20, elsPerThread = 20, kernelsPerGroup = 1)
  //            ),
  //            conv.Experiment.Config(
  //              conv.Experiment.Config.Dimensions(nKernels = 8, kernelSize = 8),
  //              conv.Experiment.Config.OptimisationalParams(inputTileSize = 8, elsPerThread = 1, kernelsPerGroup = 1)
  //            )
  //          ),
  //          fcConfig = List(
  //            fc.Experiment.Config(
  //              fc.Experiment.Config.Dimensions(nNeurons = 16),
  //              fc.Experiment.Config.OptimisationalParams(multsPerThread = 1, neuronsPerWrg = 1)
  //            ),
  //            fc.Experiment.Config(
  //              fc.Experiment.Config.Dimensions(nNeurons = 10),
  //              fc.Experiment.Config.OptimisationalParams(multsPerThread = 1, neuronsPerWrg = 1)
  //            )
  //          )
  //        ),
  //        abortAfter = Some(1))
  @Test
  def TestConv(): Unit = {
    val testConfigFilenames = Array(
      "vgg_layer_1.json",
      "vgg_layer_3.json")
//      "vgg_layer_6.json",
//      "vgg_layer_8.json",
//      "vgg_layer_11.json",
//      "vgg_layer_13_15_17.json",
//      "vgg_layer_20.json",
//      "vgg_layer_22_24_26.json",
//      "vgg_layer_29_31_33_35.json")
    for (_ <- 0 until reruns) {
      for (testConfigFilename <- testConfigFilenames) 
        new TestCNN().Test(
          //        cnn.getConfigFromJSON(System.getenv("LIFT_CNN_CONFIG_PATH") + "/test_layer.json"))
          cnn.getConfigFromJSON(System.getenv("LIFT_CNN_CONFIG_PATH") + "/" + testConfigFilename),
            testConfigFilename)
    }
  }
}
