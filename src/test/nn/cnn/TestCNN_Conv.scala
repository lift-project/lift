/*
 * Some code lines here are disabled since they depend on the code that is only available in the "nn" branch and in a
 * private fork of Lift.
 * Disabled functionality mainly refers to testing and exploring the search space of optimisational parameters.
 * Actual expressions for neural layers and associated initialization logic is still functional.
 * For full functionality and code examples, contact Naums Mogers (naums.mogers@ed.ac.uk).
 */
package nn.cnn

import nn.cnn
import opencl.executor.Executor
import org.junit.{AfterClass, BeforeClass, Test, Ignore}

/**
  * Created by s1569687 on 8/17/17.
  */

@Ignore
object TestCNN_Conv {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
//    Executor.init(/*avus*/1, 1)
    Executor.init(0, 0)
//     MySQL is disabled in this version
//    nn.cnn.mysql.CreateTable()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
    //     MySQL is disabled in this version
//    Connector.close()
  }

}

@Ignore
class TestCNN_Conv {
  val reruns: Int = 1

  def testConvUsingJSON(): Unit = {
    val testConfigFilenames = Array(
      //"vgg_layer_1.json",
            "vgg_layer_3.json")
      for (_ <- 0 until reruns) {
        for (testConfigFilename <- testConfigFilenames)
          new TestCNN().test(
            cnn.getConfigFromJSON(System.getenv("LIFT_CNN_CONFIG_PATH") + "/" + testConfigFilename),
            testConfigFilename)
      }
  }

  // The test below is disabled due to a missing dependency (Caffe configuration file parsing) that currently
  // resides in a separate fork
  /*@Test
  def TestConv(): Unit = {
    // This function is disabled as the Caffe package that is required for parsing the NN specifications currently only
    // resides in the "nn" branch.
    val protoFiles = Seq(
//      System.getenv("LIFT_CNN_CONFIG_PATH") + "/" + "VGG_ILSVRC_19_layers_deploy_1.prototxt")
      System.getenv("LIFT_CNN_CONFIG_PATH") + "/" + "/ResNet-101-deploy-1.prototxt")

      for (_ <- 0 until reruns) {
        for (protoFile <- protoFiles) {
          val configs = nn.caffe.proto.Config.configToExperimentParams(protoFile) // For debugging purposes
          println(configs.distinct.length + " unique layers to process. Namely:")
          for (config <- configs.distinct)
            print(config.layerNo + ", ")
          println()

          for (config <- configs.distinct) {
            new TestCNN().test(config, protoFile)
            // Below is an example of the TestCNN().test() argument defining the first configuration
            // not to skip in the whole search space
            /*, {
              //              val iC = InputConfig(nBatches = 1,
              //                nInputs = 1,
              //                inputSize = 226,
              //                nChannels = 64)
              //              val cD = conv.Experiment.Config.Dimensions(
              //                nKernels = 64,
              //                kernelSize = 3,
              //                kernelStride = 1)
              val iC = configs.distinct.head.exactParams.get.inputConfig
              val cD = configs.distinct.head.exactParams.get.convDimensions

              new Experiment(
                layerNo = 1,
                inputConfig = iC,
                convConfigs = Vector(
                  conv.Experiment.Config(
                    dim = cD,
                    optParams = conv.Experiment.Config.OptimisationalParams(
                      inputTileSize = cD.kernelSize,
                      elsPerThread = 7,
                      kernelsPerGroup = 1,
                      vectorLen = 1,
                      coalesce = false,
                      unrollReduce = false
                    ))),
                fcConfigs = Vector(new fc.Experiment.Config(
                  dim = fc.Experiment.Config.Dimensions(nNeurons = 1),
                  optParams = fc.Experiment.Config.OptimisationalParams(
                    multsPerThread = 1,
                    neuronsPerWrg = 1
                  ))),
                pathToInputs = cnn.Experiment.pathToInputs(iC, cD),
                pathToParams = cnn.Experiment.pathToParams(iC, cD),
                pathToTargets = cnn.Experiment.pathToTargets(iC, cD))
            })*/

          }
        }
      }
  }*/
}
