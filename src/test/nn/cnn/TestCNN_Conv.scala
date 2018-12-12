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

  def main(args: Array[String]): Unit = {
    // For running from the command line
    (new TestCNN_Conv).testConv()
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
  @Test
  def testConv(): Unit = {
    // This function is disabled as the Caffe package that is required for parsing the NN specifications currently only
    // resides in the "nn" branch.
    if (System.getenv("LIFT_CNN_CONFIG_PATH") == null)
      return

    val protoFiles = Seq(
//      System.getenv("LIFT_CNN_CONFIG_PATH") + "/" + "VGG_ILSVRC_19_layers_deploy_1.prototxt")
//      System.getenv("LIFT_CNN_CONFIG_PATH") + "/" + "/ResNet-101-deploy-1.prototxt")
      System.getenv("LIFT_CNN_CONFIG_PATH") + "/" + "/GoogleNet.prototxt")
    
      for (_ <- 0 until reruns) {
        for (protoFile <- protoFiles) {
          val configs = nn.caffe.proto.Config.configToExperimentParams(protoFile) // For debugging purposes
          println(configs.distinct.length + " unique layers to process. Namely:")
          for (config <- configs.distinct)
            print(config.layerNo + ", ")
          println()

          for (config <- configs.distinct) {
            new TestCNN().test(config, protoFile)//, {
//              val iC = InputConfig(nBatches = 1,
//                nInputs = 50,
//                inputSize = 55,
//                nChannels = 64)
//              val cD = nn.conv.Experiment.Config.Dimensions(
//                nKernels = 64,
//                kernelSize = 1,
//                kernelStride = 1)
//              val oP = nn.conv.Experiment.Config.OptimisationalParams(
//                inputTileSize = 1,
//                elsPerThread = 2,
//                kernelsPerGroup = 1,
//                vectorLen = 1,
//                coalesce = true,
//                unrollReduce = true
//              )
////              val iC = configs.distinct.head.exactParams.get.inputConfig
////              val cD = configs.distinct.head.exactParams.get.convDimensions
//
//              new Experiment(
//                layerNo = 6,
//                inputConfig = iC,
//                convConfigs = Vector(
//                  nn.conv.Experiment.Config(
//                    dim = cD,
//                    optParams = oP)),
//                fcConfigs = Vector(new nn.fc.Experiment.Config(
//                  dim = nn.fc.Experiment.Config.Dimensions(nNeurons = 1),
//                  optParams = nn.fc.Experiment.Config.OptimisationalParams(
//                    multsPerThread = 1,
//                    neuronsPerWrg = 1
//                  ))),
//                pathToInputs = cnn.Experiment.pathToInputs(iC, cD),
//                pathToParams = cnn.Experiment.pathToParams(iC, cD),
//                pathToTargets = cnn.Experiment.pathToTargets(iC, cD))
//            })
            
          }
        }
      }
  }
}
