package nn.cnn

import nn.{cnn, conv, fc}
import nn.mysql.Connector
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
    nn.cnn.mysql.CreateTable()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
    Connector.close()
  }
}

class TestCNN_Conv {
  val reruns: Int = 1

  @Test
  def TestConv(): Unit = {
    for (_ <- 0 until reruns)
      new TestCNN().Test(
        cnn.getConfigFromJSON("/home/s1569687/lift/src/test/nn/cnn/cnn_experiments.json"),
      continueFrom = Experiment(
        cnn.Experiment.InputConfig(
          nBatches = 2,
          nInputs = 64,
          imageSize = 64,
          nChannels = 1),
        convConfig = List(
          conv.Experiment.Config(
            conv.Experiment.Config.Dimensions(nKernels = 16, kernelSize = 20),
            conv.Experiment.Config.OptimisationalParams(inputTileSize = 20, elsPerThread = 20, kernelsPerGroup = 1)
          ),
          conv.Experiment.Config(
            conv.Experiment.Config.Dimensions(nKernels = 12, kernelSize = 20),
            conv.Experiment.Config.OptimisationalParams(inputTileSize = 20, elsPerThread = 5, kernelsPerGroup = 4)
          )
        ),
        fcConfig = List(
          fc.Experiment.Config(
            fc.Experiment.Config.Dimensions(nNeurons = 16),
            fc.Experiment.Config.OptimisationalParams(multsPerThread = 1, neuronsPerWrg = 1)
          ),
          fc.Experiment.Config(
            fc.Experiment.Config.Dimensions(nNeurons = 10),
            fc.Experiment.Config.OptimisationalParams(multsPerThread = 1, neuronsPerWrg = 1)
          )
        )
      ),
      abortAfter = Some(1))
  }
}
