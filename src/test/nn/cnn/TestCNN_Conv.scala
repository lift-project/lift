package nn.cnn

import nn.cnn
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
    for (_ <- 0 to reruns)
      new TestCNN().Test(
        cnn.getConfigFromJSON("/home/s1569687/lift/src/test/nn/cnn/cnn_experiments.json")
//        cnn.ExperimentsSet(
//        nInputsRange = List(64/*8, 16, 32, 64, 128*//*, 256, 512, 1024, 2048, 2048*/),
//        imageSizeRange = List(64/*8, 16, 32, 64, 128, 256, 512*/),
//
//        nKernelsL0 = 16,
//        nKernelsL1Range = List(12),//(12 to 48 by 4).toList,
//        kernelSizeRange = List(20),//(8 to 64 by 4).toList,
//
//        neuronsL1Range = List(16),
//
//        kernelsPerGroupL0 = 4,
//        inputTileSizeRange = (kernelSize, imageSize) => (kernelSize to imageSize by 4).toList,
//        elsPerThreadL1Range = kernelSize => List(1) ++ (2 to kernelSize by 1),
//        kernelsPerGroupL1Range = nKernelsL1 => List(1) ++ (2 to nKernelsL1 by 1),
//
//        multsPerThreadRange = _ => List(1),
//        neuronsPerWrgRange = _ => List(1))
        /*,


        continueFrom = cnn.Experiment(
          nInputs = 64,
          imageSize = 64,

          nKernelsL1 = 12,
          kernelSize = 20,
          neuronsL1 = 16,

          inputTileSize = 20,
          elsPerThreadL1 = 1,
          kernelsPerGroupL1 = 1,

          multsPerThread = 1,
          neuronsPerWrg = 1)*/)
  }
}
