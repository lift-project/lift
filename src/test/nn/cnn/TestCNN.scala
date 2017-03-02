package nn.cnn

import java.io.File
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get

import nn.Shape
import opencl.executor.{Execute, Executor}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

import scala.util.Try

/**
  * Created by s1569687 on 01/03/17.
  */
object TestCNN {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init(1, 0)
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestCNN {
  val precision: Float = 1f

  //@Test
  def Sanity_CNN(): Unit = {
    val (lift_result: Array[Float], runtime) = Execute(1,1)(
      CNN.Seq(2, 3, nn.Linear), input_K, input_b, input_X)

    println(f"\n1. Convolution sanity check.\n" +
      f"Runtime: $runtime%1.5f ms")

    val lift_result3d = nn.group(lift_result, (gold.length, gold.head.length, gold.head.head.length))
    for ((gold2d, lift_result2d) <- gold zip lift_result3d) {
      println(lift_result2d.flatten.mkString(", "))
      println(gold2d.flatten.mkString(", "))
      for ((gold1d, lift_result1d) <- gold2d zip lift_result2d) {
        assertArrayEquals(gold1d, lift_result1d, precision)
      }
    }
  }

  //@Test
  /*def CNN_Seq(): Unit = {
    val (tf_X, tf_Wc, tf_Bc, tf_Wd, tf_Bd, tf_result) = loadDatasets("/home/nm/tf_cnn/experiment/", 1)
    val (output_conv1_flat: Array[Float], runtime_conv1) =
      Execute(1, 1)(
        CNN.Seq(tf_Wc(0).length, tf_Wc(0).head.length, nn.ReLU), tf_Wc(0), tf_Bc(0),
        nn.group(tf_X(0), (28, 28, 1)))


    val (output_conv2_flat: Array[Float], runtime_conv2) =
      Execute(1, 1)(
        CNN.Seq(tf_Wc(1).length, tf_Wc(1).head.length, nn.ReLU), tf_Wc(1), tf_Bc(1),
        nn.group(output_conv1_flat, (28 - tf_Wc(0).length + 1, 28 - tf_Wc(0).head.length + 1,
          tf_Wc(0).head.head.head.length)))

    /*val (output_mlp1_flat: Array[Float], runtime_mlp1) =
      Execute(1, 1)(
        LLayerSeq(LReLU), tf_Wd(0), tf_Bd(0), output_conv2_flat)

    val (output_mlp2_flat: Array[Float], runtime_mlp2) =
      Execute(1, 1)(
        LLayerSeq(Linear), tf_Wd(1), tf_Bd(1), output_mlp1_flat)
    println(output_mlp2_flat.mkString(", "))
    println(tf_result.flatten.mkString(", "))
    val output_mlp2 = nn.group(output_mlp2_flat, (tf_result.length, tf_result.head.length))
    for ((tf_result_1d, output_mlp2_1d) <- tf_result zip output_mlp2) {
      assertArrayEquals(tf_result_1d, output_mlp2_1d, precision)
    }*/
  }*/

  @Test
  def TestCNN(): Unit = {
    // If rerunsAllowed == False, the experiment will not be rerun if result files from previous runs
    // are found. Otherwise, new results will be added with a datetime timestamp
    val rerunsAllowed: Boolean = true
    /** Build an array of experimental parameters filtering out the experiments that where
      * already run or for which the data was not provided; load data for all experiments
      */

    val experiments = for {
      nKernelsL0 <- 8 to 32 by 4
      nKernelsL1 <- 8 to 32 by 4
      kernelWidth <- 5 to 25 by 5
      kernelHeight <- 5 to 25 by 5
      pathToInputs = Experiment.getPathToInputs(
        Shape(l0=nKernelsL0, l1=nKernelsL1), Shape(w=kernelWidth, h=kernelWidth))
      pathToResults = Experiment.getPathToResults(pathToInputs)
      if exists(get(pathToInputs))
      // Results dir doesn't exist (then create it) or it does, but reruns are allowed:
      if rerunsAllowed || {if (!exists(get(pathToResults))) {
        createDirectory(get(pathToResults))
        true} else false}
      nInputs <- 32 to 512 by 32
      // Results dir exists, but doesn't contain results of this experiment or it does, but reruns are allowed:
      if rerunsAllowed || new File(pathToResults).listFiles.toList.count {
        file => file.getName.endsWith("_n%d.csv".format(nInputs))} == 0
      // Load datasets once for all experiments (across all multsPerThread and neuronsPerWrg)
      loadingDatasets = Try(Experiment.loadDatasets(layerSize, nInputs, pathToInputs))
      if loadingDatasets.isSuccess
      (tfX, tfW, tfB, tfResult) = loadingDatasets.get
      multsPerThread <- 1 to 16
      neuronsPerWrg <- 1 to 16
    } yield new Experiment(multsPerThread, neuronsPerWrg, layerSize, nInputs, tfX, tfW, tfB, tfResult)


    for (e <- experiments) {
      println("-----------------------------------------------------------------")
      println(f"Starting the experiment (multsPerThread=${e.multsPerThread}%d, " +
        f"neuronsPerWrg=${e.neuronsPerWrg}%d, " + f"layerSize=${e.layerSize}%d, " +
        f"nInputs=${e.nInputs}%d).")

      try {
        singleTest(new MLP(MLP.Par, Array(nn.ReLU, nn.ReLU, MLP.Linear), Array(e.multsPerThread, e.multsPerThread, e.multsPerThread),
          Array(e.neuronsPerWrg, e.neuronsPerWrg, e.neuronsPerWrg), e.tfX, e.tfW, e.tfB, e.tfResult, "MLP.Par",
          f"(MNIST dataset) x3 2D-parallel kernels (across inputs). Workgroup per partition of neurons per " +
            f"partition of inputs. Memory accesses are coalesced.", e.pathToInputs))
      } catch {
        case e: java.lang.IllegalArgumentException =>
          println(e.getMessage)
          println("SKIPPING EXPERIMENT.")
      }
    }
  }
}