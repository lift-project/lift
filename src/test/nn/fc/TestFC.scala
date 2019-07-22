package nn.fc

/**
  * Created by Naums Mogers
  * This file implements the Lift version of the Multilayer Perceptron forward-propagation.
  *
  * Some code lines here are disabled since they depend on the code that is only available in the "nn" branch and in a
  * private fork of Lift.
  * Disabled functionality mainly refers to testing and exploring the search space of optimisational parameters.
  * Actual expressions for neural layers and associated initialization logic is still functional.
  * For full functionality and code examples, contact Naums Mogers (naums.mogers@ed.ac.uk).
  */

import java.io._
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get
import opencl.executor.{Execute, Executor}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test, Ignore}
import scala.util.Try
import nn.{Shape}

@Ignore
object TestFC {
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


@Ignore
class TestFC {

  val precision: Float = 0.01f

  // This test needs to be checked before reenabling it
  @Test
  def sanityMLPThreeSeqKernels(): Unit = {
    val (output_layer1: Array[Float], runtime_layer1) = Execute(1,1)[Array[Float]](
      FC.Seq(nn.Linear), input_W1, input_b1, input_X)
    val (output_layer2: Array[Float], runtime_layer2) = Execute(1,1)[Array[Float]](
      FC.Seq(nn.Linear), input_W2, input_b2, output_layer1)
    val (lift_result: Array[Float], runtime_layerout) = Execute(1,1)[Array[Float]](
      FC.Seq(nn.Linear), input_Wout, input_bout, output_layer2)
    println(f"\nx3 sequential kernels.\n" +
      f"Runtime: $runtime_layer1%1.5f + $runtime_layer2%1.5f + $runtime_layerout%1.5f = " +
      f"${runtime_layer1 + runtime_layer2 + runtime_layerout}%1.5f ms")

    assertArrayEquals(gold, lift_result, precision)
  }

  // The tests below are temporarily deprecated since they were not fixed to comply with new changes
  /*
  @Test
  def testMLP(): Unit = {
    // If rerunsAllowed == False, the experiment will not be rerun if result files from previous runs
    // are found. Otherwise, new results will be added with a datetime timestamp
    val rerunsAllowed: Boolean = true
    /** Build an array of experimental parameters filtering out the experiments that where
      * already run or for which the data was not provided; load data for all experiments
      */

    val experiments = for {
      layerSize <- 32 to 288 by 32 // inputs
      pathToInputs = Experiment.getPathToInputs(layerSize)
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
        singleTest(new FullyConnected(FullyConnected.Par, Array(nn.ReLU, nn.ReLU, nn.Linear), Array(e.multsPerThread, e.multsPerThread, e.multsPerThread),
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

  def singleTest(mlp: FullyConnected): Unit = {
    var inputNo = 0
    println(f"\n" + mlp.testDescription)

    for (layerNo <- 0 until mlp.nLayers) {
      // Padding
      mlp.padInputsAndNeurons(layerNo)

      val (outputFlat: Array[Float], runtime) =
        Execute(mlp.localSize0(layerNo), mlp.localSize1(layerNo), mlp.globalSize0(layerNo), mlp.globalSize1(layerNo),
          (true, true))(
          mlp.liftMLP(mlp.activationFun(layerNo), Tile(mults=mlp.multsPerThread(layerNo),
            inputs=mlp.localSize1(layerNo), neurons=mlp.neuronsPerWrg(layerNo)),
            Shape(in=mlp.inputLenPadded(layerNo), out=mlp.nNeuronsPadded(layerNo)), mlp.nLayerInputsPadded(layerNo)),
          // TODO: check mlp.weightsPadded(layerNo), mlp.biasesNonPadded(layerNo), mlp.layerInputsPadded(layerNo))
          mlp.weightsPadded(layerNo), mlp.biasesPadded(layerNo), mlp.layerInputsPadded(layerNo))

      mlp.runTimes(layerNo) = runtime
      def getGroupedOutputs = nn.group(outputFlat, (mlp.nLayerInputsPadded(layerNo), mlp.nNeuronsPadded(layerNo)))
      mlp.outputsPadded = if (mlp.outputsPadded == null) Array(getGroupedOutputs) else
        mlp.outputsPadded :+ getGroupedOutputs

      println(f"Layer $layerNo%d runtime: $runtime%1.5f ms")

    }
    mlp.unPadOutputs()
    println()

    /* Check and save results */
    val file = new File(nn.resultsFilename(mlp.pathToResults, mlp.nInputsNonPadded))
    file.getParentFile.mkdirs()
    val pw = new PrintWriter(file)
    var noErrors = false
    try {
      pw.write("device_name,f_name,n_inputs_l0_nonpadded,mults_per_thread,neurons_per_wrg," +
        {for (layerNo <- 0 until mlp.nLayers) yield f"n_neurons_l$layerNo%d_nonpadded"}.mkString(",") + "," +
        {for (layerNo <- 0 until mlp.nLayers) yield f"n_neurons_l$layerNo%d_padded"}.mkString(",") + "," +
        {for (layerNo <- 0 until mlp.nLayers) yield f"activation_f$layerNo%d"}.mkString(",") + "," +
        {for (layerNo <- 0 until mlp.nLayers) yield f"runtime_l$layerNo%d"}.mkString(",") + "\n")

      pw.write(nn.deviceName + "," + mlp.liftFunName + f",${mlp.nInputsNonPadded}%d,"+
        f"${mlp.multsPerThread(0)}%d,${mlp.neuronsPerWrg(0)}%d,")
      for (layerNo <- 0 until mlp.nLayers)
        pw.write(f"${mlp.nNeuronsNonPadded(layerNo)}%d,")
      for (layerNo <- 0 until mlp.nLayers)
        pw.write(f"${mlp.nNeuronsPadded(layerNo)}%d,")
      for (layerNo <- 0 until mlp.nLayers)
        pw.write(mlp.activationFun(layerNo).toString + ",")
      pw.write(f"${mlp.runTimes(0)}%1.5f,${mlp.runTimes(1)}%1.5f,${mlp.runTimes(2)}%1.5f\n")

      for ((liftSingleResult, targetSingleResult) <- mlp.outputsNonPadded zip mlp.targets) {
        //println(liftSingleResult.mkString(", "))
        //println(targetSingleResult.mkString(", "))
        assertArrayEquals(f"The lift output #$inputNo%d is different to the Tensorflow output", targetSingleResult,
          liftSingleResult, precision)
        inputNo = inputNo + 1
      }
      noErrors = true
      print(f"Done. Processed ${mlp.nInputsNonPadded}%d (padded: ${mlp.nLayerInputsPadded(0)}%d")
      for (layerNo <- 1 until mlp.nLayers)
        print(f", ${mlp.nLayerInputsPadded(layerNo)}%d")
      println(f") inputs, the results were equal to targets (precision=$precision%1.4f)")
    }
    finally {
      pw.close()
      if (!noErrors)
        new File(nn.resultsFilename(mlp.pathToResults, mlp.nInputsNonPadded)).delete()
    }
  }
  */
}
