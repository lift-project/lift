package nn.mlp

/**
  * Created by Naums Mogers
  * This file implements the Lift version of the Multilayer Perceptron forward-propagation.
  */

import java.io._
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get

import nn.TestUtils
import opencl.executor.{Execute, Executor}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

import scala.util.Try


object TestMLP {
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


class TestMLP {

  val precision: Float = 0.01f


  @Test
  def SanityMLPThreeSeqKernels(): Unit = {
    val (output_layer1: Array[Float], runtime_layer1) = Execute(1,1)(
      MLP.Seq(MLP.Linear), input_W1, input_b1, input_X)
    val (output_layer2: Array[Float], runtime_layer2) = Execute(1,1)(
      MLP.Seq(MLP.Linear), input_W2, input_b2, output_layer1)
    val (lift_result: Array[Float], runtime_layerout) = Execute(1,1)(
      MLP.Seq(MLP.Linear), input_Wout, input_bout, output_layer2)
    println(f"\nx3 sequential kernels.\n" +
      f"Runtime: $runtime_layer1%1.5f + $runtime_layer2%1.5f + $runtime_layerout%1.5f = " +
      f"${runtime_layer1 + runtime_layer2 + runtime_layerout}%1.5f ms")

    assertArrayEquals(gold, lift_result, precision)
  }

  @Test
  def TestMLP(): Unit = {
    // If rerunsAllowed == False, the experiment will not be rerun if result files from previous runs
    // are found. Otherwise, new results will be added with a datetime timestamp
    val rerunsAllowed: Boolean = true
    /** Build an array of experimental parameters filtering out the experiments that where
      * already run or for which the data was not provided; load data for all experiments
      */

    val experiments = for {
      layerSize <- 32 to 288 by 32
      pathToInputs = Experiment.getPathToInputs(layerSize)
      pathToResults = Experiment.getPathToResults(pathToInputs)
      if exists(get(pathToInputs))
      // Results dir doesn't exist (create it) or it does, but reruns are allowed:
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
      println(f"Starting the experiment (multsPerThread=${e.multsPerThread}%d, " +
        f"neuronsPerWrg=${e.neuronsPerWrg}%d, " + f"layerSize=${e.layerSize}%d, " +
        f"nInputs=${e.nInputs}%d).")

      try {
        singleTest(new MLP(MLP.Par, Array(MLP.ReLU, MLP.ReLU, MLP.Linear), e.multsPerThread, e.neuronsPerWrg,
          e.tfX, e.tfW, e.tfB, e.tfResult, "MLP.Par", f"(MNIST dataset) x3 2D-parallel kernels (across inputs). " +
            f"Workgroup per partition of neurons per partition of inputs. " +
            f"Memory accesses are coalesced.", e.pathToInputs))
      } catch {
        case e: java.lang.IllegalArgumentException =>
          println(e.getMessage())
      //          case e: java.io.FileNotFoundException =>
      //          case e: DeviceCapabilityException =>
      //            println("ERROR: Not enough OpenCL memory. Skipping the experiment.")
      //          case NotEvaluableException =>
      //            println("ERROR: Not enough OpenCL memory. Skipping the experiment.")
      //        }
      }
    }
  }

  def singleTest(mlp: MLP) = {
    var inputNo = 0
    print(f"\n" + mlp.testDescription + "\nRuntime:\n")

    for (layerNo <- 0 until mlp.nLayers) {
      val (outputFlat: Array[Float], runtime) =
        Execute(mlp.localSize0(layerNo), mlp.localSize1(layerNo), mlp.globalSize0(layerNo), mlp.globalSize1(layerNo), (true, true))(
          mlp.liftMLP(mlp.activationFun(layerNo), mlp.multsPerThread, mlp.localSize1(layerNo), mlp.neuronsPerWrg,
            mlp.nNeurons(layerNo), mlp.inputLen(layerNo), mlp.nInputsPadded),
          mlp.weights(layerNo), mlp.biases(layerNo), mlp.layerInputs(layerNo))

      mlp.runTimes(layerNo) = runtime
      mlp.outputs(layerNo) = TestUtils.group(outputFlat, (mlp.nInputsPadded, mlp.nNeurons(layerNo)))

      println(f"Layer $layerNo%d: $runtime%1.5f ms")

    }
    println()

    /* Check and save results */
    val file = new File(TestUtils.resultsFilename(mlp.pathToResults, mlp.nInputsPadded))
    file.getParentFile.mkdirs()
    val pw = new PrintWriter(file)
    var finished_without_errors = false
    try {
      //TODO: check inline for output
      println("device_name,f_name,n_inputs,mults_per_thread,neurons_per_wrg," +
        {for (layerNo <- 0 until mlp.nLayers) yield f"layer_len$layerNo%d"}.mkString(",") +
        ",layer_len2,activation_f0,activation_f1,activation_f2,runtime_l0,runtime_l1,runtime_l2\n")
      pw.write("device_name,f_name,n_inputs,mults_per_thread,neurons_per_wrg," +
        {for (layerNo <- 0 until mlp.nLayers) yield f"layer_len$layerNo%d"}.mkString(",") +
        ",layer_len2,activation_f0,activation_f1,activation_f2,runtime_l0,runtime_l1,runtime_l2\n")

      pw.write(TestUtils.deviceName + "," + mlp.liftFunName + f",${mlp.nInputsPadded}%d,"+
        f"${mlp.multsPerThread}%d,${mlp.neuronsPerWrg}%d,")
      for (layer_i <- 0 until mlp.nLayers) {
        pw.write(f"${mlp.biases(layer_i).length}%d,")
      }
      for (layer_i <- 0 until mlp.nLayers) {
        pw.write(mlp.activationFun(layer_i).toString + ",")
      }
      pw.write(f"${mlp.runTimes(0)}%1.5f,${mlp.runTimes(1)}%1.5f,${mlp.runTimes(2)}%1.5f\n")

      // TODO: do something about such big error
      for ((lift_single_result, target_single_result) <- mlp.outputs.last zip mlp.targets) {
        //          println(lift_single_result.mkString(", "))
        //          println(tf_single_result.mkString(", "))
        assertArrayEquals(f"The lift output #$inputNo%d is different to the Tensorflow output", target_single_result,
          lift_single_result, precision)
        inputNo = inputNo + 1
      }
      finished_without_errors = true
      println(f"Done. Processed ${mlp.nInputs}%d (padded: ${mlp.nInputsPadded}%d) inputs, " +
        f"the results were equal to targets (precision=$precision%1.4f)")
    }
    finally {
      pw.close()
      /*if (!finished_without_errors) {
        new File(TestUtils.resultsFilename(mlp.pathToResults, mlp.nInputsPadded)).delete()
        print(f"Input $inputNo%d: ")
        println(mlp.inputsNonPadded(inputNo).mkString("\t"))
        println(f"Output $inputNo%d: ")
        print("\nWeights L0, N0: ")
        println(mlp.weights(0)(0).mkString("\t"))
        print("Layer 0: ")
        println(mlp.outputs(0)(inputNo).mkString("\t"))
        print("\nWeights L1, N0: ")
        println(mlp.weights(1)(0).mkString("\t"))
        print("Layer 1: ")
        println(mlp.outputs(1)(inputNo).mkString("\t"))
        print("\nWeights L2, N0: ")
        println(mlp.weights(2)(0).mkString("\t"))
        print("Layer 2: ")
        println(mlp.outputs.last(inputNo).mkString("\t"))
        println(f"Target output $inputNo%d: ")
        print("Layer X: ")
        println(mlp.targets(inputNo).mkString("\t"))
      }*/
    }
  }
}
