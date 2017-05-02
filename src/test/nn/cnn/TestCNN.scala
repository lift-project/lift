package nn.cnn

import java.io.File
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get

import nn.{PaddedArray, Shape}
import opencl.executor.{Execute, Executor}
import org.junit.Assert.assertArrayEquals
import org.junit.{AfterClass, BeforeClass, Test}

import scala.util.Try

/**
  * Created by s1569687 on 01/03/17.
  */
object TestCNN {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init(0, 0)
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

    println(f"\n1. Convolution sanity check.\nRuntime: $runtime%1.5f ms")

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

    var cnn: CNN = null
    val cnns = for {
      nKernelsL0 <- 16 to 48 by 4//8 to 48 by 4
      kernelSize <- 5 to 25 by 5
      pathToInputs = Experiment.getPathToInputs(
        nKernelsL0, Shape(w=kernelSize, h=kernelSize))
      if exists(get(pathToInputs))
      pathToResults = Experiment.getPathToResults(pathToInputs)
      // Results dir doesn't exist (then create it) or it does, but reruns are allowed:
      if rerunsAllowed || {if (!exists(get(pathToResults))) {
        createDirectory(get(pathToResults))
        true} else false}
      nInputs <- 8 to 512 by 32
      // Results dir exists, but doesn't contain results of this experiment or it does, but reruns are allowed:
      if rerunsAllowed || new File(pathToResults).listFiles.toList.count {
        file => file.getName.endsWith("_n%d.csv".format(nInputs))} == 0
      // Load datasets once for all experiments (across all multsPerThread and neuronsPerWrg)
      loadingDatasets = Try(Experiment.loadDatasets(nInputs, pathToInputs))
      if loadingDatasets.isSuccess
      (tfX, tfWconv, tfBconv, tfResult) = loadingDatasets.get
      elsPerThreadL0 <- 1 to 16
      kernelsPerGroupL0 <- 2 to nKernelsL0//1 to nKernelsL0
      // TODO: remove + 1
      inputTileSize <- kernelSize + 1 to tfX.nonPadded.head.head.size
      // Check if CNN can be created with the selected parameters (e.g. if WrgGroupSize < maxWrgGroupSize)
      if {
        try {
          cnn = new CNN(CNN.Par, Array(nn.ReLU, nn.ReLU), elsPerThreadL0, kernelsPerGroupL0, inputTileSize,
            tfX, tfWconv, tfBconv, tfResult, pathToResults)
//          println(f"Prepared the experiment (nKernelsL0=$nKernelsL0%d, " +
//            f"inputTileSize=$inputTileSize%d, elsPerThreadL0=$elsPerThreadL0%d, " +
//            f"kernelsPerGroupL0=$kernelsPerGroupL0%d,\nkernelSize=$kernelSize%d, " +
//            f"nBatches=${tfX.nonPadded.length}%d, nInputs=$nInputs%d).")
          true
        }
        catch {
          // TODO: replace with IllegalArgumentException
          case e: java.lang.ArithmeticException =>
//            println("-----------------------------------------------------------------")
//            println(f"Cannot start the experiment (nKernelsL0=$nKernelsL0%d, " +
//              f"inputTileSize=$inputTileSize%d, elsPerThreadL0=$elsPerThreadL0%d, " +
//              f"kernelsPerGroupL0=$kernelsPerGroupL0%d,\nkernelSize=$kernelSize%d, " +
//              f"nBatches=${tfX.nonPadded.length}%d, nInputs=$nInputs%d).")
//            println(e.getMessage)
//            println("SKIPPING EXPERIMENT.")
            false
        }
      }
    } yield cnn

    println("-----------------------------------------------------------------")

    for (cnn <- cnns) {
      println("-----------------------------------------------------------------")
      println(f"Starting the experiment (inputTileSizeL0=${cnn.inputTileSize(0)}%d, nKernelsL0=${cnn.nKernels(0)}%d, " +
        f"elsPerThreadL0=${cnn.elsPerThread(0)}%d,\nkernelsPerGroupL0=${cnn.kernelsPerGroup(0)}%d, " +
        f"kernelSize=${cnn.kernelShape(0).s}%d, nBatches=${cnn.nBatches}%d, nInputs=${cnn.nInputs}%d).")

      singleTest(cnn)
    }
  }

  def singleTest(cnn: CNN): Unit = {
    var inputNo = 0
    //println(f"\n" + cnn.testDescription)

    for (layerNo <- 0 until cnn.nLayers) {
      cnn.updateInputs(layerNo)
      /* Padding */
      cnn.padInputs(layerNo)

      val (outputFlat: Array[Float], runtime) =
        Execute(
          cnn.localSize(0)(layerNo), cnn.localSize(1)(layerNo), cnn.localSize(2)(layerNo),
          cnn.globalSize(0)(layerNo), cnn.globalSize(1)(layerNo), cnn.globalSize(2)(layerNo), (true, true))(

          cnn.liftCNN(
            cnn.activationFun(layerNo), cnn.inputShape(layerNo), cnn.kernelShape(layerNo),
            cnn.nInputs, cnn.nBatches, cnn.nInChannels(layerNo), cnn.nKernels(layerNo),
            Tile(kernels_per_group=cnn.kernelsPerGroup(layerNo), els_per_thread=cnn.elsPerThread(layerNo),
              inputTileSize=cnn.inputTileSize(layerNo), inputTileSlideStep=cnn.inputTileStep(layerNo),
              nInputTilesPerDim=cnn.nTilesPerDim(layerNo),
              n_windows_per_tile_per_dim=cnn.nWindowsPerTilePerDim(layerNo))),

          cnn.kWeights(layerNo), cnn.kBiases(layerNo), cnn.inputs(layerNo).padded)
      cnn.runTimes(layerNo) = runtime

      /* Group and unpad */
      cnn.outputs = {
        def getShapedOutputs = nn.group(outputFlat, (cnn.nBatches, cnn.nInputs, cnn.outputShape(layerNo).hPadded,
          cnn.outputShape(layerNo).wPadded, cnn.nKernels(layerNo))).map(
          batch => batch.map(
            input => input.map(
              row => row.slice(0, cnn.outputShape(layerNo).wNonPadded)
            ).slice(0, cnn.outputShape(layerNo).hNonPadded)
          ))
        if (cnn.outputs == null) Array(PaddedArray(getShapedOutputs)) else
          cnn.outputs :+ PaddedArray(getShapedOutputs)
      }

      println(f"Layer $layerNo%d runtime: $runtime%1.5f ms")

    }
    // TODO: unpad
    //cnn.unPadOutputs()
    //cnn.outputs = cnn.layerOutputsPadded.last
    println()

    /* Check and save results */
    // TODO: check and save
//    val file = new File(nn.resultsFilename(cnn.pathToResults, cnn.nInputs))
//    file.getParentFile.mkdirs()
//    val pw = new PrintWriter(file)
//    var noErrors = false
    try {
      /*pw.write("device_name,f_name,n_inputs_l0_nonpadded,mults_per_thread,neurons_per_wrg," +
        {for (layerNo <- 0 until cnn.nLayers) yield f"n_neurons_l$layerNo%d_nonpadded"}.mkString(",") + "," +
        {for (layerNo <- 0 until cnn.nLayers) yield f"n_neurons_l$layerNo%d_padded"}.mkString(",") + "," +
        {for (layerNo <- 0 until cnn.nLayers) yield f"activation_f$layerNo%d"}.mkString(",") + "," +
        {for (layerNo <- 0 until cnn.nLayers) yield f"runtime_l$layerNo%d"}.mkString(",") + "\n")

      pw.write(nn.deviceName + "," + cnn.liftFunName + f",${cnn.nInputsNonPadded}%d,"+
        f"${cnn.multsPerThread(0)}%d,${cnn.neuronsPerWrg(0)}%d,")
      for (layerNo <- 0 until cnn.nLayers)
        pw.write(f"${cnn.nNeuronsNonPadded(layerNo)}%d,")
      for (layerNo <- 0 until cnn.nLayers)
        pw.write(f"${cnn.nNeuronsPadded(layerNo)}%d,")
      for (layerNo <- 0 until cnn.nLayers)
        pw.write(cnn.activationFun(layerNo).toString + ",")
      pw.write(f"${cnn.runTimes(0)}%1.5f,${cnn.runTimes(1)}%1.5f,${cnn.runTimes(2)}%1.5f\n")*/

//      for ((liftSingleResult, targetSingleResult) <- cnn.outputs zip cnn.targets) {
//        println(liftSingleResult.mkString(", "))
//        println(targetSingleResult.mkString(", "))
//        assertArrayEquals(f"The lift output #$inputNo%d is different to the Tensorflow output",
//          targetSingleResult.flatten.flatten.flatten, liftSingleResult.flatten.flatten.flatten, precision)
//        inputNo = inputNo + 1
//      }
//      noErrors = true
      println(f"SUCCESS. Processed ${cnn.nInputs}%d inputs, the results were equal to targets " +
        f"(precision=$precision%1.4f).")
    }
    finally {
//      pw.close()
//      if (!noErrors)
//        new File(nn.resultsFilename(cnn.pathToResults, cnn.nInputs)).delete()
    }
  }
}