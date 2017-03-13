package nn.cnn

import java.io.{File, PrintWriter}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get

import nn.Shape
import opencl.executor.{Execute, Executor}
import org.junit.Assert.{assertArrayEquals, _}
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
      nKernelsL0 <- 8 to 48 by 4
      kernelWidth <- 5 to 25 by 5
      kernelHeight <- 5 to 25 by 5
      pathToInputs = Experiment.getPathToInputs(
        nKernelsL0, Shape(w=kernelWidth, h=kernelHeight))
      if exists(get(pathToInputs))
      pathToResults = Experiment.getPathToResults(pathToInputs)
      // Results dir doesn't exist (then create it) or it does, but reruns are allowed:
      if rerunsAllowed || {if (!exists(get(pathToResults))) {
        createDirectory(get(pathToResults))
        true} else false}
      nInputs <- 32 to 512 by 32
      // Results dir exists, but doesn't contain results of this experiment or it does, but reruns are allowed:
      if rerunsAllowed || new File(pathToResults).listFiles.toList.count {
        file => file.getName.endsWith("_n%d.csv".format(nInputs))} == 0
      // Load datasets once for all experiments (across all multsPerThread and neuronsPerWrg)
      loadingDatasets = Try(Experiment.loadDatasets(nInputs, pathToInputs))
      if loadingDatasets.isSuccess
      (tfX, tfWconv, tfBconv, tfResult) = loadingDatasets.get
//      multsPerThread <- 1 to 16
//      neuronsPerWrg <- 1 to 16
    } yield new Experiment(nKernelsL0, nInputs, Shape(w=kernelWidth, h=kernelHeight),
      pathToInputs, pathToResults, tfX, tfWconv, tfBconv, tfResult)


    for (e <- experiments) {
      println("-----------------------------------------------------------------")
      println(f"Starting the experiment (nKernelsL0=${e.nKernelsL0}%d, " +
        f"kernelWidth=${e.kernelL0Shape.w}%d, " + f"kernelHeight=${e.kernelL0Shape.h}%d, " +
        f"nInputs=${e.nInputs}%d).")

      try {
        singleTest(new CNN(CNN.Par, Array(nn.Linear), Array(e.nKernelsL0, e.tfBconv.head.length),
          e.tfX, e.tfWconv, e.tfBconv, e.tfResult, "CNN.Par",
          f"(MNIST dataset) x2 parallel convolutional kernels.", e.pathToInputs))
      } catch {
        case e: java.lang.IllegalArgumentException =>
          println(e.getMessage)
          println("SKIPPING EXPERIMENT.")
      }
    }
  }

  def singleTest(cnn: CNN): Unit = {
    var inputNo = 0
    println(f"\n" + cnn.testDescription)

    for (layerNo <- 0 until cnn.nLayers) {
      // TODO: Padding
      //cnn.padInputsAndNeurons(layerNo)
      cnn.layerInputsPadded = if (cnn.layerInputsPadded == null) Array(cnn.getLayerInputs(layerNo)) else
        cnn.layerInputsPadded :+ cnn.getLayerInputs(layerNo)

      cnn.nLayerInputsPadded(layerNo) = cnn.layerInputsPadded(layerNo).head.length
      // Shapes
      cnn.layerInputShapePadded(layerNo) = Shape(w=cnn.layerInputsPadded(layerNo).head.head.length,
        h=cnn.layerInputsPadded(layerNo).head.head.head.length)
      cnn.layerOutputShapePadded(layerNo) =
        Shape(w=cnn.layerInputShapePadded(layerNo).w - (cnn.kernelShapePadded(layerNo).w - 1),
          h=cnn.layerInputShapePadded(layerNo).h - (cnn.kernelShapePadded(layerNo).h - 1))

      val (outputFlat: Array[Float], runtime) =
        Execute(
          cnn.localSize0(layerNo), cnn.localSize1(layerNo), cnn.localSize2(layerNo),
          cnn.globalSize0(layerNo), cnn.globalSize1(layerNo), cnn.globalSize2(layerNo), (true, true))(
          // TODO: padding
          cnn.liftCNN(cnn.activationFun(layerNo), cnn.kernelShapePadded(layerNo), cnn.layerOutputShapePadded(layerNo),
            cnn.nKernels(layerNo)),
          cnn.kernelWeightsPadded(layerNo), cnn.kernelBiasesPadded(layerNo), cnn.layerInputsPadded(layerNo))

      cnn.runTimes(layerNo) = runtime

      def getGroupedOutputs = nn.group(outputFlat, (cnn.nBatches, cnn.nLayerInputsPadded(layerNo),
        cnn.layerOutputShapePadded(layerNo).h, cnn.layerOutputShapePadded(layerNo).w, cnn.nKernels(layerNo)))
      cnn.layerOutputsPadded = if (cnn.layerOutputsPadded == null) Array(getGroupedOutputs) else
        cnn.layerOutputsPadded :+ getGroupedOutputs

      println(f"Layer $layerNo%d runtime: $runtime%1.5f ms")

    }
    // TODO: unpad
    //cnn.unPadOutputs()
    println()

    /* Check and save results */
    // TODO: check and save
    /*val file = new File(nn.resultsFilename(cnn.pathToResults, cnn.nInputsNonPadded))
    file.getParentFile.mkdirs()
    val pw = new PrintWriter(file)
    var noErrors = false
    try {
      pw.write("device_name,f_name,n_inputs_l0_nonpadded,mults_per_thread,neurons_per_wrg," +
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
      pw.write(f"${cnn.runTimes(0)}%1.5f,${cnn.runTimes(1)}%1.5f,${cnn.runTimes(2)}%1.5f\n")

      for ((liftSingleResult, targetSingleResult) <- cnn.outputsNonPadded zip cnn.targets) {
        //println(liftSingleResult.mkString(", "))
        //println(targetSingleResult.mkString(", "))
        assertArrayEquals(f"The lift output #$inputNo%d is different to the Tensorflow output", targetSingleResult,
          liftSingleResult, precision)
        inputNo = inputNo + 1
      }
      noErrors = true
      print(f"Done. Processed ${cnn.nInputsNonPadded}%d (padded: ${cnn.nLayerInputsPadded(0)}%d")
      for (layerNo <- 1 until cnn.nLayers)
        print(f", ${cnn.nLayerInputsPadded(layerNo)}%d")
      println(f") inputs, the results were equal to targets (precision=$precision%1.4f)")
    }
    finally {
      pw.close()
      if (!noErrors)
        new File(nn.resultsFilename(cnn.pathToResults, cnn.nInputsNonPadded)).delete()
    }*/
  }
}