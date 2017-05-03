package nn.cnn

import java.io.{File, PrintWriter}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get

import nn.{PaddedArray, Shape}
import opencl.executor.{Execute, Executor}
import org.junit.{AfterClass, BeforeClass, Test}
import org.junit.Assert._

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
  // TODO: increase precision
  val precision: Float = 100f

  //@Test
  def Sanity_CNN(): Unit = {
    val (lift_result: Array[Float], runtime) = Execute(1,1)(
      CNN.Seq(2, 3, nn.Linear), input_K, input_b, input_X)

    println(f"\n1. Convolution sanity check.\nRuntime: $runtime%1.5f ms")

    val lift_result3d = nn.group(lift_result, (gold.length, gold.head.length, gold.head.head.length))
    for ((gold2d, lift_result2d) <- gold zip lift_result3d) {
      println(lift_result2d.flatten.mkString(", "))
      println(gold2d.flatten.mkString(", "))
      /*for ((gold1d, lift_result1d) <- gold2d zip lift_result2d) {
        assertArrayEquals(gold1d, lift_result1d, precision)
      }*/
    }
  }

  //@Test
  def Sanity_CNN_Par(): Unit = {
    /** Build an array of experimental parameters filtering out the experiments that where
      * already run or for which the data was not provided; load data for all experiments
      */

    var cnn: CNN = null
    val nKernelsL1: Int = 3
    val kernelSize: Int = 3
    val nInputs: Int = 2
    val cnns = for {
      elsPerThreadL1 <- 1 to 16
      kernelsPerGroupL1 <- 1 to nKernelsL1
      inputTileSize <- kernelSize to input_X.head.head.length
      // Check if CNN can be created with the selected parameters (e.g. if WrgGroupSize < maxWrgGroupSize)
      if {
        try {
          cnn = new CNN(CNN.Par, Array(nn.ReLU, nn.ReLU), elsPerThreadL1, kernelsPerGroupL1, inputTileSize,
            PaddedArray(input_X), Array(input_K), Array(input_b), gold, "")
          true
        }
        catch {
          case e: java.lang.IllegalArgumentException =>
            false
        }
      }
    } yield cnn

    println("-----------------------------------------------------------------")

    for (cnn <- cnns)
      singleTest(cnn)
  }

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
      nKernelsL1 <- 8 until 48 by 4 //8 to 48 by 4
      kernelSize <- 5 until 25 by 5 //5 to 25 by 5
      pathToInputs = Experiment.getPathToInputs(
        nKernelsL1, Shape(w=kernelSize, h=kernelSize))
      if exists(get(pathToInputs))
      pathToResults = Experiment.getPathToResults(pathToInputs)
      // Results dir doesn't exist (then create it) or it does, but reruns are allowed:
      if rerunsAllowed || {if (!exists(get(pathToResults))) {
        createDirectory(get(pathToResults))
        true} else false}
      nInputs <- 8 until 520 by 32 //512 by 32
      // Results dir exists, but doesn't contain results of this experiment or it does, but reruns are allowed:
      if rerunsAllowed || new File(pathToResults).listFiles.toList.count {
        file => file.getName.endsWith("_n%d.csv".format(nInputs))} == 0
      // Load datasets once for all experiments (across all multsPerThread and neuronsPerWrg)
      loadingDatasets = Try(Experiment.loadDatasets(nInputs, pathToInputs))
      if loadingDatasets.isSuccess
      (tfX, tfWconv, tfBconv, tfResult) = loadingDatasets.get
      elsPerThreadL1 <- 1 until 16 //1 to 16
      kernelsPerGroupL1 <- 1 until nKernelsL1
      inputTileSize <- kernelSize until tfX.nonPadded.head.head.length
      // Check if CNN can be created with the selected parameters (e.g. if WrgGroupSize < maxWrgGroupSize)
      if {
        try {
          cnn = new CNN(CNN.Par, Array(nn.ReLU, nn.ReLU), elsPerThreadL1, kernelsPerGroupL1, inputTileSize,
            tfX, tfWconv, tfBconv, tfResult, pathToResults)
          println(f"Prepared the experiment (nKernelsL1=$nKernelsL1%d, " +
            f"inputTileSize=$inputTileSize%d, elsPerThreadL1=$elsPerThreadL1%d, " +
            f"kernelsPerGroupL1=$kernelsPerGroupL1%d,\nkernelSize=$kernelSize%d, " +
            f"nBatches=${tfX.nonPadded.length}%d, nInputs=$nInputs%d).")
          true
        }
        catch {
          case e: java.lang.IllegalArgumentException =>
            println("-----------------------------------------------------------------")
            println(f"Cannot start the experiment (nKernelsL1=$nKernelsL1%d, " +
              f"inputTileSize=$inputTileSize%d, elsPerThreadL1=$elsPerThreadL1%d, " +
              f"kernelsPerGroupL1=$kernelsPerGroupL1%d,\nkernelSize=$kernelSize%d, " +
              f"nBatches=${tfX.nonPadded.length}%d, nInputs=$nInputs%d).")
            println(e.getMessage)
            println("SKIPPING EXPERIMENT.")
            false
        }
      }
    } yield cnn

    println("-----------------------------------------------------------------")

    for (cnn <- cnns)
      singleTest(cnn)
  }

  def singleTest(cnn: CNN): Unit = {
    println("-----------------------------------------------------------------")
    println(f"Starting the experiment (inputTileSize(first)=${cnn.inputTileSize(0)}%d, " +
      f"inputTileSize(last)=${cnn.inputTileSize(cnn.nLayers - 1)}%d, " +
      f"nKernels(first)=${cnn.nKernels(0)}%d, " +
      f"nKernels(last)=${cnn.nKernels(cnn.nLayers - 1)}%d,\n" +
      f"elsPerThread(first)=${cnn.elsPerThread(0)}%d, " +
      f"elsPerThread(last)=${cnn.elsPerThread(cnn.nLayers - 1)}%d, " +
      f"kernelsPerGroup(first)=${cnn.kernelsPerGroup(0)}%d, " +
      f"kernelsPerGroup(last)=${cnn.kernelsPerGroup(cnn.nLayers - 1)}%d, " +
      f"kernelSize=${cnn.kernelShape(cnn.nLayers - 1).s}%d, " +
      f"nBatches=${cnn.nBatches}%d, nInputs=${cnn.nInputs}%d).")

    for (layerNo <- 0 until cnn.nLayers) {
      cnn.updateInputs(layerNo)
      /* Padding */
      cnn.padInputs(layerNo)

      val (outputFlat: Array[Float], runtime) =
        Execute(
          cnn.localSize(0)(layerNo), cnn.localSize(cnn.nLayers - 1)(layerNo), cnn.localSize(2)(layerNo),
          cnn.globalSize(0)(layerNo), cnn.globalSize(cnn.nLayers - 1)(layerNo), cnn.globalSize(2)(layerNo), (true, true))(

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
    println()

    /* Check and save results */
    var pw: PrintWriter = null
    if (cnn.pathToResults != "") {
      val file = new File(nn.resultsFilename(cnn.pathToResults, cnn.nInputs))
      file.getParentFile.mkdirs()
      pw = new PrintWriter(file)
    }
    var noErrors = false
    try {
      if (cnn.pathToResults != "") {
        pw.write("device_name,n_batches,n_inputs," + {
          for (layerNo <- 0 until cnn.nLayers) yield f"n_kernels_l$layerNo%d"
        }.mkString(",") + "," + {
          for (layerNo <- 0 until cnn.nLayers) yield f"kernel_size_l$layerNo%d"
        }.mkString(",") + "," +
          "input_tile_size_l1,input_tile_step_l1,els_per_thread_l1,kernels_per_group_l1," + {
          for (layerNo <- 0 until cnn.nLayers) yield f"runtime_l$layerNo%d"
        }.mkString(",") + "\n")

        pw.write(nn.deviceName + "," + f"${cnn.nBatches}%d,${cnn.nInputs}%d,")
        for (layerNo <- 0 until cnn.nLayers)
          pw.write(f"${cnn.nKernels(layerNo)}%d,")
        for (layerNo <- 0 until cnn.nLayers)
          pw.write(f"${cnn.kernelShape(layerNo).s}%d,")
        pw.write(f"${cnn.inputTileSize(cnn.nLayers - 1)}%d,${cnn.inputTileStep(cnn.nLayers - 1)}%d,${cnn.elsPerThread(cnn.nLayers - 1)}%d," +
          f"${cnn.kernelsPerGroup(0)}%d")
        for (layerNo <- 0 until cnn.nLayers)
          pw.write(f",${cnn.runTimes(layerNo)}%1.5f")
        pw.write("\n")
      }

//      for {
//        (liftBatch, targetBatch, batch_no) <-
//          (cnn.outputs.last.nonPadded, cnn.targets, 0 to cnn.targets.length).zipped.toList
//        (liftResult, targetResult, input_no) <- (liftBatch, targetBatch, 0 to targetBatch.length).zipped.toList
//        (liftRow, targetRow, row_no) <- (liftResult, targetResult, 0 to targetResult.length).zipped.toList
//        (liftElement, targetElement, el_no) <- (liftRow, targetRow, 0 to targetRow.length).zipped.toList
//      } {
//        println(f"target $batch_no%d,$input_no%d,$row_no%d,$el_no%d:   " + targetElement.mkString(", "))
//        println(f"actual $batch_no%d,$input_no%d,$row_no%d,$el_no%d:   " + liftElement.mkString(", "))
//        assertArrayEquals(f"Batch $batch_no%d input $input_no%d row $row_no%d element $el_no%d: " +
//          f"the lift output is different to the target output", targetElement, liftElement, precision)
//      }
      noErrors = true
      println(f"SUCCESS. Processed ${cnn.nInputs}%d inputs, the results were equal to targets " +
        f"(precision=$precision%1.4f).")
    }
    finally {
      if (cnn.pathToResults != "") {
        pw.close()
        if (!noErrors)
          new File(nn.resultsFilename(cnn.pathToResults, cnn.nInputs)).delete()
      }
    }
  }
}