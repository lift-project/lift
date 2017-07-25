package nn.cnn

import java.io.{File, PrintWriter}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get

import com.typesafe.scalalogging.Logger
import nn.{PaddedArray, Shape}
import opencl.executor.{Execute, Executor}
import org.junit.{AfterClass, BeforeClass, Test}
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals

/**
  * Created by s1569687 on 01/03/17.
  */
object TestCNN {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init(/*monaco*/0, 0)
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestCNN {
  private val logger = Logger(this.getClass)

  // TODO: increase precision
  val precision: Float = 0.01f

  //@Test
  def Sanity_CNN(): Unit = {
    val (lift_result: Array[Float], runtime) = Execute(1,1)(
      CNN.Seq(2, 3, nn.Linear), input_K, input_b, input_X)

    logger.info(f"\n1. Convolution sanity check.\nRuntime: $runtime%1.5f ms")

    val lift_result3d = nn.group(lift_result, (gold.length, gold.head.length, gold.head.head.length))
    for ((gold2d, lift_result2d) <- gold zip lift_result3d) {
      logger.info(lift_result2d.flatten.mkString(", "))
      logger.info(gold2d.flatten.mkString(", "))
      /*for ((gold1d, lift_result1d) <- gold2d zip lift_result2d) {
        assertArrayEquals(gold1d, lift_result1d, precision)
      }*/
    }
  }

  //@Test
  /*def Sanity_CNN_Par(): Unit = {
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

    logger.info("-----------------------------------------------------------------")

    for (cnn <- cnns)
      singleTest(cnn)
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
    val nLayers: Int = 2
    val nBatches: Int = 2
    for {
      nKernelsL1 <- 8 until 48 by 4//16 until 17 by 4
      kernelSize <- 4 until 64 by 4 //8 until 64 by 4
      imageSize <- 64 until 65 by 4//8 until 64 by 8//16 until 512 by 16
      pathToInputs = Experiment.getPathToInputs(
        nKernelsL1, Shape(w=kernelSize, h=kernelSize), Shape(w=imageSize, h=imageSize))
      if exists(get(pathToInputs))
      pathToResults = Experiment.getPathToResults(pathToInputs)
      // Results dir doesn't exist (then create it) or it does, but reruns are allowed:
      if rerunsAllowed || {if (!exists(get(pathToResults))) {
        createDirectory(get(pathToResults))
        true} else false}
      nInputs <- 8 until 9/*104*/ by 32//520 by 32 //512 by 32
      // Results dir exists, but doesn't contain results of this experiment or it does, but reruns are allowed:
      if rerunsAllowed || new File(pathToResults).listFiles.toList.count {
        file => file.getName.endsWith("_n%d.csv".format(nInputs))} == 0
      // Load datasets once for all experiments (across all multsPerThread and neuronsPerWrg)
      if Experiment.datasetsExist(pathToInputs)
      elsPerThreadL1 <- List(1) ++ (4 until 16 by 4)
      kernelsPerGroupL1 <- List(1) ++ (1 until nKernelsL1 by 4)// until nKernelsL1 //1 until nKernelsL1
      inputTileSize <- kernelSize until imageSize by 4 // kernelSize
      // Check if CNN can be created with the selected parameters (e.g. if WrgGroupSize < maxWrgGroupSize)
      if {
        try {
          cnn = new CNN(CNN.Par, Array(nn.ReLU, nn.ReLU), elsPerThreadL1, kernelsPerGroupL1, inputTileSize,
            nLayers, nBatches, nInputs, Array(16, nKernelsL1), Array(1, 16),
            {
              val inputShape: Array[Shape] = Array.fill[Shape](nLayers)(Shape())
              inputShape(0) = Shape(w=imageSize, h=imageSize, ch=1)
              inputShape
            }, {for (_ <- 0 until nLayers) yield Shape(w=kernelSize, h=kernelSize)}.toArray,
            pathToInputs, pathToResults, Experiment.loadDatasets, cnn)
//          logger.info(f"Prepared the experiment (nKernelsL1=$nKernelsL1%d, " +
//            f"inputTileSize=$inputTileSize%d, elsPerThreadL1=$elsPerThreadL1%d, " +
//            f"kernelsPerGroupL1=$kernelsPerGroupL1%d,\nkernelSize=$kernelSize%d, " +
//            f"nBatches=$nBatches%d, nInputs=$nInputs%d).")
          true
        }
        catch {
          case e: java.lang.IllegalArgumentException =>
            logger.warn("-----------------------------------------------------------------")
            logger.warn(f"Cannot start the experiment (nKernelsL1=$nKernelsL1%d, " +
              f"inputTileSize=$inputTileSize%d, elsPerThreadL1=$elsPerThreadL1%d, " +
              f"kernelsPerGroupL1=$kernelsPerGroupL1%d,\nkernelSize=$kernelSize%d, " +
              f"nBatches=$nBatches%d, nInputs=$nInputs%d).")
            logger.warn(e.getMessage)
            logger.warn("SKIPPING EXPERIMENT.")
            false
        }
      }
    } {
      //cnn.setData(Experiment.loadDatasets(nInputs, pathToInputs))
      try {
        singleTest(cnn)
      } catch {
        case e: opencl.executor.Executor.ExecutorFailureException =>
          logger.warn("EXCEPTION: opencl.executor.Executor.ExecutorFailureException")
          logger.warn(e.getMessage)
        case e: opencl.executor.DeviceCapabilityException =>
          logger.warn("EXCEPTION: opencl.executor.DeviceCapabilityException")
          logger.warn(e.getMessage)
      }
    }
  }

  def singleTest(cnn: CNN): Unit = {
    logger.info("-----------------------------------------------------------------")
    System.out.println(f"Starting the experiment (inputTileSize(first)=${cnn.inputTileSize(0)}%d, " +
      f"inputTileSize(last)=${cnn.inputTileSize(cnn.nLayers - 1)}%d, " +
      f"nKernels(first)=${cnn.nKernels(0)}%d, " +
      f"nKernels(last)=${cnn.nKernels(cnn.nLayers - 1)}%d,\n" +
      f"elsPerThread(first)=${cnn.elsPerThread(0)}%d, " +
      f"elsPerThread(last)=${cnn.elsPerThread(cnn.nLayers - 1)}%d, " +
      f"kernelsPerGroup(first)=${cnn.kernelsPerGroup(0)}%d, " +
      f"kernelsPerGroup(last)=${cnn.kernelsPerGroup(cnn.nLayers - 1)}%d, " +
      f"kernelSize=${cnn.kernelShape(cnn.nLayers - 1).s}%d, " +
      f"nBatches=${cnn.nBatches}%d, nInputs=${cnn.nInputs}%d, " +
      f"imageSize=${cnn.inputShape(0).s}%d).")

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
      logger.info(f"Layer $layerNo%d runtime: $runtime%1.5f ms")

    }
    logger.info("")

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
        }.mkString(",") + ",tag\n")

        pw.write(nn.deviceName + "," + f"${cnn.nBatches}%d,${cnn.nInputs}%d,")
        for (layerNo <- 0 until cnn.nLayers)
          pw.write(f"${cnn.nKernels(layerNo)}%d,")
        for (layerNo <- 0 until cnn.nLayers)
          pw.write(f"${cnn.kernelShape(layerNo).s}%d,")
        pw.write(f"${cnn.inputTileSize(cnn.nLayers - 1)}%d,${cnn.inputTileStep(cnn.nLayers - 1)}%d,${cnn.elsPerThread(cnn.nLayers - 1)}%d," +
          f"${cnn.kernelsPerGroup(1)}%d")
        for (layerNo <- 0 until cnn.nLayers)
          pw.write(f",${cnn.runTimes(layerNo)}%1.5f")
        pw.write(",3\n")
      }

      for {
        (liftBatch, targetBatch, batch_no) <-
          (cnn.outputs.last.nonPadded, cnn.targets, 0 to cnn.targets.length).zipped.toList
        (liftResult, targetResult, input_no) <- (liftBatch, targetBatch, 0 to targetBatch.length).zipped.toList
        (liftRow, targetRow, row_no) <- (liftResult, targetResult, 0 to targetResult.length).zipped.toList
        (liftElement, targetElement, el_no) <- (liftRow, targetRow, 0 to targetRow.length).zipped.toList
      } {
//        logger.info(f"target $batch_no%d,$input_no%d,$row_no%d,$el_no%d:  " + targetElement.mkString(", "))
//        logger.info(f"actual $batch_no%d,$input_no%d,$row_no%d,$el_no%d:  " + liftElement.mkString(", "))
        var testFailed: Boolean = false
        for {(liftElementKernel, targetElementKernel, elk_no) <-
             (liftElement, targetElement, 0 to targetElement.length).zipped.toList} {
          try {
//            assertArrayEquals(f"Batch $batch_no%d input $input_no%d row $row_no%d element $el_no%d: " +
//              f"the lift output is different to the target output", targetElement, liftElement, precision)
            assertEquals("", targetElementKernel, liftElementKernel, precision)
          }
          catch {
            case e: AssertionError =>
              logger.info(f"$batch_no%d,$input_no%d,$row_no%d,$el_no%d,$elk_no%d:  " +
                          targetElementKernel + " != " + liftElementKernel)
              testFailed = true
          }
        }
        if (testFailed)
          throw new AssertionError()
      }
      noErrors = true
      logger.info(f"SUCCESS. Processed ${cnn.nInputs}%d inputs, the results were equal to targets " +
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