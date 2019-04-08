package nn.conv

import java.io.{File, PrintWriter}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get
import java.util.Calendar

import com.typesafe.scalalogging.Logger
import nn.conv.versions.{Conv1, Conv2, Conv3, Conv4}
import nn.{PaddedArray, Shape}
import opencl.executor.Compile
//import nn.mysql.Connector
import opencl.executor.{Execute, Executor}
import org.junit.{AfterClass, BeforeClass, Test, Ignore}
import org.junit.Assert.assertArrayEquals

/**
  * Created by s1569687 on 01/03/17.
  */
//@Ignore
object TestConv {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init(0, 0)
    // MySQL is disabled in this version
//    nn.conv.mysql.CreateTable()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
    // MySQL is disabled in this version

    //    Connector.close()
  }
}

@Ignore
class TestConv {
  private val logger = Logger(this.getClass)

  val precision: Float = 1f
  val codeVersion: Int = 1

  // The tests below are temporarily deprecated since they were not fixed to comply with new changes
  @Test
  def Sanity_Conv(): Unit = {
    val (lift_result: Array[Float], runtime) = Execute(1,1)[Array[Float]](
      Conv2.Seq(3, 3, nn.Linear), input_K, input_b, input_X(0))

    logger.info(f"\n1. Convolution sanity check.\nRuntime: $runtime%1.5f ms")

    val lift_result3d = patterns.nn.group(lift_result, (gold.length, gold.head.length, gold.head.head.length))
    for ((gold2d, lift_result2d) <- gold zip lift_result3d) {
      logger.info(lift_result2d.flatten.mkString(", "))
      logger.info(gold2d.flatten.mkString(", "))
//      for ((gold1d, lift_result1d) <- gold2d zip lift_result2d) {
//        assertArrayEquals(gold1d, lift_result1d, precision)
//      }
    }

    println(Compile(Conv2.Seq(3, 3, nn.Linear)))
  }

  /*@Test
  def Sanity_CNN_Par(): Unit = {
    /** Build an array of experimental parameters filtering out the experiments that where
      * already run or for which the data was not provided; load data for all experiments
      */

    var conv: CNN = null
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
          conv = new CNN(CNN.Par, Array(nn.ReLU, nn.ReLU), elsPerThreadL1, kernelsPerGroupL1, inputTileSize,
            PaddedArray(input_X), Array(input_K), Array(input_b), gold, "")
          true
        }
        catch {
          case e: java.lang.IllegalArgumentException =>
            false
        }
      }
    } yield conv

    logger.info("-----------------------------------------------------------------")

    for (conv <- convs)
      singleTest(conv)
  }

  @Test
  def TestConv(): Unit = {
    // If rerunsAllowed == False, the experiment will not be rerun if result files from previous runs
    // are found. Otherwise, new results will be added with a datetime timestamp
    val rerunsAllowed: Boolean = true
    /** Build an array of experimental parameters filtering out the experiments that where
      * already run or for which the data was not provided; load data for all experiments
      */

    var aConv: Conv = null
    val nLayers: Int = 2
    val nBatches: Int = 2
    for {
      rerun <- 1 until 10
      nKernelsL1 <- 8 until 48 by 4//16 until 17 by 4
      kernelSize <- 4 until 64 by 4 //8 until 64 by 4
      imageSize <- 8 until 64 by 4//8 until 64 by 8//16 until 512 by 16
      pathToInputs = Experiment.getPathToInputs(
        nKernelsL1, Shape(size=kernelSize, size=kernelSize), Shape(size=imageSize, size=imageSize))
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
      // Check if Conv can be created with the selected parameters (e.g. if WrgGroupSize < maxWrgGroupSize)
      if {
        try {
          aConv = new Conv(Conv.Par, Array(nn.ReLU, nn.ReLU), elsPerThreadL1, kernelsPerGroupL1, inputTileSize,
            nLayers, nBatches, nInputs, Array(16, nKernelsL1), Array(1, 16),
            {
              val inputShape: Array[Shape] = Array.fill[Shape](nLayers)(Shape())
              inputShape(0) = Shape(size=imageSize, size=imageSize, nChannels=1)
              inputShape
            }, {for (_ <- 0 until nLayers) yield Shape(size=kernelSize, size=kernelSize)}.toArray,
            pathToInputs, pathToResults, Experiment.loadDatasets, aConv)
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
            recordFailureInSQL(e)
            false
        }
      }
    } {
      try {
        singleTest(aConv)
      } catch {
        case e: opencl.executor.Executor.ExecutorFailureException =>
          logger.warn("EXCEPTION: opencl.executor.Executor.ExecutorFailureException")
          logger.warn(e.getMessage)
          recordFailureInSQL(e)
        case e: opencl.executor.DeviceCapabilityException =>
          logger.warn("EXCEPTION: opencl.executor.DeviceCapabilityException")
          logger.warn(e.getMessage)
          recordFailureInSQL(e)
      }
    }
  }

  def singleTest(aconv: Conv): Unit = {
    logger.info("-----------------------------------------------------------------")
    System.out.println(f"Starting the experiment (inputTileSize(first)=${aconv.inputTileSize(0)}%d, " +
      f"inputTileSize(last)=${aconv.inputTileSize(aconv.nLayers - 1)}%d, " +
      f"nKernels(first)=${aconv.nKernels(0)}%d, " +
      f"nKernels(last)=${aconv.nKernels(aconv.nLayers - 1)}%d,\n" +
      f"elsPerThread(first)=${aconv.elsPerThread(0)}%d, " +
      f"elsPerThread(last)=${aconv.elsPerThread(aconv.nLayers - 1)}%d, " +
      f"kernelsPerGroup(first)=${aconv.kernelsPerGroup(0)}%d, " +
      f"kernelsPerGroup(last)=${aconv.kernelsPerGroup(aconv.nLayers - 1)}%d, " +
      f"kernelSize=${aconv.kernelShape(aconv.nLayers - 1).s}%d, " +
      f"nBatches=${aconv.nBatches}%d, nInputs=${aconv.nInputs}%d, " +
      f"imageSize=${aconv.inputShape(0).s}%d).")

    val now = Calendar.getInstance().getTime

    for (layerNo <- 0 until aconv.nLayers) {
      aconv.updateInputs(layerNo)
      /* Padding */
      aconv.padInputs(layerNo)
      val (outputFlat: Array[Float], runtime) =
        Execute(
          aconv.localSize(0)(layerNo), aconv.localSize(1)(layerNo), aconv.localSize(2)(layerNo),
          aconv.globalSize(0)(layerNo), aconv.globalSize(1)(layerNo), aconv.globalSize(2)(layerNo), (true, true))(

          aconv.liftConv(
            aconv.activationFun(layerNo), aconv.inputShape(layerNo), aconv.kernelShape(layerNo),
            aconv.nInputs, aconv.nBatches, aconv.nInChannels(layerNo), aconv.nKernels(layerNo),
            Tile(kernels_per_group=aconv.kernelsPerGroup(layerNo), els_per_thread=aconv.elsPerThread(layerNo),
              inputTileSize=aconv.inputTileSize(layerNo), inputTileSlideStep=aconv.inputTileStep(layerNo),
              nInputTilesPerDim=aconv.nTilesPerDim(layerNo),
              n_windows_per_tile_per_dim=aconv.nWindowsPerTilePerDim(layerNo))),

          aconv.kWeights(layerNo), aconv.kBiases(layerNo), aconv.inputs(layerNo).padded)
      aconv.runTimes(layerNo) = runtime

      /* Group and unpad */
      aconv.outputs = {
        def getShapedOutputs = nn.group(outputFlat, (aconv.nBatches, aconv.nInputs, aconv.outputShape(layerNo).hPadded,
          aconv.outputShape(layerNo).wPadded, aconv.outputShape(layerNo).ch)).map(
          batch => batch.map(
            input => input.map(
              row => row.slice(0, aconv.outputShape(layerNo).wNonPadded)
            ).slice(0, aconv.outputShape(layerNo).hNonPadded)
          ))
        if (aconv.outputs == null) Array(PaddedArray(getShapedOutputs)) else
          aconv.outputs :+ PaddedArray(getShapedOutputs)
      }
      logger.info(f"Layer $layerNo%d runtime: $runtime%1.5f ms")

    }
    logger.info("")

    /* Check and save results */
    var testFailed: Boolean = false
    for {
      (liftBatch, targetBatch, batch_no) <-
        (aconv.outputs.last.nonPadded, aconv.targets, 0 to aconv.targets.length).zipped.toList
      (liftResult, targetResult, input_no) <- (liftBatch, targetBatch, 0 to targetBatch.length).zipped.toList
      (liftRow, targetRow, row_no) <- (liftResult, targetResult, 0 to targetResult.length).zipped.toList
      (liftElement, targetElement, el_no) <- (liftRow, targetRow, 0 to targetRow.length).zipped.toList
    } {
//        logger.info(f"target $batch_no%d,$input_no%d,$row_no%d,$el_no%d:  " + targetElement.mkString(", "))
//        logger.info(f"actual $batch_no%d,$input_no%d,$row_no%d,$el_no%d:  " + liftElement.mkString(", "))
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
    }
    if (!testFailed)
      logger.info(f"SUCCESS. Processed ${aconv.nInputs}%d inputs, the results were equal to targets " +
        f"(precision=$precision%1.4f).")


    /* JSON */
    if (aconv.pathToResults != "") {
      var pw: PrintWriter = null
      if (aconv.pathToResults != "") {
        val file = new File(nn.resultsFilename(aconv.pathToResults, aconv.nInputs))
        file.getParentFile.mkdirs()
        pw = new PrintWriter(file)
      }
      pw.write("device_name,n_batches,n_inputs," + {
        for (layerNo <- 0 until aconv.nLayers) yield f"n_kernels_l$layerNo%d"
      }.mkString(",") + "," + {
        for (layerNo <- 0 until aconv.nLayers) yield f"kernel_size_l$layerNo%d"
      }.mkString(",") + "," +
        "input_tile_size_l1,input_tile_step_l1,els_per_thread_l1,kernels_per_group_l1," + {
        for (layerNo <- 0 until aconv.nLayers) yield f"runtime_l$layerNo%d"
      }.mkString(",") + ",tag\n")

      pw.write(nn.deviceName + "," + f"${aconv.nBatches}%d,${aconv.nInputs}%d,")
      for (layerNo <- 0 until aconv.nLayers)
        pw.write(f"${aconv.nKernels(layerNo)}%d,")
      for (layerNo <- 0 until aconv.nLayers)
        pw.write(f"${aconv.kernelShape(layerNo).s}%d,")
      pw.write(f"${aconv.inputTileSize(aconv.nLayers - 1)}%d,${aconv.inputTileStep(aconv.nLayers - 1)}%d," +
        f"${aconv.elsPerThread(aconv.nLayers - 1)}%d,${aconv.kernelsPerGroup(1)}%d")
      for (layerNo <- 0 until aconv.nLayers)
        pw.write(f",${aconv.runTimes(layerNo)}%1.5f")
      pw.write(f",$codeVersion\n")
      pw.close()
      if (!testFailed)
        new File(nn.resultsFilename(aconv.pathToResults, aconv.nInputs)).delete()
    }

    /* SQL */
    Connector.statement.execute("INSERT INTO lift_results_conv " +
      "(batches, images, imagesize, kernels_l0, kernels_l1, kernelsize_l0, kernelsize_l1, " +
      "elsperthread_l0, elsperthread_l1, kernelspergroup_l0, kernelspergroup_l1, inputtilesize_l0, " +
      "inputtilesize_l1, ran, success, runtime_l0, runtime_l1, code_version, datetime) " +
      f"VALUES (${aconv.nBatches}%d, ${aconv.nInputs}%d, ${aconv.inputShape(0).s}%d, ${aconv.nKernels(0)}%d, " +
      f"${aconv.nKernels(1)}%d, ${aconv.kernelShape(0).s}%d, ${aconv.kernelShape(1).s}, ${aconv.elsPerThread(0)}%d, " +
      f"${aconv.elsPerThread(1)}%d, ${aconv.kernelsPerGroup(0)}%d, ${aconv.kernelsPerGroup(1)}%d, " +
      f"${aconv.inputTileSize(0)}%d, ${aconv.inputTileSize(1)}%d, true, ${!testFailed}%b, ${aconv.runTimes(0)}%1.5f, " +
      f"${aconv.runTimes(1)}%1.5f, $codeVersion%d, " +
      f"'${new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(now)}%s');")
  }*/

  def recordFailureInSQL(e: Exception): Unit = {
    /* SQL */
    // Commented out due to MySQL being disabled in this version
//    Connector.statement.execute("INSERT INTO lift_results_conv (ran, abort_reason) VALUES " +
//      "(false, '" + e.getClass.getSimpleName + ": " + e.getMessage + "')")
  }
}