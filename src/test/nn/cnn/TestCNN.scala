package nn.cnn

import java.io.{File, PrintWriter}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get
import java.util.Calendar

import com.typesafe.scalalogging.Logger
import ir.ast.{FunDecl, UserFun}
import nn.conv.{Conv, SlidingWindowConfig}
import nn.mysql.Connector
import nn.{Layer, PaddedArray, Shape}
import opencl.executor.{Execute, Executor}
import org.junit.Assert.assertEquals
import org.junit.{AfterClass, BeforeClass, Test}

/**
  * Created by s1569687 on 01/03/17.
  */
object TestCNN {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init(/*monaco*/0, 0)
    nn.conv.mysql.CreateTable()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
    Connector.close()
  }
}

class TestCNN {
  private val logger = Logger(this.getClass)

  val precision: Float = 1f
  val codeVersion: Int = 1

  @Test
  def TestCNN(): Unit = {
    // If rerunsAllowed == False, the experiment will not be rerun if result files from previous runs
    // are found. Otherwise, new results will be added with a datetime timestamp
    val rerunsAllowed: Boolean = true
    /** Build an array of experimental parameters filtering out the experiments that where
      * already run or for which the data was not provided; load data for all experiments
      */

    val aCNN: Array[Layer] = new Array[Layer](4)
    val nLayers: Int = 2
    val nBatches: Int = 2
    val nChannels: Int = 1
    val kernelStride: Int = 1
    for {
      //rerun <- 1 until 10
      nKernelsL1 <- 8 until 48 by 4//16 until 17 by 4
      kernelSize <- 4 until 64 by 4 //8 until 64 by 4
      imageSize <- 8 until 64 by 4//8 until 64 by 8//16 until 512 by 16
      pathToInputs = Experiment.getPathToInputs(nKernelsL1, kernelSize, imageSize)
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
      inputTileSize <- kernelSize until imageSize by 4 // kernelSize
      elsPerThreadL1 <- List(1) ++ (4 until 16 by 4)
      kernelsPerGroupL1 <- List(1) ++ (1 until nKernelsL1 by 4)
      multsPerThread <- 4 until 5 by 2
      neuronsPerWrg <- 4 until 5 by 5
      // Check if CNN can be created with the selected parameters (e.g. if WrgGroupSize < maxWrgGroupSize)
      if {
        try {
          // TODO: Why can't I pass Conv.Par as an argument directly?
          val convPar: (UserFun, Shape, SlidingWindowConfig, Int, SlidingWindowConfig, Int, Int) => FunDecl = Conv.Par
          /* ---------------------------- BUILD NETWORK (BEGIN) ---------------------------- */
          aCNN(0) = Conv(convPar, nn.ReLU, 1, 4, kernelSize, 16,
            Shape(nBatches=nBatches, nInputs=nInputs, size=imageSize, nChannels=nChannels), kernelSize, kernelStride)
          aCNN(1) = Conv(convPar, nn.ReLU, elsPerThreadL1, kernelsPerGroupL1, inputTileSize, nKernelsL1,
            aCNN(0).asInstanceOf[Conv].outputShape.copy(), kernelSize, kernelStride)
          /* ---------------------------- BUILD NETWORK (END) ---------------------------- */
          true
        }
        catch {
          case e: java.lang.IllegalArgumentException =>
            logger.warn("-----------------------------------------------------------------")
            logger.warn(e.getMessage)
            logger.warn("SKIPPING EXPERIMENT.")
            recordFailureInSQL(e)
            false
        }
      }
    } {
      try {
        singleTest(aCNN)
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
    System.out.println(f"Starting the experiment:\n" + aconv.configToString)

    val now = Calendar.getInstance().getTime

    for (layerNo <- 0 until aconv.nLayers) {
      aconv.updateInputs(layerNo)
      /* Padding */
      aconv.padInputs(layerNo)
      val (outputFlat: Array[Float], runtime) =
        Execute(
          aconv.localSize(0)(layerNo), aconv.localSize(1)(layerNo), aconv.localSize(2)(layerNo),
          aconv.globalSize(0)(layerNo), aconv.globalSize(1)(layerNo), aconv.globalSize(2)(layerNo), (true, true))(

          aconv.liftCNN(
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
      "inputtilesize_l1, ran, success, runtime_l0, runtime_l1, experiment_id, datetime) " +
      f"VALUES (${aconv.nBatches}%d, ${aconv.nInputs}%d, ${aconv.inputShape(0).s}%d, ${aconv.nKernels(0)}%d, " +
      f"${aconv.nKernels(1)}%d, ${aconv.kernelShape(0).s}%d, ${aconv.kernelShape(1).s}, ${aconv.elsPerThread(0)}%d, " +
      f"${aconv.elsPerThread(1)}%d, ${aconv.kernelsPerGroup(0)}%d, ${aconv.kernelsPerGroup(1)}%d, " +
      f"${aconv.inputTileSize(0)}%d, ${aconv.inputTileSize(1)}%d, true, ${!testFailed}%b, ${aconv.runTimes(0)}%1.5f, " +
      f"${aconv.runTimes(1)}%1.5f, $codeVersion%d, " +
      f"'${new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(now)}%s');")
  }

  def recordFailureInSQL(e: Exception): Unit = {
    /* SQL */
    Connector.statement.execute("INSERT INTO lift_results_conv (ran, abort_reason) VALUES " +
      "(false, '" + e.getClass.getSimpleName + ": " + e.getMessage + "')")
  }
}