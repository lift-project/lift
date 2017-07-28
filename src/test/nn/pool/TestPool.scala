package nn.pool

import java.io.{File, PrintWriter}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get
import java.util.Calendar

import com.typesafe.scalalogging.Logger
import nn.mysql.Connector
import nn.{PaddedArray, Shape}
import opencl.executor.{Execute, Executor}
import org.junit.Assert.assertEquals
import org.junit.{AfterClass, BeforeClass, Test}

/**
  * Created by s1569687 on 01/03/17.
  */
object TestPool {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init(/*monaco*/0, 0)
    nn.pool.mysql.CreateTable
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
    Connector.close()
  }
}

class TestPool {
  private val logger = Logger(this.getClass)

  val precision: Float = 0.01f
  val codeVersion: Int = 1

  @Test
  def TestPool(): Unit = {
    // If rerunsAllowed == False, the experiment will not be rerun if result files from previous runs
    // are found. Otherwise, new results will be added with a datetime timestamp
    val rerunsAllowed: Boolean = true
    /** Build an array of experimental parameters filtering out the experiments that where
      * already run or for which the data was not provided; load data for all experiments
      */

    var aPool: Pool = null
    val nLayers: Int = 2
    val nBatches: Int = 2
    val inputChannels: Int = 8
    for {
      //rerun <- 1 until 10
      kernelSize <- 4 until 64 by 4
      imageSize <- 8 until 128 by 8
      pathToInputs = Experiment.getPathToInputs(Shape(w=kernelSize, h=kernelSize), Shape(w=imageSize, h=imageSize))
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
      inputTileSize <- kernelSize until imageSize by 4 // kernelSize
      // Check if CNN can be created with the selected parameters (e.g. if WrgGroupSize < maxWrgGroupSize)
      if {
        try {
          aPool = new Pool(Pool.Par, Array(nn.max, nn.max), elsPerThreadL1, inputTileSize,
            nLayers, nBatches, nInputs, Array(inputChannels, inputChannels),
            {
              val inputShape: Array[Shape] = Array.fill[Shape](nLayers)(Shape())
              inputShape(0) = Shape(w=imageSize, h=imageSize, ch=inputChannels)
              inputShape
            }, {for (_ <- 0 until nLayers) yield Shape(w=kernelSize, h=kernelSize)}.toArray,
            Array(kernelSize / 2, kernelSize), pathToInputs, pathToResults, Experiment.loadDatasets, aPool)
          true
        }
        catch {
          case e: java.lang.IllegalArgumentException =>
            logger.warn("-----------------------------------------------------------------")
            logger.warn(f"Cannot start the experiment\n" + poolToString(aPool))
            logger.warn(e.getMessage)
            logger.warn("SKIPPING EXPERIMENT.")
            recordFailureInSQL(e)
            false
        }
      }
    } {
      try {
        singleTest(aPool)
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

  def singleTest(aPool: Pool): Unit = {
    logger.info("-----------------------------------------------------------------")
    System.out.println(f"Starting the experiment\n" + poolToString(aPool))

    val now = Calendar.getInstance().getTime

    for (layerNo <- 0 until aPool.nLayers) {
      aPool.updateInputs(layerNo)
      /* Padding */
      aPool.padInputs(layerNo)
      val (outputFlat: Array[Float], runtime) =
        Execute(
          aPool.localSize(0)(layerNo), aPool.localSize(1)(layerNo), aPool.localSize(2)(layerNo),
          aPool.globalSize(0)(layerNo), aPool.globalSize(1)(layerNo), aPool.globalSize(2)(layerNo), (true, true))(

          aPool.liftPool(
            aPool.activationFun(layerNo), aPool.inputShape(layerNo),
            aPool.kernelShape(layerNo), aPool.kernelStride(layerNo),
            aPool.nInputs, aPool.nBatches, aPool.nInChannels(layerNo),
            Tile(els_per_thread=aPool.elsPerThread(layerNo),
              input_tile_size=aPool.inputTileSize(layerNo),
              input_tile_stride=aPool.inputTileStep(layerNo),
              n_input_tiles_per_dim=aPool.nTilesPerDim(layerNo),
              n_kwindows_per_tile_per_dim=aPool.nWindowsPerTilePerDim(layerNo))),

          aPool.inputs(layerNo).padded)
      aPool.runTimes(layerNo) = runtime

      /* Group and unpad */
      aPool.outputs = {
        def getShapedOutputs = nn.group(outputFlat, (aPool.nBatches, aPool.nInputs, aPool.outputShape(layerNo).hPadded,
          aPool.outputShape(layerNo).wPadded, aPool.outputShape(layerNo).ch)).map(
          batch => batch.map(
            input => input.map(
              row => row.slice(0, aPool.outputShape(layerNo).wNonPadded)
            ).slice(0, aPool.outputShape(layerNo).hNonPadded)
          ))
        if (aPool.outputs == null) Array(PaddedArray(getShapedOutputs)) else
          aPool.outputs :+ PaddedArray(getShapedOutputs)
      }
      logger.info(f"Layer $layerNo%d runtime: $runtime%1.5f ms")

    }
    logger.info("")

    /* Check and save results */
    var testFailed: Boolean = false
    for {
      (liftBatch, targetBatch, batch_no) <-
        (aPool.outputs.last.nonPadded, aPool.targets, 0 to aPool.targets.length).zipped.toList
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
      logger.info(f"SUCCESS. Processed ${aPool.nInputs}%d inputs, the results were equal to targets " +
        f"(precision=$precision%1.4f).")


    /* JSON */
    if (aPool.pathToResults != "") {
      var pw: PrintWriter = null
      if (aPool.pathToResults != "") {
        val file = new File(nn.resultsFilename(aPool.pathToResults, aPool.nInputs))
        file.getParentFile.mkdirs()
        pw = new PrintWriter(file)
      }
      pw.write("device_name,n_batches,n_inputs,image_size,n_in_channels," + {
        for (layerNo <- 0 until aPool.nLayers) yield f"kernel_size_l$layerNo%d"
      }.mkString(",") + "," + {
        for (layerNo <- 0 until aPool.nLayers) yield f"kernel_stride_l$layerNo%d"
      }.mkString(",") + "," +
        "input_tile_size_l1,input_tile_step_l1,els_per_thread_l1," + {
        for (layerNo <- 0 until aPool.nLayers) yield f"runtime_l$layerNo%d"
      }.mkString(",") + ",tag\n")

      pw.write(nn.deviceName + "," + f"${aPool.nBatches}%d,${aPool.nInputs}%d,${aPool.inputShape(0).s}%d, " +
        f"${aPool.inputShape(0).ch}%d")
      for (layerNo <- 0 until aPool.nLayers)
        pw.write(f"${aPool.kernelShape(layerNo).s}%d,")
      for (layerNo <- 0 until aPool.nLayers)
        pw.write(f"${aPool.kernelStride(layerNo)}%d,")
      pw.write(f"${aPool.inputTileSize(aPool.nLayers - 1)}%d,${aPool.inputTileStep(aPool.nLayers - 1)}%d," +
        f"${aPool.elsPerThread(aPool.nLayers - 1)}%d,")
      for (layerNo <- 0 until aPool.nLayers)
        pw.write(f",${aPool.runTimes(layerNo)}%1.5f")
      pw.write(f",$codeVersion\n")
      pw.close()
      if (!testFailed)
        new File(nn.resultsFilename(aPool.pathToResults, aPool.nInputs)).delete()
    }

    /* SQL */
    Connector.statement.execute("INSERT INTO lift_results_pool " +
      "(batches, images, imagesize, input_channels, kernelsize_l0, kernelsize_l1, kernelstride_l0, kernelstride_l1, " +
      "elsperthread_l0, elsperthread_l1, inputtilesize_l0, " +
      "inputtilesize_l1, ran, success, runtime_l0, runtime_l1, experiment_id, datetime) " +
      f"VALUES (${aPool.nBatches}%d, ${aPool.nInputs}%d, ${aPool.inputShape(0).s}%d, ${aPool.inputShape(0).ch}%d," +
      f"${aPool.kernelShape(0).s}%d, ${aPool.kernelShape(1).s}, " +
      f"${aPool.kernelStride(0)}%d, ${aPool.kernelStride(1)}, " +
      f"${aPool.elsPerThread(0)}%d, ${aPool.elsPerThread(1)}%d, " +
      f"${aPool.inputTileSize(0)}%d, ${aPool.inputTileSize(1)}%d, true, " +
      f"${!testFailed}%b, ${aPool.runTimes(0)}%1.5f, ${aPool.runTimes(1)}%1.5f, $codeVersion%d, " +
      f"'${new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(now)}%s');")
  }

  def recordFailureInSQL(e: Exception): Unit = {
    /* SQL */
    Connector.statement.execute("INSERT INTO lift_results_pool (ran, abort_reason) VALUES " +
      "(false, '" + e.getClass.getSimpleName + ": " + e.getMessage + "')")
  }
}