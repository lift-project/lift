package nn.cnn

import java.io.{File, PrintWriter}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get
import java.util.Calendar

import com.sun.javaws.exceptions.InvalidArgumentException
import com.typesafe.scalalogging.Logger
import nn._
import nn.conv.{Conv, ConvDatasets}
import nn.fc.{FC, FCDatasets}
import nn.mysql.Connector
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
    nn.cnn.mysql.CreateTable()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
    Connector.close()
  }
}

class TestCNN {
  private val logger = Logger(this.getClass)

  val precision: Float = 10f
  val codeVersion: Int = 1

  @Test
  def TestCNN(): Unit = {
    // If rerunsAllowed == False, the experiment will not be rerun if result files from previous runs
    // are found. Otherwise, new results will be added with a datetime timestamp
    val rerunsAllowed: Boolean = true

    val nBatches: Int = 2
    val nChannels: Int = 1
    val kernelStride: Int = 1
    val fcSize: Array[Int] = Array[Int](256, 10)
    var aCNN: CNN = null
    var data: NetDatasetsCollection = null
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
          /* ---------------------------- BUILD NETWORK (BEGIN) ---------------------------- */
          aCNN = new CNN(nConvLayers = 2, nFCLayers = 2,
            inputShape = Shape(nBatches = nBatches, nInputs = nInputs, nChannels = nChannels),
            pathToResults = pathToResults)
          //noinspection ConvertibleToMethodValue
          aCNN.layers(0) = Conv(Conv.Par(_, _, _, _, _, _, _), nn.ReLU, 1, 4, kernelSize, 16,
            inputShape = Shape(nBatches=nBatches, nInputs=nInputs, size=imageSize, nChannels=nChannels),
            kernelSize, kernelStride)
          aCNN.convLayers(0) = aCNN.layers(0).asInstanceOf[Conv]

          //noinspection ConvertibleToMethodValue
          aCNN.layers(1) = Conv(Conv.Par(_, _, _, _, _, _, _), nn.ReLU, elsPerThreadL1, kernelsPerGroupL1,
            inputTileSize, nKernelsL1, inputShape = aCNN.convLayers(0).outputShape.copy(), kernelSize, kernelStride)
          aCNN.convLayers(1) = aCNN.layers(1).asInstanceOf[Conv]

          aCNN.layers(2) = FC(FC.Par, nn.ReLU,
            inputShape = Shape(nInputs = nBatches * nInputs, size =
              aCNN.convLayers(1).outputShape.size * aCNN.convLayers(1).outputShape.size *
                aCNN.convLayers(1).outputShape.nChannels),
            neuronShape = Shape(size = fcSize(0)),
            multsPerThread, neuronsPerWrg)
          aCNN.fcLayers(0) = aCNN.layers(2).asInstanceOf[FC]

          aCNN.layers(3) = FC(FC.Par, nn.ReLU,
            inputShape = Shape(nInputs = nBatches * nInputs, size = fcSize(0)),
            neuronShape = Shape(size = fcSize(1)),
            multsPerThread, neuronsPerWrg)
          aCNN.fcLayers(1) = aCNN.layers(3).asInstanceOf[FC]
          /* ---------------------------- BUILD NETWORK (END) ---------------------------- */

          /* ----------------------------- LOAD DATA (BEGIN) ----------------------------- */
          // Now that we know that layers can be built we the chosen parameters, load the data.
          // Load the data only if it wasn't loaded before for a similar experiment
          if (data == null || data.pathToInputs != pathToInputs || data.nInputs != nInputs)
            data = NetDatasetsCollection(
              pathToInputs = pathToInputs,
              nInputs = nInputs,
              layers = Array[NetDatasets](
                nn.conv.ConvExperiment.loadDatasets(
                  path = pathToInputs,
                  inputFilePrefix = "test_images_n" + nInputs,
                  paramFileInfix = "conv1"),

                nn.conv.ConvExperiment.loadDatasets(
                  path = pathToInputs,
                  paramFileInfix = "conv2"),

                nn.fc.FCExperiment.loadDatasets(
                  path = pathToInputs,
                  paramFileInfix = "mlp1"),

                nn.fc.FCExperiment.loadDatasets(
                  path = pathToInputs,
                  targetFilePrefix = "test_tf_results_n" + nInputs,
                  paramFileInfix = "out")))
          /* ----------------------------- LOAD DATA (END) ----------------------------- */
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
        /* ---------------------------- RUN EXPERIMENT (BEGIN) ---------------------------- */
        // Now that we know that layers can be built and data can be loaded, run the experiment
        logger.info("-----------------------------------------------------------------")
        System.out.println(f"Starting the experiment:\n" + aCNN.configToString)

        val now = Calendar.getInstance().getTime

        for (layerNo <- 0 until aCNN.nLayers) {
          val layer: Layer = aCNN.layers(layerNo)
          val layerData: NetDatasets = data.layers(layerNo)

          /* Padding */
          layer match {
            case cl: Conv => Conv.pad(layerData.asInstanceOf[ConvDatasets].inputs, cl.inputShape)
            case fl: FC =>
              val fcData: FCDatasets = layerData.asInstanceOf[FCDatasets]
              FC.pad(fcData.inputs, fl.inputShape, fcData.weights, fcData.biases, fl.neuronShape)
          }

          /* Execute */
          val (outputsFlat: Array[Float], runtime) =
            Execute(
              layer.localSize(0), layer.localSize(1), layer.localSize(2),
              layer.globalSize(0), layer.globalSize(1), layer.globalSize(2), (true, true))(
              layer.liftFProp,
              layerData match {
                case cd: ConvDatasets => cd.weights
                case fd: FCDatasets => fd.weights.padded},
              layerData match {
                case cd: ConvDatasets => cd.biases
                case fd: FCDatasets => fd.biases.padded},
              layerData match {
                case cd: ConvDatasets => cd.inputs.padded
                case fd: FCDatasets => fd.inputs.padded})
          layer.runtime = runtime
          logger.info(f"Layer $layerNo%d runtime: $runtime%1.5f ms")

          /* Group and unpad */
          layer.groupAndUnpad(outputsFlat, layerData)

          /* Pass outputs to the next layer*/
          if (layerNo != aCNN.nLayers - 1) {
            (layer, aCNN.layers(layerNo + 1)) match {
                case (_: Conv, _: FC) =>
                  // If the current layer is convolutional and the next one is fully connected
                  data.layers(layerNo + 1).asInstanceOf[FCDatasets].inputs.nonPadded =
                    // (n_batches, n_inputs) -> (n_batches * n_inputs)
                    layerData.asInstanceOf[ConvDatasets].outputs.nonPadded.flatMap(batch => batch.map(
                      // (h, w, n_channels) -> (h * w * n_channels)
                      input => input.map(row => row.flatten).flatten
                ))
                case (_: Conv, _: Conv) =>
                  data.layers(layerNo + 1).asInstanceOf[ConvDatasets].inputs.nonPadded =
                    layerData.asInstanceOf[ConvDatasets].outputs.nonPadded
                case (_: FC, _: FC) =>
                  data.layers(layerNo + 1).asInstanceOf[FCDatasets].inputs.nonPadded =
                    layerData.asInstanceOf[FCDatasets].outputs.nonPadded
                case (_: FC, _: Conv) =>
                  throw new java.lang.IllegalArgumentException(
                    "Fully connected layer must not precede a convolutional layer")
            }
          }
        }
        logger.info("")

        /* Check and save results */
        var testFailed: Boolean = false
        val netOutput: Array2D[Float] = data.layers.last.asInstanceOf[FCDatasets].outputs.nonPadded
        val netTarget: Array2D[Float] = data.layers.last.asInstanceOf[FCDatasets].targets.asInstanceOf[Array2D[Float]]
        for ((liftResult, targetResult, input_no) <- (netOutput, netTarget, 0 to netTarget.length).zipped.toList) {
          //        logger.info(f"target $batch_no%d,$input_no%d,$row_no%d,$el_no%d:  " + targetElement.mkString(", "))
          //        logger.info(f"actual $batch_no%d,$input_no%d,$row_no%d,$el_no%d:  " + liftElement.mkString(", "))
          for ((liftElement, targetElement, el_no) <-
               (liftResult, targetResult, 0 to targetResult.length).zipped.toList) {
            try {
              assertEquals("", targetElement, liftElement, precision)
            }
            catch {
              case e: AssertionError =>
                logger.info(f"$input_no%d,$el_no%d,:  " + targetElement + " != " + liftElement)
                testFailed = true
            }
          }
        }
        if (!testFailed)
          logger.info(f"SUCCESS. Processed ${aCNN.inputShape.nBatches * aCNN.inputShape.nInputs}%d inputs, " +
            f"the results were equal to targets (precision=$precision%1.4f).")


        /* JSON */
        if (aCNN.pathToResults != "") {
          var pw: PrintWriter = null
          if (aCNN.pathToResults != "") {
            val file = new File(nn.resultsFilename(aCNN.pathToResults, aCNN.inputShape.nInputs))
            file.getParentFile.mkdirs()
            pw = new PrintWriter(file)
          }
          /* Headers */
          pw.write("device_name, n_batches, n_inputs, n_conv_layers, n_fc_layers," + {
            for (layerNo <- aCNN.convLayers.indices)
              yield f"n_kernels_l$layerNo%d, kernel_size_l$layerNo%d, kernel_stride_l$layerNo%d" +
                f"input_tile_size_l$layerNo%d, input_tile_stride_l$layerNo%d" +
                f"els_per_thread_l$layerNo%d, kernels_per_group_l$layerNo%d"
          }.mkString(", ") + ", " + {
            for (layerNo <- aCNN.fcLayers.indices.map(i => i + aCNN.convLayers.length))
              yield f"input_len_l$layerNo%d_nonpadded, input_len_l$layerNo%d_padded, " +
                f"n_neurons_l$layerNo%d_nonpadded, n_neurons_l$layerNo%d_padded, " +
                f"mults_per_thread_l$layerNo%d, neurons_per_wrg_l$layerNo%d"}.mkString(", ") + ", " + {
            for (layerNo <- 0 until aCNN.nLayers)
              yield f"runtime_l$layerNo%d"}.mkString(", ") + ", tag\n")

          pw.write(nn.deviceName + ", " + f"${aCNN.inputShape.nBatches}%d, ${aCNN.inputShape.nInputs}%d, " +
            f"${aCNN.nConvLayers}%d, ${aCNN.nFCLayers}%d" + {
            for (layerNo <- aCNN.convLayers.indices) yield {
              val c: Conv = aCNN.layers(layerNo).asInstanceOf[Conv]
              f"${c.outputShape.nChannels}%d, ${c.kernelSliding.size}%d, ${c.kernelSliding.stride}%d, " +
                f"${c.inputTiling.size}%d, ${c.inputTiling.stride}%d, " +
                f"${c.elsPerThread}%d, ${c.kernelsPerGroup}%d"
            }}.mkString(", ") + ", " + {
            for (layerNo <- aCNN.fcLayers.indices.map(i => i + aCNN.convLayers.length)) yield {
              val f: FC = aCNN.layers(layerNo).asInstanceOf[FC]
              f"${f.inputShape.size}%d, ${f.inputShape.sizePadded}%d, " +
                f"${f.neuronShape.size}%d, ${f.neuronShape.sizePadded}%d, " +
                f"${f.multsPerThread}%d, ${f.neuronsPerWrg}%d"
            }}.mkString(", ") + ", " + {
              for (layerNo <- 0 until aCNN.nLayers)
                yield f"${aCNN.layers(layerNo).runtime}%1.5f"
          }.mkString(", ") + f", $codeVersion%d\n")

          pw.close()
          if (!testFailed)
            new File(nn.resultsFilename(aCNN.pathToResults, aCNN.inputShape.nInputs)).delete()
        }

        /* SQL */
        val sqlCommand: String = "INSERT INTO lift_results_cnn " +
          "(device_name, n_batches, n_inputs, n_conv_layers, n_fc_layers, " + {
          for (layerNo <- aCNN.convLayers.indices)
            yield f"n_kernels_l$layerNo%d, kernel_size_l$layerNo%d, kernel_stride_l$layerNo%d, " +
              f"input_tile_size_l$layerNo%d, input_tile_stride_l$layerNo%d, " +
              f"els_per_thread_l$layerNo%d, kernels_per_group_l$layerNo%d"
        }.mkString(", ") + ", " + {
          for (layerNo <- aCNN.fcLayers.indices.map(i => i + aCNN.convLayers.length))
            yield f"input_len_l$layerNo%d_nonpadded, input_len_l$layerNo%d_padded, " +
              f"n_neurons_l$layerNo%d_nonpadded, n_neurons_l$layerNo%d_padded, " +
              f"mults_per_thread_l$layerNo%d, neurons_per_wrg_l$layerNo%d"}.mkString(", ") + ", " + {
          for (layerNo <- 0 until aCNN.nLayers)
            yield f"runtime_l$layerNo%d"}.mkString(", ") +
          ", ran, success, experiment_id, datetime) VALUES (" +
          "'" + nn.deviceName + "', " + f"${aCNN.inputShape.nBatches}%d, ${aCNN.inputShape.nInputs}%d, " +
          f"${aCNN.nConvLayers}%d, ${aCNN.nFCLayers}%d, " + {
          for (layerNo <- aCNN.convLayers.indices) yield {
            val c: Conv = aCNN.layers(layerNo).asInstanceOf[Conv]
            f"${c.outputShape.nChannels}%d, ${c.kernelSliding.size}%d, ${c.kernelSliding.stride}%d, " +
              f"${c.inputTiling.size}%d, ${c.inputTiling.stride}%d, " +
              f"${c.elsPerThread}%d, ${c.kernelsPerGroup}%d"
          }}.mkString(", ") + ", " + {
          for (layerNo <- aCNN.fcLayers.indices.map(i => i + aCNN.convLayers.length)) yield {
            val f: FC = aCNN.layers(layerNo).asInstanceOf[FC]
            f"${f.inputShape.size}%d, ${f.inputShape.sizePadded}%d, " +
              f"${f.neuronShape.size}%d, ${f.neuronShape.sizePadded}%d, " +
              f"${f.multsPerThread}%d, ${f.neuronsPerWrg}%d"
          }}.mkString(", ") + ", " + {
          for (layerNo <- 0 until aCNN.nLayers)
            yield f"${aCNN.layers(layerNo).runtime}%1.5f"
        }.mkString(", ") + f", true, ${!testFailed}%b, $codeVersion%d, " +
          f"'${new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(now)}%s');"
        println(sqlCommand)
        Connector.statement.execute(sqlCommand)
        /* ---------------------------- RUN EXPERIMENT (END) ---------------------------- */
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

  def recordFailureInSQL(e: Exception): Unit = {
    /* SQL */
    Connector.statement.execute("INSERT INTO lift_results_cnn (ran, abort_reason) VALUES " +
      "(false, '" + e.getClass.getSimpleName + ": " + e.getMessage + "')")
  }
}