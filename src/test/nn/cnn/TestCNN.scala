package nn.cnn

import java.io.{File, PrintWriter}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get
import java.util.{Calendar, Date}

import com.typesafe.scalalogging.Logger
import nn._
import nn.conv.{Conv, ConvDatasets}
import nn.fc.{FC, FCDatasets}
import nn.mysql.Connector
import nn.poolScala.ScalaPool
import opencl.executor.{Execute, Executor}
import org.junit.Assert.assertEquals
import org.junit.{AfterClass, BeforeClass, Test}

import util.control.Breaks._

/**
  * Created by s1569687 on 01/03/17.
  */
object TestCNN {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init(/*avus*/1, 0)
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
  val codeVersion: Int = 15
  val reruns: Int = 1

  @Test
  def TestFC(): Unit = {
    for (_ <- 0 to reruns)
      Test(ExperimentsSet(nKernelsL1Range = List(2),
        kernelSizeRange = List(2),
        inputTileSizeRange = (kernelSize, _) => List(kernelSize),
        elsPerThreadL1Range = _ => List(1),
        kernelsPerGroupL1Range = _ => List(1),
        multsPerThreadRange = imageSize => List(1) ++ (2 to imageSize * imageSize by 2),
        //multsPerThreadRange = imageSize => List(16),
        neuronsPerWrgRange = fcSize => List(1) ++ (2 to fcSize by 2), nKernelsL0 = 2,
        imageSizeRange = List(8, 16, 32, 64, 128/*, 256, 512*/),
        neuronsL1Range = List(16, 32, 64, 128, 256, 512),
        kernelsPerGroupL0 = 2,
        nInputsRange = List(8, 16, 32, 64, 128/*, 256, 512, 1024, 2048, 2048*/)))

//        continueFrom = cnn.Experiment(
//          nKernelsL1= 2,
//          kernelSize= 2,
//          inputTileSize= 2,
//          elsPerThreadL1= 1,
//          kernelsPerGroupL1= 1,
//          multsPerThread= 38,
//          neuronsPerWrg= 10,
//          imageSize= 8,
//          neuronsL1= 512,
//          nInputs = 64))
  }


  def Test(e: cnn.ExperimentsSet,
           continueFrom: Experiment = null): Unit = {
    // If rerunsAllowed == False, the experiment will not be rerun if result files from previous runs
    // are found. Otherwise, new results will be added with a datetime timestamp
    val rerunsAllowed: Boolean = true

    val kernelStride: Int = 1
    var aCNN: CNN = null
    var data: NetDatasetsCollection = null
    var initParams: Layer.InitParameters = null
    var now: Date = null
    var skip: Boolean = continueFrom != null
    var currentLayer: Int = 0
    for {
      //rerun <- 1 until 10
      nBatches <- e.nBatchesRange
      nKernelsL1 <- e.nKernelsL1Range
      kernelSize <- e.kernelSizeRange
      imageSize <- e.imageSizeRange
      neuronsL1 <- e.neuronsL1Range

      pathToInputs = ExperimentsSet.getPathToInputs(imageSize)
      pathToParams = ExperimentsSet.getPathToParams(nKernelsL1, kernelSize, imageSize, neuronsL1)
      if exists(get(pathToInputs))
      pathToResults = ExperimentsSet.getPathToResults(pathToParams)
      // Results dir doesn't exist (then create it) or it does, but reruns are allowed:
      if rerunsAllowed || {if (!exists(get(pathToResults))) {
        createDirectory(get(pathToResults))
        true} else false}
      nInputs <- e.nInputsRange
      // Results dir exists, but doesn't contain results of this experiment or it does, but reruns are allowed:
      if rerunsAllowed || new File(pathToResults).listFiles.toList.count {
        file => file.getName.endsWith("_n%d.csv".format(nInputs))} == 0
      // Load datasets once for all experiments (across all multsPerThread and neuronsPerWrg)
      if ExperimentsSet.datasetsExist(pathToParams)

      inputTileSize <- e.inputTileSizeRange(kernelSize, imageSize)
      elsPerThreadL1 <- e.elsPerThreadL1Range(kernelSize)
      kernelsPerGroupL1 <- e.kernelsPerGroupL1Range(nKernelsL1)
      multsPerThread <- e.multsPerThreadRange(imageSize)
      neuronsPerWrg <- e.neuronsPerWrgRange(neuronsL1)

      if {
        if (skip) {
          if (continueFrom.nKernelsL1 == nKernelsL1 &&
            continueFrom.kernelSize == kernelSize &&
            continueFrom.inputTileSize == inputTileSize &&
            continueFrom.elsPerThreadL1 == elsPerThreadL1 &&
            continueFrom.kernelsPerGroupL1 == kernelsPerGroupL1 &&
            continueFrom.multsPerThread == multsPerThread &&
            continueFrom.neuronsPerWrg == neuronsPerWrg &&
            continueFrom.imageSize == imageSize &&
            continueFrom.neuronsL1 == neuronsL1 &&
            continueFrom.nInputs == nInputs) {
            skip = false
            true
          } else false
        } else true}

      // Check if CNN can be created with the selected parameters (e.g. if WrgGroupSize < maxWrgGroupSize)
      if {
        try {
          /* ---------------------------- BUILD NETWORK (BEGIN) ---------------------------- */
          now = Calendar.getInstance().getTime
          aCNN = new CNN(nConvLayers = 2, nFCLayers = 2, //14,
            inputShape = Shape(nBatches = nBatches, nInputs = nInputs, size=imageSize, nChannels = e.nChannels),
            pathToResults = pathToResults)

          //noinspection ConvertibleToMethodValue
          initParams = Conv.InitParameters(0, Conv.Par(_, _, _, _, _, _, _), nn.ReLU,
            elsPerThread=kernelSize, e.kernelsPerGroupL0, kernelSize, e.nKernelsL0,
            inputShape = Shape(nBatches=nBatches, nInputs=nInputs, size=imageSize, nChannels = e.nChannels),
            kernelSize, kernelStride)
          currentLayer = 0
          aCNN.layers(currentLayer) = Conv(initParams.asInstanceOf[Conv.InitParameters])
          aCNN.convLayers(0) = aCNN.layers(currentLayer).asInstanceOf[Conv]

          currentLayer = currentLayer + 1
          //noinspection ConvertibleToMethodValue
          initParams = Conv.InitParameters(1, Conv.Par(_, _, _, _, _, _, _), nn.ReLU, elsPerThreadL1, kernelsPerGroupL1,
            inputTileSize, nKernelsL1, inputShape = aCNN.convLayers(0).outputShape.copy(), kernelSize, kernelStride)
          aCNN.layers(currentLayer) = Conv(initParams.asInstanceOf[Conv.InitParameters])
          aCNN.convLayers(1) = aCNN.layers(currentLayer).asInstanceOf[Conv]


          /* Pooling */
          val aPool: ScalaPool = ScalaPool()
          aPool.conv2SizeInOneDimension = imageSize - (kernelSize - kernelStride) * 2
          aPool.mlpInputlenL2NonVerified = nKernelsL1 * aPool.conv2SizeInOneDimension * aPool.conv2SizeInOneDimension
          
          if (aPool.mlpInputlenL2NonVerified >= aPool.mlpInputLenLimit) {
            // Get minimum pool size
            aPool.poolSize = Math.ceil(aPool.mlpInputlenL2NonVerified.toFloat / aPool.mlpInputLenLimit).toInt
            // Find the pool size that is greater than the minimum pool size and that is a factor of inputlen
            while (aPool.conv2SizeInOneDimension % aPool.poolSize != 0)
              aPool.poolSize = aPool.poolSize + 1 
            if (aPool.conv2SizeInOneDimension.toFloat % aPool.poolSize != 0)
              throw new java.lang.IllegalArgumentException()
            aPool.mlpInputlenL2 = (aPool.mlpInputlenL2NonVerified.toFloat / Math.pow(aPool.poolSize, 2)).toInt
            aPool.nChannels = nKernelsL1
          }
          if (aPool.poolSize > 0) {
            currentLayer = currentLayer + 1
            aCNN.nPoolLayers = aCNN.nPoolLayers + 1
            aCNN.nLayers = aCNN.nLayers + 1
            aCNN.layers(currentLayer) = aPool
          }
          /* Pooling */

          currentLayer = currentLayer + 1
          initParams = FC.InitParameters(2, FC.Par, nn.ReLU,
            inputShape = Shape(nBatches = 1, nInputs = nBatches * nInputs, size =
              {
                if (aCNN.nPoolLayers == 0)
                  aCNN.convLayers(1).outputShape.size * aCNN.convLayers(1).outputShape.size *
                    aCNN.convLayers(1).outputShape.nChannels
                else
                  aPool.mlpInputlenL2
              }),
            neuronShape = Shape(size = neuronsL1),
            multsPerThread, neuronsPerWrg)
          aCNN.layers(currentLayer) = FC(initParams.asInstanceOf[FC.InitParameters])
          aCNN.fcLayers(0) = aCNN.layers(currentLayer).asInstanceOf[FC]

          currentLayer = currentLayer + 1
          initParams = FC.InitParameters(3, FC.Par, nn.ReLU,
            inputShape = Shape(nBatches = 1, nInputs = nBatches * nInputs, size = neuronsL1),
            neuronShape = Shape(size = 10),
            multsPerThread = 1, neuronsPerWrg = 1)
          aCNN.layers(currentLayer) = FC(initParams.asInstanceOf[FC.InitParameters])
          aCNN.fcLayers(1) = aCNN.layers(currentLayer).asInstanceOf[FC]
          /* ---------------------------- BUILD NETWORK (END) ---------------------------- */

          /* ----------------------------- LOAD DATA (BEGIN) ----------------------------- */
          // Now that we know that layers can be built we the chosen parameters, load the data.
          // Load the data only if it wasn't loaded before for a similar experiment
          if (data == null || data.pathToParams != pathToParams || data.nInputs != nInputs)
            data = NetDatasetsCollection(
              pathToParams = pathToParams,
              nInputs = nInputs,
              layers = Array[NetDatasets](
                nn.conv.ConvExperiment.loadDatasets(
                  paramsPath = pathToParams,
                  inputShape = aCNN.convLayers(0).inputShape,
                  inputsPath = pathToInputs + "/test_images_n" + nInputs + ".binary",
                  paramFileInfix = "conv1",
                  kernelSliding = aCNN.convLayers(0).kernelSliding),

                nn.conv.ConvExperiment.loadDatasets(
                  paramsPath = pathToParams,
                  inputShape = aCNN.convLayers(1).inputShape,
                  paramFileInfix = "conv2",
                  kernelSliding = aCNN.convLayers(1).kernelSliding),

                nn.fc.FCExperiment.loadDatasets(
                  paramsPath = pathToParams,
                  inputShape = aCNN.fcLayers(0).inputShape,
                  paramFileInfix = "mlp1",
                  neuronShape = aCNN.fcLayers(0).neuronShape),

                nn.fc.FCExperiment.loadDatasets(
                  paramsPath = pathToParams,
                  inputShape = aCNN.fcLayers(1).inputShape,
                  targetFilePrefix = "test_tf_results_n" + nInputs,
                  paramFileInfix = "out",
                  neuronShape = aCNN.fcLayers(1).neuronShape)))
          /* ----------------------------- LOAD DATA (END) ----------------------------- */
          true
        }
        catch {
          case e: java.lang.IllegalArgumentException =>
            logger.warn("-----------------------------------------------------------------")
            val msg = f"Layer $currentLayer%d: EXCEPTION: java.lang.IllegalArgumentException\n" +
              cnn.configToString(aCNN.inputShape.nBatches, aCNN.inputShape.nInputs,
                aCNN.inputShape.size, aCNN.nLayers) + e.getMessage
            logger.warn(msg)
            recordFailureInSQL(msg, initParams, now)
            logger.warn("SKIPPING EXPERIMENT.")
//            if (currentLayer != 0)
//              throw e
            false
        }
      }
    } {
      try {
        /* ---------------------------- RUN EXPERIMENT (BEGIN) ---------------------------- */
        // Now that we know that layers can be built and data can be loaded, run the experiment
        logger.info("-----------------------------------------------------------------")
        System.out.println(f"Starting the experiment:\n" + aCNN.configToString)

        now = Calendar.getInstance().getTime

        for (layerNo <- 0 until 2/*aCNN.nLayers 14*/) {
          val layer: Layer = aCNN.layers(layerNo)
          val layerData: NetDatasets = data.layers({
            if (aCNN.nPoolLayers > 0)
              layerNo match {
                case 0 => 0
                case 1 => 1
                case 2 => 2
                case 3 => 2
                case 4 => 3
              }
            else
              layerNo
          })
          breakable {
            layer match {
              case poolLayer: ScalaPool =>
                poolLayer.run()
                logger.info(f"Layer $layerNo%d (pooling) completed")
                break
              case _ =>
            }


            /* Padding */
            layer match {
              case cL: Conv => Conv.pad(layerData.asInstanceOf[ConvDatasets].inputs, cL.inputShape)
              case fL: FC =>
                val fcData: FCDatasets = layerData.asInstanceOf[FCDatasets]
                FC.pad(fcData.inputs, fL.inputShape, fcData.weights, fcData.biases, fL.neuronShape)
            }

            /* Execute */
            val (outputsFlat: Array[Float], runtime) =
              Execute(
                layer.localSize(0), layer.localSize(1), layer.localSize(2),
                layer.globalSize(0), layer.globalSize(1), layer.globalSize(2), (true, true))(
                layer.liftFProp,
                layerData match {
                  case cd: ConvDatasets => cd.weights
                  case fd: FCDatasets => fd.weights.padded
                },
                layerData match {
                  case cd: ConvDatasets => cd.biases
                  case fd: FCDatasets => fd.biases.padded
                },
                layerData match {
                  case cd: ConvDatasets => cd.inputs.padded
                  case fd: FCDatasets => fd.inputs.padded
                })
            layer.runtime = runtime
            logger.info(f"Layer $layerNo%d runtime: $runtime%1.5f ms")

            /* Group and unpad */
            layer.groupAndUnpad(outputsFlat, layerData)
          }

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
              case (_: Conv, poolLayer: ScalaPool) =>
                poolLayer.inputs = layerData.asInstanceOf[ConvDatasets].outputs.nonPadded
              case (poolLayer: ScalaPool, _: FC) =>
                data.layers(/*no +1 on purpose */layerNo).asInstanceOf[FCDatasets].inputs.nonPadded =
                  poolLayer.outputs.flatMap(batch => batch.map(
                    // (h, w, n_channels) -> (h * w * n_channels)
                    input => input.map(row => row.flatten).flatten
                ))
              case (_: Conv, _: Conv) =>
                data.layers(layerNo + 1).asInstanceOf[ConvDatasets].inputs.nonPadded =
                  layerData.asInstanceOf[ConvDatasets].outputs.nonPadded
              case (_: FC, _: FC) =>
                data.layers({
                  if (aCNN.nPoolLayers > 0)
                    layerNo
                  else
                    layerNo + 1
                }).asInstanceOf[FCDatasets].inputs.nonPadded =
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
        var testVerified: Boolean = false
        if (data.layers.last.asInstanceOf[FCDatasets].targets.asInstanceOf[Array2D[Float]] == Array.empty) {
          testVerified = true
          val netOutput: Array2D[Float] = data.layers.last.asInstanceOf[FCDatasets].outputs.nonPadded
          val netOutputTemp: Array2D[Float] = data.layers(2).asInstanceOf[FCDatasets].outputs.nonPadded
          val netTarget: Array2D[Float] = data.layers.last.asInstanceOf[FCDatasets].targets.asInstanceOf[Array2D[Float]]
          for ((liftResult, targetResult, input_no) <- (netOutput, netTarget, 0 to netTarget.length).zipped.toList) {
            //          logger.info(f"target $input_no%d:  " + targetResult.mkString(", "))
            //          logger.info(f"actual $input_no%d:  " + liftResult.mkString(", "))
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
        }
        if (!testFailed)
          logger.info(f"SUCCESS. Processed ${aCNN.inputShape.nBatches * aCNN.inputShape.nInputs}%d inputs, " +
            f"the results were equal to targets (precision=$precision%1.4f).")
        else
          if (!testVerified)
            logger.info(f"NOT VERIFIED. Processed ${aCNN.inputShape.nBatches * aCNN.inputShape.nInputs}%d inputs.")
          else
            throw new AssertionError


        /* JSON */
        if (aCNN.pathToResults != "" && ! testFailed)
          recordInJSON(aCNN, now)

        /* SQL */
        recordInSQL(aCNN, testRan = true, testFailed, testVerified, now)
        /* ---------------------------- RUN EXPERIMENT (END) ---------------------------- */
      } catch {
        case e: opencl.executor.Executor.ExecutorFailureException =>
          val msg = "EXCEPTION: opencl.executor.Executor.ExecutorFailureException" + e.getMessage
          logger.warn(msg)
          recordFailureInSQL(msg, aCNN, now)
          throw e
        case e: opencl.executor.DeviceCapabilityException =>
          val msg = "EXCEPTION: opencl.executor.DeviceCapabilityException" + e.getMessage
          logger.warn(msg)
          recordFailureInSQL(msg, aCNN, now)
//        case e: lift.arithmetic.NotEvaluableException =>
//          val msg = "EXCEPTION: lift.arithmetic.NotEvaluableException" + e.getMessage
//          logger.warn(msg)
//          recordFailureInSQL(msg, aCNN, now)
      }
    }
  }

  def recordFailureInSQL(exceptionMsg: String, iP: Layer.InitParameters, runDate: Date): Unit = {
    Connector.statement.execute("INSERT INTO lift_results_cnn " +
      "(device_name, n_batches, n_inputs, image_size, " + {
      iP match {
        case _: Conv.InitParameters =>
          f"n_kernels_l${iP.layerNo}%d, kernel_size_l${iP.layerNo}%d, kernel_stride_l${iP.layerNo}%d, " +
            f"input_tile_size_l${iP.layerNo}%d, " +
            f"els_per_thread_l${iP.layerNo}%d, kernels_per_group_l${iP.layerNo}%d, "
        case _: FC.InitParameters =>
          f"input_len_l${iP.layerNo}%d_nonpadded, n_neurons_l${iP.layerNo}%d_nonpadded, " +
            f"mults_per_thread_l${iP.layerNo}%d, neurons_per_wrg_l${iP.layerNo}%d, "
      }
    } + "ran, abort_reason, experiment_id, datetime) VALUES (" +
      "'" + nn.deviceName + "', " + f"${iP.inputShape.nBatches}%d, ${iP.inputShape.nInputs}%d, " +
      f"${iP.inputShape.size}%d, " + {
      iP match {
        case cIP: Conv.InitParameters =>
          f"${cIP.nKernels}%d, ${cIP.kernelSize}%d, ${cIP.kernelStride}%d, " +
            f"${cIP.inputTileSize}%d, ${cIP.elsPerThread}%d, ${cIP.kernelsPerGroup}%d, "
        case fcIP: FC.InitParameters =>
          f"${fcIP.inputShape.size}%d, ${fcIP.neuronShape.size}%d, " +
            f"${fcIP.multsPerThread}%d, ${fcIP.neuronsPerWrg}%d, "
      }
    } + f"false, '" + exceptionMsg + f"', $codeVersion%d, " +
      f"'${new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(runDate)}%s');")
  }

  def recordFailureInSQL(exceptionMsg: String, aCNN: CNN, runDate: Date): Unit =
    recordInSQL(aCNN, testRan = true, testFailed= false, testVerified = true, runDate, exceptionMsg)

  def recordInSQL(aCNN: CNN, testRan: Boolean, testFailed: Boolean = false, testVerified: Boolean, runDate: Date,
                  exceptionMsg: String = ""): Unit = {
    val cmd = "INSERT INTO lift_results_cnn " +
      "(device_name, n_batches, n_inputs, image_size, n_conv_layers, n_fc_layers, " + {
      for (layerNo <- aCNN.convLayers.indices)
        yield f"n_kernels_l$layerNo%d, kernel_size_l$layerNo%d, kernel_stride_l$layerNo%d, " +
          f"input_tile_size_l$layerNo%d, input_tile_stride_l$layerNo%d, " +
          f"els_per_thread_l$layerNo%d, kernels_per_group_l$layerNo%d"
    }.mkString(", ") + ", " + {
      for (layerNo <- aCNN.fcLayers.indices.map(i => i + aCNN.convLayers.length))
        yield f"input_len_l$layerNo%d_nonpadded, input_len_l$layerNo%d_padded, " +
          f"n_neurons_l$layerNo%d_nonpadded, n_neurons_l$layerNo%d_padded, " +
          f"mults_per_thread_l$layerNo%d, neurons_per_wrg_l$layerNo%d"
    }.mkString(", ") + ", " + {
      for (layerNo <- 0 until aCNN.nLayers - aCNN.nPoolLayers)
        yield f"runtime_l$layerNo%d"
    }.mkString(", ") +
      ", ran, verified, success, abort_reason, experiment_id, datetime, pool_size, l1_out_len_original, l1_out_len_new) VALUES (" +
      "'" + nn.deviceName + "', " + f"${aCNN.inputShape.nBatches}%d, ${aCNN.inputShape.nInputs}%d, " +
      f"${aCNN.inputShape.size}%d, " + f"${aCNN.nConvLayers}%d, ${aCNN.nFCLayers}%d, " + {
      for (layerNo <- aCNN.convLayers.indices) yield {
        val c: Conv = aCNN.layers(layerNo).asInstanceOf[Conv]
        f"${c.outputShape.nChannels}%d, ${c.kernelSliding.size}%d, ${c.kernelSliding.stride}%d, " +
          f"${c.inputTiling.size}%d, ${c.inputTiling.stride}%d, " +
          f"${c.elsPerThread}%d, ${c.kernelsPerGroup}%d"
      }
    }.mkString(", ") + ", " + {
      for (layerNo <- aCNN.fcLayers.indices) yield {
        val f: FC = aCNN.fcLayers(layerNo)
        f"${f.inputShape.size}%d, ${f.inputShape.sizePadded}%d, " +
          f"${f.neuronShape.size}%d, ${f.neuronShape.sizePadded}%d, " +
          f"${f.multsPerThread}%d, ${f.neuronsPerWrg}%d"
      }
    }.mkString(", ") + ", " + {
      for (layerNo <- 0 until aCNN.nLayers)
        yield {
          aCNN.layers(layerNo) match {
            case pl: ScalaPool => ""
            case _ => f"${aCNN.layers(layerNo).runtime}%1.5f, "
          }
        }
    }.mkString("") + f" $testRan%b, $testVerified%b, ${!testFailed}%b, '" + exceptionMsg.replaceAll("'", "''") + f"', $codeVersion%d, " +
      f"'${new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(runDate)}%s'," +
      f"${
        if (aCNN.nPoolLayers > 0)
          aCNN.layers(2).asInstanceOf[ScalaPool].poolSize
        else
          0
      }%d, ${
        if (aCNN.nPoolLayers > 0)
          aCNN.layers(2).asInstanceOf[ScalaPool].mlpInputlenL2NonVerified
        else
          0
      }%d, ${
        if (aCNN.nPoolLayers > 0)
          aCNN.layers(2).asInstanceOf[ScalaPool].mlpInputlenL2
        else
          0
      }%d);"
    println(cmd)
    Connector.statement.execute(cmd)
  }


  def recordInJSON(aCNN: CNN, runDate: Date): Unit = {
    var pw: PrintWriter = null
    val file = new File(nn.resultsFilename(aCNN.pathToResults, aCNN.inputShape.nInputs))
    file.getParentFile.mkdirs()
    pw = new PrintWriter(file)
    /* Headers */
    pw.write("device_name, n_batches, n_inputs, image_size, n_conv_layers, n_fc_layers," + {
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
      f"${aCNN.inputShape.size}%d, " +
      f"${aCNN.nConvLayers}%d, ${aCNN.nFCLayers}%d" + {
      for (layerNo <- aCNN.convLayers.indices) yield {
        val c: Conv = aCNN.layers(layerNo).asInstanceOf[Conv]
        f"${c.outputShape.nChannels}%d, ${c.kernelSliding.size}%d, ${c.kernelSliding.stride}%d, " +
          f"${c.inputTiling.size}%d, ${c.inputTiling.stride}%d, " +
          f"${c.elsPerThread}%d, ${c.kernelsPerGroup}%d"
      }}.mkString(", ") + ", " + {
      for (layerNo <- aCNN.fcLayers.indices) yield {
        val f: FC = aCNN.fcLayers(layerNo)
        f"${f.inputShape.size}%d, ${f.inputShape.sizePadded}%d, " +
          f"${f.neuronShape.size}%d, ${f.neuronShape.sizePadded}%d, " +
          f"${f.multsPerThread}%d, ${f.neuronsPerWrg}%d"
      }}.mkString(", ") + ", " + {
      for (layerNo <- 0 until aCNN.nLayers)
        yield f"${aCNN.layers(layerNo).runtime}%1.5f"
    }.mkString(", ") + f", $codeVersion%d\n")

    pw.close()
  }
}