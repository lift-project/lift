/*
 * Some code lines here are disabled since they depend on the code that is only available in the "nn" branch and in a
 * private fork of Lift.
 * Disabled functionality mainly refers to testing and exploring the search space of optimisational parameters.
 * Actual expressions for neural layers and associated initialization logic is still functional.
 * For full functionality and code examples, contact Naums Mogers (naums.mogers@ed.ac.uk).
 */
package nn.cnn

import java.io._
import java.util.{Calendar, Date}

import com.typesafe.scalalogging.Logger
import nn._
import nn.cnn.Experiment.{pathToInputs, pathToParams, pathToTargets, verifyOutputs}
import nn.conv.ConvDatasets
import nn.fc.{FC, FCDatasets}
//import nn.mysql.Connector
//import nn.poolScala.ScalaPool
import opencl.executor.{Execute, Executor}
import org.junit.{AfterClass, BeforeClass, Ignore}

import scala.util.control.Breaks._

/**
  * TestCNN().test() is the main function for testing CNNs. It needs to be given experimental parameters
  * to test, so a wrapper is needed. Since TestCNN() is meant to be generic for all CNN-related tests,
  * testing of one-convolutional-layer network is driven by TestCNN_Conv().TestConv()
  */
@Ignore
object TestCNN {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
//     MySQL is disabled in this version
//    nn.cnn.mysql.CreateTable()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
    // MySQL is disabled in this version
//    Connector.close()
  }
}

@Ignore
class TestCNN {
  private val logger = Logger(this.getClass)

  val precision: Float = 0.1f
  val codeVersion: Int = 23
  val reruns: Int = 1
  val padData: Boolean = false
  val changeDataLayout: Boolean = true
  val Conv = conv.versions.Conv4
  type Conv = conv.versions.Conv4

  def test(param: cnn.ExperimentParams,
           testConfigFilename: String = "",
           continueFrom: Experiment = null,
           abortAfter: Option[Int] = None): Unit = {
    // If rerunsAllowed == False, the experiment will not be rerun if result files from previous runs
    // are found. Otherwise, new results will be added with a datetime timestamp
    val rerunsAllowed: Boolean = true
    val compileOnly: Boolean = System.getenv("LIFT_NN_MICROBENCHMARK_COMPILE_ONLY") != null
    
    var aCNN: CNN = null
    var data: NetDatasetsCollection = null

    var initParams: Layer.InitParameters = null

    var now: Date = null
    var skip: Boolean = continueFrom != null
    var currentLayer: Int = 0
    var experimentNo: Int = 0
    for {
    // Construct an experiment with all parameters

    // Workload parameters
      inputConfig <- param.inputConfigs
      convDimensions <- param.convDimensions
      fcDimensions <- param.fcDimensions

      if {List(1).contains(param.layerNo)} // Run specific layers only

      if cnn.Experiment.isFirstRun(inputConfig) || rerunsAllowed

      if cnn.Experiment.inputsExist(inputConfig, convDimensions.head, param.netName) ||
        (// Try generating files and recheck
          cnn.Experiment.generateFiles(param) &&
            cnn.Experiment.inputsExist(inputConfig, convDimensions.head, param.netName))

      if cnn.Experiment.targetsExist(inputConfig, convDimensions.head, param.netName)

      if Experiment.datasetsExist(Experiment.pathToParams(inputConfig, convDimensions.head))

      pI: String = pathToInputs(inputConfig, convDimensions.head)
      pP: String = pathToParams(inputConfig, convDimensions.head)
      pT: String = pathToTargets(inputConfig, convDimensions.head)
      inputTileSize <- param.inputTileSizeRange.head(inputConfig, convDimensions.head)
      // Here and below layer 1-related parameters are commented out. This needs to be generalized
      // better for multilayer CNNs
      //      _inputTileSizeL1 <- e.inputTileSizeRange(1)(inputConfig, convDimensions(1))
      elsPerThread <- param.elsPerThreadRange.head(inputConfig, convDimensions.head)
      //      _elsPerThreadL1 <- e.elsPerThreadRange(1)(inputConfig, convDimensions(1))
      kernelsPerGroup <- param.kernelsPerGroupRange.head(inputConfig, convDimensions.head)
      //      _kernelsPerGroupL1 <- e.kernelsPerGroupRange(1)(inputConfig, convDimensions(1))
      vectorLen <- param.vectorLenRange.head

      coalesce <- param.coalesceRange.head

      unrollReduce <- param.unrollReduceRange.head
      convConfig =
      Vector(conv.Experiment.Config(
        convDimensions.head, conv.Experiment.Config.OptimisationalParams(
          inputTileSize = inputTileSize, elsPerThread = elsPerThread,
          kernelsPerGroup = kernelsPerGroup, vectorLen = vectorLen,
          coalesce = coalesce, unrollReduce = unrollReduce)))


      _multsPerThreadL0 <- param.multsPerThreadRange.head(inputConfig, fcDimensions.head)
      //      _multsPerThreadL1 <- e.multsPerThreadRange(1)(inputConfig, fcDimensions(1))
      _neuronsPerWrgL0 <- param.neuronsPerWrgRange.head(inputConfig, fcDimensions.head)


      fcConfig =
      Vector(fc.Experiment.Config(
        fcDimensions.head, fc.Experiment.Config.OptimisationalParams(
          multsPerThread = _multsPerThreadL0, neuronsPerWrg = _neuronsPerWrgL0)))
      // Optimisational parameters are traversed with cnn.Experiment
      exp = new Experiment(param.layerNo, inputConfig, convConfig, fcConfig, pI, pP, pT)
      //f"Creating experiments for layer ${layerExperimentParams.layerName}%s")

      // If enabled, skip experiments until the one specified
      if !skip || {
        if (continueFrom == exp) {
          skip = false
          true
        } else false
      }

      // Check if CNN can be created with the selected parameters (e.g. if WrgGroupSize < maxWrgGroupSize)
      if {
        try {
          /* ---------------------------- BUILD THE NETWORK ---------------------------- */
          now = Calendar.getInstance().getTime
          aCNN = new CNN(nConvLayers = 1, nFCLayers = 0, //14,
            inputConfig = exp.inputConfig, pathToResults = Experiment.pathToResults)

          //noinspection ConvertibleToMethodValue
          initParams = Conv.InitParameters(param.layerNo, Conv.Par(_, _, _, _, _, _, _, _, _, _), nn.Linear, //nn.ReLU,
            optParams = exp.convConfigs.head.optParams,
            inputShape = Shape(nBatches = exp.inputConfig.nBatches, nInputs = exp.inputConfig.nInputs,
              size = exp.inputConfig.inputSize, nChannels = exp.inputConfig.nChannels),
            dim = exp.convConfigs.head.dim,
            padData = padData, testConfigFilename)

          currentLayer = 0
          aCNN.layers(currentLayer) = Conv(initParams.asInstanceOf[Conv.InitParameters])
          aCNN.convLayers(0) = aCNN.layers(currentLayer).asInstanceOf[Conv]

          /* ----------------------------- LOAD DATA  ----------------------------- */
          // Now that we know that layers can be built we the chosen parameters, load the data.
          // Load the data only if it wasn't loaded before for a similar experiment
          if (data == null || data.pathToParams != exp.pathToParams || data.nInputs != exp.inputConfig.nInputs)
            data = exp.loadData(aCNN, compileOnly)

          true
        }
        catch {
          case e: java.lang.IllegalArgumentException =>
            logger.warn("-----------------------------------------------------------------")
            val msg = f"Layer ${param.layerNo}%d (experiment $experimentNo%d): EXCEPTION: java.lang.IllegalArgumentException\n" +
              cnn.configToString(exp.inputConfig) + e.getMessage
            logger.warn(msg)
            recordFailureInSQL(msg, aCNN, initParams, now)
            logger.warn("SKIPPING EXPERIMENT.")
            false
        }
      }
    } {
      try {
        /* ---------------------------- RUN THE EXPERIMENT (BEGIN) ---------------------------- */
        // Now that we know that layers can be built and data is loaded, run the experiment
        logger.info("-----------------------------------------------------------------")
        System.out.println(f"Starting the experiment:\n" + aCNN.configToString(param.layerNo))

        now = Calendar.getInstance().getTime

        for (layerNo <- 0 until 1 /*aCNN.nLayers 14*/) {
          val layer: Layer = aCNN.layers(layerNo)
          val layerData: NetDatasets = data.perLayer(layerNo)
          breakable {
            // Pooling is disabled as work in progress
//            layer match {
//              case poolLayer: ScalaPool =>
//                poolLayer.run()
//                logger.info(f"Layer ${param.layerNo}%d (pooling) completed")
//                break
//              case _ =>
//            }


            /* Padding */
            layer match {
              case convLayer: Conv => Conv.pad(layerData.asInstanceOf[ConvDatasets].inputs, convLayer.inputShape)
              case fcLayer: FC =>
                val fcData: FCDatasets = layerData.asInstanceOf[FCDatasets]
                FC.pad(fcData.inputs, fcLayer.inputShape, fcData.weights, fcData.biases, fcLayer.neuronShape)
            }

            /* Compile and execute */
            var outputsFlat: Array[Float] = null
            var outputsFlat1: Array[Float] = null
            var runtime: Double = 0.0f
            var runtime1: Double = 0.0f

            layer match {
              case convLayer: conv.versions.Conv4 =>
                /* Two-kernel convolution */

                // Kernel 1
                // Disabling returning the OpenCL kernel as String as it depended on a temporary
                // hack of the Executor that allowed to get the kernel and run it without recompiling.
                // This hack is disabled in this version.
//                val ((_outputsFlat1: Array[Float], _runtime1), openclKernel1) =
                val (_outputsFlat1: Array[Float], _runtime1) =
                  Execute(
                    layer.localSize(0), layer.localSize(1), layer.localSize(2),
                    layer.globalSize(0), layer.globalSize(1), layer.globalSize(2),
                    (true, true))[Array[Float]](
                    layer.liftFProp(0), compileOnly,
                    layerData match {
                      case cd: ConvDatasets => {
                        if (changeDataLayout)
                        // (k, c, h, w) -> (k, h, w, c)
                          cd.weights.map(
                            kernel => kernel.transpose.map(
                              row => row.transpose))
                        else cd.weights

                      }
                      case fd: FCDatasets => fd.weights.padded
                    },
                    layerData match {
                      case cd: ConvDatasets => {
                        if (changeDataLayout)
                        // (b, i, c, h, w) -> (b, i, h, w, c)
                          cd.inputs.padded.map(
                            batch => batch.map(
                              input => input.transpose.map(
                                row => row.transpose)))
                        else cd.inputs.padded
                      }
                      case fd: FCDatasets => fd.inputs.padded
                    })

                if (compileOnly) {
                  outputsFlat1 = new Array[Float](
                    /* nTilesTotal */ convLayer.inputTiling.n * convLayer.inputTiling.n *
                      convLayer.inputShape.nInputs * convLayer.inputShape.nBatches *
                      /* nKernels */convLayer.kernelSliding.nChannels *
                      /* nWindowsInTile */convLayer.kernelSliding.n * convLayer.kernelSliding.n *
                      /* nSeqTilesInWindow */convLayer.inputShape.nChannels * convLayer.kernelSliding.size *
                      convLayer.kernelSliding.size / convLayer.elsPerThread)

                  runtime1 = 0.0d
                } else {
//                  outputsFlat1 = _outputsFlat1
                  runtime1 = _runtime1
                }

                // Kernel 2
                // Disabling returning the OpenCL kernel as String as it depended on a temporary
                // hack of the Executor that allowed to get the kernel and run it without recompiling.
                // This hack is disabled in this version.
//                val ((_outputsFlat2: Array[Float], runtime2), openclKernel2) =
                val (_outputsFlat2: Array[Float], runtime2) =
                  Execute(
                    layer.localSize(0 + 3), layer.localSize(1 + 3),
                    layer.globalSize(0 + 3), layer.globalSize(1 + 3), (true, true))[Array[Float]](
                    layer.liftFProp(1), compileOnly,
                    layerData match {
                      case cd: ConvDatasets => cd.biases
                      case fd: FCDatasets => fd.biases.padded
                    },
                    outputsFlat1)//nn.group(outputsFlat1, convLayer.intermediateDataShape))

//                outputsFlat = _outputsFlat2
                runtime = runtime1 + runtime2

                // Disabling temporarily since getting the generated OpenCL kernel as a String and running it
                // without recompilation needs to be reimplemented above
                /*saveKernelToFile(experimentNo, testConfigFilename, layer, param.layerName, openclKernel1,
                  twoKernels = true,
                  localSize = Array(layer.localSize(0), layer.localSize(1), layer.localSize(2)),
                  globalSize = Array(layer.globalSize(0), layer.globalSize(1), layer.globalSize(2)),
                  kernelPath = System.getenv("LIFT_NN_KERNELS_LOCATION") + "/" + param.netName + "/" +
                    param.kernelOutputSubfolder + "/lift_generated_kernel" + experimentNo.toString + "_first.cl")
                saveKernelToFile(experimentNo, testConfigFilename, layer, param.layerName, openclKernel2,
                  twoKernels = true,
                  localSize = Array(layer.localSize(3), layer.localSize(4), 1),
                  globalSize = Array(layer.globalSize(3), layer.globalSize(4), 1),
                  kernelPath = System.getenv("LIFT_NN_KERNELS_LOCATION") + "/" + param.netName + "/" +
                    param.kernelOutputSubfolder + "/lift_generated_kernel" + experimentNo.toString + "_final.cl")*/

              case _ =>
                /* One-kernel layer */
//                val ((outputsFlat: Array[Float], runtime), openclKernel) =
                val (outputsFlat: Array[Float], runtime)=
                  Execute(
                    layer.localSize(0), layer.localSize(1), layer.localSize(2),
                    layer.globalSize(0), layer.globalSize(1), layer.globalSize(2), (true, true))[Array[Float]](
                    layer.liftFProp(0), compileOnly,
                    layerData match {
                      case cd: ConvDatasets => cd.weights
                      case fd: FCDatasets => fd.weights.padded
                    },
                    layerData match {
                      case cd: ConvDatasets => cd.biases
                      case fd: FCDatasets => fd.biases.padded
                    },
                    layerData match {
                      case cd: ConvDatasets => {
                        if (changeDataLayout)
                        // (b, i, c, h, w) -> (b, i, h, w, c)
                          cd.inputs.padded.map(batch => batch.map(input => input.transpose.map(
                            input2 => input2.transpose)))
                      }
                      case fd: FCDatasets => fd.inputs.padded
                    })
                // Disabling temporarily (see the comment above)
                /*saveKernelToFile(experimentNo, testConfigFilename, layer, param.layerName, openclKernel,
                  twoKernels = false,
                  localSize = Array(layer.localSize(0), layer.localSize(1), layer.localSize(2)),
                  globalSize = Array(layer.globalSize(0), layer.globalSize(1), layer.globalSize(2)),
                  kernelPath = System.getenv("LIFT_NN_KERNELS_LOCATION") + "/" +
                    param.kernelOutputSubfolder + "/lift_generated_kernel" + experimentNo.toString + ".cl")*/
            }

            layer.runtime = runtime
            logger.info(f"Layer ${param.layerNo}%d (experiment $experimentNo%d) runtime: $runtime%1.5f ms")

            /* Group and unpad */
            if (!compileOnly)
              layer.groupAndUnpad(outputsFlat, layerData)
          }

          /* Pass outputs to the next layer*/
          if (layerNo != aCNN.nLayers - 1) {
            (layer, aCNN.layers(layerNo + 1)) match {
              case (_: Conv, _: FC) =>
                // If the current layer is convolutional and the next one is fully connected
                data.perLayer(layerNo + 1).asInstanceOf[FCDatasets].inputs.nonPadded =
                  // (n_batches, n_inputs) -> (n_batches * n_inputs)
                  layerData.asInstanceOf[ConvDatasets].outputs.nonPadded.flatMap(batch => batch.map(
                    // (h, w, n_channels) -> (h * w * n_channels)
                    input => input.map(row => row.flatten).flatten
                  ))
                // Disabling pooling temporarily as work in progress
//              case (_: Conv, poolLayer: ScalaPool) =>
//                poolLayer.inputs = layerData.asInstanceOf[ConvDatasets].outputs.nonPadded
//              case (poolLayer: ScalaPool, _: FC) =>
//                data.perLayer(/*no +1 on purpose */layerNo).asInstanceOf[FCDatasets].inputs.nonPadded =
//                  poolLayer.outputs.flatMap(batch => batch.map(
//                    // (h, w, n_channels) -> (h * w * n_channels)
//                    input => input.map(row => row.flatten).flatten
//                  ))
              case (_: Conv, _: Conv) =>
                data.perLayer(layerNo + 1).asInstanceOf[ConvDatasets].inputs.nonPadded =
                  layerData.asInstanceOf[ConvDatasets].outputs.nonPadded
              case (_: FC, _: FC) =>
                data.perLayer({
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

        if (!compileOnly) {
          /* Check and save results */
          var testFailed: Boolean = false
          var testVerified: Boolean = true


          verifyOutputs(
            netOutputs = data.perLayer(0).asInstanceOf[ConvDatasets].outputs.nonPadded,
            targetOutputs = data.perLayer(0).asInstanceOf[ConvDatasets].targets,
            precision) match {
            case Some((ix, unmatchedTarget, wrongOutput)) =>
              logger.info(ix.mkString(", ") + ": " + unmatchedTarget + " != " + wrongOutput)
              testFailed = true
            case None =>
            // Verification successful
          }

          if (!testFailed)
            logger.info(f"SUCCESS. Processed ${aCNN.inputConfig.nBatches * aCNN.inputConfig.nInputs}%d inputs, " +
              f"the results were equal to targets (precision=$precision%1.4f).")
          else if (!testVerified)
            logger.info(f"NOT VERIFIED. Processed ${aCNN.inputConfig.nBatches * aCNN.inputConfig.nInputs}%d inputs.")
          else
            throw new AssertionError


          /* JSON */
          if (aCNN.pathToResults != "" && !testFailed)
            recordInCSV(aCNN, now)

          /* SQL */
          recordInSQL(aCNN, testRan = true, testFailed, testVerified, now)
        } else {
          logger.info(f"Kernels compiled. SKIPPING VERIFICATION.")
        }
        /* ---------------------------- RUN EXPERIMENT (END) ---------------------------- */
      } catch {
        case e: opencl.executor.Executor.ExecutorFailureException =>
          val msg = "EXCEPTION: opencl.executor.Executor.ExecutorFailureException\n" + e.getMessage
          logger.warn(msg)
          recordFailureInSQL(msg, aCNN, now)
        //          throw e
        case e: opencl.executor.DeviceCapabilityException =>
          val msg = "EXCEPTION: opencl.executor.DeviceCapabilityException\n" + e.getMessage
          logger.warn(msg)
          recordFailureInSQL(msg, aCNN, now)
        //        case e: lift.arithmetic.NotEvaluableException =>
        //          val msg = "EXCEPTION: lift.arithmetic.NotEvaluableException" + e.getMessage
        //          logger.warn(msg)
        //          recordFailureInSQL(msg, aCNN, now)
      }
      experimentNo = experimentNo + 1
      abortAfter match {
        case Some(v) =>
          if (experimentNo == v)
            return
        case None =>
      }
    }
  }

  def recordFailureInSQL(exceptionMsg: String, aCNN: CNN, iP: Layer.InitParameters, runDate: Date): Unit = {
    // MySQL is disabled in this version
    /*Connector.statement.execute("INSERT INTO lift_results_cnn " +
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
    } + "ran, abort_reason, code_version, datetime) VALUES (" +
      "'" + nn.deviceName + "', " + f"${aCNN.inputConfig.nBatches}%d, ${aCNN.inputConfig.nInputs}%d, " +
      f"${aCNN.inputConfig.inputSize}%d, " + {
      iP match {
        case cIP: Conv.InitParameters =>
          f"${cIP.dim.nKernels}%d, ${cIP.dim.kernelSize}%d, ${cIP.dim.kernelStride}%d, " +
            f"${cIP.optParams.inputTileSize}%d, ${cIP.optParams.elsPerThread}%d, " +
            f"${cIP.optParams.kernelsPerGroup}%d, "
        case fcIP: FC.InitParameters =>
          f"${fcIP.inputShape.size}%d, ${fcIP.neuronShape.size}%d, " +
            f"${fcIP.optParams.multsPerThread}%d, ${fcIP.optParams.neuronsPerWrg}%d, "
      }
    } + f"false, '" + exceptionMsg + f"', $codeVersion%d, " +
      f"'${new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(runDate)}%s');")*/
  }

  def recordFailureInSQL(exceptionMsg: String, aCNN: CNN, runDate: Date): Unit =
    recordInSQL(aCNN, testRan = true, testFailed = true, testVerified = false, runDate, exceptionMsg)

  def recordInSQL(aCNN: CNN, testRan: Boolean, testFailed: Boolean = false, testVerified: Boolean, runDate: Date,
                  exceptionMsg: String = ""): Unit = {
    // MySQL is disabled in this version
    /*val cmd = "INSERT INTO lift_results_cnn " +
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
      ", ran, verified, success, abort_reason, code_version, datetime, pool_size, l1_out_len_original, " +
      "l1_out_len_new) VALUES (" +
      "'" + nn.deviceName + "', " + f"${aCNN.inputConfig.nBatches}%d, ${aCNN.inputConfig.nInputs}%d, " +
      f"${aCNN.inputConfig.inputSize}%d, " + f"${aCNN.nConvLayers}%d, ${aCNN.nFCLayers}%d, " + {
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
    Connector.statement.execute(cmd)*/
  }


  def recordInCSV(aCNN: CNN, runDate: Date): Unit = {
    var pw: PrintWriter = null
    val file = new File(nn.resultsFilename(aCNN.pathToResults, aCNN.inputConfig.nInputs))
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

    pw.write(nn.deviceName + ", " + f"${aCNN.inputConfig.nBatches}%d, ${aCNN.inputConfig.nInputs}%d, " +
      f"${aCNN.inputConfig.inputSize}%d, " +
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
