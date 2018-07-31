package nn

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Paths.get
import java.util.Calendar

import com.typesafe.scalalogging.Logger
import nn.conv.Conv
import org.junit.Assert.assertEquals

import scala.sys.process._
import scala.util.parsing.json.JSON

/**
  * Created by nm on 08/02/17.
  */

package object cnn {

  // For debugging
  def time[R](block: => R, msg: String): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println(msg + "\nElapsed time: " + (t1 - t0) + " ms")
    result
  }

  def generateVector(config: Map[String, Any], inputConfig: cnn.InputConfig,
                  layerSizeConfig: Layer.Experiment.Config.Dimensions): Vector[Int] = {
    val configEvaluated: Map[String, Int] =
      config.map {
        case (key: String, v: Double) => (key, v.toInt)
        case (key: String, v: String) => (key, {
          v match {
            case "n_batches" => inputConfig.nBatches
            case "n_inputs" => inputConfig.nInputs
            case "image_size" => inputConfig.inputSize
            case "input_channels" => inputConfig.nChannels
            case _ =>
              layerSizeConfig match {
                case c: conv.Experiment.Config.Dimensions =>
                  v match {
                    case "n_kernels" => c.nKernels
                    case "kernel_size" => c.kernelSize
                    case _ => throw new java.lang.IllegalArgumentException()
                  }
                case f: fc.Experiment.Config.Dimensions =>
                  v match {
                    case "n_neurons" => f.nNeurons
                    case _ => throw new java.lang.IllegalArgumentException()
                  }
              }
          }
        }) }
    generateVector(configEvaluated)
  }

  def generateVectorsOfInts(jWorkload: Map[String, Any], blockName: String): Vector[Vector[Int]] = {
    val paramBlock = jWorkload(blockName).asInstanceOf[Vector[Map[String, Double]]]
    //            Vector(generateVector(paramBlock.head), generateVector(paramBlock(1))
    Vector(generateVector(paramBlock.head))
  }
  
  def generateVectorsOfFuns(jOptParams: Map[String, Any], blockName: String): Vector[
    (cnn.InputConfig, Layer.Experiment.Config.Dimensions) => Vector[Int]] = {
    val paramBlock = jOptParams(blockName).asInstanceOf[Vector[Map[String, Any]]]
    Vector(
      (inputConfig, convLayerSizeConfig) =>
        generateVector(paramBlock.head, inputConfig, convLayerSizeConfig))
    //              (inputConfig, convLayerSizeConfig) =>
    //                generateVector(paramBlock(1), inputConfig, convLayerSizeConfig))
  }

  def generateVector(config: Map[String, Any]): Vector[Int] = {
    val _config: Map[String, Int] = config.map{
      case (k, v: Double) => (k, v.toInt)
      case (k, v: java.lang.Integer) => (k, v.toInt)
      case (k, v: Int) => (k, v)
    }
    val mainVector: Vector[Int] = {
      if (_config.contains("step_multiply")) {
        (0 to Math.floor(
          Math.log(_config("end").toFloat / _config("start")) /
            Math.log(_config("step_multiply"))).toInt).map(
          p => (_config("start") * Math.pow(_config("step_multiply"), p)).toInt).toVector
      } else if (_config.contains("step_add")) {
        (0 to Math.floor((_config("end") - _config("start")).toFloat
          / _config("step_add")).toInt).map(
          x => _config("start") + x * _config("step_add")).toVector
      } else
        null
    }
    if (_config.contains("initial"))
      if (mainVector != null)
        Vector(_config("initial").toInt) ++ mainVector
      else
        Vector(_config("initial").toInt)
    else
      mainVector
  }

  def getConfigFromJSON(jsonFilePath: String): ExperimentParams = {
    val source = scala.io.Source.fromFile(jsonFilePath)
    val jsonString = source.getLines.mkString("\n")
    source.close()
    val jsonMap: Option[Any] = JSON.parseFull(jsonString)
    
    jsonMap match {
      case None => 
        throw new java.lang.IllegalArgumentException()
      case Some(someJ) =>
        val j = someJ.asInstanceOf[Map[String, Any]]

        Logger(this.getClass).info("Processing JSON config file \"" + j("name").asInstanceOf[String] + "\"\n" +
          "located in " + jsonFilePath)
        
        val jWorkload = j("workload").asInstanceOf[Map[String, Any]]
        val jOptParams = j("optimisational_parameters").asInstanceOf[Map[String, Map[String, Double]]]

        new ExperimentParams(
          netName = j("name").asInstanceOf[String],
          kernelOutputSubfolder = j("kernel_output_subfolder").asInstanceOf[String],
          layerName = "", //TODO
          layerNo = 0, //TODO
          
          exactParams = None,
          
          dim = Some(ExperimentParams.DimensionRanges(
            nBatchesRange = generateVector(jWorkload("n_batches").asInstanceOf[Map[String, Double]]),
            nInputsRange = generateVector(jWorkload("n_inputs").asInstanceOf[Map[String, Double]]),
            inputSizeRange = generateVector(jWorkload("image_size").asInstanceOf[Map[String, Double]]),
            inputChannelRange = generateVector(jWorkload("input_channels").asInstanceOf[Map[String, Double]]),

            nKernelsRange = generateVectorsOfInts(jWorkload, "n_kernels"),
            kernelSizeRange = generateVectorsOfInts(jWorkload, "kernel_size"),
            kernelStrideRange = generateVectorsOfInts(jWorkload, "kernel_stride"),

            neuronsRange = generateVectorsOfInts(jWorkload, "n_neurons"))),
  
  
            inputTileSizeRange = generateVectorsOfFuns(jOptParams, "input_tile_size"),
            elsPerThreadRange = generateVectorsOfFuns(jOptParams, "els_per_thread"),
            kernelsPerGroupRange = generateVectorsOfFuns(jOptParams, "kernels_per_group"),
          
            vectorLenRange = Vector(Vector(1, 2, 4)), //TODO: generateVectorsOfFuns(jOptParams, "vector_len"),
            coalesceRange = Vector(Vector(true, false)),
            unrollReduceRange = Vector(Vector(true, false)),
  
            multsPerThreadRange = generateVectorsOfFuns(jOptParams, "mults_per_thread"),
            neuronsPerWrgRange = generateVectorsOfFuns(jOptParams, "neurons_per_wrg"))
    }
  }

  
  def configToString(iC: cnn.InputConfig): String = {
    f"nBatches=${iC.nBatches}%d, nInputs=${iC.nInputs}%d, imageSize=${iC.inputSize}%d\n"
  }

  
  case class InputConfig(nBatches: Int,
                         nInputs: Int,
                         inputSize: Int,
                         nChannels: Int) {
  override def toString: String =
    f"InputConfig(" +
      f"\nnBatches = $nBatches%d, nInputs = $nInputs%d, inputSize = $inputSize%d, nChannels = $nChannels%d)"}
  

  object Experiment {

    val cnnDir: String = nn.nnDir + "/cnn"
    val pathToResults: String = System.getenv("LIFT_NN_CAFFE_HARNESS") + f"/neural_net_outputs/"
    val pathToLiftResults: String = pathToResults + "/lift_results"

    
    def pathToInputs(iC: cnn.InputConfig, cD: conv.Experiment.Config.Dimensions): String =
      System.getenv("LIFT_NN_CAFFE_HARNESS") + f"/neural_net_inputs/input_lmdb_IN_${iC.nInputs}%d_IC_${iC.nChannels}%d_" +
        f"IS_${iC.inputSize}%d_" +
        f"KC_${cD.nKernels}%d_KSI_${cD.kernelSize}%d_KSTR_${cD.kernelStride}%d"

    
    def pathToParams(iC: cnn.InputConfig, cD: conv.Experiment.Config.Dimensions): String =
      System.getenv("LIFT_NN_CAFFE_HARNESS") + f"/neural_net_params/micro_IN_${iC.nInputs}%d_IC_${iC.nChannels}%d_" +
        f"IS_${iC.inputSize}%d_" +
        f"KC_${cD.nKernels}%d_KSI_${cD.kernelSize}%d_KSTR_${cD.kernelStride}%d"
    
    
    def pathToTargets(iC: cnn.InputConfig, cD: conv.Experiment.Config.Dimensions): String = 
      pathToResults + f"/outputs_IN_${iC.nInputs}%d_IC_${iC.nChannels}%d_IS_${iC.inputSize}%d_" +
      f"KC_${cD.nKernels}%d_KSI_${cD.kernelSize}%d_KSTR_${cD.kernelStride}%d.binary"


    def inputsExist(iC: cnn.InputConfig, cD: conv.Experiment.Config.Dimensions,
                    experimentName: String): Boolean = {
      val path: String = pathToInputs(iC, cD) + "/test_images_n" + iC.nInputs + ".binary"
      if (exists(get(path)))
        true
      else {
        System.out.println(
          f"No inputs provided for $experimentName%s (nInputs=${iC.nInputs}%d, imageSize=${iC.inputSize}%d, " +
            f"nChannels=${iC.nChannels}%d) at:\n$path%s")
        false
      }
    }
    
    def targetsExist(iC: cnn.InputConfig, cD: conv.Experiment.Config.Dimensions,
                    experimentName: String): Boolean =
      if (exists(get(Experiment.pathToTargets(iC, cD))))
        true
      else {
        System.out.println(
          f"No targets provided for $experimentName%s (nInputs=${iC.nInputs}%d, imageSize=${iC.inputSize}%d, " +
            f"nChannels=${iC.nChannels}%d\nnKernels=${cD.nKernels}%s, kernelSize=${cD.kernelSize}%s, " +
            f"kernelStride=${cD.kernelStride}%s)")
        false
      }
    
    def generateFiles(benchmark: ExperimentParams): Boolean = {
      if (System.getenv("LIFT_NN_GENERATE_FILES_CMD") != null) {
        
        val configFileName: String = benchmark.netName + "_" + benchmark.layerName + "_layer_no_" + 
          benchmark.layerNo + ".csv"
        val configPath: String = System.getenv("LIFT_NN_CAFFE_HARNESS") + "/neural_net_specs/" + configFileName
        Logger(this.getClass).info(f"Generating neural network files.\nCreating Caffe harness config in $configPath%s")
        // Generate the neural network configuration file
        var pw: PrintWriter = null
        val file = new File(configPath)
        pw = new PrintWriter(file)
        pw.write(f"${"NN"}%-30s, ${"LN"}%-10s, ${"LORD"}%4s, ${"IN"}%3s, ${"IC"}%3s, ${"IS"}%4s, " +
          f"${"KC"}%4s, ${"KSI"}%3s, ${"KSTR"}%2s\n")
  
        {
          val iC: InputConfig = benchmark.exactParams.get.inputConfig
          val cD: nn.conv.Experiment.Config.Dimensions = benchmark.exactParams.get.convDimensions

          pw.write(f"${benchmark.netName}%-30s, ${benchmark.layerName}%-10s, ${benchmark.layerNo}%4d, " +
            f"${iC.nInputs}%3d, ${iC.nChannels}%3d, ${iC.inputSize}%4d, " +
            f"${cD.nKernels}%4d, ${cD.kernelSize}%3d, ${cD.kernelStride}%2d\n")
        }
        
        pw.write("\n")
        pw.close()
        
        // Generate neural network files
        // FYI: example command:
        // ssh avus -x /home/s1569687/caffe_clblas/vcs_caffes/deliverable/microbenchmark/microbenchmarking.sh 
        //   /s1569687/caffe_clblas/vcs_caffes/deliverable/microbenchmark/neural_net_specs/%s build
        val cmd: String = System.getenv("LIFT_NN_GENERATE_FILES_CMD").format(configFileName)
        Logger(this.getClass).info(f"Starting Caffe harness:\n> " + cmd)
        cmd.!
        true
      } else false
    }
    
    
    def isFirstRun(iC: cnn.InputConfig) = {
      if (!exists(get(pathToResults))) {
        createDirectory(get(pathToResults))
        true
      } else {
        new File(pathToResults).listFiles.toVector.count {
          file => file.getName.endsWith(f"_n${iC.nInputs}%d.csv")
        } == 0 
      }
    } 
    

    def datasetsExist(pathToParams: String): Boolean = exists(get(pathToParams + "/wconv1.binary"))

    
    def verifyOutputs(netOutputs: Any, targetOutputs: Any, precision: Float):
    Option[(Vector[Int], Float, Float)] = {
      (netOutputs, targetOutputs) match {
        case (n: Array[Float], t: Array[Float]) =>
          for ((netOutput, targetOutput, i) <- (n, t, 0 to t.length).zipped.toVector) {
            try {
              assertEquals("", targetOutput, netOutput, precision)
            }
            catch {
              case _: AssertionError =>
                return Some(Vector(i), targetOutput, netOutput)
            }
          }
          None
        case (n: Array[_], t: Array[_]) =>
          for ((netOutput, targetOutput, i) <- (n, t, t.indices).zipped.toVector) {
            verifyOutputs(netOutput, targetOutput, precision) match {
              case Some((ix, unmatchedTarget, wrongResult)) =>
                return Some(Vector(i) ++ ix, unmatchedTarget, wrongResult)
              case None =>
            }
          }
          None
      }
    }
    
    def apply(benchmark: cnn.ExperimentParams,
              iC: cnn.InputConfig,
              cD: conv.Experiment.Config.Dimensions,
              fD: fc.Experiment.Config.Dimensions): Vector[Experiment] = {
      val pI: String = pathToInputs(iC, cD)
      val pP: String = pathToParams(iC, cD)
      val pT: String = pathToTargets(iC, cD)
      for {
        convConfig <- benchmark.convConfig(iC, cD)
        fcConfig <- benchmark.fcConfig(iC, fD)
      }
        yield new Experiment(benchmark.layerNo, iC, convConfig, fcConfig, pI, pP, pT)
    }
  }

  def saveKernelToFile(experimentNo: Int, testConfigFilename: String, layer: Layer, layerName: String, 
                       openclKernel: String, 
                       twoKernels: Boolean, localSize: Array[Int], globalSize: Array[Int], kernelPath: String): Unit = {
    val logger = Logger(this.getClass)
    
    /* Save the OpenCL code into text file */
    val kernelFile = new File(kernelPath)

    /* Make sure all directories in the path exist */
    kernelFile.getParentFile.mkdirs()
      
    // UTF8 to solve the OpenCL compilation error "source file is not valid UTF-8"
    val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(kernelFile, false),
      StandardCharsets.UTF_8))
    // Insert workgroup dimensions and optimisational parameters
    bw.write("//L0=" + localSize(0).toString + "\n")
    bw.write("//L1=" + localSize(1).toString + "\n")
    bw.write("//L2=" + localSize(2).toString + "\n")
    bw.write("//G0=" + globalSize(0).toString + "\n")
    bw.write("//G1=" + globalSize(1).toString + "\n")
    bw.write("//G2=" + globalSize(2).toString + "\n")
    layer match {
      case cL: Conv => {
        bw.write("//input_tile_size=" + cL.inputTiling.size + "\n")
        bw.write("//kernels_per_group=" + cL.kernelsPerGroup + "\n")
        bw.write("//els_per_thread=" + cL.elsPerThread + "\n")
        bw.write("//vector_len=" + cL.vectorLen + "\n")
        bw.write("//coalesce=" + cL.coalesce + "\n")
        bw.write("//unroll_reduce=" + cL.unrollReduce + "\n")
        bw.write("//experiment_no=" + experimentNo + "\n")
        bw.write("//test_config=" + testConfigFilename + "\n")
        bw.write("// Layer name: " + layerName + "\n")
        bw.write("// Full configuration:\n//" + cL.toString.replace("\n", "\n//") + "\n")
      }
    }
    bw.write("//Generated on " + 
      new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(Calendar.getInstance().getTime) + "\n\n")
    // Insert offset handling
    val openclKernelWithOffsets = {
      if (!twoKernels)
        openclKernel.replaceFirst(
          raw"void KERNEL\(const global float\* restrict v__(\d+), " +
            raw"const global float\* restrict v__(\d+), " +
            raw"const global float\* restrict v__(\d+), " +
            raw"global float\* v__(\d+)\)\{ \n" +
            raw"\#ifndef WORKGROUP_GUARD\n" +
            raw"\#define WORKGROUP_GUARD\n" +
            raw"\#endif\n" +
            raw"WORKGROUP_GUARD\n" +
            raw"\{",
          "void KERNEL(const global float* restrict v__$1, const global float* restrict v__$2, " +
            "const global float* restrict v__$3, global float* v__$4, int const offsetX, int const offsetOut){\n" +
            "#ifndef WORKGROUP_GUARD\n" +
            "#define WORKGROUP_GUARD\n" +
            "#endif\n" +
            "WORKGROUP_GUARD\n" +
            "{\n" +
            "  /* Apply offsets */\n" +
            "  v__$3 += offsetX;\n" +
            "  v__$4 += offsetOut;")
      else openclKernel.replaceFirst(
        raw"void KERNEL\(const global float\* restrict v__(\d+), " +
          raw"const global float\* restrict v__(\d+), " +
          raw"global float\* v__(\d+)\)\{.*\n" +
          raw".*\#ifndef WORKGROUP_GUARD\n" +
          raw".*\#define WORKGROUP_GUARD\n" +
          raw".*\#endif\n" +
          raw".*WORKGROUP_GUARD\n" +
          raw".*\{",
        "void KERNEL(const global float* restrict v__$1, const global float* restrict v__$2, " +
          "global float* v__$3, int const offsetX, int const offsetOut){\n" +
          "#ifndef WORKGROUP_GUARD\n" +
          "#define WORKGROUP_GUARD\n" +
          "#endif\n" +
          "WORKGROUP_GUARD\n" +
          "{\n" +
          "  /* Apply offsets */\n" +
          "  v__$2 += offsetX;\n" +
          "  v__$3 += offsetOut;")
    }

    bw.write(openclKernelWithOffsets)
    bw.close()
    logger.info(f"Saved the generated OpenCL kernel into $kernelPath%s")
  }


  case class Experiment(layerNo: Int,
                        inputConfig: cnn.InputConfig,
                        convConfigs: Vector[conv.Experiment.Config],
                        fcConfigs: Vector[fc.Experiment.Config],
                        pathToInputs: String,
                        pathToParams: String,
                        pathToTargets: String) {
    
    def loadData(aCNN: CNN, compileOnly: Boolean) = NetDatasetsCollection(
      pathToParams = pathToParams,
      nInputs = inputConfig.nInputs,
      perLayer = Array[NetDatasets](
        nn.conv.Experiment.loadDatasets(
          paramsPath = pathToParams,
          inputsPath = pathToInputs + "/test_images_n" + inputConfig.nInputs + ".binary",
          targetOutputsPath = pathToTargets,
          inputShape = aCNN.convLayers(0).inputShape,
          outputShape = aCNN.convLayers(0).outputShape,
          //                  targetFilePrefix = "test_caffe_results_n" + inputConfig.nInputs,
          paramFileInfix = "conv1",
          kernelSliding = aCNN.convLayers(0).kernelSliding,
          generateDummies = compileOnly)))
  }
  
  object ExperimentParams {
    case class Exact(inputConfig: cnn.InputConfig,
                     convDimensions: conv.Experiment.Config.Dimensions,
                     fcDimensions: fc.Experiment.Config.Dimensions)
    
    case class DimensionRanges(nBatchesRange: Vector[Int],
                               nInputsRange: Vector[Int],
                               inputSizeRange: Vector[Int],
                               inputChannelRange: Vector[Int],

                               nKernelsRange: Vector[Vector[Int]],
                               kernelSizeRange: Vector[Vector[Int]],
                               kernelStrideRange: Vector[Vector[Int]],

                               neuronsRange: Vector[Vector[Int]])
    
//    object Invalid {
//      def restore(): Option[Invalid] = {        
//        val path: String = System.getenv("LIFT_NN_INVALID_PARAMS_PATH")
//        if (path != null && exists(get(path))) {
//          val ois = new ObjectInputStream(new FileInputStream(path))
//          val invalidParams = ois.readObject.asInstanceOf[Invalid]
//          ois.close
//          Logger(this.getClass).info("Restored invalid parameter combinations from $path%s")
//          Some(invalidParams)
//        }
//        else None
//      }
//
//      def append(i: Invalid, comb: (cnn.InputConfig, nn.conv.Experiment.Config, nn.fc.Experiment.Config)): Unit = {
//        i.combinations += comb
//        i.appendsToSave -= 1
//
//        if (i.appendsToSave == 0) {
//          // Save
//          val path: String = System.getenv("LIFT_NN_INVALID_PARAMS_PATH")
//          if (path != null) {
//            val oos = new ObjectOutputStream(new FileOutputStream(path))
//            oos.writeObject(i)
//            oos.flush()
//            oos.close()
//          }
//          i.appendsToSave = i.savingPeriod
//        }
//      }
//    }
//        
//    @SerialVersionUID(100L)
//    case class Invalid(combinations: mutable.ArrayBuffer[
//      (cnn.InputConfig, nn.conv.Experiment.Config, nn.fc.Experiment.Config)]) extends Serializable {
//      
//      val savingPeriod: Int = 50
//      var appendsToSave: Int = savingPeriod
//
//
//    }
  }

  case class ExperimentParams(netName: String,
                              kernelOutputSubfolder: String,
                              layerName: String,
                              layerNo: Int,

                              // ExperimentParams must have either exactParams or dim not null
                             
                              // Exact param combinations specified in prototxt files
                              exactParams: Option[ExperimentParams.Exact],

                              // Ranges for exploring multiple workloads specified in JSON files
                              dim: Option[ExperimentParams.DimensionRanges],

                              inputTileSizeRange: Vector[
                              (cnn.InputConfig, conv.Experiment.Config.Dimensions) => Vector[Int]],
                              elsPerThreadRange: Vector[
                              (cnn.InputConfig, conv.Experiment.Config.Dimensions) => Vector[Int]],
                              kernelsPerGroupRange: Vector[
                              (cnn.InputConfig, conv.Experiment.Config.Dimensions) => Vector[Int]],

                              coalesceRange: Vector[Vector[Boolean]],
                              unrollReduceRange: Vector[Vector[Boolean]],
                              vectorLenRange: Vector[Vector[Int]],

                              multsPerThreadRange: Vector[
                              (cnn.InputConfig, fc.Experiment.Config.Dimensions) => Vector[Int]],
                              neuronsPerWrgRange: Vector[
                              (cnn.InputConfig, fc.Experiment.Config.Dimensions) => Vector[Int]]) {
    
    def canEqual(a: Any) = a.isInstanceOf[ExperimentParams]
    
    override def equals(that: Any): Boolean = that match {
      case that: ExperimentParams => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }

    override def hashCode: Int = {
      val prime = 31
      var result = 1
      exactParams match {
        case Some(p) =>
          result = prime * result + p.inputConfig.hashCode
          result = prime * result + p.convDimensions.hashCode
          result = prime * result + p.fcDimensions.hashCode
          result
        case None => throw new NotImplementedError("ExperimentParams.hashCode() not implemented for ranges of " +
          "parameters")
      }
    }
    
    def inputConfigs: Vector[cnn.InputConfig] = {
      exactParams match {
        case Some(params) => Vector(params.inputConfig)
        case None =>
          for {
            nBatches <- dim.get.nBatchesRange
            inputSize <- dim.get.inputSizeRange
            inputChannels <- dim.get.inputChannelRange
            nInputs <- dim.get.nInputsRange}
          // Wrap input parameters into an object
            yield cnn.InputConfig(
              nBatches = nBatches, nInputs = nInputs, inputSize = inputSize, nChannels = inputChannels)
      }
    }
    
    def convDimensions: Vector[Vector[conv.Experiment.Config.Dimensions]] = {
      exactParams match {
        case Some(params) => Vector(Vector(params.convDimensions))
        case None =>
          for {
            nKernelsL0 <- dim.get.nKernelsRange.head
            //      _nKernelsL1 <- e.nKernelsRange(1)
            kernelStrideL0 <- dim.get.kernelStrideRange.head
            kernelSizeL0 <- dim.get.kernelSizeRange.head
          //      _kernelSizeL1 <- e.kernelSizeRange(1)
          }
            yield Vector(conv.Experiment.Config.Dimensions(nKernelsL0, kernelSizeL0, kernelStrideL0))
        //        conv.Experiment.Config.Dimensions(_nKernelsL1, _kernelSizeL1, /*TODO*/1)))
      }
    }
    
    def convConfig(iC: cnn.InputConfig, cD: conv.Experiment.Config.Dimensions):
    Vector[Vector[nn.conv.Experiment.Config]] = {
      for {
        inputTileSize <- inputTileSizeRange.head(iC, cD)
        //      _inputTileSizeL1 <- e.inputTileSizeRange(1)(inputConfig, convDimensions(1))
        elsPerThread <- elsPerThreadRange.head(iC, cD)
        //      _elsPerThreadL1 <- e.elsPerThreadRange(1)(inputConfig, convDimensions(1))
        kernelsPerGroup <- kernelsPerGroupRange.head(iC, cD)
        //      _kernelsPerGroupL1 <- e.kernelsPerGroupRange(1)(inputConfig, convDimensions(1))
        vectorLen <- vectorLenRange.head
      
        coalesce <- coalesceRange.head
      
        unrollReduce <- unrollReduceRange.head
      }
      // Wrap conv parameters into an object
        yield Vector(
          conv.Experiment.Config(
            cD, conv.Experiment.Config.OptimisationalParams(
              inputTileSize = inputTileSize, elsPerThread = elsPerThread,
              kernelsPerGroup = kernelsPerGroup, vectorLen = vectorLen,
              coalesce = coalesce, unrollReduce = unrollReduce)))
    }

    def fcDimensions: Vector[Vector[fc.Experiment.Config.Dimensions]] = {
      exactParams match {
        case Some(params) => Vector(Vector(params.fcDimensions))
        case None =>
          for {
            nNeuronsL0 <- dim.get.neuronsRange.head
          //      _nNeuronsL1 <- e.neuronsRange(1)
          }
            yield Vector(fc.Experiment.Config.Dimensions(nNeuronsL0))
        //        fc.Experiment.Config.Dimensions(_nNeuronsL1)))
      }
    }

    def fcConfig(iC: cnn.InputConfig, fD: fc.Experiment.Config.Dimensions):
    Vector[Vector[fc.Experiment.Config]] = {
      for {
        _multsPerThreadL0 <- multsPerThreadRange.head(iC, fD)
        //      _multsPerThreadL1 <- e.multsPerThreadRange(1)(inputConfig, fcDimensions(1))
        _neuronsPerWrgL0 <- neuronsPerWrgRange.head(iC, fD)
      //      _neuronsPerWrgL1 <- e.neuronsPerWrgRange(1)(inputConfig, fcDimensions(1))
      }
        yield Vector(
          fc.Experiment.Config(
            fD, fc.Experiment.Config.OptimisationalParams(
              multsPerThread = _multsPerThreadL0, neuronsPerWrg = _neuronsPerWrgL0)))
    }
  }
}
