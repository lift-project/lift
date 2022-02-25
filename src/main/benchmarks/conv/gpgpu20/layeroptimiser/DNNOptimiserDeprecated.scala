//package layeroptimisers
//
//import java.io.{File, FileNotFoundException, FileWriter}
//import java.nio.file.FileAlreadyExistsException
//
//import dnnoptimiser._
//import paramspaces.layerconfig.NetConfigGeneratorDeprecated
//import paramspaces.layeropt.OptParamGeneratorDeprecated
//
//import scala.io.Source
//
///**
//  * The DNN optimiser.
//  *
//  * Parses command line arguments
//  * Parses JSON settings
//  * Generates network configuration parameter space
//  * For each valid paramspaces.netconfig parameter combination creates a Python ONNX factory in a temporary file and runs it with
//  * a Python interpreter to generate an ONNX file
//  */
//abstract class DNNOptimiserDeprecated
//[NetConfigGeneratorT <: NetConfigGeneratorDeprecated, OptParamGeneratorT <: OptParamGeneratorDeprecated]
//(args: Array[String]) {
//
//  /** Parameter space generators **/
//  val netConfigGenerator: NetConfigGeneratorT
//
//  def optParamGeneratorFactory(netConfigParams: Seq[DNNParameter[Any]],
//                               netConfigValues: Seq[Any]): OptParamGeneratorT
//
//
//  /** Initialisation **/
//  val (cmdArgs, settings) = init(args)
//
//
//  /** Network configurations **/
//  // Create parameter space
//  val netConfigSpace: LayerParameterSpace = generateNetConfigParamSpace()
//  // Save space as ONNX files
//  generateONNXFiles()
//
//  /* Save space as MySQL records */
//  // Initialise MySQL
//  private val connector = new mysql.Connector(settings)
//  // Execute MySQL updates
//  saveNetConfigsInMySQL(connector)
//
//  // Close MySQL connection while we are not using it
//  connector.close()
//
//  /** Network optimisations **/
//  // Create parameter spaces
//  val optParamSpaceGenerators: Seq[OptParamGeneratorT] = generateOptParamSpaceGenerators()
//  val optParamSpaces: Seq[LayerParameterSpace] = generateOptParamSpaces()
//
//  // Reconnect to MySQL
//  connector.connect()
//
//  // Save optParamSpace in the MySQL database
//  saveOptParamsInMySQL(connector)
//
//
//  /*********** Implementations ***********/
//
//  def generateNetConfigParamSpace(): LayerParameterSpace = {
//    // TODO: explore the whole space in steps
//    netConfigGenerator(
//      countOnly = false,
//      skipFirstNCombinations = Some(1),
//      maxCombinations = Some(10))
//  }
//
//
//  def generateONNXFiles(): Unit = {
//    def ensureONNXDirExists(): Unit = {
//      val path = utils.joinPaths(settings.onnxConfigsRootDir, "onnx", "generated_files")
//      val dir = new File(path)
//      if (!dir.exists) new File(path).mkdirs()
//      else {
//        if (!dir.isDirectory) dir.delete()
//        new File(path).mkdirs()
//      }
//    }
//
//
//    def createONNXFactory(combination: Seq[Any]): String = {
//      // Make sure ONNX Python factory template exists
//      val onnxFactoryTemplate: File = new File(utils.joinPaths(
//        settings.onnxConfigsRootDir, "onnx", "factories", netConfigGenerator.pythonFactoryName))
//
//      if (!onnxFactoryTemplate.exists()) throw new FileNotFoundException("ONNX Python factory does not exist")
//
//      /* Create ONNX Python factory with current combination values */
//      val factoryTemplate = Source.fromFile(onnxFactoryTemplate.getAbsolutePath).mkString
//
//      // Replace independent variables with actual values in the template of the factory
//      val factoryTemplateWithoutIndependentParams =
//        netConfigSpace.parameters.zipWithIndex.foldLeft(factoryTemplate)((src, paramAndIndex) => {
//          val param = paramAndIndex._1
//          val paramIndex = paramAndIndex._2
//          param match {
//            case p: IndependentDNNParameter[Any] =>
//              src.replace("$" + p.name + "%" + p.`type`.formatSpecifier, combination(paramIndex).toString)
//
//            case p@_ =>
//              throw new IllegalArgumentException(f"Expected parameter $p to be an instance of IndependentParameter")
//          }
//        })
//
//      // Replace dependent variables with actual values
//      netConfigGenerator.dependentParameters.foldLeft(factoryTemplateWithoutIndependentParams)((src, param) =>
//        src.replace("$" + param.name + "%" + param.`type`.formatSpecifier,
//          param.function(combination, settings).toString)
//      )
//    }
//
//
//    def generateONNXFileUsingFactory(factorySource: String, combination: Seq[Any]): Unit = {
//      // Create new onnx factory script
//      val factoryFile = File.createTempFile("onnxFactory", ".py")
//      factoryFile.deleteOnExit()
//      val factoryWriter = new FileWriter(factoryFile)
//
//      factoryWriter.write(factorySource)
//
//      factoryWriter.close()
//
//      // Make sure the onnx file does not exist
//      val onnxFile: File = new File(netConfigGenerator.getOnnxConfigPath(combination, settings))
//
//      if (onnxFile.exists)
//        if (settings.allowOverwritingOnnxConfigs) onnxFile.delete()
//        else throw new FileAlreadyExistsException("Cannot overwrite " + onnxFile.getAbsolutePath)
//
//      // Produce the onnx file using the Python factory
//      val (out, err) = utils.runShell(Seq(settings.pythonExec, factoryFile.getAbsolutePath))
//      if (!err.isEmpty)
//        throw new IllegalStateException(f"Python ONNX factory failed.\nSTDERR:\n$err\nSTDOUT:\n$out")
//    }
//
//    /* Generate ONNX files */
//    ensureONNXDirExists()
//    for (combination <- netConfigSpace.combinations) {
//      val factorySource = createONNXFactory(combination)
//      generateONNXFileUsingFactory(factorySource, combination)
//    }
//  }
//
//
//  def saveNetConfigsInMySQL(connector: mysql.Connector): Unit = {
//    netConfigGenerator.mysqlTable.createTable(connector)
//    netConfigSpace.combinations.foreach{
//      combination => netConfigGenerator.mysqlTable.insertIfNotExists(combination, connector)
//    }
//  }
//
//  def generateOptParamSpaceGenerators(): Seq[OptParamGeneratorT] =
//    netConfigSpace.combinations.map(netConfigValue =>
//      optParamGeneratorFactory(
//        netConfigParams = netConfigSpace.parameters,
//        netConfigValues = netConfigValue))
//
//
//  def generateOptParamSpaces(): Seq[LayerParameterSpace] = {
//    // TODO: explore the whole space in steps
//    optParamSpaceGenerators.map(optParamGenerator =>
//      optParamGenerator(
//        countOnly = false,
//        skipFirstNCombinations = Some(1),
//        maxCombinations = Some(10)))
//  }
//
//
//  def saveOptParamsInMySQL(connector: mysql.Connector): Unit = {
//    optParamSpaceGenerators.zip(optParamSpaces).foreach { pair =>
//      val optParamSpaceGenerator = pair._1
//      val optParamSpace = pair._2
//
//      optParamSpaceGenerator.mysqlTable.createTable(connector)
//      optParamSpace.combinations.foreach{
//        combination => optParamSpaceGenerator.mysqlTable.insertIfNotExists(combination, connector)
//      }
//    }
//  }
//}
