package benchmarks

import java.io._
import java.nio.file.{Files, Paths}

import ir.ast.Lambda
import opencl.executor.Executor.ExecutorFailureException
import opencl.executor._
import opencl.generator.Verbose

import scala.sys.process._

@deprecated(message="Use the never version called Benchmark", "")
abstract class DeprecatedBenchmark(val name: String,
                                   val defaultInputSizes: Seq[Int],
                                   val f: Seq[(String, Array[Lambda])],
                                   val delta: Float,
                                   val defaultLocalSizes: Array[Int] = Array(128,1,1)) {

  var variant = -1
  var checkResult = false
  var printResult = false
  var trials = 10
  var inputs = Seq[Any]()
  var scalaResult = Array.emptyFloatArray
  var runtimes = Array.emptyDoubleArray
  var generatedKernel = Array.empty[String]

  // Parser options
  case class Config(trials: Int = 10,
                    platform: Int = 0,
                    device: Int = 0,
                    saveOutput: Option[String] = None,
                    loadOutput: Option[String] = None,
                    localSize: Array[Int] = defaultLocalSizes,
                    globalSize: Array[Int] = Array(inputSizes().product, 1, 1),
                    loadKernel: Option[String] = None,
                    saveAll: Boolean = false,
                    loadAll: Boolean = false,
                    csvFileName: Option[String] = None,
                    size: List[Int] = null,
                    verbose: Boolean = false,
                    variant: Int = 0,
                    variantRegex: String = ".*",
                    negateRegex: Boolean = false,
                    all: Boolean = false,
                    timeout: Option[Int] = None,
                    checkResult: Boolean = false,
                    printResult: Boolean = false,
                    injectLocal: Boolean = false,
                    injectGroup: Boolean = false,
                    experimentID: Option[String] = None,
                    input: File = null)

  //  val builder = OParser.builder[Config]
  //  val parser = {
  //    import builder._
  //    OParser.sequence(
  //      programName("Benchmark"),
  //      opt[Int]('i', "trials").text("Total trials (Default: 10)")
  //        .action((arg, c) => c.copy(trials = arg)),
  //
  //      opt[Int]('p', "platform").text("Id of the OpenCL platform to use (Default: 0)")
  //        .action((arg, c) => c.copy(platform = arg)),
  //
  //      opt[Int]('d', "device").text("Id of the OpenCL device to use (Default: 0)")
  //        .action((arg, c) => c.copy(device = arg)),
  //
  //      opt[String]("saveOutput").text("Save the gold result of the computation to a file")
  //        .action((arg, c) => c.copy(saveOutput = Some(arg))),
  //
  //      opt[String]("loadOutput").text("Load the gold result of the computation from a file. Takes precedence over saveOutput")
  //        .action((arg, c) => c.copy(loadOutput = Some(arg))),
  //
  //      opt[Seq[Int]]('l', "localSize").text("Local size(s) to use " +
  //        "(Defaults: " + defaultLocalSizes.mkString(", ") + ")")
  //        .action((arg, c) => c.copy(localSize = arg.toArray))
  //        .minOccurs(3).maxOccurs(3),
  //
  //      opt[Seq[Int]]('g', "globalSize").text("Global size(s) to use")
  //        .action((arg, c) => c.copy(globalSize = arg.toArray))
  //        .minOccurs(3).maxOccurs(3),
  //
  //      opt[String]("loadKernel").text("Load an OpenCL kernel source file")
  //        .action((arg, c) => c.copy(loadKernel = Some(arg))),
  //
  //      // TODO: Implement kernel saving/loading functionality.
  //      opt[Unit]("save-kernels").text("Save all kernels.")
  //        .action((_, c) => c.copy(saveAll = true)),
  //
  //      opt[Unit]("load-kernels").text("Load kernel for execution previously generated using -save-kernels")
  //        .action((_, c) => c.copy(loadAll = true)),
  //
  //      opt[String]("csvFileName").abbr("csv").text("If specified, results are stored in .csv file with given name")
  //        .action((arg, c) => c.copy(csvFileName = Some(arg))),
  //
  //      opt[Seq[Int]]('s', "inputSize").text("Size of the input to use, expecting ${defaultInputSizes.length}%d sizes.")
  //        .required()
  //        .action((arg, c) => c.copy(size = arg.toList))
  //        .minOccurs(defaultInputSizes.length).maxOccurs(defaultInputSizes.length),
  //
  //      opt[Unit]('v', "verbose").text("Print allocated memory and source code")
  //        .action((_, c) => c.copy(verbose = true)),
  //
  //      opt[Int]("variant").abbr("var").text("Which of the following variants to run (Default: 0):\n" +
  //        f.zipWithIndex.map(x => x._2 + " = " + x._1._1).mkString("\n"))
  //        .action((arg, c) => c.copy(variant = arg)),
  //
  //      opt[String]("variant-regex").abbr("var-regex").text("Which variant(s) to run, based on a regular expression")
  //        .action((arg, c) => c.copy(variantRegex = arg)),
  //
  //      opt[Unit]("negate-regex").abbr("nr").text("Negate the regular expression matching variants, " +
  //        "i.e. only select variants which do not match the regex")
  //        .action((_, c) => c.copy(negateRegex = true)),
  //
  //      opt[Unit]('a', "all").text("Run all variants, takes precedence over the variant option.")
  //        .action((_, c) => c.copy(all = true)),
  //
  //      opt[Int]('t', "timeout").text("If the kernel execution is longer than time, ignore any remaining trials.")
  //        .action((arg, c) => c.copy(timeout = Some(arg))),
  //
  //      opt[Unit]('c', "check").text("Check the result")
  //        .action((_, c) => c.copy(checkResult = true)),
  //
  //      opt[Unit]("print").text("Print the result")
  //        .action((_, c) => c.copy(printResult = true)),
  //
  //      opt[Unit]("inject").abbr("il").text("Inject the local size into the kernel as a constant, " +
  //        "possibly replacing some for loops with if statements.")
  //        .action((_, c) => c.copy(injectLocal = true)),
  //
  //      opt[Unit]("injectGroup").abbr("ig").text("Inject the number of groups into the kernel as a constant, " +
  //        "possibly replacing some for loops with if statements.")
  //        .action((_, c) => c.copy(injectGroup = true)),
  //
  //      opt[String]("experimentId").abbr("eid").text("A unique ID for this experiement for reporting data")
  //        .action((arg, c) => c.copy(experimentID = Some(arg))),
  //
  //      opt[File]("input").text("Input files to read")
  //        .required()
  //        .action((arg, c) => c.copy(input = arg))
  //        .validate(f => if (f.exists) success else failure("File \"" + f.getName + "\" does not exist")),
  //
  //      help("help").text("Show this message.")
  //    )}

  var cmdArgs = Config()
  val parser = new scopt.OptionParser[Unit]("DeprecatedBenchmark") {
    override val errorOnUnknownArgument = false

    opt[Int]('i', "trials").text("Total trials (Default: 10)")
      .foreach(arg => cmdArgs = cmdArgs.copy(trials = arg))

    opt[Int]('p', "platform").text("Id of the OpenCL platform to use (Default: 0)")
      .foreach(arg => cmdArgs = cmdArgs.copy(platform = arg))

    opt[Int]('d', "device").text("Id of the OpenCL device to use (Default: 0)")
      .foreach(arg => cmdArgs = cmdArgs.copy(device = arg))

    opt[String]("saveOutput").text("Save the gold result of the computation to a file")
      .foreach(arg => cmdArgs = cmdArgs.copy(saveOutput = Some(arg)))

    opt[String]("loadOutput").text("Load the gold result of the computation from a file. Takes precedence over saveOutput")
      .foreach(arg => cmdArgs = cmdArgs.copy(loadOutput = Some(arg)))

    opt[Seq[Int]]('l', "localSize").text("Local size(s) to use " +
      "(Defaults: " + defaultLocalSizes.mkString(", ") + ")")
      .foreach(arg => cmdArgs = cmdArgs.copy(localSize = arg.toArray))
      .minOccurs(3).maxOccurs(3)

    opt[Seq[Int]]('g', "globalSize").text("Global size(s) to use")
      .foreach(arg => cmdArgs = cmdArgs.copy(globalSize = arg.toArray))
      .minOccurs(3).maxOccurs(3)

    opt[String]("loadKernel").text("Load an OpenCL kernel source file")
      .foreach(arg => cmdArgs = cmdArgs.copy(loadKernel = Some(arg)))

    // TODO: Implement kernel saving/loading functionality.
    opt[Unit]("save-kernels").text("Save all kernels.")
      .foreach(_ => cmdArgs = cmdArgs.copy(saveAll = true))

    opt[Unit]("load-kernels").text("Load kernel for execution previously generated using -save-kernels")
      .foreach(_ => cmdArgs = cmdArgs.copy(loadAll = true))

    opt[String]("csvFileName").abbr("csv").text("If specified, results are stored in .csv file with given name")
      .foreach(arg => cmdArgs = cmdArgs.copy(csvFileName = Some(arg)))

    opt[Seq[Int]]('s', "inputSize").text("Size of the input to use, expecting ${defaultInputSizes.length}%d sizes.")
      .required()
      .foreach(arg => cmdArgs = cmdArgs.copy(size = arg.toList))
      .minOccurs(defaultInputSizes.length).maxOccurs(defaultInputSizes.length)

    opt[Unit]('v', "verbose").text("Print allocated memory and source code")
      .foreach(_ => cmdArgs = cmdArgs.copy(verbose = true))

    opt[Int]("variant").abbr("var").text("Which of the following variants to run (Default: 0):\n" +
      f.zipWithIndex.map(x => x._2 + " = " + x._1._1).mkString("\n"))
      .foreach(arg => cmdArgs = cmdArgs.copy(variant = arg))

    opt[String]("variant-regex").abbr("var-regex").text("Which variant(s) to run, based on a regular expression")
      .foreach(arg => cmdArgs = cmdArgs.copy(variantRegex = arg))

    opt[Unit]("negate-regex").abbr("nr").text("Negate the regular expression matching variants, " +
      "i.e. only select variants which do not match the regex")
      .foreach(_ => cmdArgs = cmdArgs.copy(negateRegex = true))

    opt[Unit]('a', "all").text("Run all variants, takes precedence over the variant option.")
      .foreach(_ => cmdArgs = cmdArgs.copy(all = true))

    opt[Int]('t', "timeout").text("If the kernel execution is longer than time, ignore any remaining trials.")
      .foreach(arg => cmdArgs = cmdArgs.copy(timeout = Some(arg)))

    opt[Unit]('c', "check").text("Check the result")
      .foreach(_ => cmdArgs = cmdArgs.copy(checkResult = true))

    opt[Unit]("print").text("Print the result")
      .foreach(_ => cmdArgs = cmdArgs.copy(printResult = true))

    opt[Unit]("inject").abbr("il").text("Inject the local size into the kernel as a constant, " +
      "possibly replacing some for loops with if statements.")
      .foreach(_ => cmdArgs = cmdArgs.copy(injectLocal = true))

    opt[Unit]("injectGroup").abbr("ig").text("Inject the number of groups into the kernel as a constant, " +
      "possibly replacing some for loops with if statements.")
      .foreach(_ => cmdArgs = cmdArgs.copy(injectGroup = true))

    opt[String]("experimentId").abbr("eid").text("A unique ID for this experiement for reporting data")
      .foreach(arg => cmdArgs = cmdArgs.copy(experimentID = Some(arg)))

    opt[File]("input").text("Input files to read")
      .required()
      .foreach(arg => cmdArgs = cmdArgs.copy(input = arg))
      .validate(f => if (f.exists) success else failure("File \"" + f.getName + "\" does not exist"))

    help("help").text("Show this message.")
  }

  def runScala(inputs: Any*): Array[Float]

  def generateInputs(): Seq[Any]

  def inputSizes(): Seq[Int] = {
    if (cmdArgs.size.length == defaultInputSizes.length) cmdArgs.size else defaultInputSizes
  }

  def runOpenCL(inputs: Any*): (Array[Float], Double) = {

    var realInputs = inputs
    val realGlobalSizes = globalSize
    var totalRuntime = 0.0
    var finalOutput = Array.emptyFloatArray

    val lambdas: Seq[Lambda] = f(variant)._2
    var generateKernels = false


    if (generatedKernel.length == 0) {
      generateKernels = true
      generatedKernel = Array.ofDim(lambdas.length)
    }


    for (i <- lambdas.indices) {

      if (generateKernels)
        generatedKernel(i) = Utils.compile(lambdas(i),
          realInputs,
          localSize(0),
          localSize(1),
          localSize(2),
          realGlobalSizes(0),
          realGlobalSizes(1),
          realGlobalSizes(2),
          (cmdArgs.injectLocal, cmdArgs.injectGroup))

      val (output, runtime) = Execute(
        localSize(0),
        localSize(1),
        localSize(2),
        realGlobalSizes(0),
        realGlobalSizes(1),
        realGlobalSizes(2),
        (cmdArgs.injectLocal, cmdArgs.injectGroup)
      )[Array[Float]](generatedKernel(i), f, realInputs)

      // Adjust parameters for the next kernel, if any
      realInputs = Seq(output)
      realGlobalSizes(0) = output.length
      realGlobalSizes(1) = 1
      realGlobalSizes(2) = 1

      totalRuntime += runtime
      finalOutput = output
    }

    (finalOutput, totalRuntime)
  }

  protected def localSize: Array[Int] = {
    val localSizes = defaultLocalSizes.clone()
    cmdArgs.localSize.copyToArray(localSizes)
    localSizes
  }

  protected def globalSize: Array[Int] = {
    val globalSizes = Array(inputSizes().product, 1, 1)
    cmdArgs.globalSize.copyToArray(globalSizes)
    globalSizes
  }

  protected def printParams(): Unit = {}

  protected def printResults(time: Double): Unit = {
    println("BANDWIDTH: " + bandwidth(time) + " GB/s" )
  }

  def printCSVFile(filename: String,
                   kernel: String,
                   commit: String,
                   branch: String,
                   date: String,
                   dce: Boolean,
                   cse: Boolean): Unit = {
    if(!Files.exists(Paths.get(filename))) {
      val fw = new FileWriter(filename, true)
      try {
        fw.write("Benchmark;Kernel;Date;Commit;Branch;")
        inputSizes().zipWithIndex.foreach{case(e, i) => fw.write("Size" + i + ";")}
        fw.write("InjectGroup;InjectLocal;")
        fw.write("Iterations;GlobalSize0;GlobalSize1;GlobalSize2;")
        fw.write("LocalSize0;LocalSize1;LocalSize2;")
        fw.write("DCE;CSE;")
        fw.write("Platform;Device;Median;Bandwidth\n")
      } finally fw.close()
    }

    val fw = new FileWriter(filename, true)
    try {

      fw.write(name + "_" + f(variant)._1 + ";" + kernel + ";\"" + date + "\";")
      fw.write(commit + ";" + branch + ";" )
      inputSizes().zipWithIndex.foreach{case(e, i) => fw.write(e + ";")}
      fw.write(cmdArgs.injectGroup + ";" + cmdArgs.injectLocal + ";")
      fw.write(trials + ";" + globalSize(0) + ";" + globalSize(1) + ";" + globalSize(2) + ";")
      fw.write(localSize(0) + ";" + localSize(1) + ";" + localSize(2) + ";")
      fw.write(dce + ";" + cse + ";")
      fw.write("\"" + Executor.getPlatformName + "\";\"" + Executor.getDeviceName + "\";")
    } finally fw.close()
  }

  def printMedianAndBandwidth(median: Double, bandwidth: Double): Unit = {
    val fw = new FileWriter(cmdArgs.csvFileName.get, true)
    try {
      fw.write(median + ";" + bandwidth + "\n")
    } finally fw.close()
  }

  def runBenchmark(): Unit = {
    val kernel = if (cmdArgs.loadKernel.isDefined)
      cmdArgs.loadKernel.get.replaceAll("(.*?/)*", "")
    else "generated"
    val commit = "git rev-parse HEAD".!!.trim
    val branch = "git rev-parse --abbrev-ref HEAD".!!.trim
    val date = "date".!!.trim
    val dce = System.getenv("LIFT_DCE") != null
    val cse = System.getenv("LIFT_CSE") != null

    if (cmdArgs.csvFileName.isDefined)
      printCSVFile(cmdArgs.csvFileName.get, kernel, commit, branch, date, dce, cse)

    println(date)
    println("Benchmark: " + name + " " + f(variant)._1)
    println("Kernel: " + kernel)
    println("Size(s): " + inputSizes().mkString(", "))
    println("Dead Code Elimination: " + dce)
    println("Common Subexpression Extraction: " + cse)
    println("Total trials: " + trials)
    println("Checking results: " + checkResult)
    println("Global sizes: " + globalSize.mkString(", "))
    println("Local sizes: " + localSize.mkString(", "))
    println("Inject local: " + cmdArgs.injectLocal)
    println("Inject global: " + cmdArgs.injectGroup)
    println("Platform: " + Executor.getPlatformName)
    println("Device: " + Executor.getDeviceName)
    printParams()
    print("Machine: " + "hostname".!!)
    print("finger".!!)
    println("Commit: " + commit)
    println("Branch: " + branch)
    print("Diff:\n" + "git diff".!!)

    println()

    val lambdas = f(variant)._2
    if (lambdas.length == 1) {
      // Just one kernel, iterate inside SkelCL

      val timeout = if (cmdArgs.timeout.isDefined) cmdArgs.timeout.get.toDouble else Double.MaxValue

      if (cmdArgs.loadKernel.isDefined) {
        Execute(
          localSize(0),
          localSize(1),
          localSize(2),
          globalSize(0),
          globalSize(1),
          globalSize(2),
          (cmdArgs.injectLocal, cmdArgs.injectGroup)
        )[Array[Float]](0, timeout, lambdas.head, inputs:_*)

        val code = scala.io.Source.fromFile(cmdArgs.loadKernel.get).mkString

        val (output, runtimes) = Execute(
          localSize(0),
          localSize(1),
          localSize(2),
          globalSize(0),
          globalSize(1),
          globalSize(2),
          (cmdArgs.injectLocal, cmdArgs.injectGroup)
        ).benchmark[Array[Float]](trials, timeout, code, lambdas.head, inputs:_*)

        if (checkResult)
          checkResult(output)

        if (printResult)
          printResult(output)

        val median = runtimes.sortWith(_<_)(runtimes.length/2)

        println("MEDIAN: " + median + " ms")
        printResults(median)
        if(cmdArgs.csvFileName.isDefined) printMedianAndBandwidth(median, bandwidth(median))

      }
      else {
        val (output, runtimes) = Execute(
          localSize(0),
          localSize(1),
          localSize(2),
          globalSize(0),
          globalSize(1),
          globalSize(2),
          (cmdArgs.injectLocal, cmdArgs.injectGroup)
        )[Array[Float]](trials, timeout, lambdas.head, inputs:_*)

        if (checkResult)
          checkResult(output)

        if (printResult)
          printResult(output)

        val median = runtimes.sortWith(_<_)(runtimes.length/2)

        println("MEDIAN: " + median + " ms")
        printResults(median)
        if(cmdArgs.csvFileName.isDefined) printMedianAndBandwidth(median, bandwidth(median))
      }

    } else {
      for (i <- 0 until trials) {

        if (i == 1)
          Verbose(false)

        println("Iteration: " + i)

        val (output, runtime) = runOpenCL(inputs: _*)

        runtimes(i) = runtime
        println("Runtime: " + runtime + " ms")

        if (checkResult && i == 0)
          checkResult(output)

        if (printResult)
          printResult(output)

      }

      val sorted = runtimes.sorted

      println()
      println("MIN: " + sorted(0) + " ms")
      println("MAX: " + sorted(trials - 1) + " ms")
      val medianTime = median(sorted)
      println("MEDIAN: " + medianTime + " ms")
      printResults(medianTime)
      if(cmdArgs.csvFileName.isDefined) printMedianAndBandwidth(medianTime, bandwidth(medianTime))
      println()
    }
  }

  private def checkResult(output: Array[Float]): Unit = {
    if (output.length != scalaResult.length) {
      println(s"Output length is wrong, ${output.length} vs ${scalaResult.length}")

    } else {
      var numErrors = 0

      for (j <- scalaResult.indices) {
        if (check(output(j), scalaResult(j))) {
          numErrors += 1
        }
      }

      if (numErrors != 0)
        println(s"Output differed in $numErrors positions!")
      else
        println("Sequential test passed!")
    }
  }

  private def printResult(output: Array[Float]): Unit = {
    val fw = new FileWriter("result.txt")
    try {
      fw.write(output.map(_.toInt).mkString("\n"))
      //fw.write(output.mkString("\n"))
    } finally fw.close()
  }

  protected def bandwidth(time: Double): Double = {
    4 * inputSizes().product.toDouble / time * 0.000001
  }

  protected def check(x: Float, y: Float): Boolean = {
    (x - y).abs > delta
  }

  protected def beforeBenchmark(): Unit = {}

  private def median(sorted: Array[Double]): Double = {
    val trials = sorted.length
    if (trials % 2 == 0)
      sorted((trials - 1) / 2)
    else
      (sorted((trials - 1) / 2) + sorted(trials / 2)) / 2
  }

  def run(args: Array[String]): Unit = {
    try {
      //      cmdArgs = Some(CommandLineParser(parser, args, Config()))
      if (!parser.parse(args))
        throw new IllegalArgumentException("Wrong command line arguments passed")

      Executor.loadLibrary()
      Executor.init(cmdArgs.platform, cmdArgs.device)
      Verbose(cmdArgs.verbose)

      variant = cmdArgs.variant
      checkResult = cmdArgs.checkResult
      printResult = cmdArgs.printResult

      trials = cmdArgs.trials
      inputs = generateInputs()
      runtimes = Array.ofDim[Double](trials)

      if (checkResult) {
        if (cmdArgs.loadOutput.isEmpty) {
          scalaResult = runScala(inputs:_*)

          if (cmdArgs.saveOutput.isDefined) {
            val oos = new ObjectOutputStream(new FileOutputStream(cmdArgs.saveOutput.get))
            oos.writeObject(scalaResult)
            oos.close()
          }

        } else {
          val ois = new ObjectInputStream(new FileInputStream(cmdArgs.loadOutput.get))
          val readResult = ois.readObject()
          ois.close()
          scalaResult = readResult.asInstanceOf[Array[Float]]
        }
      }

      beforeBenchmark()

      if (cmdArgs.all) {
        for (i <- f.indices) {
          variant = i
          runBenchmark()
        }

      } else {
        runBenchmark()
      }

      Executor.shutdown()

    } catch {
      case x: ExecutorFailureException => x.printStackTrace(); printMedianAndBandwidth(0,0)
    }
  }
}
