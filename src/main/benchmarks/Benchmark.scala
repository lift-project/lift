package benchmarks

import java.io._

import ir.ast.Lambda
import opencl.executor.Decoder.DecodeTypes.DecodeType
import opencl.executor.Executor.ExecutorFailureException
import opencl.executor._
import opencl.generator.Verbose
import _root_.utils.CommandLineParser
import scopt.OParser

import scala.collection.immutable
import scala.reflect.ClassTag
import scala.sys.process._


case class BenchmarkConfiguration
(
  var checkResult: Boolean, // config information
  var printResult: Boolean, // config information
  var trials: Int, // config information
  var timeout: Double,
  var injectLocal: Boolean,
  var injectGroup: Boolean,
  var experimentID: String
)

object BenchmarkUtils {
  def bandwidth(time: Double, sizes: Seq[Int]): Double = {
    4 * sizes.product.toDouble / time * 0.000001
  }

  def median(sorted: Array[Double]): Double = {
    val trials = sorted.length
    if (trials % 2 == 0)
      sorted((trials - 1) / 2)
    else
      (sorted((trials - 1) / 2) + sorted(trials / 2)) / 2
  }
}

abstract class Benchmark[T: ClassTag](val name: String,
                                      var defaultInputSizes: Seq[Int],
                                      val f:Seq[(String, Array[Lambda])],
                                      val areEqual: (T, T) => Boolean,
                                      val defaultLocalSizes: Array[Int] = Array(128,1,1))(implicit outT: DecodeType[T]) {

  /*
   To make it easier to implement the iterative benchmarks (which would inherit from this)
    we declare a type for the return type of the runScala function, which we can change when we subclass
   */
  type SRes = T

  /*
   So that we can report individiual kernel execution times, we define an "instance statistic" type, which
   Encodes a number of trials, (outer array), each of which has (possibly) a number of sub-statistics (e.g. if
   we have multiple kernels, or an iteration in an instance), as well as a correctness result.
   */
  type Trial = (Array[Double], RunResult)
  type InstanceStatistic = Array[Trial]

  /*
    Classes that inherit from benchmark2 must implement the following methods, to create input and gold values
   */
  // generate inputs for this benchmark
  def generateInputs() : Seq[Any]

  // run a scala gold version for this benchmark
  def runScala(inputs: Seq[Any]) : SRes

  /*
    Classes that inherit from benchmark2 may *optionally* implement the following methods, which customise
    the behaviour of the benchmark for edge cases (e.g. for a variant which needs a transposed input)
   */
  // preprocess the inputs for a specific variant
  def preProcessInputs(variant: Int, name: String, inputs: Seq[Any]): Seq[Any] = inputs

  // postprocess the results for a specific variant
  def postProcessResult(variant: Int, name: String, result: T): T = result

  // Mutate the lambdas of a particular variant, e.g. for a case that depends on last-minute information
  protected def updateLambdas(variant: Int, name: String, lambdas: Array[Lambda]) : Array[Lambda] = {
    lambdas
  }

  /* Size information for the benchmark */
  def inputSizes(): Seq[Int] = {
    if (cmdArgs.get.size.length == defaultInputSizes.length) cmdArgs.get.size else defaultInputSizes
  }


  /*
    Inheritable functions for printing runtime information - such as git stuff
    Split into generic benchmark header information, for information that's common to all instances, and
    instance header information, for data relevant to specific instances
   */
  def printBenchmarkHeader(configuration: BenchmarkConfiguration,
                           variants: Seq[(Int, String, Array[Lambda])]) : Unit = {
        val commit = "git rev-parse HEAD".!!.trim
        val branch = "git rev-parse abbrev-ref HEAD".!!.trim
        val date = "date".!!.trim
        val dce = System.getenv("LIFT_DCE") != null
        val cse = System.getenv("LIFT_CSE") != null

    //    if (csvFileName.isDefined)
    //      printCSVFile(csvFileName.get, kernel, commit, branch, date, dce, cse)

        println(date)
        println("Benchmark: " + name + ", variant(s): " +
          variants.map {case (_,name,_) => name}.mkString("[",",","]"))
        println("Size(s): " + inputSizes().mkString(", "))
        println("Dead Code Elimination: " + dce)
        println("Common Subexpression Extraction: " + cse)
        println("Total trials: " + configuration.trials)
        println("Checking results: " + configuration.checkResult)
        println("Global sizes: " + cmdArgs.get.globalSize.mkString(", "))
        println("Local sizes: " + cmdArgs.get.localSize.mkString(", "))
        println("Inject local: " + cmdArgs.get.injectLocal)
        println("Inject global: " + cmdArgs.get.injectGroup)
        println("Platform: " + Executor.getPlatformName)
        println("Device: " + Executor.getDeviceName)
        printParams()
        print("Machine: " + "hostname".!!)
        print("finger".!!)
        println("Commit: " + commit)
        println("Branch: " + branch)
        print("Diff:\n" + "git diff".!!)

        println()
  }

  /*
   Print a header for a specific benchmark instance (i.e. a variant run)
   */
  def printInstanceHeader(configuration: BenchmarkConfiguration, variant: (Int,String,Array[Lambda])) : Unit = {
    println(s"Variant ${variant._1}: ${variant._2}")
    println(s"Kernels: (${variant._3.length} total)")
    variant._3.zipWithIndex.foreach{ case (lambda, kIdx) =>
        println(s"=== kernel $kIdx ===")
        println(s"Lambda: ${lambda}")
        val code = Compile(lambda)
        println(code)
    }
  }

  /*
    Print parameters used for the run
   */
  protected def printParams(): Unit = {}

  /*
    Print the result of the computation
   */
  def printResult(result: T) : Unit = {
    result match {
      case a: Array[_] => println(a.map(_.toString).mkString("[", ",", "]"))
      case _ => throw new NotImplementedError(result.toString)
    }
  }

  /*
    build a string showing the SQL header for inserting results of a particular instance
   */
  def buildInstanceSQLHeader() : String = {
    return "INSERT INTO RESULTS (variant, name, time, correctness) VALUES "
  }

  /*
    build a string showing the SQL values produced for a variant and instance
   */
  def buildInstanceSQLValues(variant: Int,
                             name: String,
                             lambdas: Array[Lambda],
                             configuration: BenchmarkConfiguration,
                             iStats: InstanceStatistic): String = {

    // INCLUDE THE DATE/TIME HERE SOMEHOW!
//    return s"(${variant}, '${name}', ${time}, ${correctness})"
    return ""
  }

  /*
    build a string showing the SQL header for inserting some aggregate statistic
   */
  def buildAggregateSQLHeader() : String = {
    return ""
  }

  /*
    build a string inserting the SQL values produced for an aggregate of a set of results
    This results in one query, so should produce fewer results...
   */
  def buildAggregateSQLValues(varint: Int,
                              name: String,
                              lambdas: Array[Lambda],
                              configuration: BenchmarkConfiguration,
                              iStats: InstanceStatistic): String = {
    return ""
  }


  /*
    Run a benchmark instance - i.e. a specific variant of a benchmark
    Usually this will only be called once (e.g. with variantOpt set), but it may be called for multiple variants
    (e.g. with all or variantRegex), hence it should not set state.
  */
  def runBenchmarkInstance(variant: Int, name: String, lambdas: Array[Lambda],
                           inputs: Array[Any], expectedResult: SRes,
                           configuration: BenchmarkConfiguration) : InstanceStatistic = {
    val kernel = if (cmdArgs.get.loadKernel.isDefined)
      cmdArgs.get.loadKernel.get.replaceAll("(.*?/)*", "")
    else "generated"

    lambdas.length match {
      case 1 => runSingleLambda(
        lambdas.head,
        inputs,
        expectedResult,
        variant,
        name,
        configuration
      )
      case _ =>
        runMultipleLambdas(
        lambdas,
        inputs,
        expectedResult,
        variant,
        name,
        configuration
      )
    }
  }

  /*
    Produce opencl Kernel code for a given instance, either loading it from a file, or compiling the kernel
   */
  def getKernelSource(lambda: Lambda,
                      variant: Int,
                      name: String,
                      configuration: BenchmarkConfiguration) : String = {
    val kernelSource = if (cmdArgs.get.loadKernel.isDefined || cmdArgs.get.loadAll) {
      val kFileName = if (cmdArgs.get.loadKernel.isDefined) {
        cmdArgs.get.loadKernel.get
      } else {
        s".kernels/${name}.kernel"
      }
      scala.io.Source.fromFile(kFileName).mkString
    } else {
      val KS = (configuration.injectLocal, configuration.injectGroup) match {
        case (true, false) => Compile(
          lambda,
          cmdArgs.get.localSize(0),
          cmdArgs.get.localSize(1),
          cmdArgs.get.localSize(2)
        )
        case (true, true) => Compile(
          lambda,
          cmdArgs.get.localSize(0),
          cmdArgs.get.localSize(1),
          cmdArgs.get.localSize(2),
          cmdArgs.get.globalSize(0),
          cmdArgs.get.globalSize(1),
          cmdArgs.get.globalSize(2),
          immutable.Map()
        )
        case _ => Compile(lambda)
      }

      // if we want to save the compilation result, do it
      if (cmdArgs.get.saveAll) {
        rewriting.utils.DumpToFile.dumpToFile(KS, s"${name}.kernel", ".kernels")
      }
      KS
    }

    // finally return the source
    kernelSource
  }

  /*
    Run a single lambda, return performance information, and a correctness result
   */
  def runSingleLambda(lambda: Lambda,
                      inputs: Seq[Any],
                      expectedResult: SRes,
                      variant: Int,
                      name: String,
                      configuration: BenchmarkConfiguration): InstanceStatistic = {

    var kernelSource = getKernelSource(lambda, variant, name, configuration)

    val (unprocessed_output, runtimes : Array[Double]) = Execute(
      cmdArgs.get.localSize(0),
      cmdArgs.get.localSize(1),
      cmdArgs.get.localSize(2),
      cmdArgs.get.globalSize(0),
      cmdArgs.get.globalSize(1),
      cmdArgs.get.globalSize(2),
      (configuration.injectLocal, configuration.injectGroup)
    ).benchmark[T](configuration.trials, configuration.timeout, kernelSource, lambda, inputs: _*)

    val output = postProcessResult(variant, name, unprocessed_output)
    val validity = configuration.checkResult match {
      case true => {
        val valid = checkResult(output, expectedResult)
        println(valid.toString)
        valid
      }
      case _ => NotChecked()
    }
    val validities = Array.fill[RunResult](configuration.trials)(validity)


    //    for (i <- 0 until configuration.trials){
//      val (unProcessedOutput: Array[T], runtime) = Execute(
//        localSize(0),
//        localSize(1),
//        localSize(2),
//        globalSize(0),
//        globalSize(1),
//        globalSize(2),
//        (configuration.injectLocal, configuration.injectGroup)
//      )(kernelSource, lambda, inputs: _*)
//
//      val output = postProcessResult(variant, name, unProcessedOutput)
//      val validity = configuration.checkResult match {
//        case true => {
//          val valid = checkResult(output, expectedResult)
//          println(valid.toString)
//          valid
//        }
//        case _ => NotChecked()
//      }
//
//      if (configuration.printResult)
//        printResult(output)
//      runtimesAndValidities(i) = (runtime, validity)
//    }

    // wrap the times in an array.
    (runtimes zip validities).map(p => (Array(p._1), p._2))
  }

  /*
    Run a list of lambdas, return performance information, and a correctness result
   */
  def runMultipleLambdas(lambdas: Array[Lambda],
                                   inputs: Seq[Any],
                                   expectedResult: SRes,
                                   variant: Int,
                                   name: String,
                                   configuration: BenchmarkConfiguration) : InstanceStatistic = {
    var runtimes = Array.ofDim[Double](configuration.trials, lambdas.length)
    var validities = Array.ofDim[RunResult](configuration.trials)
    // cache the compiled kernels
    val kernels = Array.ofDim[String](lambdas.length)
    for (i <- 0 until configuration.trials) {
      if (i == 1)
        Verbose(false)
      println("Iteration: " + i)

      /*
        Actually run the lambdas - inlined from the old "runOpenCL" function
       */
      var realInputs = inputs
      val realGlobalSizes = cmdArgs.get.globalSize
      var totalRuntime = 0.0
      var finalOutput: Array[T] = Array.ofDim[T](1)

      for (j <- lambdas.indices) {
        if(i == 0)
          kernels(j) = Utils.compile(lambdas(j),
            realInputs,
            cmdArgs.get.localSize(0),
            cmdArgs.get.localSize(1),
            cmdArgs.get.localSize(2),
            realGlobalSizes(0),
            realGlobalSizes(1),
            realGlobalSizes(2),
            (configuration.injectLocal, configuration.injectGroup))

        val (kOutput, kRuntime) = Execute(
          cmdArgs.get.localSize(0),
          cmdArgs.get.localSize(1),
          cmdArgs.get.localSize(2),
          realGlobalSizes(0),
          realGlobalSizes(1),
          realGlobalSizes(2),
          (configuration.injectLocal, configuration.injectGroup)
        )[T](kernels(j), realInputs)

        // update parameters for next kernel, if any
        realInputs = Seq(kOutput)
        //realGlobalSizes(0) = kOutput.length
        realGlobalSizes(0) = 128
        realGlobalSizes(1) = 1
        realGlobalSizes(2) = 1

        totalRuntime += kRuntime
        runtimes(i)(j) = kRuntime
        finalOutput(0) = kOutput
      }

      val output = postProcessResult(variant, name, finalOutput(0))

      validities(i) = configuration.checkResult match {
        case true => {
          val valid = checkResult(output, expectedResult)
          println(valid.toString)
          valid
        }
        case _ => NotChecked()
      }

      // add early bailout code here?
      if (configuration.printResult)
        printResult(output)

    }

    (runtimes zip validities)
  }


  /*
    Benchmark entrypoint
   */
  def run(args: Array[String]) : Unit = {
    try {
      println("Parsing arguments.")

      cmdArgs = Some(CommandLineParser(parser, args, Config()))

      println("Loading Executor.")
      Executor.loadLibrary()
      Executor.init(cmdArgs.get.platform, cmdArgs.get.device)
      Verbose(cmdArgs.get.verbose)

      val gitHash = "git rev-parse HEAD".!!.trim()
      val nowString = java.time.LocalDateTime.now().toString()

      println("Initialising Configuration.")
      // initialise a configuration object that we can pass around cheaply
      val configuration = new BenchmarkConfiguration (
        checkResult = cmdArgs.get.checkResult,
        printResult = cmdArgs.get.printResult,
        trials = cmdArgs.get.trials,
        timeout = if (cmdArgs.get.timeout.isDefined) cmdArgs.get.timeout.get.toDouble else Double.MaxValue,
        injectLocal = cmdArgs.get.injectLocal,
        injectGroup = cmdArgs.get.injectGroup,
        experimentID = cmdArgs.get.experimentID.getOrElse(gitHash + "-" + nowString)
      )

      println("Generating Inputs.")
      val inputs = generateInputs()
      println("Running Scala version.")
      var scalaResult = getScalaResult(inputs, configuration)

      // refactored into a more functional style. In this stage, we build a sequence of variants to process
      // which we map over, processing to get a correctness result and a time taken
      var variants = f.zipWithIndex.map{ case ((name, lambdas), variant) => (variant, name, lambdas)}

      // Select specific variant(s)
      // There are three options for picking variant, which override each other in this priorty:
      //  - all - which should select all variants (priority 1)
      //  - variantRegex - which should select variants with names matching a regex (priority 2)
      //  - variant - which selects a specific variant (numerically)
      if (!cmdArgs.get.all) { // all not defined
        if (cmdArgs.get.variantRegex != None) { // a regex is defined
          val VariantPattern = cmdArgs.get.variantRegex.r
          val nr = cmdArgs.get.negateRegex
          variants = variants.filter { case (variant, name, lambdas) =>
            name match {
              case VariantPattern() => !nr
              case _ => nr
            }
          }
        } else { // use the variantOpt
          variants = Seq(variants(cmdArgs.get.variant))
        }
      }

      print(s"Running ${variants.length} variants: ")
      println(variants.map(_._2).mkString("(",",",")"))

      printBenchmarkHeader(configuration, variants)

      // iterate over the variants, running them and collecting correctness and time
      val results = variants.map { case (variant, name, lambdas) =>
        val newLambdas = updateLambdas(variant, name, lambdas)
        printInstanceHeader(configuration, (variant, name, newLambdas))
        val preProcessedInputs = preProcessInputs(variant, name, inputs).toArray
        val instanceResult = runBenchmarkInstance(variant, name, newLambdas, preProcessedInputs, scalaResult, configuration)
        println(s"Results for ${variant}/${name}")
//        println(s"   time (ms): ${time}")
//        println(s"   correctness: ${correctness}")

        // print out an SQL statement for the instance, inserting the results
        println("INSTANCE SQL: " + buildInstanceSQLHeader() +
          buildInstanceSQLValues(variant, name, lambdas, configuration, instanceResult) + ";")
        (variant, name, lambdas, instanceResult)
      }

      // print out an overall SQL query:
      val sqlQuery = results.map { case (variant, name, lambdas, instanceResult) =>
        buildAggregateSQLValues(variant, name, lambdas, configuration, instanceResult)
      }.mkString(buildAggregateSQLHeader(), ",", ";")
      println("AGGREGATE SQL QUERY: ")
      println(sqlQuery)

      // finally, shut down the executor
      Executor.shutdown()

    } catch {
      case x: ExecutorFailureException => x.printStackTrace()//; printMedianAndBandwidth(0,0)
    }
  }

  private final def getScalaResult(inputs: Seq[Any], configuration: BenchmarkConfiguration) : SRes = {
    val scalaRes = runScala(inputs)
    if (configuration.checkResult) {
      if (cmdArgs.get.loadOutput.isEmpty) {

        if (cmdArgs.get.saveOutput.isDefined) {
          val oos = new ObjectOutputStream(new FileOutputStream(cmdArgs.get.saveOutput.get))
          oos.writeObject(scalaRes)
          oos.close()
        }

        scalaRes
      } else {
        val ois = new ObjectInputStream(new FileInputStream(cmdArgs.get.loadOutput.get))
        val readResult = ois.readObject()
        ois.close()
        readResult.asInstanceOf[SRes]
      }
    }else{
      scalaRes
    }
  }

  // check the result of a run
  sealed trait RunResult
  case class Correct() extends RunResult {
    override def toString = "correct"
  }
  case class NotChecked() extends RunResult {
    override def toString = "notchecked"
  }
  case class GenericFailure() extends RunResult {
    override def toString = "genericfailure"
  }
  case class BadLength(expected: Int, actual: Int) extends RunResult {
    override def toString = "badlength"
  }
  case class BadValues() extends RunResult{
    override def toString = "badvalues"
  }
  // generic way of reporting bad values without exact data
  case class GenericBadValues() extends RunResult {
    override def toString = "badvalues"
  }

  def checkResult(output: SRes, expected: SRes): RunResult = {
    if (areEqual(expected, output))
      Correct()
    else
      BadValues()
  }

  // how bad is a given run result
  private final def badness(validity: RunResult) : Int = {
    validity match {
      case Correct() => 0
      case NotChecked() => 1
      case GenericFailure() => 2
      case BadLength(_, _) => 3
      case GenericBadValues() => 4
      case BadValues() => 5
    }
  }
  // find the worst result in a list of results :)
  private final def getWorstResult(validities: Array[RunResult]) : RunResult = {
    validities.fold(Correct()) { (acc, x) =>
      if(badness(acc) > badness(x)){
        acc
      }else{
        x
      }
    }
  }

  /*
    ===================================================================================================================
  // Configuration options etc for the command line interface
    ===================================================================================================================
   */
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

  val builder = OParser.builder[Config]
  var cmdArgs: Option[Config] = None
  val parser = {
    import builder._
    OParser.sequence(
      programName("Benchmark"),
      opt[Int]('i', "trials").text("Total iterations (Default: 10)")
        .action((arg, c) => c.copy(trials = arg)),

      opt[Int]('p', "platform").text("Id of the OpenCL platform to use (Default: 0)")
        .action((arg, c) => c.copy(platform = arg)),

      opt[Int]('d', "device").text("Id of the OpenCL device to use (Default: 0)")
        .action((arg, c) => c.copy(device = arg)),

      opt[String]("saveOutput").text("Save the gold result of the computation to a file")
        .action((arg, c) => c.copy(saveOutput = Some(arg))),

      opt[String]("loadOutput").text("Load the gold result of the computation from a file. Takes precedence over saveOutput")
        .action((arg, c) => c.copy(loadOutput = Some(arg))),

      opt[Seq[Int]]('l', "localSize").text("Local size(s) to use " +
        "(Defaults: " + defaultLocalSizes.mkString(", ") + ")")
        .action((arg, c) => c.copy(localSize = arg.toArray))
        .minOccurs(3).maxOccurs(3),

      opt[Seq[Int]]('g', "globalSize").text("Global size(s) to use")
        .action((arg, c) => c.copy(globalSize = arg.toArray))
        .minOccurs(3).maxOccurs(3),

      opt[String]("loadKernel").text("Load an OpenCL kernel source file")
        .action((arg, c) => c.copy(loadKernel = Some(arg))),

      // TODO: Implement kernel saving/loading functionality.
      opt[Unit]("save-kernels").text("Save all kernels.")
        .action((_, c) => c.copy(saveAll = true)),

      opt[Unit]("load-kernels").text("Load kernel for execution previously generated using -save-kernels")
        .action((_, c) => c.copy(loadAll = true)),

      opt[String]("csvFileName").abbr("csv").text("If specified, results are stored in .csv file with given name")
        .action((arg, c) => c.copy(csvFileName = Some(arg))),

      opt[Seq[Int]]('s', "inputSize").text("Size of the input to use, expecting ${defaultInputSizes.length}%d sizes.")
        .required()
        .action((arg, c) => c.copy(size = arg.toList))
        .minOccurs(defaultInputSizes.length).maxOccurs(defaultInputSizes.length),

      opt[Unit]('v', "verbose").text("Print allocated memory and source code")
        .action((_, c) => c.copy(verbose = true)),

      opt[Int]("variant").abbr("var").text("Which of the following variants to run (Default: 0):\n" +
        f.zipWithIndex.map(x => x._2 + " = " + x._1._1).mkString("\n"))
        .action((arg, c) => c.copy(variant = arg)),

      opt[String]("variant-regex").abbr("var-regex").text("Which variant(s) to run, based on a regular expression")
        .action((arg, c) => c.copy(variantRegex = arg)),

      opt[Unit]("negate-regex").abbr("nr").text("Negate the regular expression matching variants, " +
        "i.e. only select variants which do not match the regex")
        .action((_, c) => c.copy(negateRegex = true)),

      opt[Unit]('a', "all").text("Run all variants, takes precedence over the variant option.")
        .action((_, c) => c.copy(all = true)),

      opt[Int]('t', "timeout").text("If the kernel execution is longer than time, ignore any remaining trials.")
        .action((arg, c) => c.copy(timeout = Some(arg))),

      opt[Unit]('c', "check").text("Check the result")
        .action((_, c) => c.copy(checkResult = true)),

      opt[Unit]("print").text("Print the result")
        .action((_, c) => c.copy(printResult = true)),

      opt[Unit]("inject").abbr("il").text("Inject the local size into the kernel as a constant, " +
        "possibly replacing some for loops with if statements.")
        .action((_, c) => c.copy(injectLocal = true)),

      opt[Unit]("injectGroup").abbr("ig").text("Inject the number of groups into the kernel as a constant, " +
        "possibly replacing some for loops with if statements.")
        .action((_, c) => c.copy(injectGroup = true)),

      opt[String]("experimentId").abbr("eid").text("A unique ID for this experiement for reporting data")
        .action((arg, c) => c.copy(experimentID = Some(arg))),

      opt[File]("input").text("Input files to read")
        .required()
        .action((arg, c) => c.copy(input = arg))
        .validate(f => if (f.exists) success else failure("File \"" + f.getName + "\" does not exist")),

      help("help").text("Show this message.")
    )}

}
