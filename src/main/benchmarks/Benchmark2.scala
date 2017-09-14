package benchmarks

import java.io._
import java.nio.file.{Files, Paths}
import java.time

import ir.ast.Lambda
import lift.arithmetic.{?, ArithExpr}

import opencl.executor.Executor.ExecutorFailureException
import opencl.executor._
import opencl.generator.Verbose
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._

import scala.io.Source
import scala.reflect.ClassTag
import scala.sys.process._
import scala.collection.immutable


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

abstract class Benchmark2[T: ClassTag](val name: String,
                 var defaultInputSizes: Seq[Int],
                 val f:Seq[(String, Array[Lambda])],
                 val areEqual: (T, T) => Boolean,
                 val defaultLocalSizes: Array[Int] = Array(128,1,1)) {

  /*
   To make it easier to implement the iterative benchmarks (which would inherit from this)
    we declare a type for the return type of the runScala function, which we can change when we subclass
   */
  type SRes = Array[T]

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
  def preProcessInputs(variant: Int, name: String, inputs: Seq[Any]) : Seq[Any] = {
    inputs
  }

  // postprocess the results for a specific variant
  def postProcessResult(variant: Int, name: String, result: Any) : Array[T] = {
    result.asInstanceOf[Array[T]]
  }

  // Mutate the lambdas of a particular variant, e.g. for a case that depends on last-minute information
  protected def updateLambdas(variant: Int, name: String, lambdas: Array[Lambda]) : Array[Lambda] = {
    lambdas
  }

  /* Size information for the benchmark */
  def inputSizes(): Seq[Int] = {
    if (size.value.length == defaultInputSizes.length) size.value else defaultInputSizes
  }

  protected def localSize: Array[Int] = {
    val localSizes = defaultLocalSizes.clone()
    localSizeOpt.value.copyToArray(localSizes)
    localSizes
  }

  protected def globalSize: Array[Int] = {
    val globalSizes = Array(inputSizes().product, 1, 1)
    globalSizeOpt.value.copyToArray(globalSizes)
    globalSizes
  }

  /*
    Inheritable functions for printing runtime information - such as git stuff
    Split into generic benchmark header information, for information that's common to all instances, and
    instance header information, for data relevant to specific instances
   */
  def printBenchmarkHeader(configuration: BenchmarkConfiguration,
                           variants: Seq[(Int, String, Array[Lambda])]) : Unit = {
        val commit = "git rev-parse HEAD".!!.trim
        val branch = "git rev-parse --abbrev-ref HEAD".!!.trim
        val date = "date".!!.trim
        val dce = System.getenv("LIFT_DCE") != null
        val cse = System.getenv("LIFT_CSE") != null

    //    if (csvFileName.value.isDefined)
    //      printCSVFile(csvFileName.value.get, kernel, commit, branch, date, dce, cse)

        println(date)
        println("Benchmark: " + name + ", variant(s): " +
          variants.map {case (_,name,_) => name}.mkString("[",",","]"))
        println("Size(s): " + inputSizes().mkString(", "))
        println("Dead Code Elimination: " + dce)
        println("Common Subexpression Extraction: " + cse)
        println("Total trials: " + configuration.trials)
        println("Checking results: " + configuration.checkResult)
        println("Global sizes: " + globalSize.mkString(", "))
        println("Local sizes: " + localSize.mkString(", "))
        println("Inject local: " + injectLocal.value.getOrElse(false))
        println("Inject global: " + injectGroup.value.getOrElse(false))
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
  def printResult(result: Array[T]) : Unit = {
    println(result.map(_.toString).mkString("[",",","]"))
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
    (e.g. with --all or --variantRegex), hence it should not set state.
  */
  def runBenchmarkInstance(variant: Int, name: String, lambdas: Array[Lambda],
                   inputs: Array[Any], expectedResult: SRes,
                   configuration: BenchmarkConfiguration) : InstanceStatistic = {
    val kernel = if (loadKernel.value.isDefined)
      loadKernel.value.get.replaceAll("(.*?/)*", "")
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
    val kernelSource = if (loadKernel.value.isDefined || loadAll.value.getOrElse(false)) {
      val kFileName = if (loadKernel.value.isDefined) {
        loadKernel.value.get
      } else {
        s".kernels/${name}.kernel"
      }
      scala.io.Source.fromFile(kFileName).mkString
    } else {
      val KS = (configuration.injectLocal, configuration.injectGroup) match {
        case (true, false) => Compile(
          lambda,
          localSize(0),
          localSize(1),
          localSize(2)
        )
        case (true, true) => Compile(
          lambda,
          localSize(0),
          localSize(1),
          localSize(2),
          globalSize(0),
          globalSize(1),
          globalSize(2),
          immutable.Map()
        )
        case _ => Compile(lambda)
      }

      // if we want to save the compilation result, do it
      if (saveAll.value.getOrElse(false)) {
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

    val (unprocessed_output : Array[T], runtimes : Array[Double]) = Execute(
      localSize(0),
      localSize(1),
      localSize(2),
      globalSize(0),
      globalSize(1),
      globalSize(2),
      (configuration.injectLocal, configuration.injectGroup)
    ).benchmark(configuration.trials, configuration.timeout, kernelSource, lambda, inputs: _*)

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
      val realGlobalSizes = globalSize
      var totalRuntime = 0.0
      var finalOutput = Array.ofDim[T](0)

      for (j <- lambdas.indices) {
        if(i == 0)
          kernels(j) = Utils.compile(lambdas(j),
            realInputs,
            localSize(0),
            localSize(1),
            localSize(2),
            realGlobalSizes(0),
            realGlobalSizes(1),
            realGlobalSizes(2),
            (configuration.injectLocal, configuration.injectGroup))

        val (kOutput: Array[T], kRuntime) = Execute(
          localSize(0),
          localSize(1),
          localSize(2),
          realGlobalSizes(0),
          realGlobalSizes(1),
          realGlobalSizes(2),
          (configuration.injectLocal, configuration.injectGroup)
        )(kernels(j), realInputs)

        // update parameters for next kernel, if any
        realInputs = Seq(kOutput)
        realGlobalSizes(0) = kOutput.length
        realGlobalSizes(1) = 1
        realGlobalSizes(2) = 1

        totalRuntime += kRuntime
        runtimes(i)(j) = kRuntime
        finalOutput = kOutput
      }

      val output = postProcessResult(variant, name, finalOutput)

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
      parser.parse(args)

      println("Loading Executor.")
      Executor.loadLibrary()
      Executor.init(platform.value.getOrElse(0), device.value.getOrElse(0))
      Verbose(verbose.value.getOrElse(false))

      val gitHash = "git rev-parse HEAD".!!.trim()
      val nowString = java.time.LocalDateTime.now().toString()

      println("Initialising Configuration.")
      // initialise a configuration object that we can pass around cheaply
      val configuration = new BenchmarkConfiguration (
        checkResult = checkResultOpt.value.getOrElse(false),
        printResult = printResultOpt.value.getOrElse(false),
        trials = trialsOpt.value.getOrElse(10),
        timeout = if (timeoutOpt.value.isDefined) timeoutOpt.value.get.toDouble else Double.MaxValue,
        injectLocal = injectLocal.value.getOrElse(false),
        injectGroup = injectGroup.value.getOrElse(false),
        experimentID = experimentIDOpt.value.getOrElse(gitHash + "-" + nowString)
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
      if (!all.value.getOrElse(false)) { // all not defined
        if (variantRegex.value != None) { // a regex is defined
          val VariantPattern = variantRegex.value.getOrElse(".*").r
          val nr = negateRegex.value.getOrElse(false)
          variants = variants.filter { case (variant, name, lambdas) =>
            name match {
              case VariantPattern() => !nr
              case _ => nr
            }
          }
        } else { // use the variantOpt
          variants = Seq(variants(variantOpt.value.getOrElse(0)))
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
//        println(s"  -- time (ms): ${time}")
//        println(s"  -- correctness: ${correctness}")

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
      case e: ArgotUsageException => println(e.message)
      case x: ExecutorFailureException => x.printStackTrace()//; printMedianAndBandwidth(0,0)
    }
  }

  private final def getScalaResult(inputs: Seq[Any], configuration: BenchmarkConfiguration) : SRes = {
    val scalaRes = runScala(inputs)
    if (configuration.checkResult) {
      if (loadOutput.value.isEmpty) {

        if (saveOutput.value.isDefined) {
          val oos = new ObjectOutputStream(new FileOutputStream(saveOutput.value.get))
          oos.writeObject(scalaRes)
          oos.close()
        }

        scalaRes
      } else {
        val ois = new ObjectInputStream(new FileInputStream(loadOutput.value.get))
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
  case class BadValues(positions: Array[(Int, T, T)]) extends RunResult{
    override def toString = "badvalues"
  }
  // generic way of reporting bad values without exact data
  case class GenericBadValues() extends RunResult {
    override def toString = "badvalues"
  }
  def checkResult(output: SRes, expected: SRes) : RunResult = {
    if (output.length != expected.length)  {
      return BadLength(expected.length, output.length)
    }else {
      val compareResult = (output zip expected).zipWithIndex.filter {
        case ((o, e), i) => !areEqual(o,e)
      }.map {
        case ((o,e), i) => (i,o,e)
      }

      if (compareResult.length > 0){
        return BadValues(compareResult)
      }else {
        return Correct()
      }
    }
  }
  // how bad is a given run result
  private final def badness(validity: RunResult) : Int = {
    validity match {
      case Correct() => 0
      case NotChecked() => 1
      case GenericFailure() => 2
      case BadLength(_, _) => 3
      case GenericBadValues() => 4
      case BadValues(_) => 5
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

  val parser = new ArgotParser(name)

  val trialsOpt = parser.option[Int](List("i", "trials"), "n",
    "Total trials (Default: 10)")

  val platform = parser.option[Int](List("p", "platform"), "platId",
    "Id of the OpenCL platform to use (Default: 0)")

  val device = parser.option[Int](List("d", "device"), "devId",
    "Id of the OpenCL device to use (Default: 0)")

  val saveOutput = parser.option[String](List("saveOutput"), "filename",
    "Save the gold result of the computation to a file")

  val loadOutput = parser.option[String](List("loadOutput"), "filename",
    "Load the gold result of the computation from a file. Takes precedence over saveOutput")

  val localSizeOpt = parser.multiOption[Int](List("l", "localSize"), "lclSize",
    "Local size(s) to use (Defaults: " + defaultLocalSizes.mkString(", ") + ")")

  val globalSizeOpt = parser.multiOption[Int](List("g", "globalSize"), "glbSize",
    "Global size(s) to use")

  val loadKernel = parser.option[String](List("loadKernel"), "filename",
    "Load an OpenCL kernel source file")

  // TODO: Implement kernel saving/loading functionality.
  val saveAll = parser.flag[Boolean](List("save-kernels"),
    "Save all kernels.")

  val loadAll = parser.flag[Boolean](List("load-kernels"),
    "Load kernel for execution previously generated using --save-kernels")

  val csvFileName = parser.option[String](List("csv"), "csvFileName",
    "If specified, results are stored in .csv file with given name")

  val size = parser.multiOption[Int](List("s", "size"), "inputSize",
    "Size of the input to use, expecting " + defaultInputSizes.length + " sizes.")

  val verbose = parser.flag[Boolean](List("v", "verbose"),
    "Print allocated memory and source code")

  val variantOpt = parser.option[Int](List("variant"), "var",
    "Which of the following variants to run (Default: 0):\n" + f.zipWithIndex.map(x => x._2 + " = " + x._1._1).mkString("\n"))

  val variantRegex = parser.option[String](List("variant-regex"), "var-regex",
    "Which variant(s) to run, based on a regular expression")

  val negateRegex = parser.flag[Boolean](List("negate-regex", "nr"),
    "Negate the regular expression matching variants, i.e. only select variants which do not match the regex")

  val all = parser.flag[Boolean](List("a", "all"),
    "Run all variants, takes precedence over the variant option.")

  val timeoutOpt = parser.option[Int](List("t", "timeout"), "time",
    "If the kernel execution is longer than time, ignore any remaining trials.")

  val checkResultOpt = parser.flag[Boolean](List("c", "check"),
    "Check the result")

  val printResultOpt = parser.flag[Boolean](List("print"),
    "Print the result")

  val injectLocal = parser.flag[Boolean](List("il", "inject"),
    "Inject the local size into the kernel as a constant, " +
      "possibly replacing some for loops with if statements.")

  val injectGroup = parser.flag[Boolean](List("ig", "injectGroup"),
    "Inject the number of groups into the kernel as a constant, " +
      "possibly replacing some for loops with if statements.")

  val experimentIDOpt = parser.option[String](List("eid", "experimentId"), "experimentId",
    "A unique ID for this experiement for reporting data")

  val help = parser.flag[Boolean](List("h", "help"),
    "Show this message.") {
    (sValue, opt) =>
      parser.usage()
      sValue
  }

  val input = parser.multiParameter[File]("input",
    "Input files to read. If not " +
      "specified, generate randomly",
    optional = true) {
    (s, opt) =>

      val file = new File(s)
      if (! file.exists)
        parser.usage("Input file \"" + s + "\" does not exist")

      file
  }

}
