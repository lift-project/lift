package benchmarks

import java.io._
import java.nio.file.{Files, Paths}

import ir.ast.Lambda
import opencl.executor.Executor.ExecutorFailureException
import opencl.executor._
import opencl.generator.Verbose
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._

import scala.sys.process._

abstract class Benchmark(val name: String,
                         val defaultInputSizes: Seq[Int],
                         val f: Seq[(String, Array[Lambda])],
                         val delta: Float,
                         val defaultLocalSizes: Array[Int] = Array(128,1,1)) {

  var variant = -1
  var checkResult = false
  var printResult = false
  var iterations = 10
  var inputs = Seq[Any]()
  var scalaResult = Array.emptyFloatArray
  var runtimes = Array.emptyDoubleArray
  var generatedKernel = Array.empty[String]

  // Parser options
  val parser = new ArgotParser(name)

  val iterationsOpt = parser.option[Int](List("i", "iterations"), "n",
    "Total iterations (Default: 10)")

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

  val csvFileName = parser.option[String](List("csv"), "csvFileName",
    "If specified, results are stored in .csv file with given name")

  val size = parser.multiOption[Int](List("s", "size"), "inputSize",
    "Size of the input to use, expecting " + defaultInputSizes.length + " sizes.")

  val verbose = parser.flag[Boolean](List("v", "verbose"),
    "Print allocated memory and source code")

  val variantOpt = parser.option[Int](List("variant"), "var",
    "Which of the following variants to run (Default: 0):\n" + f.zipWithIndex.map(x => x._2 + " = " + x._1._1).mkString("\n"))

  val all = parser.flag[Boolean](List("a", "all"),
    "Run all variants, takes precedence over the variant option.")

  val timeoutOpt = parser.option[Int](List("t", "timeout"), "time",
    "If the kernel execution is longer than time, ignore any remaining iterations.")

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

  def runScala(inputs: Any*): Array[Float]

  def generateInputs(): Seq[Any]

  def inputSizes(): Seq[Int] = {
    if (size.value.length == defaultInputSizes.length) size.value else defaultInputSizes
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
          (injectLocal.value.getOrElse(false), injectGroup.value.getOrElse(false)))

      val (output: Array[Float], runtime) = Execute(
        localSize(0),
        localSize(1),
        localSize(2),
        realGlobalSizes(0),
        realGlobalSizes(1),
        realGlobalSizes(2),
        (injectLocal.value.getOrElse(false), injectGroup.value.getOrElse(false))
      )(generatedKernel(i), f, realInputs)

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
    localSizeOpt.value.copyToArray(localSizes)
    localSizes
  }

  protected def globalSize: Array[Int] = {
    val globalSizes = Array(inputSizes().product, 1, 1)
    globalSizeOpt.value.copyToArray(globalSizes)
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
        fw.write(injectGroup.value.getOrElse(false) + ";" + injectLocal.value.getOrElse(false) + ";")
        fw.write(iterations + ";" + globalSize(0) + ";" + globalSize(1) + ";" + globalSize(2) + ";")
        fw.write(localSize(0) + ";" + localSize(1) + ";" + localSize(2) + ";")
        fw.write(dce + ";" + cse + ";")
        fw.write("\"" + Executor.getPlatformName + "\";\"" + Executor.getDeviceName + "\";")
      } finally fw.close()
  }

  def printMedianAndBandwidth(median: Double, bandwidth: Double): Unit = {
    val fw = new FileWriter(csvFileName.value.get, true)
      try {
        fw.write(median + ";" + bandwidth + "\n")
      } finally fw.close()
  }

  def runBenchmark(): Unit = {
    val kernel = if (loadKernel.value.isDefined)
          loadKernel.value.get.replaceAll("(.*?/)*", "")
        else "generated"
    val commit = "git rev-parse HEAD".!!.trim
    val branch = "git rev-parse --abbrev-ref HEAD".!!.trim
    val date = "date".!!.trim
    val dce = System.getenv("APART_DCE") != null
    val cse = System.getenv("APART_CSE") != null

    if (csvFileName.value.isDefined)
      printCSVFile(csvFileName.value.get, kernel, commit, branch, date, dce, cse)

    println(date)
    println("Benchmark: " + name + " " + f(variant)._1)
    println("Kernel: " + kernel)
    println("Size(s): " + inputSizes().mkString(", "))
    println("Dead Code Elimination: " + dce)
    println("Common Subexpression Extraction: " + cse)
    println("Total iterations: " + iterations)
    println("Checking results: " + checkResult)
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

    val lambdas = f(variant)._2
    if (lambdas.length == 1) {
      // Just one kernel, iterate inside SkelCL

      val timeout = if (timeoutOpt.value.isDefined) timeoutOpt.value.get.toDouble else Double.MaxValue

      if (loadKernel.value.isDefined) {
        Execute(
          localSize(0),
          localSize(1),
          localSize(2),
          globalSize(0),
          globalSize(1),
          globalSize(2),
          (injectLocal.value.getOrElse(false), injectGroup.value.getOrElse(false))
        )(0, timeout, lambdas.head, inputs:_*)
        
        val code = scala.io.Source.fromFile(loadKernel.value.get).mkString

        val (output: Array[Float], runtime) = Execute(
          localSize(0),
          localSize(1),
          localSize(2),
          globalSize(0),
          globalSize(1),
          globalSize(2),
          (injectLocal.value.getOrElse(false), injectGroup.value.getOrElse(false))
        ).benchmark(iterations, timeout, code, lambdas.head, inputs:_*)

        if (checkResult)
          checkResult(output)

        if (printResult)
          printResult(output)

        println("MEDIAN: " + runtime + " ms")
        printResults(runtime)
        if(csvFileName.value.isDefined) printMedianAndBandwidth(runtime, bandwidth(runtime))

      }
      else {
        val (output: Array[Float], runtime) = Execute(
          localSize(0),
          localSize(1),
          localSize(2),
          globalSize(0),
          globalSize(1),
          globalSize(2),
          (injectLocal.value.getOrElse(false), injectGroup.value.getOrElse(false))
        )(iterations, timeout, lambdas.head, inputs:_*)

        if (checkResult)
          checkResult(output)

        if (printResult)
          printResult(output)

        println("MEDIAN: " + runtime + " ms")
        printResults(runtime)
        if(csvFileName.value.isDefined) printMedianAndBandwidth(runtime, bandwidth(runtime))
      }

    } else {
      for (i <- 0 until iterations) {

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
      println("MAX: " + sorted(iterations - 1) + " ms")
      val medianTime = median(sorted)
      println("MEDIAN: " + medianTime + " ms")
      printResults(medianTime)
      if(csvFileName.value.isDefined) printMedianAndBandwidth(medianTime, bandwidth(medianTime))
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
    println("output: " + output.mkString(", "))
  }

  protected def bandwidth(time: Double): Double = {
    4 * inputSizes().product.toDouble / time * 0.000001
  }

  protected def check(x: Float, y: Float): Boolean = {
    (x - y).abs > delta
  }

  protected def beforeBenchmark(): Unit = {}

  private def median(sorted: Array[Double]): Double = {
    val iterations = sorted.length
    if (iterations % 2 == 0)
      sorted((iterations - 1) / 2)
    else
      (sorted((iterations - 1) / 2) + sorted(iterations / 2)) / 2
  }

  def run(args: Array[String]): Unit = {
    try {
      parser.parse(args)

      Executor.loadLibrary()
      Executor.init(platform.value.getOrElse(0), device.value.getOrElse(0))
      Verbose(verbose.value.getOrElse(false))

      variant = variantOpt.value.getOrElse(0)
      checkResult = checkResultOpt.value.getOrElse(false)
      printResult = printResultOpt.value.getOrElse(false)

      iterations = iterationsOpt.value.getOrElse(10)
      inputs = generateInputs()
      runtimes = Array.ofDim[Double](iterations)

      if (checkResult) {
        if (loadOutput.value.isEmpty) {
          scalaResult = runScala(inputs:_*)

          if (saveOutput.value.isDefined) {
            val oos = new ObjectOutputStream(new FileOutputStream(saveOutput.value.get))
            oos.writeObject(scalaResult)
            oos.close()
          }

        } else {
          val ois = new ObjectInputStream(new FileInputStream(loadOutput.value.get))
          val readResult = ois.readObject()
          ois.close()
          scalaResult = readResult.asInstanceOf[Array[Float]]
        }
      }

      beforeBenchmark()

      if (all.value.getOrElse(false)) {
        for (i <- f.indices) {
          variant = i
          runBenchmark()
        }

      } else {
        runBenchmark()
      }

      Executor.shutdown()

    } catch {
      case e: ArgotUsageException => println(e.message)
      case x: ExecutorFailureException => x.printStackTrace(); printMedianAndBandwidth(0,0)
    }
  }
}
