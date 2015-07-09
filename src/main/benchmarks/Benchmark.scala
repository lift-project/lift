package benchmarks

import java.io._

import scala.sys.process._

import ir.Lambda
import opencl.executor._
import opencl.generator.Verbose
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._

abstract class Benchmark(val name: String,
                         val defaultInputSizes: Seq[Int],
                         val f: Seq[(String, Array[Lambda])],
                         val delta: Float,
                         val defaultLocalSizes: Array[Int] = Array(128,1,1)) {

  var variant = -1
  var checkResult = false
  var iterations = 10
  var inputs = Seq[Any]()
  var scalaResult = Array.emptyFloatArray
  var runtimes = Array.emptyDoubleArray
  var generatedCode = Array.empty[String]

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


    if (generatedCode.length == 0) {
      generateKernels = true
      generatedCode = Array.ofDim(lambdas.length)
    }


    for (i <- lambdas.indices) {

      if (generateKernels)
        generatedCode(i) = Utils.compile(lambdas(i),
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
      )(generatedCode(i), lambdas(i), realInputs:_*)

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

  def runBenchmark(): Unit = {

    print("date".!!)
    println("Benchmark: " + name + " " + f(variant)._1)
    println("Size(s): " + inputSizes().mkString(", "))
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
    print("Commit: " + "git rev-parse HEAD".!!)
    print("Diff:\n" + "git diff".!!)

    println()

    val lambdas = f(variant)._2
    if (lambdas.length == 1) {
      // Just one kernel, iterate inside SkelCL

      val timeout = if (timeoutOpt.value.isDefined) timeoutOpt.value.get.toDouble else Double.MaxValue

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

      println("MEDIAN: " + runtime + " ms")
      printResults(runtime)

    } else {
      for (i <- 0 until iterations) {

        if (i == 1)
          Verbose.verbose = false

        println("Iteration: " + i)

        val (output, runtime) = runOpenCL(inputs: _*)

        runtimes(i) = runtime
        println("Runtime: " + runtime + " ms")

        if (checkResult && i == 0)
          checkResult(output)
      }

      val sorted = runtimes.sorted

      println()
      println("MIN: " + sorted(0) + " ms")
      println("MAX: " + sorted(iterations - 1) + " ms")
      val medianTime = median(sorted)
      println("MEDIAN: " + medianTime + " ms")
      printResults(medianTime)
      println()
    }
  }

  private def checkResult(output: Array[Float]): Unit = {
    if (output.length != scalaResult.length) {
      println(s"Output length is wrong, ${output.length} vs ${scalaResult.length}")

    } else {
      var numErrors = 0

      for (j <- scalaResult.indices) {
        if (check(output(j), scalaResult(j)))
          numErrors += 1
      }

      if (numErrors != 0)
        println(s"Output differed in $numErrors positions!")
    }
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
      Verbose.verbose = verbose.value.getOrElse(false)

      checkResult = checkResultOpt.value.getOrElse(false)

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
        variant = variantOpt.value.getOrElse(0)
        runBenchmark()
      }

      Executor.shutdown()

    } catch {
      case e: ArgotUsageException => println(e.message)
    }
  }
}
