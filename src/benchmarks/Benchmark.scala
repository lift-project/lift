package benchmarks

import java.io.File

import scala.sys.process._

import ir.Lambda
import opencl.executor.{Execute, Executor}
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

  // Parser options
  val parser = new ArgotParser(name)

  val iterationsOpt = parser.option[Int](List("i", "iterations"), "n",
    "Total iterations (Default: 10)")

  val platform = parser.option[Int](List("p", "platform"), "platId",
    "Id of the OpenCL platform to use (Default: 0)")

  val device = parser.option[Int](List("d", "device"), "devId",
    "Id of the OpenCL device to use (Default: 0)")

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
    val sizes: Seq[Int] = inputSizes()

    var realInputs = inputs
    var realSizes = sizes
    val realGlobalSizes = globalSize
    var totalRuntime = 0.0
    var finalOutput = Array.emptyFloatArray

    val lambdas: Seq[Lambda] = f(variant)._2
    for (i <- 0 until lambdas.length) {

      val (output, runtime) = Execute(
        localSize(0),
        localSize(1),
        localSize(2),
        realGlobalSizes(0),
        realGlobalSizes(1),
        realGlobalSizes(2),
        (injectLocal.value.getOrElse(false), injectGroup.value.getOrElse(false))
      )(lambdas(i), realInputs ++ realSizes:_*)

      // Adjust parameters for the next kernel, if any
      realInputs = Seq(output)
      realSizes = Seq(output.length)
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

  def runBenchmark(): Unit = {
    val commit = "hg id -i".!!.dropRight(1)

    println("Benchmark: " + name + " " + f(variant)._1)
    println("Size(s): " + inputSizes().mkString(", "))
    println("Total iterations: " + iterations)
    println("Checking results: " + checkResult)
    println("Global sizes: " + globalSize.mkString(", "))
    println("Local sizes: " + localSize.mkString(", "))
    println("Machine: " + "hostname".!!.dropRight(1))
    println("Commit: " + commit)
    if (commit.last == '+')
      println("Diff:\n" + "hg diff -X scripts".!!.dropRight(1))

    println()

    for (i <- 0 until iterations) {

      if (i == 1)
        Verbose.verbose = false

      println("Iteration: " + i)

      val (output, runtime) = runOpenCL(inputs:_*)

      runtimes(i) = runtime
      println("Runtime: " + runtime + " ms")

      if (checkResult && i == 0) {

        if (output.length != scalaResult.length)
          println("Output length is wrong, " + output.length + " vs " + scalaResult.length)

        for (j <- 0 until scalaResult.length) {
          if (check(output(j), scalaResult(j))) {
            println("Output at position " + j + " differs more than " + delta + ". " + output(j) + " vs " + scalaResult(j))

          }
        }
      }
    }

    val sorted = runtimes.sorted

    println()
    println("MIN: " + sorted(0) + " ms")
    println("MAX: " + sorted(iterations-1) + " ms")
    println("MEDIAN: " + median(sorted) + " ms")
    println("BANDWIDTH: " + bandwidth(median(sorted)) + " GB/s" )
    println()
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
        scalaResult = runScala(inputs:_*)
      }

      beforeBenchmark()

      if (all.value.getOrElse(false)) {
        for (i <- 0 until f.length) {
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
