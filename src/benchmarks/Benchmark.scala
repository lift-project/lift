package benchmarks

import java.io.File

import scala.util.control._
import scala.sys.process._

import ir.Lambda
import opencl.executor.{Execute, Executor}
import opencl.generator.Verbose
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._

abstract class Benchmark(val name: String,
                         val defaultSizes: Seq[Int],
                         val f: Seq[(String, Seq[Lambda])],
                         val delta: Float) {

  var variant = -1
  var checkResult = false
  var iterations = 10
  var scalaResult = Array.emptyFloatArray
  var inputs = Seq[Any]()
  var runtimes = Array.emptyDoubleArray


  // Parser options
  val parser = new ArgotParser(name)

  val iterationsOpt = parser.option[Int](List("i", "iterations"), "n",
    "Total iterations (Default: 10)")

  val platform = parser.option[Int](List("p", "platform"), "platId",
    "Id of the OpenCL platform to use (Default: 0)")

  val device = parser.option[Int](List("d", "device"), "devId",
    "Id of the OpenCL device to use (Default: 0)")

  val localSizeOpt = parser.option[Int](List("l", "localSize"), "lclSize",
    "Local size to use (Default: 128)")

  val globalSizeOpt = parser.option[Int](List("g", "globalSize"), "glbSize",
    "Global size to use")

  val size = parser.multiOption[Int](List("s", "size"), "inputSize",
    "Size of the input to use, expecting " + defaultSizes.length)

  val verbose = parser.flag[Boolean](List("v", "verbose"),
    "Print allocated memory and source code")

  val variantOpt = parser.option[Int](List("variant"), "var",
    "Which of the following variants to run (Default: 0):\n" + f.zipWithIndex.map(x => x._2 + " = " + x._1._1).mkString("\n"))

  val all = parser.flag[Boolean](List("a", "all"),
    "Run all variants, takes precedence over the variant option.")

  val checkResultOpt = parser.flag[Boolean](List("c", "check"),
    "Check the result")

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
    if (size.value.length == defaultSizes.length) size.value else defaultSizes
  }
  def runOpenCL(inputs: Any*): (Array[Float], Double) = {
    val sizes: Seq[Int] = inputSizes()

    var realInputs = inputs
    var realSizes = sizes
    var totalRuntime = 0.0
    var finalOutput = Array.emptyFloatArray

    val lambdas: Seq[Lambda] = f(variant)._2
    for (i <- 0 until lambdas.length) {
      val (output, runtime) = Execute(localSize,
        globalSizeOpt.value.getOrElse(sizes.product)
      )(lambdas(i), realInputs ++ realSizes:_*)
      realInputs = Seq(output)
      realSizes = Seq(output.length)
      totalRuntime += runtime
      finalOutput = output
    }

    (finalOutput, totalRuntime)
  }

  private def localSize: Int = {
    localSizeOpt.value.getOrElse(128)
  }

  private def globalSize: Int = {
    globalSizeOpt.value.getOrElse(inputSizes().product)
  }

  def runBenchmark(): Unit = {
    val commit = "hg id -i".!!.dropRight(1)

    println("Benchmark: " + name + " " + f(variant)._1)
    println("Size(s): " + inputSizes().mkString(", "))
    println("Total iterations: " + iterations)
    println("Checking results: " + checkResult)
    println("Global size: " + globalSize)
    println("Local size: " + localSize)
    println("Machine: " + "hostname".!!.dropRight(1))
    println("Commit: " + commit)
    if (commit.last == '+')
      println("Diff:\n" + "hg diff -X scripts".!!.dropRight(1))

    println()

    for (i <- 0 until iterations) {

      println("Iteration: " + i)

      val (output, runtime) = runOpenCL(inputs:_*)

      runtimes(i) = runtime
      println("Runtime: " + runtime + " ms")

      if (i == 1)
        Verbose.verbose = false

      if (checkResult && i == 0) {

        val loop = new Breaks

        loop.breakable {
          for (j <- 0 until scalaResult.length) {
            if (check(output(j), scalaResult(j))) {
              println("Output at position " + j + " differs more than " + delta)
              // loop.break()
            }
          }
        }
      }

    }

    val sorted = runtimes.sorted

    println()
    println("MIN: " + sorted(0) + " ms")
    println("MAX: " + sorted(iterations-1) + " ms")
    println("MEDIAN: " + median(sorted) + " ms")
    println("BANDWIDTH: " + 4 * inputSizes().product / median(sorted) * 0.000001 + " GB/s" )
    println()
  }

  protected def check(x: Float, y: Float): Boolean = {
    (x - y).abs >= delta
  }

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
