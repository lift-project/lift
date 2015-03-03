package benchmarks

import java.io.File

import scala.util.control._

import ir.Lambda
import opencl.executor.{Execute, Executor}
import opencl.generator.Debug
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._

abstract class Benchmark(val name: String,
                         val defaultSizes: Seq[Int],
                         val f: Seq[(String, Lambda)],
                         val delta: Float) {



  val parser = new ArgotParser(name)

  val iterationsOpt = parser.option[Int](List("i", "iterations"), "n",
    "Total iterations")

  val platform = parser.option[Int](List("p", "platform"), "platId",
    "Id of the OpenCL platform to use")

  val device = parser.option[Int](List("d", "device"), "devId",
    "Id of the OpenCL device to use")

  val localSize = parser.option[Int](List("l", "localSize"), "lclSize",
    "Id of the OpenCL device to use")

  val globalSize = parser.option[Int](List("g", "globalSize"), "glbSize",
    "Id of the OpenCL device to use")

  val size = parser.multiOption[Int](List("s", "size"), "inputSize",
    "Size of the input to use")

  val verbose = parser.flag[Boolean](List("v", "verbose"),
    "Print allocated memory and source code")

  val variant = parser.option[Int](List("variant"), "var",
    "Which of the following variants to run:\n" + f.zipWithIndex.map(x => x._2 + " = " + x._1._1).mkString("\n"))

  val checkResultOpt = parser.flag[Boolean](List("c", "check"),
    "Check the result.")

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

  def inputSizes(): Seq[Int]

  def runOpenCL(inputs: Any*): (Array[Float], Double) = {
    val sizes: Seq[Int] = inputSizes()
    Execute(localSize.value.getOrElse(128), globalSize.value.getOrElse(sizes.product))(f(variant.value.getOrElse(0))._2, inputs ++ sizes:_*)
  }

  def runBenchmark(): Unit = {
    Executor.loadLibrary()
    Executor.init(platform.value.getOrElse(0), device.value.getOrElse(0))
    Debug.verbose = verbose.value.getOrElse(false)

    println(name)

    val checkResult: Boolean = checkResultOpt.value.getOrElse(false)

    val iterations: Int = iterationsOpt.value.getOrElse(10)
    val runtimes = Array.ofDim[Double](iterations)
    val inputs = generateInputs()
    var scalaResult = Array.emptyFloatArray

    if (checkResult) {
      scalaResult = runScala(inputs:_*)
    }

    for (i <- 0 until iterations) {

      val (output, runtime) = runOpenCL(inputs:_*)

      runtimes(i) = runtime

      if (checkResult) {

        val loop = new Breaks

        loop.breakable {
          for (j <- 0 until scalaResult.length) {
            if (output(j) - scalaResult(j) >= delta) {
              println("Output at position [" + j + "] differs more than " + delta)
              loop.break()
            }
          }
        }
      }

    }

    val sorted = runtimes.sorted

    println("MIN: " + sorted(0))
    println("MAX: " + sorted(iterations-1))
    println("MEDIAN: " + sorted((iterations-1)/2))

    Executor.shutdown()
  }

  def run(args: Array[String]): Unit = {
    try {
      parser.parse(args)

      runBenchmark()

    } catch {
      case e: ArgotUsageException => println(e.message)
    }
  }
}
