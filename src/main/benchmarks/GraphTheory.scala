package benchmarks

import arithmetic.Var
import ir._
import opencl.ir._
import java.io._

class GraphTheory(override val f: Seq[(String, Array[Lambda])]) extends Benchmark("Graph theory", Seq(4096), f, 0.01f) {
  val graphData = parser.option[File](List("f","file"), "Data", "Data file to read from"){
    (s, opt) =>
      val file = new File(s)
      if(! file.exists)
        parser.usage("Graph data file \"" + s + "\" does not exist")

      file
  }
  override def runScala(inputs: Any*) : Array[Float] = {
    Array.tabulate(4096)((_) => 1.0f)
  }
  override def generateInputs() :Seq[Any] = {
    val filenames = input.value
    val inputSize = inputSizes().head
    val inputData = Array.fill(inputSize)(util.Random.nextFloat())
    Seq(inputData)
  }
}