package benchmarks

import lift.arithmetic.{StartFromRange, Var, Cst}
import ir._
import ir.ast.{Pad2D, _}
import opencl.ir._
import opencl.ir.pattern._
import opencl.executor.Utils

@deprecated("Uses an old benchmark infrastructure", "")
class HarrisCornerDetection(override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("HarrisCornerDetection", Seq(1536, 2560), f, 0.01f) {

  // same as Halide SIGGRAPH 16
  override def generateInputs(): Seq[Any] = {
    Seq(Array.tabulate(1536, 2560)((r, c) => scala.util.Random.nextFloat()),
        Array.fill[Float](9)(1.0f))
  }

  // no scala checks
  override def runScala(inputs: Any*): Array[Float] = {
    throw new IllegalArgumentException("no scala check defined for this benchmark")
  }

  override def runOpenCL(inputs: Any*): (Array[Float], Double) = {
    val (output, runtime) = super.runOpenCL(inputs:_*)
    (Array(output.sum), runtime)
  }

  // same as Halide SIGGRAPH 16
  override def globalSize: Array[Int] = {
    Array(1536, 2560, 1)
  }

  // same as Halide SIGGRAPH 16
  override def localSize: Array[Int] = {
    Array(16, 8, 1)
  }
}

object HarrisCornerDetection{

  def runScala(input: Array[Array[Float]], weights: Array[Float],
               size1: Int, step1: Int,
               size2: Int, step2: Int,
               top: Int, bottom: Int,
               left: Int, right: Int,
               boundary: (Int, Int) => Int): Array[Float] = {
    Utils.scalaCompute2DStencil(input, size1,step1, size2,step2, top,bottom,left,right, weights, boundary)
  }

  /////////////////// LAMBDAS
  def convolutionSimple(): Lambda = {
    fun(
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 1536), 2560),
      ArrayTypeWSWC(Float, 9),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeq(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(3,1, 3,1) $ matrix
      })
  }

  def apply() = new HarrisCornerDetection(
    Seq(("CONVOLUTION_SIMPLE", Array[Lambda](convolutionSimple())))
  )

  def main(args: Array[String]): Unit = {
    HarrisCornerDetection().run(args)
  }
}
