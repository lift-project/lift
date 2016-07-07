package benchmarks

import apart.arithmetic.{StartFromRange, Var}
import ir._
import ir.ast.{Pad2D, _}
import opencl.ir._
import opencl.ir.pattern._
import opencl.executor.Utils

class StencilRefs(override val f: Seq[(String, Array[Lambda])]) extends Benchmark("StencilRefs", Seq(4096, 4096), f, 0.01f) {

  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes()(0)
    val inputSizeM = inputSizes()(1)
    // shoc input (i,j) = i*j
    val inputData = Array.tabulate(inputSizeM, inputSizeN)((r, c) => (r * c) * 1.0f)

    Seq(inputData, variant match {
      // shoc weights
      case 0 => Array(0.05, 0.15, 0.05,
                      0.15, 0.25, 0.15,
                      0.05, 0.15, 0.05).map(_.toFloat)
      case _ => Array.fill[Float](9)(1.0f)
    })
    //val inputData = Array.tabulate(inputSizeM, inputSizeN)((r, c) => (r * inputSizeM + c) * 1.0f)
    //val inputData = Array.tabulate(inputSizeM, inputSizeN)((r, c) => util.Random.nextFloat())
  }

  // no scala checks
  override def runScala(inputs: Any*): Array[Float] = {
    throw new IllegalArgumentException("no scala check defined for this benchmark")
  }

  override def runOpenCL(inputs: Any*): (Array[Float], Double) = {
    val (output, runtime) = super.runOpenCL(inputs:_*)
    (Array(output.sum), runtime)
  }
}

object StencilRefs{

  /////////////////// LAMBDAS
  def shoc(): Lambda = {
    fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(10))), Var("M", StartFromRange(10))),
      ArrayType(Float, 9),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

          MapLcl(1)(MapLcl(0)(
            // stencil computation
            fun(elem => {
              toGlobal(MapSeqUnroll(id)) o
                ReduceSeqUnroll(fun((acc, pair) => {
                  val pixel = Get(pair, 0)
                  val weight = Get(pair, 1)
                  multAndSumUp.apply(acc, pixel, weight)
                }), 0.0f) $ Zip(Join() $ elem, weights)
            })
            // create neighbourhoods in tiles
          )) o Slide2D(3,1, 3,1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(18,16, 18,16) $ matrix
      }
    )
  }

  def apply() = new StencilRefs(
    Seq(
      ("SHOC_9POINT_NO_PAD", Array[Lambda](shoc()))
    )
  )

  def main(args: Array[String]): Unit = {
    StencilRefs().run(args)
  }
}
