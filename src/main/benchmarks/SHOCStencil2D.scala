package benchmarks

import lift.arithmetic.{StartFromRange, Var}
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class SHOCStencil2D(override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("SHOCStencil2D", Seq(8194, 8194), f, 0.01f) {

  override def generateInputs(): Seq[Any] = {
    val shocHaloSize = 1
    val inputSizeN = inputSizes()(0) + 2 * shocHaloSize
    val inputSizeM = inputSizes()(1) + 2 * shocHaloSize

    var input = Array.tabulate(inputSizeN, inputSizeM) { (i, j) => (i-shocHaloSize) * (j-shocHaloSize) * 1.0f }
    input(0) = input(0).map((_*0.0f))
    input(inputSizeN -1) = input(inputSizeN -1).map(_*0.0f)
    input = input.transpose
    input(0) = input(0).map(_*0.0f)
    input(inputSizeM -1) = input(inputSizeM -1).map(_*0.0f)
    input = input.transpose

    val weights = Array(0.05, 0.15, 0.05,
                        0.15, 0.25, 0.15,
                        0.05, 0.15, 0.05).map(_.toFloat)
    //val weights = Array(0.05, 0.15, 0.05)
    Seq(input, weights)
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

object SHOCStencil2D{

  /////////////////// LAMBDAS
  def shoc(tileCenterX: Int, tileCenterY: Int): Lambda = {
    fun(
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(6))), Var("M", StartFromRange(6))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 8194), 8194),
      ArrayTypeWSWC(Float, 9),
      (matrix, weights) => {
        Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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
          Slide2D(tileCenterY + 2,tileCenterY, tileCenterX + 2,tileCenterX) $ matrix
      }
    )
  }

  def shocGeneric(tileCenterX: Int, tileCenterY: Int): Lambda = {
    fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(6))), Var("M", StartFromRange(6))),
      ArrayTypeWSWC(Float, 9),
      (matrix, weights) => {
        Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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
          Slide2D(tileCenterY + 2,tileCenterY, tileCenterX + 2,tileCenterX) $ matrix
      }
    )
  }

  def shocNoTiling(): Lambda = {
    fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(6))), Var("M", StartFromRange(6))),
      ArrayTypeWSWC(Float, 9),
      (matrix, weights) => {

        MapGlb(1)(MapGlb(0)(
          // stencil computation
          fun(elem => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun( (acc, pair) => {
                val pixel = pair._0
                val weight = pair._1
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ elem, weights)
          })
          // create neighbourhoods in tiles
        )) o Slide2D(3, 1, 3, 1) $ matrix
      }
    )
  }

  def apply() = new SHOCStencil2D(
    Seq(
      ("SHOC_STENCIL2D_BASE", Array[Lambda](shoc(256,1))),
      ("SHOC_STENCIL2D_TUNE", Array[Lambda](shoc(128,16))),
      ("SHOC_STENCIL2D_TILE_GENERIC", Array[Lambda](shocGeneric(128,16))),
      ("SHOC_NO_TILING", Array[Lambda](shocNoTiling()))
    )
  )

  def main(args: Array[String]): Unit = {
    SHOCStencil2D().run(args)
  }
}
