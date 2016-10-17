package benchmarks

import apart.arithmetic.{StartFromRange, Var}
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

class RodiniaHotspot(override val f: Seq[(String, Array[Lambda])]) extends Benchmark("RodiniaHotspot", Seq(1036, 1036), f, 0.01f) {

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

    /*
    val weights = Array(0.05, 0.15, 0.05,
                        0.15, 0.25, 0.15,
                        0.05, 0.15, 0.05).map(_.toFloat)
                        */
    val weights = Array(0.05, 0.15, 0.05)
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

object RodiniaHotspot{

  /////////////////// LAMBDAS
  def hotspot(tilesize: Int): Lambda = {
  fun(
    //ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
    ArrayType(ArrayType(Float, 1036), 1036),
    ArrayType(ArrayType(Float, 1036), 1036),
    //ArrayType(Float, 17*17),
    (heat, power) => {
      Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

        MapLcl(1)(MapLcl(0)(
          fun(elem => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeq(add, 0.0f) o Join() $ elem
          })
        )) o Slide2D(3,1, 3,1) o
          toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
      ))) o
        Slide2D(tilesize,tilesize-2, tilesize,tilesize-2) o
        Pad2D(1,1, 1,1, Pad.Boundary.MirrorUnsafe) $ heat
    }
  )}

  def apply() = new RodiniaHotspot(
    Seq(
      ("HOTSPOT_16", Array[Lambda](hotspot(16))),
      ("HOTSPOT_32", Array[Lambda](hotspot(32)))
  ))

  def main(args: Array[String]): Unit = {
    RodiniaHotspot().run(args)
  }
}
