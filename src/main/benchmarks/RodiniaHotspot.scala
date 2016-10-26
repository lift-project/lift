package benchmarks

import apart.arithmetic.{StartFromRange, Var}
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

class RodiniaHotspot(override val f: Seq[(String, Array[Lambda])]) extends Benchmark("RodiniaHotspot", Seq(1036, 1036), f, 0.01f) {

  override def generateInputs(): Seq[Any] = {
    val heat = Array.tabulate(1036, 1036) { (i, j) => i * 1036.0f + j }
    val power = Array.tabulate(1036, 1036) { (i, j) => i * 1036.0f + j }
    val x = 0.1f; val y = 0.1f; val z = 1024000; val c = 1.068e-7f
    val coeff = Array(0, c*y, 0, c*x, c*(-2*y-2*x-z+1), c*x, 0, c*y, 0)

    Seq(heat, power, coeff)
  }

  override def globalSize: Array[Int] = {
    Array(1184,1184,1)
  }

  override def localSize: Array[Int] = {
    Array(16,16,1)
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
  val addAmbientTemp = UserFun("addAmbientTemp", Array("x", "y"), "{ return x + y + (0.1f * 1.068e-7f * 80.0f); }", Seq(Float, Float), Float)
  def hotspot(): Lambda = {
    fun(
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(Float, 9),
      (heat, power, coeff) => {
        MapWrg(1)(MapWrg(0)(
          fun(tiles => {
            val powerTile = Get(tiles, 0)
            val heatTile = Get(tiles, 1)
            MapLcl(1)(MapLcl(0)(
              fun(nbhs => {
                val powerValue = Get(nbhs, 0) // Float
                val heatNbh = Get(nbhs, 1)    // [[Float]_3]_3
                toGlobal(MapSeq(id)) o
                  MapSeq( \(x => addAmbientTemp(powerValue, x))) o
                  ReduceSeqUnroll(\((acc, next) =>
                    multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() $ heatNbh, coeff)
              })
            )) o
              Split(14) $
              Zip(Join() o toLocal(MapLcl(1)(MapLcl(0)(id))) $ powerTile,
                Join() o Slide2D(3,1,3,1) o
                  toLocal(MapLcl(1)(MapLcl(0)(id))) $ heatTile)
          })
        )) o
          Split(74) $
          Zip(Join() o Slide2D(14,14) $ power,
            Join() o Slide2D(16,14) o
              Pad2D(1,1,Pad.Boundary.Wrap) $ heat) //actually its mirror
      }
    )
  }

  def apply() = new RodiniaHotspot(
    Seq(
      ("HOTSPOT", Array[Lambda](hotspot()))
  ))

  def main(args: Array[String]): Unit = {
    RodiniaHotspot().run(args)
  }
}
