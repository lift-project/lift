package benchmarks

import apart.arithmetic.{StartFromRange, Var}
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

class RodiniaHotspot(override val f: Seq[(String, Array[Lambda])]) extends Benchmark("RodiniaHotspot", Seq(1036, 1036), f, 0.01f) {

  override def generateInputs(): Seq[Any] = {
    // change below as well //
    //val inputSize = 1036
    val inputSize = 8204

    val heat = Array.tabulate(inputSize, inputSize) { (i, j) => i * inputSize.toFloat + j }
    val power = Array.tabulate(inputSize, inputSize) { (i, j) => i * inputSize.toFloat + j }
    val x = 0.1f; val y = 0.1f; val z = 1024000; val c = 1.068e-7f
    val coeff = Array(0, c*y, 0, c*x, c*(-2*y-2*x-z+1), c*x, 0, c*y, 0)

    Seq(heat, power, coeff)
  }

  override def globalSize: Array[Int] = {
    //Array(1184,1184,1) // 1024
    Array(9376,9376,1) // 8192
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

  // input rodinia 1024:
  //val inputSize = 1036
  //val nbhs = 74

  // input rodinia 8192:
  val inputSize = 8204
  val nbhs = 586

  /////////////////// LAMBDAS
  val addAmbientTemp = UserFun("addAmbientTemp", Array("x", "y"), "{ return x + y + (0.1f * 1.068e-7f * 80.0f); }", Seq(Float, Float), Float)
  def hotspot(): Lambda = {
    fun(
      ArrayType(ArrayType(Float, inputSize), inputSize),
      ArrayType(ArrayType(Float, inputSize), inputSize),
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
          Split(nbhs) $
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
