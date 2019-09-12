package benchmarks

import lift.arithmetic.{StartFromRange, Var}
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class RodiniaHotspot(override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("RodiniaHotspot", Seq(8192, 8192), f, 0.01f) {

  override def generateInputs(): Seq[Any] = {
    val inputSize = 8192

    val heat = Array.tabulate(inputSize, inputSize) { (i, j) => i * inputSize.toFloat + j }
    val power = Array.tabulate(inputSize, inputSize) { (i, j) => i * inputSize.toFloat + j }
    val x = 0.1f; val y = 0.1f; val z = 1024000; val c = 1.068e-7f
    val coeff = Array(0, c*y, 0, c*x, c*(-2*y-2*x-z+1), c*x, 0, c*y, 0)

    Seq(power, heat)
  }

  override def globalSize: Array[Int] = {
    Array(256,4096,1)
  }

  override def localSize: Array[Int] = {
    Array(32,8,1)
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

  // input rodinia 4096:
  //val inputSize = 4102
  //val nbhs = 293

  // input rodinia 8192:
  val inputSize = 8204
  val nbhs = 586

  /////////////////// LAMBDAS
  def rodiniaUserFun = UserFun("rodiniaUserFun",
      Array("power", "top", "bottom", "left", "right", "center"), "{ float step_div_cap = 1.365333e+00; return center + step_div_cap*(power + 0.1f*(bottom + top - 2*center) + 0.1*(left + right - 2*center) + 4.882813e-05*(80.0f - center)); }",
      Seq(Float, Float, Float, Float, Float, Float), Float)

  def rodiniaStencil = \(tuple => {
    val nbh = tuple._0
    val powerValue = tuple._1

    val top = Get(tuple,0).at(0).at(1)
    val bottom = tuple._0.at(2).at(1)
    val left = tuple._0.at(1).at(0)
    val right = tuple._0.at(1).at(2)
    val center = tuple._0.at(1).at(1)

    toGlobal(id) o toPrivate(fun(x => rodiniaUserFun(x, top, bottom, left, right, center))) $ powerValue
  })

  def hotspotTiled() = {
    val M = Var("M", StartFromRange(4096))
    val N = Var("N", StartFromRange(4096))
    fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    (power, heat) => {
      Map(Join()) o Join() o Map(TransposeW()) o
        MapWrg(1)(MapWrg(0)(\(tile => {
          MapLcl(1)(MapLcl(0)(rodiniaStencil)) $ Zip2D(
            Slide2D(3, 1) o toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile._0,
            tile._1)
        }))) $ Zip2D(
        Slide2D(6, 4, 514, 512) o Pad2D(1, 1, Pad.Boundary.Clamp) $ heat,
        Slide2D(4, 4, 512, 512) $ power)
    }
  )}

  def hotspotAt() = {
    val M = Var("M", StartFromRange(4096))
    val N = Var("N", StartFromRange(4096))
    fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (power, heat) => {
        MapGlb(1)(MapGlb(0)(rodiniaStencil)) $ Zip2D(
          Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.Clamp) $ heat,
          power)
      }
    )
  }

  def apply() = new RodiniaHotspot(
    Seq(
      ("HOTSPOT", Array[Lambda](hotspotAt())),
      ("HOTSPOT_TILED", Array[Lambda](hotspotTiled()))
  ))

  def main(args: Array[String]): Unit = {
    RodiniaHotspot().run(args)
  }
}
