package benchmarks

import lift.arithmetic.{Cst, SizeVar, StartFromRange, Var}
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class RodiniaHotspotNoPad(override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("RodiniaHotspotNoPad", Seq(8192, 8192), f, 0.01f) {

  override def generateInputs(): Seq[Any] = {
    val inputSize = 8192

    val heat = Array.tabulate(inputSize + 2, inputSize + 2) { (i, j) => i * inputSize.toFloat + j }
    val power = Array.tabulate(inputSize, inputSize) { (i, j) => i * inputSize.toFloat + j }

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

object RodiniaHotspotNoPad {
  /////////////////// LAMBDAS
  def rodiniaUserFun = UserFun("rodiniaUserFun",
      Array("power", "top", "bottom", "left", "right", "center"), "{ float step_div_cap = 1.365333e+00f; return center + step_div_cap*(power + 0.1f*(bottom + top - 2*center) + 0.1*(left + right - 2*center) + 4.882813e-05f*(80.0f - center)); }",
      Seq(Float, Float, Float, Float, Float, Float), Float)

  def rodiniaStencilConstants = \(tuple => {
    val nbh = tuple._0
    //val powerValue = tuple._1
    val powerValue = 0.0f

    val top = 0.0f
    val bottom = 0.0f
    val left = 0.0f
    val right = 0.0f
    val center = 0.0f

    toGlobal(id) o toPrivate(fun(x => rodiniaUserFun(x, top, bottom, left, right, center))) $ powerValue
  })

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

  def hotspotTiledConstants() = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M+2), N+2),
    (power, heat) => {
      Map(Join()) o Join() o Map(TransposeW()) o
        MapWrg(1)(MapWrg(0)(\(tile => {
          MapLcl(1)(MapLcl(0)(rodiniaStencilConstants)) $ Zip2D(
            Slide2D(3, 1) o toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile._0,
            tile._1)
        }))) $ Zip2D(
        Slide2D(514, 512, 6, 4) $ heat,
        Slide2D(512, 512, 4, 4) $ power)
    }
  )}

  def rodiniaStencilNoHeat = \(tuple => {
    val top = 0.0f
    val bottom = 0.0f
    val left = 0.0f
    val right = 0.0f
    val center = 0.0f

    toGlobal(id) o toPrivate(fun(x => rodiniaUserFun(x, top, bottom, left, right, center))) $ tuple
  })

  def hotspotTiledNoHeat() = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M+2), N+2),
    (power, heat) => {
      Map(Join()) o Join() o Map(TransposeW()) o
        MapWrg(1)(MapWrg(0)(\(tile => {
          MapLcl(1)(MapLcl(0)(rodiniaStencilNoHeat)) $ tile
        }))) o Map(Transpose()) o Split(512) o Map(Split(4)) $ power
    }
  )}

  def hotspotTiled() = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M+2), N+2),
    (power, heat) => {
      Map(Join()) o Join() o Map(TransposeW()) o
        MapWrg(1)(MapWrg(0)(\(tile => {
          MapLcl(1)(MapLcl(0)(rodiniaStencil)) $ Zip2D(
            Slide2D(3, 1) o toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile._0,
            tile._1)
        }))) $ Zip2D(
        Slide2D(514, 512, 6, 4) $ heat,
        Map(Transpose()) o Split(512) o Map(Split(4)) $ power)
        //Slide2D(512, 512, 4, 4) $ power)
    }
  )}

  def hotspotAt() = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M+Cst(2)), N+Cst(2)),
      (power, heat) => {
        MapGlb(1)(MapGlb(0)(rodiniaStencil)) $ Zip2D(
          Slide2D(3, 1) $ heat,
          power)
      }
    )
  }

  def hotspotConstants() = {
    val M = Var("M", StartFromRange(2))
    val N = Var("N", StartFromRange(2))
    fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M+2), N+2),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (heat, power) => {
        MapGlb(1)(MapGlb(0)(rodiniaStencilConstants)) $ Zip2D(
          Slide2D(3, 1) $ heat,
          power)
      }
    )
  }

  def apply() = new RodiniaHotspotNoPad(
    Seq(
      ("HOTSPOT_NOPAD", Array[Lambda](hotspotAt())),
      ("HOTSPOT_NOPAD_CONSTANTS", Array[Lambda](hotspotConstants())),
      ("HOTSPOT_NOPAD_TILED", Array[Lambda](hotspotTiled())),
      ("HOTSPOT_NOPAD_TILED_CONSTANTS", Array[Lambda](hotspotTiledConstants())),
      ("HOTSPOT_NOPAD_NOHEAT", Array[Lambda](hotspotTiledNoHeat()))
  ))

  def main(args: Array[String]): Unit = {
    RodiniaHotspotNoPad().run(args)
  }
}
