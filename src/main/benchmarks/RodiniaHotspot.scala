package benchmarks

import lift.arithmetic.{StartFromRange, Var}
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

class RodiniaHotspot(override val f: Seq[(String, Array[Lambda])]) extends Benchmark("RodiniaHotspot", Seq(1036, 1036), f, 0.01f) {

  override def generateInputs(): Seq[Any] = {
    // change below as well //
    //val inputSize = 1036
    //val inputSize = 4102
    val inputSize = 8204

    val heat = Array.tabulate(inputSize, inputSize) { (i, j) => i * inputSize.toFloat + j }
    val power = Array.tabulate(inputSize, inputSize) { (i, j) => i * inputSize.toFloat + j }
    val x = 0.1f; val y = 0.1f; val z = 1024000; val c = 1.068e-7f
    val coeff = Array(0, c*y, 0, c*x, c*(-2*y-2*x-z+1), c*x, 0, c*y, 0)

    Seq(heat, power, coeff)
  }

  override def globalSize: Array[Int] = {
    //Array(1184,1184,1) // 1024
    //Array(4736,4736,1) // 4096
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

  // input rodinia 4096:
  //val inputSize = 4102
  //val nbhs = 293

  // input rodinia 8192:
  val inputSize = 8204
  val nbhs = 586

  /////////////////// LAMBDAS
  val addAmbientTemp = UserFun("addAmbientTemp", Array("x", "y"), "{ return x + y + (0.1f * 1.068e-7f * 80.0f); }", Seq(Float, Float), Float)

  // the two outermost dimensions of A and B have to be the same
  // zip matrices elementwise
  val zip2d = \((A,B) =>
    Map( \(tuple => Zip(tuple._0, tuple._1))) $ Zip(A,B)
  )

  // create 16x16 heat tile and 14x14 power tile
  val createTiles = \((heat, power) =>
    zip2d( Slide2D(16,14) o Pad2D(1,1,Pad.Boundary.MirrorUnsafe) $ heat,
      Slide2D(14,14) $ power)
  )

  // load into local memory and prepare data for single work-item
  // < < coeff, heat> :: [9] , power >
  val prepareData = \((coeff, tiles) =>
    zip2d(
      // first component
      Map(Map( \(heatNbh =>
        Zip(coeff, Join() $ heatNbh)
      ))) o Slide2D(3,1) o
        toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._0,
      // second component
      toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._1)
  )

  // how to compute output using required data:
  val stencil = fun(requiredData => {
    val coeffHeatTuple = requiredData._0
    val powerValue = requiredData._1

    toGlobal(MapSeq(id)) o
      MapSeq( \(x => addAmbientTemp(powerValue, x))) o
      ReduceSeqUnroll(\((acc, next) =>
        multAndSumUp(acc, next._0, next._1)), 0.0f) $ coeffHeatTuple
  })

  def hotspotGeneric(): Lambda = {
    val N = Var("N", StartFromRange(2))
    fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
    ArrayTypeWSWC(Float, 9),
    (heat, power, coeff) => {
      MapWrg(1)(MapWrg(0)( \(tiles =>
        MapLcl(1)(MapLcl(0)(stencil)) o prepareData(coeff) $ tiles)
      )) $ createTiles(heat, power)
    }
    )
  }

  def hotspotInject(): Lambda = {
    val N = Var("N", StartFromRange(2))
    fun(
    //ArrayTypeWSWC(ArrayTypeWSWC(Float, 1036), 1036),
    //ArrayTypeWSWC(ArrayTypeWSWC(Float, 1036), 1036),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, 8204), 8204),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, 8204), 8204),
    ArrayTypeWSWC(Float, 9),
    (heat, power, coeff) => {
      MapWrg(1)(MapWrg(0)( \(tiles =>
        MapLcl(1)(MapLcl(0)(stencil)) o prepareData(coeff) $ tiles)
      )) $ createTiles(heat, power)
    }
    )
  }
  def apply() = new RodiniaHotspot(
    Seq(
      ("HOTSPOT_GENERIC", Array[Lambda](hotspotGeneric())),
      ("HOTSPOT_INJECT", Array[Lambda](hotspotInject()))
  ))

  def main(args: Array[String]): Unit = {
    RodiniaHotspot().run(args)
  }
}
