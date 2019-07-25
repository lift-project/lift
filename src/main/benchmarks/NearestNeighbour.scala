package benchmarks

import lift.arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class NearestNeighbour(override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("NearestNeighbour", Seq(1024), f, 0.1f) {

  override def runScala(inputs: Any*): Array[Float] = {
    Array()
  }

  override def generateInputs(): Seq[Any] = {
    val numPoints = inputSizes().head

    val points = Array.fill(numPoints * 2)(util.Random.nextFloat())

    Seq(points, 0.5f, 1.5f)
  }

  override protected def check(x: Float, y: Float): Boolean = {
    var diff = (x-y)/x

    if (x == 0.0f)
      diff = 0.0f

    diff.abs >= delta
  }

}

object NearestNeighbour {

  val distance = UserFun("distance_", Array("loc", "lat", "lng"),
    "{ return sqrt( (lat - loc._0) * (lat - loc._0) + (lng - loc._1) * (lng - loc._1) ); }",
    Seq(TupleType(Float, Float), Float, Float), Float)

  val N = SizeVar("N")

  val rodinia = fun(
    ArrayTypeWSWC(TupleType(Float, Float), N), Float, Float,
    (locations, lat, lng) => {
      locations :>> MapGlb( \(loc => distance(loc, lat, lng)) )
    })

  def apply() = new NearestNeighbour(Seq(
    ("rodinia", Array[Lambda](rodinia))
  ))

  def main(args: Array[String]): Unit = {
    NearestNeighbour().run(args)
  }
}
