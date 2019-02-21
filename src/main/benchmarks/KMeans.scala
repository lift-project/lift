package benchmarks

import lift.arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class KMeans(override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("Kmeans", Seq(1024, 34, 5), f, 0.1f) {

  override def runScala(inputs: Any*): Array[Float] = {
    val points = inputs(0).asInstanceOf[Array[Array[Float]]]
    val centres = inputs(1).asInstanceOf[Array[Array[Float]]]

    points.transpose.map(x => {
      centres.zipWithIndex
        .map(c => ((c._1,x).zipped.map((p1, p2) => (p1-p2)*(p1-p2)).sum, c._2))
        .reduce((p1, p2) => if (p1._1 < p2._1) p1 else p2)
        ._2
    }).map(_.toFloat)
  }

  override def generateInputs(): Seq[Any] = {
    val numPoints = inputSizes().head
    val numClusters = inputSizes()(1)
    val numFeatures = inputSizes()(2)

    val points = Array.fill(numPoints, numFeatures)(util.Random.nextFloat())
    val clusters = Array.fill(numClusters, numFeatures)(util.Random.nextFloat())

    Seq(points.transpose, clusters)
  }

  override protected def check(x: Float, y: Float): Boolean = {
    var diff = (x-y)/x

    if (x == 0.0f)
      diff = 0.0f

    diff.abs >= delta
  }

}

object KMeans {

  val P = SizeVar("P") // number of points
  val C = SizeVar("C") // number of clusters
  val F = SizeVar("F") // number of features

  val featuresType    = ArrayTypeWSWC(ArrayTypeWSWC(Float, P), F)
  val clustersType    = ArrayTypeWSWC(ArrayTypeWSWC(Float, F), C)

  val update = UserFun("update", Array("dist", "pair"),
    "{ return dist + (pair._0 - pair._1) * (pair._0 - pair._1); }",
    Seq(Float, TupleType(Float, Float)), Float)

  val update2 = UserFun("update", Array("dist", "pair0", "pair1"),
    "{ return dist + (pair0 - pair1) * (pair0 - pair1); }",
    Seq(Float, Float, Float), Float)

  val test = UserFun("test", Array("dist", "tuple"),
    "{" +
      "float min_dist = tuple._0;" +
      "int i          = tuple._1;" +
      "int index      = tuple._2;" +
      "if (dist < min_dist) {" +
      "  Tuple t = {dist, i + 1, i};" +
      "  return t;" +
      "} else {" +
      "  Tuple t = {min_dist, i + 1, index};" +
      "  return t;" +
      "}" +
      "}",
    Seq(Float, TupleType(Float, Int, Int)), TupleType(Float, Int, Int))

  val select = UserFun("select_", Array("tuple"),
    "{ return tuple._2; }",
    Seq(TupleType(Float, Int, Int)), Int)

  val rodinia = fun(
      featuresType, clustersType,
      (features, clusters) => {
        features :>> Transpose() :>> MapGlb( \( feature => {
          clusters :>> ReduceSeq( \( (tuple, cluster) => {

            val dist = Zip(feature, cluster) :>> ReduceSeq(update, 0.0f )
            Zip(dist, tuple) :>> MapSeq(test)

          }), Value("{3.40282347e+38, 0, 0}", ArrayTypeWSWC(TupleType(Float, Int, Int), 1)) ) :>>
          toGlobal(MapSeq(MapSeq(select)))
        }) )
      })


  def apply() = new KMeans(Seq(
    ("rodinia", Array[Lambda](rodinia))
  ))

  def main(args: Array[String]): Unit = {
    KMeans().run(args)
  }
}
