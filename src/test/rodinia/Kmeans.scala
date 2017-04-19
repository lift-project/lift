package rodinia

import ir.{ArrayTypeWSWC, TupleType}
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import rodinia.Kmeans._

object Kmeans {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    Executor.shutdown()
  }

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

  def calculateMembership(points: Array[(Float, Float)], centres: Array[(Float, Float, Int)]): Array[Int] = {
    points.map(x => {
      centres
        .map(c => ((x._1 - c._1) * (x._1 - c._1) + (x._2 - c._2) * (x._2 - c._2), c._3))
        .reduce((p1, p2) => if (p1._1 < p2._1) p1 else p2)
        ._2
    })
  }

  def calculateMembership(points: Array[Array[Float]], centres: Array[Array[Float]]): Array[Int] = {
    points.map(x => {
      centres.zipWithIndex
        .map(c => ((c._1,x).zipped.map((p1, p2) => (p1-p2)*(p1-p2)).sum, c._2))
        .reduce((p1, p2) => if (p1._1 < p2._1) p1 else p2)
        ._2
    })
  }
}

class Kmeans {

  @Test def kMeansMembership2Dim(): Unit = {
    val inputSize = 512
    val k = 16

    val pointsX = Array.fill(inputSize)(util.Random.nextFloat())
    val pointsY = Array.fill(inputSize)(util.Random.nextFloat())
    val centresX = Array.fill(k)(util.Random.nextFloat())
    val centresY = Array.fill(k)(util.Random.nextFloat())
    val indices = Array.range(0, k)

    val distance = UserFun("dist", Array("x", "y", "a", "b", "id"), "{ Tuple t = {(x - a) * (x - a) + (y - b) * (y - b), id}; return t; }", Seq(Float, Float, Float, Float, Int), TupleType(Float, Int))
    val minimum = UserFun("minimum", Array("x", "y"), "{ return x._0 < y._0 ? x : y; }", Seq(TupleType(Float, Int), TupleType(Float, Int)), TupleType(Float, Int))
    val getSecond = UserFun("getSecond", "x", "{ return x._1; }", TupleType(Float, Int), Int)

    val points = pointsX zip pointsY
    val centres = (centresX, centresY, indices).zipped.toArray

    val gold = calculateMembership(points, centres)

    val N = SizeVar("N")
    val K = SizeVar("K")

    val function = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, K),
      ArrayTypeWSWC(Float, K),
      ArrayTypeWSWC(Int, K),
      (x, y, a, b, i) => {
        MapGlb(fun(xy => {
          toGlobal(MapSeq(idI)) o
            MapSeq(getSecond) o
            ReduceSeq(minimum, (scala.Float.MaxValue, -1)) o
            MapSeq(fun(ab => {
              distance(Get(xy, 0), Get(xy, 1), Get(ab, 0), Get(ab, 1), Get(ab, 2))
            })) $ Zip(a, b, i)
        })) $ Zip(x, y)
      }
    )

    val (output: Array[Int], _) =
      Execute(inputSize)(function, pointsX, pointsY, centresX, centresY, indices)

    assertArrayEquals(gold, output)
  }

  @Test
  def kMeans(): Unit = {

    val numPoints = 1024
    val numClusters = 5
    val numFeatures = 34

    val points = Array.fill(numPoints, numFeatures)(util.Random.nextFloat())
    val clusters = Array.fill(numClusters, numFeatures)(util.Random.nextFloat())

    val gold = calculateMembership(points, clusters)

    val kMeans = fun(
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

    val (output: Array[Int], _) = Execute(numPoints)(kMeans, points.transpose, clusters)
    assertArrayEquals(gold, output)
  }

  @Test
  def kMeansLocalMemory(): Unit = {

    val numPoints = 1024
    val numClusters = 5
    val numFeatures = 8

    val points = Array.fill(numPoints, numFeatures)(util.Random.nextFloat())
    val clusters = Array.fill(numClusters, numFeatures)(util.Random.nextFloat())

    val gold = calculateMembership(points, clusters)

    val splitFactor = 128

    val kMeans = fun(
      featuresType, clustersType,
      (features, clusters) => {
        features :>> Transpose() :>> Split(splitFactor) :>> MapWrg(\( featuresChunk =>
          clusters :>> toLocal(MapLcl(MapSeq(id))) :>> Let(localClusters =>
          MapLcl( \( feature => {
            localClusters :>> ReduceSeq( \( (tuple, cluster) => {

              val dist = Zip(feature, cluster) :>> ReduceSeq(\((acc, b) => update2(acc, Get(b, 0), Get(b,1))), 0.0f )
              Zip(dist, tuple) :>> MapSeq(test)

            }), Value("{3.40282347e+38, 0, 0}", ArrayTypeWSWC(TupleType(Float, Int, Int), 1)) ) :>>
              toGlobal(MapSeq(MapSeq(select)))
          })) $ featuresChunk
          )
        )) :>> Join()
      })

    val (output: Array[Int], _) = Execute(numPoints)(kMeans, points.transpose, clusters)
    assertArrayEquals(gold, output)
  }

  @Test
  def kMeans_swap(): Unit = {
    val kMeans_swap = fun(
      featuresType,
      features => {
        features :>> MapGlb(MapSeq(id)) :>> TransposeW()
      })

    val code = Compile(kMeans_swap)

    println(code)
  }

}
