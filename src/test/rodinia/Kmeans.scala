package rodinia

import apart.arithmetic.Var
import ir.{ArrayType, TupleType}
import ir.ast._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.{AfterClass, BeforeClass, Test}

object Kmeans {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class Kmeans {

  val P = Var("N") // number of points
  val C = Var("M") // number of clusters
  val F = Var("K") // number of features

  val featuresType    = ArrayType(ArrayType(Float, F), P)
  val clustersType    = ArrayType(ArrayType(Float, C), F)
  val membershipType  = ArrayType(Int, P)

  val update = UserFun("update", Array("dist", "pair"),
    "{ return dist + (pair._0 - pair._1) * (pair._0 - pair._1); }",
    Seq(Float, TupleType(Float, Float)), Float)

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

  @Test
  def kmeans(): Unit = {
    val kmeans = fun(
      featuresType, clustersType,
      (features, clusters) => {
        features :>> MapGlb( \( feature => {
          clusters :>> ReduceSeq( \( (tuple, cluster) => {

            val dist = Zip(feature, cluster) :>> ReduceSeq(update, 0.0f )
            Zip(dist, tuple) :>> MapSeq(test)

          }), Value("{3.40282347e+38, 0, 0}", ArrayType(TupleType(Float, Int, Int), 1)) ) :>>
          toGlobal(MapSeq(MapSeq(select)))
        }) )
      })

    val code = Compile(kmeans)
    Execute(128)(kmeans, Array.ofDim[Float](2, 2), Array.ofDim[Float](2,2))

    println(code)
  }

  @Test
  def kmeans_swap(): Unit = {
    val kmeans_swap = fun(
      featuresType,
      features => {
        features :>> MapGlb(MapSeq(id)) :>> TransposeW()
      })

    val code = Compile(kmeans_swap)

    println(code)
  }

}
