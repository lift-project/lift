package rodinia

import apart.arithmetic.{SizeVar, Var}
import ir.{ArrayType, TupleType}
import ir.ast.{UserFun, \, fun}
import opencl.executor._
import opencl.ir.Float
import opencl.ir.pattern.MapGlb
import org.junit.{AfterClass, BeforeClass, Test}

object NearestNeighbor {
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

class NearestNeighbor {
  @Test
  def main(): Unit = {

    val distance = UserFun("distance", Array("loc", "lat", "lng"),
      "{ return sqrt( (lat - loc._0) * (lat - loc._0) + (lng - loc._1) * (lng - loc._1) ); }",
      Seq(TupleType(Float, Float), Float, Float), Float)

    val N = SizeVar("N")

    val nn = fun(
      ArrayType(TupleType(Float, Float), N), Float, Float,
      (locations, lat, lng) => {
        locations :>> MapGlb( \(loc => distance(loc, lat, lng)) )
      })

    val code = Compile(nn)

    println(code)
  }
}
