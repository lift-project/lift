package opencl.generator

import benchmarks.DotProduct
import ir.{ArrayType, TypeChecker}
import ir.ast.fun
import opencl.executor.Compile
import opencl.ir.{Float, id}
import opencl.ir.pattern.MapGlb
import org.junit.Test
import org.junit.Assert._


/**
  * @author cdubach
  */
class TestArray {


  @Test
  def simple(): Unit = {
    val f = fun(
      ArrayType(Float),
      in =>
        MapGlb(id) $ in
    )

    val t = TypeChecker(f)
    assertEquals(t, ArrayType(Float))

    Compile(f)
  }

}
