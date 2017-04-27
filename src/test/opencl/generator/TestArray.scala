package opencl.generator

import ir.{ArrayType, TypeChecker}
import ir.ast.fun
import opencl.executor.Compile
import opencl.ir.{Float, id}
import opencl.ir.pattern.MapGlb
import org.junit.{Ignore, Test}
import org.junit.Assert._


class TestArray {
  @Ignore
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
