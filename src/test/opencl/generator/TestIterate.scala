package opencl.generator

import ir.ArrayType
import ir.ast.{Get, Iterate, Join, Split, Zip, fun}
import opencl.executor.{Execute, TestWithExecutor, Utils}
import opencl.ir.pattern.{MapGlb, MapSeq}
import opencl.ir.{Int, IntToValue, add}
import org.junit.Assert.assertArrayEquals
import org.junit.{Assume, Test}

object TestIterate extends TestWithExecutor

class TestIterate {
  @Test
  def IterateAfterGet(): Unit = {
    Assume.assumeFalse("Disabled on AMD GPUs.", Utils.isAmdGpu)

    def increment = fun(Int, x => add(Int).apply(1, x))
    def f = fun(
      ArrayType(Int, 128),
      ArrayType(Int, 128),
      (v, w) =>
        Join() o MapGlb(
          Iterate(5)(MapSeq(increment)) o Get(0)
        ) $ Zip(Split(32) $ v, Split(32) $ w)
    )

    val V = Array.fill(128)(util.Random.nextInt(1024))
    val W = Array.fill(128)(util.Random.nextInt(1024))
    val (output, _) = Execute(4, 4)[Array[Int]](f, V, W)
    assertArrayEquals(V.map(_ + 5), output)
  }
}
