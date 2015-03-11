package opencl.generator

import ir.UserFunDef._
import ir.{Filter, ArrayType, fun, Var}
import opencl.executor.{Executor, Execute}
import opencl.ir.{MapGlb, Int, Float}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}


object TestFilter {
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


class TestFilter {

  @Test def filter(): Unit = {
    val inputSize = 256
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val ids = Array.range(0, inputSize/2).map(_*2)

    val gold = ids.map(inputData(_))

    val N = Var("N")
    val M = Var("M")

    val compFun = fun(
      ArrayType(Float, N),
      ArrayType(Int, M),
      (input, ids) =>
        MapGlb(id) $ Filter(input, ids)
    )

    val (output, runtime) = Execute(inputSize/2)(compFun, inputData, ids, inputSize, inputSize/2)
    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

}
