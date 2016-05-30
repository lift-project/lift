package opencl.generator

import ir.ast.fun
import ir.UnknownLengthArrayType
import opencl.executor.{Executor, Execute}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestUBArray {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestUBArray {

  @Test def reduce(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun(
      UnknownLengthArrayType(Float),
      a => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, x) => add(acc, x)), 0.0f) $ a
    )

    val (output: Array[Float], runtime) = Execute(inputData.length)( l, inputData)

    assertEquals(inputData.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

}