package opencl.generator

import ir.ast.fun
import ir.{ArrayType, RuntimeSizedArrayType}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

object TestRuntimeSizedArrays {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init(1, 0)
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestRuntimeSizedArrays {

  @Test
  def reduce(): Unit = {
    val inputSize = 1023
    val inputData = Array(inputSize) ++ Array.fill(inputSize)(util.Random.nextInt(5))

    val l = fun(
      RuntimeSizedArrayType(Int),
      a => toGlobal(MapSeq(idI)) o ReduceSeq(fun((acc, x) => addI(acc, x)), 0) $ a
    )

    val (output: Array[Int], runtime) = Execute(inputData.length)(l, inputData)

    assertEquals(inputData.drop(1).sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test
  def reduceFloat(): Unit = {
    val inputSize = 1023
    val inputData = Array(java.lang.Float.intBitsToFloat(inputSize)) ++
      Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun(
      RuntimeSizedArrayType(Float),
      a => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, x) => add(acc, x)), 0.0f) $ a
    )

    val (output: Array[Float], runtime) = Execute(inputData.length)(l, inputData)

    assertEquals(inputData.drop(1).sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Ignore
  @Test
  def nestedReduceFloat(): Unit = {
    def createRow(inputSize: Int) = {
      Array(java.lang.Float.intBitsToFloat(inputSize)) ++
        Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    }

    val size = 128
    val inputData = Array.tabulate(size)(i => createRow(100 + i))

    val N = SizeVar("N")

    val l = fun(
      ArrayType(RuntimeSizedArrayType(Float), N),
      a => MapGlb(toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, x) => add(acc, x)), 0.0f)) $ a
    )

    val (output: Array[Float], runtime) = Execute(inputData.length)(l, inputData)

    assertEquals(inputData.map(_.drop(1).sum).sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

}
