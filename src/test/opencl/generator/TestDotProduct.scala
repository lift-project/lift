package opencl.generator

import benchmarks.DotProduct
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor, TestWithExecutor, Utils}
import org.junit.Assert._
import org.junit.Test

object TestDotProduct extends TestWithExecutor

class TestDotProduct {

  val N = SizeVar("N")
  val M = SizeVar("M")

  private def dotProd(left: Array[Float], right: Array[Float]): Float = {
    (left,right).zipped.map(_*_).sum
  }

  @Test def DOT_PRODUCT_SIMPLE(): Unit = {

    val inputSize = 1024
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (output, runtime) = Execute(inputSize)[Array[Float]](DotProduct.dotProductSimple, leftInputData, rightInputData)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(dotProd(leftInputData, rightInputData), output.sum, 0.0)
  }

  @Test def DOT_PRODUCT_CPU(): Unit = {

    val inputSize = 262144
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) =
        Execute(inputSize)[Array[Float]](DotProduct.dotProductCPU1, leftInputData, rightInputData)

      println("output.size = " + output.length)
      println("output(0) = " + output(0))
      println("runtime = " + runtime)

      assertEquals(dotProd(leftInputData, rightInputData), output.sum, 0.0)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(firstOutput.length)[Array[Float]](DotProduct.dotProductCPU2, firstOutput)

      println("output(0) = " + output(0))
      println("runtime = " + runtime)

      assertEquals(dotProd(leftInputData, rightInputData), output.sum, 0.0)
    }
  }

  @Test def DOT_PRODUCT(): Unit = {

    // TODO: Workaround for AMD GPUs. See issue 42.
    if (Utils.isAmdGpu)
      AllocateLocalMemoryStatically(false)

    val inputSize = 262144
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (firstOutput, _) = Execute(inputSize)[Array[Float]](DotProduct.dotProduct1, leftInputData, rightInputData)

    val (output, _) = Execute(firstOutput.length)[Array[Float]](DotProduct.dotProduct2, firstOutput)

    AllocateLocalMemoryStatically(true)
    assertEquals(dotProd(leftInputData, rightInputData), firstOutput.sum, 0.0)
    assertEquals(dotProd(leftInputData, rightInputData), output.sum, 0.0)
  }

}
