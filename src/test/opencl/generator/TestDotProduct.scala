package opencl.generator

import arithmetic.Var
import benchmarks.DotProduct
import opencl.executor._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestDotProduct {
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

class TestDotProduct {

  val N = Var("N")
  val M = Var("M")

  private def dotProd(left: Array[Float], right: Array[Float]): Float = {
    (left,right).zipped.map(_*_).sum
  }

  @Test def DOT_PRODUCT_SIMPLE() {

    val inputSize = 1024
    //val leftInputData = Array.fill(inputSize)(1.0f)
    //val rightInputData = Array.fill(inputSize)(1.0f)
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (output: Array[Float], runtime) =
      Execute(inputSize)(DotProduct.dotProductSimple, leftInputData, rightInputData)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(dotProd(leftInputData, rightInputData), output.sum, 0.0)
  }

  @Test def DOT_PRODUCT_CPU() {

    val inputSize = 262144
    //val leftInputData = Array.fill(inputSize)(1.0f)
    //val rightInputData = Array.fill(inputSize)(1.0f)
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (firstOutput, _) = {
      val (output: Array[Float], runtime) =
        Execute(inputSize)(DotProduct.dotProductCPU1, leftInputData, rightInputData)

      println("output.size = " + output.length)
      println("output(0) = " + output(0))
      println("runtime = " + runtime)

      assertEquals(dotProd(leftInputData, rightInputData), output.sum, 0.0)

      (output, runtime)
    }

    {
      val (output: Array[Float], runtime) =
        opencl.executor.Execute(firstOutput.length)(DotProduct.dotProductCPU2, firstOutput)

      println("output(0) = " + output(0))
      println("runtime = " + runtime)

      assertEquals(dotProd(leftInputData, rightInputData), output.sum, 0.0)
    }
  }

  @Test def DOT_PRODUCT() {

    val inputSize = 262144
    //val leftInputData = Array.fill(inputSize)(1.0f)
    //val rightInputData = Array.fill(inputSize)(1.0f)
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (firstOutput, _) = {
      val (output: Array[Float], runtime) =
        opencl.executor.Execute(inputSize)(DotProduct.dotProduct1, leftInputData, rightInputData)

      println("output.size = " + output.length)
      println("output(0) = " + output(0))
      println("runtime = " + runtime)

      assertEquals(dotProd(leftInputData, rightInputData), output.sum, 0.0)

      (output, runtime)
    }

    {
      val (output: Array[Float], runtime) =
        opencl.executor.Execute(firstOutput.length)(DotProduct.dotProduct2, firstOutput)

      println("output(0) = " + output(0))
      println("runtime = " + runtime)

      assertEquals(dotProd(leftInputData, rightInputData), output.sum, 0.0)
    }

  }

}