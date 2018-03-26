package tutorial.applications

import ir.ArrayTypeWSWC
import ir.ast.{Join, Split, Zip, fun}
import lift.arithmetic._
import opencl.executor._
import org.junit.Test
import org.junit.Assert._
import opencl.ir._
import opencl.ir.pattern._

/**
  * Here is an example case of dot product.
  */

object DotProduct extends TestWithExecutor

class DotProduct {

  val N = SizeVar("N")

  def dotProductScala(left: Array[Float], right: Array[Float]): Float = {
    (left,right).zipped.map(_*_).sum
  }

  val inputSize = 1024
  val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

  @Test def simpleDotProduct(): Unit = {

    def dotProductScala(left: Array[Float], right: Array[Float]): Float = {
      (left,right).zipped.map(_*_).sum
    }

    val dotProductLift =
      fun(
        ArrayTypeWSWC(Float, N),
        ArrayTypeWSWC(Float, N),
        (left, right) => {
          Join() o MapGlb(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o Split(4) $ Zip(left, right)
      })

    val (output, runtime) = Execute(inputSize)[Array[Float]](dotProductLift, leftInputData, rightInputData)

    assertEquals(dotProductScala(leftInputData, rightInputData), output.sum, 0.0)

  }

  @Test def dotProductWithLocalAndWorkgroupMappings (): Unit = {

    val dotProductLift = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (left, right) => {
        Join() o MapWrg(
          Join() o  MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o Split(4)
        ) o Split(1024) $ Zip(left, right)
      })

    val (output, runtime) = Execute(inputSize)[Array[Float]](dotProductLift, leftInputData, rightInputData)

    assertEquals(dotProductScala(leftInputData, rightInputData), output.sum, 0.0)

  }


}
