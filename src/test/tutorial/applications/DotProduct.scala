package tutorial.applications

import ir.ArrayTypeWSWC
import ir.ast.{Get, Iterate, Join, Split, Zip, fun}
import ir.printer.DotPrinter
import lift.arithmetic._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

/**
  * Here is an example test case of dot product.
  */

object DotProduct extends TestWithExecutor

class DotProduct {

  val N = SizeVar("N")

  // comparison calculation in scala
  def dotProductScala(left: Array[Float], right: Array[Float]): Float = {
    (left,right).zipped.map(_*_).sum
  }

  val inputSize = 1024
  val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)


  /*
   * original dot product
   */
  @Test def simpleDotProduct(): Unit = {

    def dotProductScala(left: Array[Float], right: Array[Float]): Float = {
      (left,right).zipped.map(_*_).sum
    }

    val dotProductLift =
      fun(
        ArrayTypeWSWC(Float, N),
        ArrayTypeWSWC(Float, N),
        (left, right) => {
          Join() o 
            MapGlb(
              toGlobal(MapSeq(id)) o
                ReduceSeq(add, 0.0f) o
                MapSeq(mult)) o
            Split(4) $ Zip(left, right)
      })

    val (output, runtime) = Execute(inputSize)[Array[Float]](dotProductLift, leftInputData, rightInputData)

    assertEquals(dotProductScala(leftInputData, rightInputData), output.sum, 0.0)

    DotPrinter("./", "dotProductLift1", dotProductLift)
  }

  /*
   * dot product utilising workgroups and local memory
   */
  @Test def dotProductWithLocalAndWorkgroupMappings(): Unit = {

    val dotProductLift = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (left, right) => {
        Join() o MapWrg(
          Join() o 
            MapLcl(
              toGlobal(MapSeq(id)) o
                ReduceSeq(add, 0.0f) o
                MapSeq(mult)) o 
            Split(4)
        ) o Split(1024) $ Zip(left, right)
      })

    val (output, runtime) = Execute(inputSize)[Array[Float]](dotProductLift, leftInputData, rightInputData)

    assertEquals(dotProductScala(leftInputData, rightInputData), output.sum, 0.0)


    DotPrinter("./", "dotProductLift2", dotProductLift)
  }

  /*
   * dot product utilising workgroups, local memory and iterate
   */
  @Test def dotProductWithLocalAndWorkgroupMappingsAndIterate(): Unit = {

    val dotProductLift = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (left, right) => {
        Join() o MapWrg(
          Join() o MapLcl(toGlobal(MapSeq(id))) o Split(1) o
            Iterate(6)(
              Join() o
                MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o
                Split(2)) o Join() o
            MapLcl(
              toLocal(MapSeq(id)) o
                ReduceSeq(fun((acc, y) => multAndSumUp(acc, Get(y, 0), Get(y, 1))), 0.0f)) o 
            Split(2)
        ) o Split(128) $ Zip(left, right)
      })


    val (output, runtime) = Execute(inputSize)[Array[Float]](dotProductLift, leftInputData, rightInputData)

    assertEquals(dotProductScala(leftInputData, rightInputData), output.sum, 0.0)

    DotPrinter("./", "dotProductLift3", dotProductLift)
  }

}
