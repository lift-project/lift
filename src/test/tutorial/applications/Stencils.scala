package tutorial.applications

import ir.ArrayType
import ir.ast.{Get, Pad, PadConstant, Reduce, Slide, Zip, fun}
import opencl.executor.Execute
import opencl.ir.pattern._
import lift.arithmetic._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert.{assertArrayEquals, _}
import org.junit._

import scala.util.Random

/**
  * Stencil algorithms can be found in many fields including medical imaging, physical simulations and machine learning.
  * Below are a few example tests that show how stencils can be programmed using LIFT.
  */

object StencilUtilities
{
  /* some helper functions */
  def print1DArray[T](input: Array[T]) = {
    println(input.mkString(","))
  }

  def print2DArray[T](input: Array[Array[T]]) = {
    println(input.deep.mkString("\n"))
  }

  def print3DArray[T](input: Array[Array[Array[T]]]) = {
    for (i <- 0 to input.length-1){
      print2DArray(input(i))
      println()
    }
  }

}

class Stencils {


  @Test def jacobi3Point1DStencil(): Unit =
  {

    val randomData = Seq.fill(24)(Random.nextFloat()).toArray
    val BOUNDARY = Pad.Boundary.Clamp
    val weights = Array(1, 2, 1).map(_.toFloat)
    val gold = Utils.scalaCompute1DStencilConstantBoundary(randomData,3,1,1,1,0.0f)

    val stencil = fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeq(id)) o
              ReduceSeq(fun((acc, value) => { sumUp.apply(acc,value)}),0.0f) $ input
          })
        ) o Slide(3, 1) o PadConstant(1, 1, 0.0f) $ input
      }
    )

    val (output, _) = Execute(2,2)[Array[Float]](stencil, randomData)

    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArray(gold)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test def jacobi3Point1DStencilWithWeights(): Unit =
  {

    val randomData = Seq.fill(24)(Random.nextFloat()).toArray
    val SCALABOUNDARY = Utils.scalaClamp
    val BOUNDARY = Pad.Boundary.Clamp
    val weights = Array(1, 2, 1).map(_.toFloat)
    val gold = Utils.scalaCompute1DStencil(randomData, 3, 1, 1, 1, weights, SCALABOUNDARY)

    val stencil = fun(
      ArrayType(Float, SizeVar("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, y) => {
                multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
              }), 0.0f) $
              Zip(weights, neighbourhood)
          })
        ) o Slide(3, 1) o Pad(1, 1, BOUNDARY) $ input
      }
    )

    val (output, _) = Execute(randomData.length)[Array[Float]](stencil, randomData, weights)
    assertArrayEquals(gold, output, 0.1f)

  }


}
