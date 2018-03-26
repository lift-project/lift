package tutorial.applications

import ir.{ArrayType, ArrayTypeWSWC}
import ir.ast._
import opencl.executor.Execute
import lift.arithmetic._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit._

import scala.util.Random

/**
  * Stencil algorithms can be found in many fields including medical imaging, physical simulations and machine learning.
  * Below are a few example tests that show how stencils can be programmed using LIFT in 1, 2 and 3 dimensions.
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


  /* some helper variables */

  val delta = 0.001f

  val slidesize = 3;

  val slidestep = 1;

  val weights2D = Array(
    0.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 0.0f)

  val weights3D = Array(
    Array(Array(0.0f, 0.0f, 0.0f),
      Array(0.0f, 1.0f, 0.0f),
      Array(0.0f, 0.0f, 0.0f)),
    Array(Array(0.0f, 1.0f, 0.0f),
      Array(1.0f, 1.0f, 1.0f),
      Array(0.0f, 1.0f, 0.0f)),
    Array(Array(0.0f, 0.0f, 0.0f),
      Array(0.0f, 1.0f, 0.0f),
      Array(0.0f, 0.0f, 0.0f))
  )

}

object Stencils extends TestWithExecutor

class Stencils {


  @Test def jacobi3Point1DStencil(): Unit =
  {

    val randomData = Array.tabulate(128)(_.toFloat)
    val gold = Utils.scalaCompute1DStencilConstantBoundary(randomData,3,1,1,1,0.0f)

    val stencilLambda = fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        toGlobal(MapGlb(id)) o Join() o MapGlb(ReduceSeq(add, 0.0f)) o
          Slide(3, 1) o PadConstant(1, 1, 0.0f) $ input
      }
    )

    val (output, _) = Execute(randomData.length)[Array[Float]](stencilLambda, randomData)

    assertArrayEquals(gold, output, StencilUtilities.delta)

  }

  @Test def jacobi3Point1DStencilWithWeights(): Unit =
  {

    val randomData = Seq.fill(256)(Random.nextFloat()).toArray
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

  @Test def jacobi5Point2DStencilWithWeights(): Unit =
  {

    val stencilValues = Array.tabulate(6,6) { (i,j) => (j + 1).toFloat }

    val compareData = Array(4.0f,8.0f,12.0f,16.0f,20.0f,17.0f,
                            5.0f,10.0f,15.0f,20.0f,25.0f,23.0f,
                            5.0f,10.0f,15.0f,20.0f,25.0f,23.0f,
                            5.0f,10.0f,15.0f,20.0f,25.0f,23.0f,
                            5.0f,10.0f,15.0f,20.0f,25.0f,23.0f,
                            4.0f,8.0f,12.0f,16.0f,20.0f,17.0f)

    val stencilLambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      ArrayTypeWSWC(Float, StencilUtilities.weights2D.length),
      (mat, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) o PadConstant2D(1,1,0.0f) $ mat
      })

    val (output, runtime) = Execute(stencilValues.length, stencilValues.length)[Array[Float]](stencilLambda, stencilValues, StencilUtilities.weights2D)

    assertArrayEquals(compareData, output, StencilUtilities.delta)

  }

  @Test def jacobi7Point3DStencilWithWeights(): Unit =
  {

    val nx = 4
    val ny = 6
    val nz = 8

    val stencilValues = Array.tabulate(nx,ny,nz) { (i,j,k) => (i + j + k + 1).toFloat }

    val compareData = Array(28.0f,35.0f,42.0f,49.0f,
                            56.0f,63.0f,35.0f,42.0f,
                            49.0f,56.0f,63.0f,70.0f,
                            42.0f,49.0f,56.0f,63.0f,
                            70.0f,77.0f,49.0f,56.0f,
                            63.0f,70.0f,77.0f,84.0f,
                            35.0f,42.0f,49.0f,56.0f,
                            63.0f,70.0f,42.0f,49.0f,
                            56.0f,63.0f,70.0f,77.0f,
                            49.0f,56.0f,63.0f,70.0f,
                            77.0f,84.0f,56.0f,63.0f,
                            70.0f,77.0f,84.0f,91.0f)

    val stencilLambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")), SizeVar("O")),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      (mat, weights) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(neighbours => {
          toGlobal(MapSeq(id)) o
            ReduceSeqUnroll(\((acc, next) =>
              multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() o Join() $ neighbours, Join() o Join() $ weights)
        })))
        ) o Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    val (output, runtime) = Execute(2, 2, 2, nx, ny, nz, (true,true))[Array[Float]](stencilLambda, stencilValues, StencilUtilities.weights3D)

    assertArrayEquals(compareData, output, StencilUtilities.delta)

  }

}
