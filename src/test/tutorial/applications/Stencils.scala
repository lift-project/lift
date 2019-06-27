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


object StencilTutorialUtilities
{
  /* some helper print functions */

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

  /*
   * compute a one dimensional stencil with "re-indexed" boundary values
   */

  def scalaCompute1DStencil(data: Array[Float],
                            size: Int, step: Int,
                            left: Int, right: Int,
                            weights: Array[Float],
                            boundary: (Int, Int) => Int) =
  {

    // create padding based on function "boundary"
    val leftPadding = Array.tabulate(left)(x => data(boundary((x + 1) * -1, data.length))).reverse
    val rightPadding = Array.tabulate(right)(x => data(boundary(x + data.length, data.length)))

    // pad the input array
    val paddedInput = leftPadding ++ data ++ rightPadding

    // create the neighbourhoods of size "size" with an overlap of "step"
    val neighbourhoodArray = paddedInput.sliding(size, step).toArray

    // iterate over the sum of the neighborhoods multiplied by the weights
    neighbourhoodArray.map(_.zip(weights).foldLeft(0.0f)((acc, p) => acc + p._1 * p._2))

  }


  /*
   * compute a one dimensional stencil with constant boundary values and no weights
   */

  def scalaCompute1DStencilConstantBoundary(data: Array[Float],
                                            size: Int, step: Int,
                                            left: Int, right: Int,
                                            boundaryValue: Float) =
  {

    // create padding based on input "boundaryValue"
    val leftPadding = Array.fill(left)(boundaryValue)
    val rightPadding = Array.fill(right)(boundaryValue)
    val paddedInput = leftPadding ++ data ++ rightPadding

    val neighbourhoodArray = paddedInput.sliding(size, step).toArray

    // map over the sum of the neighborhoods
    neighbourhoodArray.map(_.foldLeft(0.0f)((acc, p) => acc + p))
  }


  /* some helper variables */

  val delta = 0.001f // how much arrays can differ by and still be considered correct

  val slidesize = 3; // size of neighbourhoods to calculate

  val slidestep = 1; // size of overlap to process neighbourhoods

  // weight value array for 2D - in these two cases (with weights3D below) we are just computing a simple unweighted stencil
  val weights2D = Array(
    0.0f, 1.0f, 0.0f,
    1.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 0.0f)

  // weight value array for 3D
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

/**
  * Stencil algorithms can be found in many fields including medical imaging, physical simulations and machine learning.
  * Below are a few example tests that show how stencils can be programmed using LIFT in 1, 2 and 3 dimensions.
  */

class Stencils {

/*
 * simple 1D unweighted stencil with neighbourhood sizes of 3 and zeros at the boundary
 */

  @Test def jacobi3Point1DStencil(): Unit =
  {

    val randomData = Array.tabulate(128)(_.toFloat)
    val comparisonData = StencilTutorialUtilities.scalaCompute1DStencilConstantBoundary(randomData,3,1,1,1,0.0f)

    val stencilLambda = fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        toGlobal(MapGlb(id)) o Join() o MapGlb(ReduceSeq(add, 0.0f)) o
          Slide(3, 1) o PadConstant(1, 1, 0.0f) $ input
      }
    )

    val (output, _) = Execute(randomData.length)[Array[Float]](stencilLambda, randomData)

    assertArrayEquals(comparisonData, output, StencilTutorialUtilities.delta)

  }


  /*
   * simple 1D weighted stencil with neighbourhood sizes of 3 and a "clamped" boundary (returning the same value as
   * the value at edge-1)
   */

  @Test def jacobi3Point1DStencilWithWeights(): Unit =
  {

    val randomData = Seq.fill(256)(Random.nextFloat()).toArray
    val SCALABOUNDARY = Utils.scalaClamp
    val BOUNDARY = Pad.Boundary.Clamp
    val weights = Array(1, 2, 1).map(_.toFloat)
    val comparisonData = StencilTutorialUtilities.scalaCompute1DStencil(randomData, 3, 1, 1, 1, weights, SCALABOUNDARY)

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
              Zip(weights, neighbourhood) // here weights are zipped together with the neighbourhood to multiply them by the correct value
          })
        ) o Slide(3, 1) o Pad(1, 1, BOUNDARY) $ input
      }
    )

    val (output, _) = Execute(randomData.length)[Array[Float]](stencil, randomData, weights)

    assertArrayEquals(comparisonData, output, 0.1f)

  }


  /*
   * simple 2D weighted stencil with neighbourhood sizes of 3x3 and a constant boundary value
   */

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
      ArrayTypeWSWC(Float, StencilTutorialUtilities.weights2D.length),
      (mat, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights) // here 2D weights passed in must be flattened first
          }))
        ) o Slide2D(StencilTutorialUtilities.slidesize, StencilTutorialUtilities.slidestep) o PadConstant2D(1,1,0.0f) $ mat
      })

    val (output, runtime) = Execute(stencilValues.length, stencilValues.length)[Array[Float]](stencilLambda, stencilValues, StencilTutorialUtilities.weights2D)

    assertArrayEquals(compareData, output, StencilTutorialUtilities.delta)

  }

  /*
   * simple 3D weighted stencil with neighbourhood sizes of 3x3x3 and a constant boundary value
   */

  @Test def jacobi7Point3DStencilWithWeights(): Unit =
  {

    // asymmetrical sized cuboid
    val nx = 8
    val ny = 4
    val nz = 2

    val stencilValues = Array.tabulate(nx,ny,nz) { (i,j,k) => (i + j + k + 1).toFloat }

    val compareData = Array(7.0f,9.0f,12.0f,15.0f,17.0f,20.0f,17.0f,19.0f,
                            12.0f,15.0f,19.0f,23.0f,25.0f,29.0f,25.0f,28.0f,
                            17.0f,20.0f,25.0f,29.0f,31.0f,35.0f,30.0f,33.0f,
                            22.0f,25.0f,31.0f,35.0f,37.0f,41.0f,35.0f,38.0f,
                            27.0f,30.0f,37.0f,41.0f,43.0f,47.0f,40.0f,43.0f,
                            32.0f,35.0f,43.0f,47.0f,49.0f,53.0f,45.0f,48.0f,
                            37.0f,40.0f,49.0f,53.0f,55.0f,59.0f,50.0f,53.0f,
                            33.0f,35.0f,45.0f,48.0f,50.0f,53.0f,43.0f,45.0f)

    val stencilLambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")), SizeVar("O")),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilTutorialUtilities.weights3D(0)(0).length), StencilTutorialUtilities.weights3D(0).length), StencilTutorialUtilities.weights3D.length),
      (mat, weights) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(neighbours => {
          toGlobal(MapSeq(id)) o
            ReduceSeqUnroll(\((acc, next) =>
              multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() o Join() $ neighbours, Join() o Join() $ weights)
        })))
        ) o Slide3D(StencilTutorialUtilities.slidesize, StencilTutorialUtilities.slidestep) o PadConstant3D(1,1,1,0.0f) $ mat
      })


    val (output, runtime) = Execute(2, 2, 2, nx, ny, nz, (true,true))[Array[Float]](stencilLambda, stencilValues, StencilTutorialUtilities.weights3D)

    assertArrayEquals(compareData, output, StencilTutorialUtilities.delta)

  }

}
