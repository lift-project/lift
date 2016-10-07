package opencl.generator

import apart.arithmetic.{SizeVar, StartFromRange, Var, Cst}
import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

import scala.util.Random

object TestHarrisCornerDetection {
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

/**
  * Tests computation stages of the harris corner detection application.
  * Inherits from TestSlide to avoid code duplication
  */
class TestHarrisCornerDetection extends TestSlide {


  val sobelX = Array(-1, 0, 1, 2, 0, 2, -1, 0, 1).map(_.toFloat)
  val sobelY = Array(-1, -2, -1, 0, 0, 0, 1, 2, 1).map(_.toFloat)

  /* **********************************************************
       STAGE 0 - compute X and Y derivatives of image
  ***********************************************************/
  @Test def computeDerivativeX(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, 1536), 2560),
      ArrayType(Float, 3 * 3),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeq(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(3, 1, 3, 1) $ matrix
      })

    val input = Array.tabulate(1536, 2560) { (i, j) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(16, 8, 1536, 2560, (true, true))(stencil, input, sobelX)
    println("Runtime: " + runtime)

    // todo implement
    //val gold = Utils.scalaCompute2DStencil(input, 17,1, 17,1, 8,8,8,8, weights, scalaClamp)
    //compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
       STAGE 1 - compute products of derivatives
  ***********************************************************/
  @Test def computeDerivativeXX(): Unit = {
    val mult = fun(
      ArrayType(ArrayType(Float, 1534), 2558),
      (derivativeX) => {
        MapGlb(1)(MapGlb(0)(square)) $ derivativeX
      }
    )

    val input = Array.tabulate(1534, 2558) { (i, j) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(16, 8, 1536, 2560, (true, true))(mult, input)
    println("Runtime: " + runtime)

    val gold = input.flatten.map((x => x * x))
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def computeDerivativeXY(): Unit = {
    val mult = fun(
      ArrayType(ArrayType(Float, 1534), 2558),
      ArrayType(ArrayType(Float, 1534), 2558),
      (derivativeX, derivativeY) => {
        MapGlb(1)(MapGlb(0)(multTuple)) o Split(1534) $ Zip(Join() $ derivativeX, Join() $ derivativeY)
      }
    )

    val input1 = Array.tabulate(1534, 2558) { (i, j) => Random.nextFloat() }
    val input2 = Array.tabulate(1534, 2558) { (i, j) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(16, 8, 1536, 2560, (true, true))(mult, input1, input2)
    println("Runtime: " + runtime)

    //todo scala check
  }

  /* **********************************************************
       STAGE 2 - compute the sums of the products of derivatives
  ***********************************************************/
  @Test def computeSumsOfProducts(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, 1534), 2558),
      (matrix) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o ReduceSeq(add, 0.0f) o Join() $ neighbours
          }))
        ) o Slide2D(3, 1, 3, 1) $ matrix
      })

    val input = Array.tabulate(1534, 2558) { (i, j) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(16, 8, 1536, 2560, (true, true))(stencil, input)
    println("Runtime: " + runtime)

    // todo implement
    //val gold = Utils.scalaCompute2DStencil(input, 17,1, 17,1, 8,8,8,8, weights, scalaClamp)
    //compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
       STAGE 3 - compute the determinant
  ***********************************************************/
  @Test def computedeterminant(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, 1532), 2556),
      ArrayType(ArrayType(Float, 1532), 2556),
      ArrayType(ArrayType(Float, 1532), 2556),
      (sXX,sXY,sYY) => {
          MapSeq(fun((pair) => {
            val a = Get(pair, 0)
            val b = Get(pair, 1)
            val c = Get(pair, 2)
            UserFun("det", Array("a","b","c"), "{ return a*c-b*b; }", Seq(Float,Float,Float), Float).apply(a,b,c)
          })) $ Zip(Join() $ sXX, Join() $ sXY, Join() $ sYY)
      })
    //val square = UserFun("square", "x", "{ return x*x; }", Float, Float)

    val input1 = Array.tabulate(1532, 2556) { (i, j) => Random.nextFloat() }
    val input2 = Array.tabulate(1532, 2556) { (i, j) => Random.nextFloat() }
    val input3 = Array.tabulate(1532, 2556) { (i, j) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(16, 8, 1536, 2560, (true, true))(stencil, input1, input2, input3)
    println("Runtime: " + runtime)

    // todo implement
    //val gold = Utils.scalaCompute2DStencil(input, 17,1, 17,1, 8,8,8,8, weights, scalaClamp)
    //compareGoldWithOutput(gold, output, runtime)
  }

}
