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
    val determinant = fun(
      ArrayType(ArrayType(Float, 1532), 2556),
      ArrayType(ArrayType(Float, 1532), 2556),
      ArrayType(ArrayType(Float, 1532), 2556),
      (sXX,sXY,sYY) => {
          MapGlb(1)(MapGlb(0)(fun((pair) => {
            val a = Get(pair, 0)
            val b = Get(pair, 1)
            val c = Get(pair, 2)
            UserFun("det", Array("a","b","c"), "{ return a*c-b*b; }", Seq(Float,Float,Float), Float).apply(a,b,c)
          }))) o Split(1532) $ Zip(Join() $ sXX, Join() $ sXY, Join() $ sYY)
      })
    //val square = UserFun("square", "x", "{ return x*x; }", Float, Float)

    val input1 = Array.tabulate(1532, 2556) { (i, j) => Random.nextFloat() }
    val input2 = Array.tabulate(1532, 2556) { (i, j) => Random.nextFloat() }
    val input3 = Array.tabulate(1532, 2556) { (i, j) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(16, 8, 1536, 2560, (true, true))(determinant, input1, input2, input3)
    println("Runtime: " + runtime)

    // todo implement
    //val gold = Utils.scalaCompute2DStencil(input, 17,1, 17,1, 8,8,8,8, weights, scalaClamp)
    //compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
       FUSED
  ***********************************************************/
  @Ignore //fix
  @Test def fusedDetection(): Unit = {
    val stencilSum = fun(neighbours => {toGlobal(MapSeqUnroll(id)) o ReduceSeq(add, 0.0f) o Join() $ neighbours})
    val ninePointSum = MapSeq(Join() o MapSeq(stencilSum)) o Slide2D(3,1)

    val determinant = UserFun("det", Array("a","b","c"), "{ return a*c-b*b; }", Seq(Float,Float,Float), Float)
    val threeTupleToDeterminant = fun((tuple) => {
            val a = Get(tuple, 0)
            val b = Get(tuple, 1)
            val c = Get(tuple, 2)
            determinant.apply(a,b,c)
          })

    val cornerDetection = fun(
      ArrayType(ArrayType(Float, 1534), 2558),
      ArrayType(ArrayType(Float, 1534), 2558),
      ArrayType(ArrayType(Float, 1534), 2558),
      (iXX,iXY,iYY) => {
          MapSeq(threeTupleToDeterminant) $ Zip(
              Join() o ninePointSum $ iXX,
              Join() o ninePointSum $ iXY,
              Join() o ninePointSum $ iYY)
      })

    val input1 = Array.tabulate(1534, 2558) { (i, j) => Random.nextFloat() }
    val input2 = Array.tabulate(1534, 2558) { (i, j) => Random.nextFloat() }
    val input3 = Array.tabulate(1534, 2558) { (i, j) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(16, 8, 1536, 2560, (true, true))(cornerDetection, input1, input2, input3)
    println("Runtime: " + runtime)

    // todo implement
    //val gold = Utils.scalaCompute2DStencil(input, 17,1, 17,1, 8,8,8,8, weights, scalaClamp)
    //compareGoldWithOutput(gold, output, runtime)
  }

    /* **********************************************************
       FUSED - simple pipeline
  ***********************************************************/
  @Ignore //fix
  @Test def simplePipeline(): Unit = {
    val sum = fun(neighbours => {toGlobal(MapSeqUnroll(id)) o ReduceSeq(add, 0.0f) o Join() $ neighbours})
    val stencil = MapLcl(1)(Join() o MapLcl(0)(sum)) o Slide2D(3,1)

    val determinant = UserFun("det", Array("a","b","c"), "{ return a*c-b*b; }", Seq(Float,Float,Float), Float)
    val threeTupleToDeterminant = fun((tuple) => {
            val a = Get(tuple, 0)
            val b = Get(tuple, 1)
            val c = Get(tuple, 2)
            determinant.apply(a,b,c)
          })

    val fuseTupleTile = MapLcl(1)(MapLcl(0)(threeTupleToDeterminant))

    val handleInputTile = fun(inputTile => fuseTupleTile o Split(18) $ Zip(
      Join() o stencil $ inputTile,
      Join() o stencil $ inputTile,
      Join() o stencil $ inputTile
    ))

    val cornerDetection = fun(
      ArrayType(ArrayType(Float, 1536), 2560),
      (input) => {MapWrg(1)(MapWrg(0)(handleInputTile)) o Slide2D(20,16) $ input
      })

    val input = Array.tabulate(1536, 2560) { (i, j) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(16, 16, 1536, 2560, (true, true))(cornerDetection, input)
    println("Runtime: " + runtime)

    // todo implement
    //val gold = Utils.scalaCompute2DStencil(input, 17,1, 17,1, 8,8,8,8, weights, scalaClamp)
    //compareGoldWithOutput(gold, output, runtime)
  }
    /* **********************************************************
       HALIDE AUTO KERNEL
  ***********************************************************/
  @Ignore //fix
  @Test def halideHarrisSchedule(): Unit = {
    val sum = fun(neighbours => {toGlobal(MapSeqUnroll(id)) o ReduceSeq(add, 0.0f) o Join() $ neighbours})
    // include sobel weights
    val sobelX = MapLcl(1)(Join() o MapLcl(0)(sum)) o Slide2D(3,1)
    val sobelY = MapLcl(1)(Join() o MapLcl(0)(sum)) o Slide2D(3,1)

    val determinant = UserFun("det", Array("a","b","c"), "{ return a*c-b*b; }", Seq(Float,Float,Float), Float)
    val threeTupleToDeterminant = fun((tuple) => {
            val a = Get(tuple, 0)
            val b = Get(tuple, 1)
            val c = Get(tuple, 2)
            determinant.apply(a,b,c)
          })

    val computeDeterminants = MapLcl(1)(MapLcl(0)(threeTupleToDeterminant))
    val computeTrace  = MapLcl(1)(MapLcl(0)(addTuple))

    val har = UserFun("har", Array("a","b"), "{ return a - 0.04f * b * b; }", Seq(Float,Float), Float)
    val harris = fun((tuple) => {
            val a = Get(tuple, 0)
            val b = Get(tuple, 1)
            har.apply(a,b)})

    val detectCorners = fun(derivatives =>
      MapLcl(1)(MapLcl(0)(
        toGlobal(MapSeq(id)) o
          harris)) o
        Split(18) $
      Zip(
        Join() o computeDeterminants $ derivatives,
        Join() o computeTrace $ derivatives))

    val computeIxx = fun(inputTile => MapLcl(1)(MapLcl(0)(square)) o sobelX $ inputTile)
    val computeIyy = fun(inputTile => MapLcl(1)(MapLcl(0)(square)) o sobelY $ inputTile)
    val computeIxy = fun(inputTile => MapLcl(1)(MapLcl(0)(multTuple)) o
      Split(18) $ Zip(
        Join() o sobelX $ inputTile,
        Join() o sobelY $ inputTile))

    val handleInputTile = fun(inputTile =>
      detectCorners o Split(18) $ Zip(
        Join() o computeIxx $ inputTile,
        Join() o computeIxy $ inputTile,
        Join() o computeIyy $ inputTile
    ))

    val cornerDetection = fun(
      ArrayType(ArrayType(Float, 1536), 2560),
      (input) => {MapWrg(1)(MapWrg(0)(
          handleInputTile) o toLocal(MapLcl(1)(MapLcl(0)(id)))) o
        Slide2D(20,16) $ input
      })

    val input = Array.tabulate(1536, 2560) { (i, j) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(16, 16, 1536, 2560, (true, true))(cornerDetection, input)
    println("Runtime: " + runtime)

    // todo implement
    //val gold = Utils.scalaCompute2DStencil(input, 17,1, 17,1, 8,8,8,8, weights, scalaClamp)
    //compareGoldWithOutput(gold, output, runtime)
  }

}
