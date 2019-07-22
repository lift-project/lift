package opencl.generator.stencil

import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast._
import ir.ast.debug.PrintType
import lift.arithmetic.{SizeVar, StartFromRange, Var}
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._

import scala.util.Random

object TestStencil extends TestWithExecutor

/**
  * Contains tests which include the combined usage of the slide
  * and pad pattern to express stencil computations.
  */
class TestStencil {

  /* **********************************************************
      UTILS
   ***********************************************************/
  // boundary condition implemented in scala to create gold versions
  val SCALABOUNDARY = Utils.scalaClamp
  val BOUNDARY = Pad.Boundary.Clamp
  val data = Array.tabulate(5)(_ * 1.0f)
  val randomData = Seq.fill(1024)(Random.nextFloat()).toArray

  /* **********************************************************
      VECTORIZATION
   ***********************************************************/
  @Test
  def stencilVectorization(): Unit = {
    val N = 18
    val gold = \(ArrayType(Float, N),
      input => Join() o MapGlb(MapSeq(toGlobal(id)) o ReduceSeq(add,0.0f)) o Slide(3,1) $ input
    )

    val vectorized = \(ArrayType(Float, N),
      input => PrintType() o Join() o Map(asScalar() o Reduce(addF4, Value("0.0f", Float).vectorize(4))) o
        Map(Join()) o Map(Map(asVector(4))) o Map(Slide(4,1)) o Slide(6,4) $ input
    )

    val vectorizedLowered = \(ArrayType(Float, N),
      input => Join() o MapGlb(asScalar() o MapSeq(toGlobal(idF4)) o ReduceSeq(addF4, Value("0.0f", Float).vectorize(4))) o
        Map(Join()) o Map(Map(asVector(4))) o Map(Slide(4,1)) o Slide(6,4) $ input
    )

    val input = Array.tabulate(N) { _ => Random.nextInt() % 20 + 20 * 1.0f }
    val (outGold, _) = Execute(1,4,(false,false))[Array[Float]](gold, input)
    val (outVect, _) = Execute(1,4,(false,false))[Array[Float]](vectorizedLowered, input)
    assertArrayEquals(outGold, outVect, 0.0f)
  }

  /* **********************************************************
      SLIDE o PAD
   ***********************************************************/
  def testCombinationPadSlide(boundary: BoundaryFun,
                              gold: Array[Float],
                              size: Int, step: Int,
                              left: Int, right: Int): Unit = {
    val f = fun(
      ArrayType(Float, Var("N", StartFromRange(2))),
      (input) => MapGlb(MapSeqUnroll(id)) o Slide(size, step) o Pad(left, right, boundary) $ input
    )

    val (output, _) = Execute(data.length, data.length)[Array[Float]](f, data)
    assertArrayEquals(gold, output, 0.1f)
  }

  @Test def create3PointNbhsUsingWrap(): Unit = {
    val boundary = Pad.Boundary.Wrap
    val gold = Array(4, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 0).map(_.toFloat)

    testCombinationPadSlide(boundary, gold, 3, 1, 1, 1)
  }

  @Test def create3PointNbhsUsingClamp(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadSlide(boundary, gold, 3, 1, 1, 1)
  }

  @Test def create3PointNeighborhoodsUsingMirror(): Unit = {
    val boundary = Pad.Boundary.Mirror
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadSlide(boundary, gold, 3, 1, 1, 1)
  }

  @Test def create3PointNeighborhoodsUsingMirrorUnsafe(): Unit = {
    val boundary = Pad.Boundary.MirrorUnsafe
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadSlide(boundary, gold, 3, 1, 1, 1)
  }

  // stencil shape: (-2, -1, 0)
  @Test def createNeighborhoodsWithTwoLeftNeighbors(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0, 0, 0, 0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4).map(_.toFloat)

    testCombinationPadSlide(boundary, gold, 3, 1, 2, 0)
  }

  @Test def create5PointNeighborhoodsUsingClamp(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(
      0, 0, 0, 1, 2,
      0, 0, 1, 2, 3,
      0, 1, 2, 3, 4,
      1, 2, 3, 4, 4,
      2, 3, 4, 4, 4).map(_.toFloat)

    testCombinationPadSlide(boundary, gold, 5, 1, 2, 2)
  }

  /* **********************************************************
      1D STENCILS
   ***********************************************************/
  @Test def simple3Point1DStencil(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)
    val gold = Utils.scalaCompute1DStencil(randomData, 3, 1, 1, 1, weights, SCALABOUNDARY)

    val stencil = fun(
      //ArrayType(Float, inputLength), // more precise information
      ArrayType(Float, Var("N", StartFromRange(2))),
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

  @Test def simple5Point1DStencil(): Unit = {
    val weights = Array(1, 2, 3, 2, 1).map(_.toFloat)
    val gold = Utils.scalaCompute1DStencil(randomData, 5, 1, 2, 2, weights, SCALABOUNDARY)

    val stencil = fun(
      ArrayType(Float, Var("N", StartFromRange(3))),
      ArrayType(Float, weights.length),
      (input, weights) => {
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(add, 0.0f) o
              MapSeqUnroll(mult) $
              Zip(weights, neighbourhood)
          })
        ) o Slide(5, 1) o Pad(2, 2, BOUNDARY) $ input
      }
    )
    val (output, _) = Execute(randomData.length)[Array[Float]](stencil, randomData, weights)
    assertArrayEquals(gold, output, 0.1f)
  }

  @Test def flattenArrayAfterApplyingStencil(): Unit = {
    val data = Array(0, 1, 2, 3).map(_.toFloat)
    val gold = Array(1, 3, 6, 8).map(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N", StartFromRange(2))),
      (input) =>
        toGlobal(MapGlb(id)) o Join() o MapGlb(ReduceSeq(add, 0.0f)) o
          Slide(3, 1) o Pad(1, 1, Pad.Boundary.Clamp) $ input
    )
    val (output, _) = Execute(data.length, data.length)[Array[Float]](f, data)
    assertArrayEquals(gold, output, 0.1f)
  }

  /* **********************************************************
       STENCILS WITH MULTIPLE INPUT ARRAYS
   ***********************************************************/
  @Test def multiInput1D(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)

    val gold = Utils.scalaCompute1DStencil(randomData, 3, 1, 1, 1, weights, SCALABOUNDARY)
    val stencil = fun(
      ArrayType(Float, SizeVar("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {

        Join() o
          MapGlb(
            fun(tuple => {
              toGlobal(MapSeqUnroll(id)) o
                ReduceSeqUnroll(
                  fun((acc, y) => {
                    multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
                  }), 0.0f) $
                Zip(weights, Get(tuple, 0))
            })

          ) $ Zip(
            Slide(3, 1) o Pad(1, 1, BOUNDARY) $ input,
            input)
      }
    )
    val (output, runtime) = Execute(randomData.length)[Array[Float]](stencil, randomData, weights)
    assertArrayEquals(gold, output, 0.1f)
  }

  /* **********************************************************
     TILING 1D
 ***********************************************************/
  def createTiled1DStencilLambda(weights: Array[Float],
                                 size: Int, step: Int,
                                 tileSize: Int, tileStep: Int,
                                 left: Int, right: Int): Lambda2 = {
    fun(
      ArrayType(Float, SizeVar("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        MapWrg(fun(tile =>
          MapLcl(
            fun(neighbourhood => {
              toGlobal(MapSeqUnroll(id)) o
                ReduceSeqUnroll(add, 0.0f) o
                MapSeqUnroll(mult) $
                Zip(weights, neighbourhood)
            })
          ) o Slide(size, step) o MapLcl(toLocal(id)) $ tile

        )) o Slide(tileSize, tileStep) o Pad(left, right, BOUNDARY) $ input
      }
    )
  }

  def create1DStencilLambda(weights: Array[Float],
                            size: Int, step: Int,
                            left: Int, right: Int) = {
    fun(
      ArrayType(Float, Var("N", StartFromRange(2))),
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
  }

  @Test def tiling1D(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)
    val stencil = createTiled1DStencilLambda(weights, 3, 1, 4, 2, 1, 1)
    val goldLambda = create1DStencilLambda(weights, 3, 1, 1, 1)

    val (output, runtime) = Execute(randomData.length)[Array[Float]](stencil, randomData, weights)
    val (gold, _) = Execute(randomData.length)[Array[Float]](goldLambda, randomData, weights)
    assertArrayEquals(gold, output, 0.1f)
  }

  @Test def tiling1DBiggerTiles(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)
    val stencil = createTiled1DStencilLambda(weights, 3, 1, 18, 16, 1, 1)
    val newLambda = create1DStencilLambda(weights, 3, 1, 1, 1)

    val (output, runtime) = Execute(randomData.length)[Array[Float]](stencil, randomData, weights)
    val (gold, _) = Execute(randomData.length)[Array[Float]](newLambda, randomData, weights)
    assertArrayEquals(gold, output, 0.1f)
  }

  /* **********************************************************
      MISC
  ***********************************************************/
  @Test def reorderStride(): Unit = {
    val lambda = fun(
      ArrayType(Float, 8),
      (input) => MapSeq(id) o ReorderStride(4) $ input
    )
    val data = Array(0, 1, 2, 3, 4, 5, 6, 7).map(_.toFloat)
    val (output, _) = Execute(data.length, data.length)[Array[Float]](lambda, data)
    val gold = Array(0, 4, 1, 5, 2, 6, 3, 7).map(_.toFloat)
    assertArrayEquals(gold, output, 0.1f)
  }

  @Test def outputViewSlideTest(): Unit = {
    val lambda = fun(
      ArrayType(Float, SizeVar("N")),
      (input) =>
        MapSeq(MapSeq(id)) o Slide(3, 1) o MapSeq(id) $ input
    )
    Compile(lambda)
  }
}
