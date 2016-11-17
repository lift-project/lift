package opencl.generator.stencil

import apart.arithmetic.{Cst, SizeVar, StartFromRange, Var}
import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

import scala.util.Random

object TestStencil {
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
  * Contains tests which include the combined usage of the slide
  * and pad pattern.
  * Inherits from TestSlide to avoid code duplication
  */
class TestStencil {

  /* **********************************************************
      STENCIL TEST SETTINGS
   ***********************************************************/
  val UNROLL = true
  val randomData = Seq.fill(1024)(Random.nextFloat()).toArray
  val randomData2D = Array.tabulate(1024, 1024) { (i, j) => Random.nextFloat() }
  // currently used for 2D stencils / refactor to run with every boundary condition
  val BOUNDARY = Pad.Boundary.Wrap
  val SCALABOUNDARY: (Int, Int) => Int = Utils.scalaWrap

  val data = Array.tabulate(5)(_ * 1.0f)
  val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }
  def createFused1DStencilLambda(weights: Array[Float],
                                 size: Int, step: Int,
                                 left: Int, right: Int): Lambda2 = {
    fun(
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
        ) o Slide(size, step) o Pad(left, right, BOUNDARY) $ input
      }
    )
  }

  /* **********************************************************
      SLIDE o PAD
   ***********************************************************/
  def testCombinationPadGroup(boundary: BoundaryFun,
                              gold: Array[Float],
                              size: Int, step: Int,
                              left: Int, right: Int): Unit = {
    val f = createPadGroupLambda(boundary, size, step, left, right)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(f, data)
    assertArrayEquals(gold, output, 0.2f)
  }

  def createPadGroupLambda(boundary: BoundaryFun,
                           size: Int, step: Int,
                           left: Int, right: Int): Lambda1 = {
    fun(
      ArrayType(Float, Var("N", StartFromRange(2))),
      (input) =>
        MapGlb(MapSeqUnroll(id)) o
          Slide(size, step) o Pad(left, right, boundary) $ input
    )
  }

  def createThesisChapter4Example(boundary: BoundaryFun,
                                  size: Int, step: Int,
                                  left: Int, right: Int): Lambda1 = {
    fun(
      ArrayType(Float, Var("N", StartFromRange(2))),
      (input) =>
        toGlobal(MapGlb(id)) o Join() o MapGlb(ReduceSeq(add, 0.0f)) o
          Slide(size, step) o Pad(left, right, boundary) $ input
    )
  }

  @Test def chapterFourExample(): Unit = {
    val data = Array(0, 1, 2, 3).map(_.toFloat)
    val gold = Array(1, 3, 6, 8).map(_.toFloat)
    val f = createThesisChapter4Example(Pad.Boundary.Clamp, 3, 1, 1, 1)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(f, data)
    println(output.mkString(","))
    assertArrayEquals(gold, output, 0.2f)
  }

  @Test def group3ElementsPadWrap(): Unit = {
    val boundary = Pad.Boundary.Wrap
    val gold = Array(4, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 0).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3, 1, 1, 1)
  }

  @Test def group3ElementsPadClamp(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3, 1, 1, 1)
  }

  @Test def group3ElementsPadMirror(): Unit = {
    val boundary = Pad.Boundary.Mirror
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3, 1, 1, 1)
  }

  @Test def group3ElementsPadMirrorUnsafe(): Unit = {
    val boundary = Pad.Boundary.MirrorUnsafe
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3, 1, 1, 1)
  }

  @Test def group5ElementsPadClamp(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(
      0, 0, 0, 1, 2,
      0, 0, 1, 2, 3,
      0, 1, 2, 3, 4,
      1, 2, 3, 4, 4,
      2, 3, 4, 4, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 5, 1, 2, 2)
  }

  /* **********************************************************
      1D STENCILS
   ***********************************************************/
  def create1DStencilLambda(weights: Array[Float], size: Int, step: Int, left: Int, right: Int): Lambda2 = {
    fun(
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
        ) o Slide(size, step) o Pad(left, right, BOUNDARY) $ input
      }
    )
  }

  def create1DStencilFusedMapReduceLambda(inputLength: Int,
                                          weights: Array[Float],
                                          size: Int, step: Int,
                                          left: Int, right: Int): Lambda2 = {
    fun(
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
        ) o Slide(size, step) o Pad(left, right, BOUNDARY) $ input
      }
    )
  }

  @Test def simple3Point1DStencil(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)

    val gold = Utils.scalaCompute1DStencil(randomData, 3, 1, 1, 1, weights, SCALABOUNDARY)
    val stencil = create1DStencilFusedMapReduceLambda(randomData.length, weights, 3, 1, 1, 1)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    assertArrayEquals(gold, output, 0.2f)
  }

  @Test def simple5Point1DStencil(): Unit = {
    val weights = Array(1, 2, 3, 2, 1).map(_.toFloat)

    val gold = Utils.scalaCompute1DStencil(randomData, 5, 1, 2, 2, weights, SCALABOUNDARY)
    val stencil = create1DStencilLambda(weights, 5, 1, 2, 2)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    assertArrayEquals(gold, output, 0.2f)
  }

  @Test def createGroupsForOneSidedPadding(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0, 0, 0, 0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3, 1, 2, 0)
  }

  /* **********************************************************
       STENCILS WITH MULTIPLE INPUT ARRAYS
   ***********************************************************/
  def create1DMultiInputLambda(inputLength: Int,
                               weights: Array[Float],
                               size: Int, step:
                               Int, left: Int, right: Int): Lambda2 = {
    fun(
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

          ) $ Zip(Slide(size, step) o Pad(left, right, BOUNDARY) $ input,
          input)
      }
    )
  }

  @Test def multiInput1D(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)

    val gold = Utils.scalaCompute1DStencil(randomData, 3, 1, 1, 1, weights, SCALABOUNDARY)
    val stencil = create1DMultiInputLambda(randomData.length, weights, 3, 1, 1, 1)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
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

  @Test def tiling1D(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)

    val stencil = createTiled1DStencilLambda(weights, 3, 1, 4, 2, 1, 1)
    val newLambda = create1DStencilLambda(weights, 3, 1, 1, 1)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    val (gold: Array[Float], _) = Execute(randomData.length)(newLambda, randomData, weights)
    assertArrayEquals(gold, output, 0.1f)
  }

  @Test def tiling1DBiggerTiles(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)

    val stencil = createTiled1DStencilLambda(weights, 3, 1, 18, 16, 1, 1)
    val newLambda = create1DStencilLambda(weights, 3, 1, 1, 1)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    val (gold: Array[Float], _) = Execute(randomData.length)(newLambda, randomData, weights)
    assertArrayEquals(gold, output, 0.1f)
  }


  /* **********************************************************
      MISC
  ***********************************************************/
  @Test def reorderStride(): Unit = {
    val lambda = fun(
      ArrayType(Float, 8),
      (input) =>
        MapSeq(id) o ReorderStride(4) $ input
    )
    val data = Array(0, 1, 2, 3, 4, 5, 6, 7).map(_.toFloat)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(lambda, data)
    val gold = Array(0, 4, 1, 5, 2, 6, 3, 7).map(_.toFloat)
    println(output.mkString(", "))
    assertArrayEquals(gold, output, 0.1f)
  }

  @Test def outputViewGroupTest(): Unit = {
    val lambda = fun(
      ArrayType(Float, SizeVar("N")),
      (input) =>
        MapSeq(MapSeq(id)) o Slide(3, 1) o MapSeq(id) $ input
    )
    Compile(lambda)
  }

  def create2DModSimplifyLambda(boundary: BoundaryFun,
                                size: Int, step: Int,
                                left: Int, right: Int): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(2))), Var("M", StartFromRange(2))),
      (domain) => {
        MapSeq(
          MapSeq(
            fun(neighbours =>
              MapSeqUnroll(MapSeqUnroll(id)) $ neighbours)
          )
        ) o Map(Map(Transpose()) o Slide(size, step) o Transpose()) o Slide(size, step) o
          Transpose() o Pad(left, right, boundary) o Transpose() o Pad(left, right, boundary) $ domain
      }
    )
  }

  @Test def modSimplifyTest(): Unit = {
    val size = 3
    val step = 1
    val left = 1
    val right = 1

    val lambda = create2DModSimplifyLambda(Pad.Boundary.Wrap, size, step, left, right)
    val (output: Array[Float], runtime) = Execute(data2D.length, data2D.length)(lambda, data2D)
    val gold = Array(15.0, 12.0, 13.0,
      3.0, 0.0, 1.0,
      7.0, 4.0, 5.0,
      12.0, 13.0, 14.0,
      0.0, 1.0, 2.0,
      4.0, 5.0, 6.0,
      13.0, 14.0, 15.0,
      1.0, 2.0, 3.0,
      5.0, 6.0, 7.0,
      14.0, 15.0, 12.0,
      2.0, 3.0, 0.0,
      6.0, 7.0, 4.0,
      3.0, 0.0, 1.0,
      7.0, 4.0, 5.0,
      11.0, 8.0, 9.0,
      0.0, 1.0, 2.0,
      4.0, 5.0, 6.0,
      8.0, 9.0, 10.0,
      1.0, 2.0, 3.0,
      5.0, 6.0, 7.0,
      9.0, 10.0, 11.0,
      2.0, 3.0, 0.0,
      6.0, 7.0, 4.0,
      10.0, 11.0, 8.0,
      7.0, 4.0, 5.0,
      11.0, 8.0, 9.0,
      15.0, 12.0, 13.0,
      4.0, 5.0, 6.0,
      8.0, 9.0, 10.0,
      12.0, 13.0, 14.0,
      5.0, 6.0, 7.0,
      9.0, 10.0, 11.0,
      13.0, 14.0, 15.0,
      6.0, 7.0, 4.0,
      10.0, 11.0, 8.0,
      14.0, 15.0, 12.0,
      11.0, 8.0, 9.0,
      15.0, 12.0, 13.0,
      3.0, 0.0, 1.0,
      8.0, 9.0, 10.0,
      12.0, 13.0, 14.0,
      0.0, 1.0, 2.0,
      9.0, 10.0, 11.0,
      13.0, 14.0, 15.0,
      1.0, 2.0, 3.0,
      10.0, 11.0, 8.0,
      14.0, 15.0, 12.0,
      2.0, 3.0, 0.0).map(_.toFloat)
    //output.grouped(3).toArray.map(x => println(x.mkString(",")))
    assertArrayEquals(gold, output, 0.1f)
  }


  /* **********************************************************
      SHOC STENCIL 2D
  ***********************************************************/
  /*
  on fuji
  $ cd /home/v1bhaged/shoc/src/opencl/level1/stencil2d
  $ ./Stencil2D --customSize 8,8 --weight-center 0.25 --weight-cardinal 0.15 --weight-diagonal 0.05 --verbose --num-iters 1

  compare to 10x10 array. SHOC does not handle boundary but provides a padded input array
   */
  @Test def shocStencil2D(): Unit = {
    val stencil = fun(
      //ArrayType(ArrayType(Float, Var("N", StartFromRange(2))), Var("M", StartFromRange(2))),
      ArrayType(ArrayType(Float, 8194), 8194),
      ArrayType(Float, 9),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun(tile =>

          MapLcl(1)(MapLcl(0)(
            // stencil computation
            fun(elem => {
              toGlobal(MapSeqUnroll(id)) o
                ReduceSeqUnroll(fun((acc, pair) => {
                  val pixel = Get(pair, 0)
                  val weight = Get(pair, 1)
                  multAndSumUp.apply(acc, pixel, weight)
                }), 0.0f) $ Zip(Join() $ elem, weights)
            })
            // create neighbourhoods in tiles
          )) o Slide2D(3, 1, 3, 1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(10, 8, 258, 256) $ matrix
      }
    )
    val weights = Array(0.05, 0.15, 0.05,
      0.15, 0.25, 0.15,
      0.05, 0.15, 0.05).map(_.toFloat)

    // testing - change tilesize!
    //val inputSize = 10
    //val haloSize = 1
    //val outputSize = inputSize - 2 * haloSize
    // testing - change tilesize!
    val inputSize = 8194
    val haloSize = 1
    val outputSize = inputSize - 2 * haloSize
    // 4k
    //val inputSize = 4096
    //val haloSize = 1
    //val outputSize = inputSize - 2 * haloSize

    // create already padded input array with inner elements (i,j) = i * j
    var input = Array.tabulate(inputSize, inputSize) { (i, j) => (i - haloSize) * (j - haloSize) * 1.0f }
    input(0) = input(0).map((_ * 0.0f))
    input(inputSize - 1) = input(inputSize - 1).map(_ * 0.0f)
    input = input.transpose
    input(0) = input(0).map(_ * 0.0f)
    input(inputSize - 1) = input(inputSize - 1).map(_ * 0.0f)
    input = input.transpose

    val (output: Array[Float], runtime) = Execute(1, 256, 1024, 8192, (false, false))(stencil, input, weights)
    println("Runtime: " + runtime)
  }

  /* **********************************************************
       ARI PJS TEST
   ***********************************************************/
  @Test def ariStencil2D(): Unit = {
    val stencil = fun(
      //ArrayType(ArrayType(Float, Var("N", StartFromRange(2))), Var("M", StartFromRange(2))),
      ArrayType(ArrayType(Float, 8192), 8192),
      ArrayType(Float, 15),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(
          fun(wgBlock =>
            MapSeq(MapSeq(
              fun(cacheBlock =>
                MapLcl(1)(MapLcl(0)(
                  fun(elem => {
                    toGlobal(MapSeqUnroll(id)) o
                      ReduceSeqUnroll(fun((acc, pair) => {
                        val pixel = Get(pair, 0)
                        val weight = Get(pair, 1)
                        multAndSumUp.apply(acc, pixel, weight)
                      }), 0.0f) $ Zip(Join() $ elem, weights)
                  })
                )) o Slide2D(15, 1, 1, 1) $ cacheBlock
              ))) o Slide2D(78, 64, 8, 8) $ wgBlock
          ))) o Slide2D(526, 512, 64, 64) o Pad2D(7, 7, Pad.Boundary.Clamp) $ matrix
      }
    )

    val weights = Array.fill[Float](15)(1.0f)
    val haloSize = 7
    val outputSize = 8192
    val inputSize = 8192

    // create already padded input array with inner elements (i,j) = i * j
    var input = Array.tabulate(inputSize, inputSize) { (i, j) => i + j * 1.0f }
    /*
     input(0) = input(0).map((_*0.0f))
     input(inputSize -1) = input(inputSize -1).map(_*0.0f)
     input = input.transpose
     input(0) = input(0).map(_*0.0f)
     input(inputSize -1) = input(inputSize -1).map(_*0.0f)
     input = input.transpose
     */

    val (output: Array[Float], runtime) = Execute(64, 4, 1024, 512, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)
  }


  /* **********************************************************
      PARBOIL
  ***********************************************************/
  @Test def parboil(): Unit = {
    //val hotspot = UserFun("hotspot", "tuple", "{ return tuple_0; }", TupleType(Float, ArrayType(ArrayType(Float, 3),3)), Float)
    // segfaults
    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 64),
      ArrayType(Float, 27),
      (input, weights) => {
        MapSeq(MapWrg(1)(MapWrg(0)( \(block =>
          MapSeq(MapLcl(1)(MapLcl(0)( \(nbh =>
            toGlobal(MapSeq(id)) o
            ReduceSeqUnroll(add, 0.0f) o Join() o Join() $ nbh )))) o
            Slide3D(3,1) $ block )))) o
        Slide3D(34,32, 6,4, 3,1) o Pad3D(1, 1, 1, Pad.Boundary.Wrap) $ input
      }
    )

    // testing
    val input = Array.tabulate(512, 512, 64) { (i, j, k) => Random.nextFloat() }
    val weights = Array.tabulate(27) { (i) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(32, 4, 1, 256, 512, 1, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)
    //println(output.mkString(","))
  }
}
