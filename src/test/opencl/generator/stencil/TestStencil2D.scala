package opencl.generator.stencil

import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast._
import lift.arithmetic.{SizeVar, StartFromRange, Var}
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._

import scala.util.Random

object TestStencil2D {
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

class TestStencil2D {
    /* **********************************************************
      UTILS
   ***********************************************************/
  // boundary condition implemented in scala to create gold versions
  val SCALABOUNDARY = Utils.scalaClamp
  val BOUNDARY = Pad.Boundary.Clamp

  val randomData2D = Array.tabulate(32, 32) { (i, j) => Random.nextFloat() }
  val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }

  val gaussWeights = Array(
      0.08f, 0.12f, 0.08f,
      0.12f, 0.20f, 0.12f,
      0.08f, 0.12f, 0.08f)

  /* **********************************************************
       SLIDE 2D o PAD 2D
   ***********************************************************/
  def runCombinedPadGroupTest(size: Int, step: Int,
                              left: Int, right: Int,
                              boundary: BoundaryFun,
                              scalaBoundary: (Int, Int) => Int,
                              data: Array[Array[Float]] = data2D): Unit = {
    val gold = Utils.scalaGenerate2DNeighbours(data, size, step, size, step, left, right, left, right, scalaBoundary)
    val goldFlat = gold.flatten.flatten.flatten

    val lambda = fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqUnroll(MapSeqUnroll(id)) $ neighbours
          ))
        ) o Slide2D(size, step) o Pad2D(left, right, boundary) $ domain
      }
    )
    val (output: Array[Float], _) = Execute(data.length, data.length)(lambda, data)

    assertArrayEquals(goldFlat, output, 0.1f)
  }

  @Test def groupClampPaddedData2D(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = Utils.scalaClamp

    runCombinedPadGroupTest(3, 1, 1, 1, boundary, scalaBoundary)
  }

  @Test def groupBigClampPaddedData2D(): Unit = {
    LongTestsEnabled()
    val data2D = Array.tabulate(10, 10) { (i, j) => i * 10.0f + j }
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = Utils.scalaClamp

    runCombinedPadGroupTest(5, 1, 2, 2, boundary, scalaBoundary, data2D)
  }

  @Test def groupMirrorPaddedData2D(): Unit = {
    val boundary = Pad.Boundary.Mirror
    val scalaBoundary = Utils.scalaMirror

    runCombinedPadGroupTest(3, 1, 1, 1, boundary, scalaBoundary)
  }

  @Test def groupWrapPaddedData2D(): Unit = {
    val boundary = Pad.Boundary.Wrap
    val scalaBoundary = Utils.scalaWrap

    runCombinedPadGroupTest(3, 1, 1, 1, boundary, scalaBoundary)
  }

  /* **********************************************************
       2D STENCILS
   ***********************************************************/
  def createSimple2DStencil(size: Int, step: Int,
                            left: Int, right: Int,
                            weights: Array[Float],
                            boundary: BoundaryFun,
                            fromRange: Int): Lambda2 = {
    createSimple2DStencil(size, step, size, step, left, right, left, right, weights, boundary, fromRange)
  }

  def createSimple2DStencil(size1: Int, step1: Int,
                            size2: Int, step2: Int,
                            top: Int, bottom: Int,
                            left: Int, right: Int,
                            weights: Array[Float],
                            boundary: BoundaryFun,
                            fromRange: Int): Lambda2 = {
    fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(fromRange))), Var("M", StartFromRange(fromRange))),
      ArrayType(Float, weights.length),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(size1, step1, size2, step2) o Pad2D(top, bottom, left, right, boundary) $ matrix
      })
  }

  def run2DStencil(stencil: Lambda2,
                   size1: Int, step1: Int,
                   size2: Int, step2: Int,
                   top: Int, bottom: Int,
                   left: Int, right: Int,
                   weights: Array[Float],
                   scalaBoundary: (Int, Int) => Int): Unit = {
    try {
      // be carefull when choosing small input size because of 'StartsFromRange(100)'
      val width = randomData2D(0).length
      val height = randomData2D.length
      val input = randomData2D

      val (output: Array[Float], _) = Execute(1, 1, width, height, (false, false))(stencil, input, weights)

      val gold = Utils.scalaCompute2DStencil(input, size1, step1, size2, step2, top, bottom, left, right, weights, scalaBoundary)
      assertArrayEquals(gold, output, 0.1f)

    } catch {
      case x: Exception => x.printStackTrace()
    }
  }

  def run2DStencil(stencil: Lambda2,
                   size: Int, step: Int,
                   left: Int, right: Int,
                   weights: Array[Float],
                   scalaBoundary: (Int, Int) => Int): Unit = {
    run2DStencil(stencil, size, step, size, step, left, right, left, right, weights, scalaBoundary)
  }

  @Test def gaussianBlur(): Unit = {
    LongTestsEnabled()
    val stencil = createSimple2DStencil(3, 1, 1, 1, gaussWeights, BOUNDARY, 2)
    run2DStencil(stencil, 3, 1, 1, 1, gaussWeights, SCALABOUNDARY)
  }

  @Test def gaussianBlur25PointStencil(): Unit = {
    val weights = Array(
      1, 4, 7, 4, 1,
      4, 16, 26, 16, 4,
      7, 26, 41, 26, 7,
      4, 16, 26, 16, 4,
      1, 4, 7, 4, 1).map(_ * 0.004219409282700422f)
    val stencil = createSimple2DStencil(5, 1, 2, 2, weights, BOUNDARY, 3)
    run2DStencil(stencil, 5, 1, 2, 2, weights, SCALABOUNDARY)
  }

  @Test def blurX3Point(): Unit = {
    LongTestsEnabled()
    val weights = Array.fill[Float](3)(1.0f)
    val stencil = createSimple2DStencil(1, 1, 3, 1, 0, 0, 1, 1, weights, Pad.Boundary.Wrap, 2)
    run2DStencil(stencil, 1, 1, 3, 1, 0, 0, 1, 1, weights, Utils.scalaWrap)
  }

  @Test def blurY3Point(): Unit = {
    val weights = Array.fill[Float](3)(1.0f)
    val stencil = createSimple2DStencil(3, 1, 1, 1, 1, 1, 0, 0, weights, Pad.Boundary.Wrap, 2)
    run2DStencil(stencil, 3, 1, 1, 1, 1, 1, 0, 0, weights, Utils.scalaWrap)
  }

  /* **********************************************************
     TILING 2D
 ***********************************************************/
  def createTiled2DStencil(size: Int, step: Int,
                           tileSize: Int, tileStep: Int,
                           left: Int, right: Int,
                           weights: Array[Float],
                           boundary: Pad.BoundaryFun): Lambda = {
    createTiled2DStencil(size, step, size, step,
      tileSize, tileStep, tileSize, tileStep,
      left, right, left, right,
      weights, boundary)
  }

  def createTiled2DStencil(size1: Int, step1: Int,
                           size2: Int, step2: Int,
                           tileSize1: Int, tileStep1: Int,
                           tileSize2: Int, tileStep2: Int,
                           top: Int, bottom: Int,
                           left: Int, right: Int,
                           weights: Array[Float],
                           boundary: Pad.BoundaryFun): Lambda = fun(
    ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
    ArrayType(Float, weights.length),
    (matrix, weights) => {
      Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

        MapLcl(1)(MapLcl(0)(
          fun(elem => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ elem, weights)
          })

        )) o Slide2D(size1, step1, size2, step2) o toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
      ))) o Slide2D(tileSize1, tileStep1, tileSize2, tileStep2) o Pad2D(top, bottom, left, right, boundary) $ matrix
    }
  )

  def createCopyTilesLambda(size: Int, step: Int,
                            left: Int, right: Int,
                            boundary: Pad.BoundaryFun): Lambda = fun(
    ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
    ArrayType(Float, 9),
    (matrix, weights) => {
      MapWrg(1)(MapWrg(0)(fun(tile =>
        toGlobal(MapLcl(1)(MapLcl(0)(id))) $ tile

      ))) o Slide2D(size, step) o Pad2D(left, right, boundary) $ matrix
    }
  )

  @Test def copyTilesIdentity(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }
    val tiled: Lambda = createCopyTilesLambda(4, 2, 1, 1, BOUNDARY)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (false, false))(tiled, data2D, gaussWeights)
    val gold = Utils.scalaGenerate2DNeighbours(data2D, 4, 2, 4, 2, 1, 1, 1, 1, SCALABOUNDARY).flatten.flatten.flatten

    assertArrayEquals(gold, output, 0.1f)
  }

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Test def tiling2DBiggerTiles(): Unit = {
    val data2D = Array.tabulate(32, 32) { (i, j) => i * 24.0f + j }
    val tiled: Lambda = createTiled2DStencil(3, 1, 10, 8, 1, 1, gaussWeights, BOUNDARY)
    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (false, false))(tiled, data2D, gaussWeights)
    val gold = Utils.scalaCompute2DStencil(data2D, 3, 1, 3, 1, 1, 1, 1, 1, gaussWeights, SCALABOUNDARY)

    assertArrayEquals(gold, output, 0.1f)
  }

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Test def tiled2D9PointStencil(): Unit = {
    LongTestsEnabled()
    val tiled: Lambda = createTiled2DStencil(3, 1, 4, 2, 1, 1, gaussWeights, BOUNDARY)
    run2DStencil(tiled, 3, 1, 1, 1, gaussWeights, SCALABOUNDARY)
  }

  def createTiled2DStencilWithTiledLoading(size1: Int, step1: Int,
                                           size2: Int, step2: Int,
                                           tileSize1: Int, tileStep1: Int,
                                           tileSize2: Int, tileStep2: Int,
                                           top: Int, bottom: Int,
                                           left: Int, right: Int,
                                           weights: Array[Float],
                                           boundary: Pad.BoundaryFun): Lambda = fun(
    ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
    ArrayType(Float, weights.length),
    (matrix, weights) => {
      Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

        MapLcl(1)(MapLcl(0)(
          //MapSeq(MapSeq((toGlobal(id))))
          fun(elem => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ elem, weights)
          })

          // create stencil neighbourhoods
        )) o Slide2D(size1, step1, size2, step2) o Map(Join()) o

          // load chunks to local memory
          toLocal(MapLcl(1)(MapSeqUnroll(MapLcl(0)(id)))) $ tile

        // spliting tile into chunks
      ))) o Map(Map(Map(Split(8)))) o
        // creating tiles
        Slide2D(tileSize1, tileStep1, tileSize2, tileStep2) o
        Pad2D(top, bottom, left, right, boundary) $ matrix
    }
  )

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Ignore // falsely classified as not valid because of barriers
  @Test def tiledBlurXTiledLoading(): Unit = {
    val weights = Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1).map(_.toFloat)
    val tiled: Lambda = createTiled2DStencilWithTiledLoading(1, 1, 17, 1, 1, 1, 24, 8, 0, 0, 8, 8, weights, Pad.Boundary.Clamp)
    run2DStencil(tiled, 1, 1, 17, 1, 0, 0, 8, 8, weights, Utils.scalaClamp)
  }

  @Ignore // todo compN(p, i) = p^i, but map^i = map o ... o map does not work yet
  @Test def alternative2DStencilExpression(): Unit = {
    val clamp = Pad.Boundary.Clamp

    // apply primitive in X and Y Dimension
    val dim2: (FunDecl) => Lambda = (primitive: FunDecl) =>
      primitive o Map(primitive)

    // compose primitive n-times
    def compN: (FunDecl, Int) => FunDecl = (primitive: FunDecl, i: Int) =>
      if (i > 1)
        primitive o compN(primitive, i-1)
      else
        primitive

    def dimN: (FunDecl, Int) => FunDecl = (primitive: FunDecl, dim: Int) =>
      if (dim > 1)
        // should be `Map` inside the compN
        dimN(primitive, dim-1) o compN(Map.asInstanceOf[FunDecl], dim-1) o primitive
      else
        primitive

    // apply 2D boundary handling
    //val boundary = (size: Int, b: Pad.BoundaryFun) => dim2(Pad(size,size,b))
    val boundary = (size: Int, b: Pad.BoundaryFun) => dimN(Pad(size,size,b), 2)

    // create 2D neighborhoods
    //val nbh = (size: Int) => Map(Transpose()) o dim2(Slide(size, size-2))
    val nbh = (size: Int) => Map(Transpose()) o dimN(Slide(size, size-2), 2)

    // 2D stencil function
    val f = toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join()


    val lambda = fun(
      ArrayType(ArrayType(Float, SizeVar("N")), SizeVar("M")),
      (input) => {
        MapGlb(1)(MapGlb(0)(f)) o nbh(3) o boundary(1, clamp) $ input
      })

    val lambda2 = fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        MapGlb(1)(id) o compN(Pad(1,1,clamp), 3) $ input
      })

    /*
    val input = Array.tabulate(512, 512) { (i,j) => scala.util.Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(16, 16, 512, 512, (false, false))(lambda, input)
    val weights = Array.tabulate(9) { i => 1.0f}
    val gold = Utils.scalaCompute2DStencil(input, 3,1,3,1, 1,1,1,1, weights, Utils.scalaClamp)
    assertArrayEquals(gold, output, 0.1f)
    */
  }

  @Test def modSimplifyTest(): Unit = {
    val size = 3
    val step = 1
    val left = 1
    val right = 1

    val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }
    val lambda = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(2))), Var("M", StartFromRange(2))),
      (domain) => {
        MapSeq(
          MapSeq(
            fun(neighbours =>
              MapSeqUnroll(MapSeqUnroll(id)) $ neighbours)
          )
        ) o Map(Map(Transpose()) o Slide(size, step) o Transpose()) o Slide(size, step) o
          Transpose() o Pad(left, right, Pad.Boundary.Wrap) o Transpose() o Pad(left, right, Pad.Boundary.Wrap) $ domain
      }
    )
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
  $ cd $(SHOC_DIR)/src/opencl/level1/stencil2d
  $ ./Stencil2D --customSize 8,8 --weight-center 0.25 --weight-cardinal 0.15 --weight-diagonal 0.05 --verbose --num-iters 1

  compare to 10x10 array. SHOC does not handle boundary but provides a padded input array
   */
  @Test def shocStencil2D(): Unit = {
    LongTestsEnabled()
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(2))), Var("M", StartFromRange(2))),
      ArrayType(Float, 9),
      (matrix, weights) => {
        Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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
    val inputSize = 512
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

    try {
      val (output: Array[Float], runtime) = Execute(1, 256, 512, 512, (false, false))(stencil, input, weights)
      println("Runtime: " + runtime)
    } catch {
      case e: DeviceCapabilityException =>
        Assume.assumeNoException("Device not supported.", e)
    }
  }

  @Test def shocStencil2DNoTiling(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(6))), Var("M", StartFromRange(6))),
      ArrayType(Float, 9),
      (matrix, weights) => {

        MapGlb(1)(MapGlb(0)(
          // stencil computation
          fun(elem => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun( (acc, pair) => {
                val pixel = pair._0
                val weight = pair._1
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ elem, weights)
          })
          // create neighbourhoods in tiles
        )) o Slide2D(3, 1, 3, 1) $ matrix
      }
    )
    val weights = Array(0.05, 0.15, 0.05,
      0.15, 0.25, 0.15,
      0.05, 0.15, 0.05).map(_.toFloat)

    val inputSize = 34
    val haloSize = 1
    val outputSize = inputSize - 2 * haloSize

    // create already padded input array with inner elements (i,j) = i * j
    var input = Array.tabulate(inputSize, inputSize) { (i, j) => (i - haloSize) * (j - haloSize) * 1.0f }
    input(0) = input(0).map((_ * 0.0f))
    input(inputSize - 1) = input(inputSize - 1).map(_ * 0.0f)
    input = input.transpose
    input(0) = input(0).map(_ * 0.0f)
    input(inputSize - 1) = input(inputSize - 1).map(_ * 0.0f)
    input = input.transpose

    try {
      val (output: Array[Float], runtime) = Execute(1, 32, 32, 32, (false, false))(stencil, input, weights)
    } catch {
      case e: DeviceCapabilityException =>
        Assume.assumeNoException("Device not supported.", e)
    }
  }

  @Ignore //todo does not compute correct result yet
  @Test def shocStencil2DNoTilingFloat3(): Unit = {
    val dotAndSumUp = UserFun("dotAndSumUp", Array("acc", "l", "r"),
      "{ return acc + dot(l, r); }",
      Seq(Float, Float3, Float3), Float)

    val stencil = fun(
      ArrayType(ArrayType(Float, Var("M", StartFromRange(6))), Var("N", StartFromRange(6))),
      ArrayType(Float, 9),
      (matrix, weights) => {
        //weights.addressSpace = ConstantMemory

        MapGlb(1)(MapGlb(0)(
          // stencil computation
          fun(elem => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun( (acc, pair) => {
                val pixel = pair._0
                val weight = pair._1
                dotAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $
              Zip(asVector(3) o Join() $ elem, asVector(3) $ weights)
          })
          // create neighbourhoods in tiles
        )) o Slide2D(3, 1) $ matrix
      }
    )
    val weights = Array(
      0.05, 0.15, 0.05,
      0.15, 0.25, 0.15,
      0.05, 0.15, 0.05 ).map(_.toFloat)

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

    try {
      val (output: Array[Float], runtime) = Execute(1, 256, 1024, 8192, (false, false))(stencil, input, weights)
      println("Runtime: " + runtime)
      println(output.take(10).mkString(", "))
    } catch {
      case e: DeviceCapabilityException =>
        Assume.assumeNoException("Device not supported.", e)
    }
  }

  /* **********************************************************
       THREE LEVEL TILING
   ***********************************************************/
  @Test def threeLevelTilingTest(): Unit = {
    LongTestsEnabled()
    val stencil = fun(
      //ArrayType(ArrayType(Float, Var("N", StartFromRange(2))), Var("M", StartFromRange(2))),
      ArrayType(ArrayType(Float, 8192), 8192),
      ArrayType(Float, 15),
      (matrix, weights) => {
        Untile2D() o MapWrg(1)(MapWrg(0)(
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

    try {
      val (output: Array[Float], runtime) = Execute(64, 4, 1024, 512, (true, true))(stencil, input, weights)
      println("Runtime: " + runtime)
    } catch {
      case e: DeviceCapabilityException =>
        Assume.assumeNoException("Device not supported.", e)
    }
  }

  @Test
  def stencil2DTilingRewriteIdentities(): Unit = {
    LongTestsEnabled()
    val N = SizeVar("N")
    val M = SizeVar("M")

    val n = 3
    val s = 1
    val u = 6
    val v = 4

    // use non-powers-of-two as input values
    // to enable powers-of-two as valid inputs for Slide step since Pad is not used here
    val input = Array.tabulate(34, 34) { (i, j) => i * 34.0f + j }

    // use abbr. notation to avoid huge expressions and simplify rewriting on paper
    // *=map (as in BMF), J=join, T=transpose, S_ab = slide a b

    // (0): *T o S_ns o *S_ns
    val gold = Map(Transpose()) o Slide(n,s) o Map(Slide(n,s))

    // Desired: 2D tiling
    // This is what we assume to be correct 2d stencil tiling. Proof by rewriting follows:
    //
    // J o J o *T o **Slide2d_ns o Slide2d_uv
    // val desired = Map(Join()) o Join() o Map(Transpose()) o Map(Map(Slide2D(n,s))) o Slide2D(u,v)
    //
    // = *J o J o *T o ***T o **S_ns o ***S_ns o *T o S_uv o *S_uv
    val desired = Map(Join()) o Join() o Map(Transpose()) o Map(Map(Map(Transpose()))) o Map(Map(Slide(n,s))) o Map(Map(Map(Slide(n,s)))) o
      Map(Transpose()) o Slide(n,s) o Map(Slide(n,s))

    // (1): *T o J o *S_ns o S_uv o *S_ns
    val f1 = Map(Transpose()) o Join() o Map(Slide(n,s)) o Slide(u,v) o Map(Slide(n,s))

    // (2): J o **T o *S_ns o S_uv o *S_ns
    val f2 = Join() o Map(Map(Transpose())) o Map(Slide(n,s)) o Slide(u,v) o Map(Slide(n,s))

    // (3): tile first S_ns -> does not lead to anything yet. see (6) for other try
    // J o **T o *(J o *S_ns o S_uv) o S_uv o *S_ns
    val f3 = Join() o Map(Map(Transpose())) o Map(Join() o Map(Slide(n,s)) o
      Slide(u,v)) o Slide(u,v) o Map(Slide(n,s))

    // (4): J o **T o *J o **S_ns o *S_uv o S_uv o *S_ns
    val f4 = Join() o Map(Map(Transpose())) o Map(Join()) o Map(Map(Slide(n,s))) o
      Map(Slide(u,v)) o Slide(u,v) o Map(Slide(n,s))

    // (5): J o *J o ***T o **S_ns o *S_uv o S_uv o *S_ns
    val f5 = Join() o Map(Join()) o Map(Map(Map(Transpose()))) o Map(Map(Slide(n,s))) o
      Map(Slide(u,v)) o Slide(u,v) o Map(Slide(n,s))


    // (6): Try tiling other S_ns from (2)
    // J o **T o *S_ns o S_uv o *(J o *S_ns o S_uv)
    val f6 = Join() o Map(Map(Transpose())) o Map(Slide(n,s)) o Slide(u,v) o Map(Join() o Map(Slide(n,s)) o Slide(u,v))

    // (7): J o **T o *S_ns o S_uv o *J o **S_ns o *S_uv
    val f7 = Join() o Map(Map(Transpose())) o Map(Slide(n,s)) o Slide(u,v) o Map(Join()) o
      Map(Map(Slide(n,s))) o Map(Slide(u,v))

    // (8): J o **T o *S_ns o **J o S_uv o **S_ns o *S_uv
    val f8 = Join() o Map(Map(Transpose())) o Map(Slide(n,s)) o Map(Map(Join())) o
      Slide(u,v) o Map(Map(Slide(n,s))) o Map(Slide(u,v))

    // (9): J o **T o ***J o *S_ns o S_uv o **S_ns o *S_uv
    val f9 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Slide(n,s)) o Slide(u,v) o
      Map(Map(Slide(n,s))) o Map(Slide(u,v))

    // (10): J o **T o ***J o *S_ns o ***S_ns o S_uv o *S_uv
    val f10 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Slide(n,s)) o
      Map(Map(Map(Slide(n,s)))) o Slide(u,v) o Map(Slide(u,v))

    // (11): J o **T o ***J o *S_ns o ***S_ns o *(T o T) o S_uv o *S_uv
    val f11 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Slide(n,s)) o
      Map(Map(Map(Slide(n,s)))) o Map(Transpose() o Transpose()) o Slide(u,v) o Map(Slide(u,v))

    // (12): J o **T o ***J o *S_ns o ***S_ns o *T o *T o S_uv o *S_uv
    val f12 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Slide(n,s)) o
      Map(Map(Map(Slide(n,s)))) o Map(Transpose()) o Map(Transpose()) o Slide(u,v) o Map(Slide(u,v))

    // (13): J o **T o ***J o *S_ns o *T o ***S_ns o *T o S_uv o *S_uv
    val f13 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Slide(n,s)) o
      Map(Transpose()) o Map(Map(Map(Slide(n,s)))) o Map(Transpose()) o Slide(u,v) o Map(Slide(u,v))

    // (14): J o **T o ***J o **T o *T o **S_ns_ o ***S_ns o *T o S_uv o *S_uv
    val f14 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Map(Transpose())) o
      Map(Transpose()) o Map(Map(Slide(n,s))) o Map(Map(Map(Slide(n,s)))) o Map(Transpose()) o
      Slide(u,v) o Map(Slide(u,v))

    // (15): J o **J o ***T o **T o **T o *T o **S_ns o ***S_ns o *T o S_uv o *S_uv
    val f15 = Join() o Map(Map(Join())) o Map(Map(Map(Transpose()))) o
      Map(Map(Transpose())) o Map(Map(Transpose())) o // they cancel out
      Map(Transpose()) o Map(Map(Slide(n,s))) o Map(Map(Map(Slide(n,s)))) o Map(Transpose()) o
      Slide(u,v) o Map(Slide(u,v))

    // (16): J o **J o ***T o *T o **S_ns o ***S_ns o *T o S_uv o *S_uv
    val f16 = Join() o Map(Map(Join())) o Map(Map(Map(Transpose()))) o
      Map(Transpose()) o Map(Map(Slide(n,s))) o Map(Map(Map(Slide(n,s)))) o Map(Transpose()) o
      Slide(u,v) o Map(Slide(u,v))

    def lambda(f: Lambda): Lambda1 = fun(
      ArrayType(ArrayType(Float, M), N),
      input =>
        MapGlb(1)(MapGlb(0)(MapSeq(MapSeq(id)))) o f $ input
    )

    val (outGold: Array[Float], runtime) = Execute(1,1,32,32,(false,false))(lambda(gold), input)
    val (outDesired: Array[Float], runtime0) = Execute(1,1,32,32,(false,false))(lambda(desired), input)
    val (outF1: Array[Float], runtime1) = Execute(1,1,32,32,(false,false))(lambda(f1), input)
    val (outF2: Array[Float], runtime2) = Execute(1,1,32,32,(false,false))(lambda(f2), input)
    val (outF3: Array[Float], runtime3) = Execute(1,1,32,32,(false,false))(lambda(f3), input)
    val (outF4: Array[Float], runtime4) = Execute(1,1,32,32,(false,false))(lambda(f4), input)
    val (outF5: Array[Float], runtime5) = Execute(1,1,32,32,(false,false))(lambda(f5), input)
    val (outF6: Array[Float], runtime6) = Execute(1,1,32,32,(false,false))(lambda(f6), input)
    val (outF7: Array[Float], runtime7) = Execute(1,1,32,32,(false,false))(lambda(f7), input)
    val (outF8: Array[Float], runtime8) = Execute(1,1,32,32,(false,false))(lambda(f8), input)
    val (outF9: Array[Float], runtime9) = Execute(1,1,32,32,(false,false))(lambda(f9), input)
    val (outF10: Array[Float], runtime10) = Execute(1,1,32,32,(false,false))(lambda(f10), input)
    val (outF11: Array[Float], runtime11) = Execute(1,1,32,32,(false,false))(lambda(f11), input)
    val (outF12: Array[Float], runtime12) = Execute(1,1,32,32,(false,false))(lambda(f12), input)
    val (outF13: Array[Float], runtime13) = Execute(1,1,32,32,(false,false))(lambda(f13), input)
    val (outF14: Array[Float], runtime14) = Execute(1,1,32,32,(false,false))(lambda(f14), input)
    val (outF15: Array[Float], runtime15) = Execute(1,1,32,32,(false,false))(lambda(f15), input)
    val (outF16: Array[Float], runtime16) = Execute(1,1,32,32,(false,false))(lambda(f16), input)

    assertArrayEquals(outGold, outDesired, 0.1f)
    assertArrayEquals(outGold, outF1, 0.1f)
    assertArrayEquals(outGold, outF2, 0.1f)
    assertArrayEquals(outGold, outF3, 0.1f)
    assertArrayEquals(outGold, outF4, 0.1f)
    assertArrayEquals(outGold, outF5, 0.1f)
    assertArrayEquals(outGold, outF6, 0.1f)
    assertArrayEquals(outGold, outF7, 0.1f)
    assertArrayEquals(outGold, outF8, 0.1f)
    assertArrayEquals(outGold, outF9, 0.1f)
    assertArrayEquals(outGold, outF10, 0.1f)
    assertArrayEquals(outGold, outF11, 0.1f)
    assertArrayEquals(outGold, outF12, 0.1f)
    assertArrayEquals(outGold, outF13, 0.1f)
    assertArrayEquals(outGold, outF14, 0.1f)
    assertArrayEquals(outGold, outF15, 0.1f)
    assertArrayEquals(outGold, outF16, 0.1f)
  }

  @Test
  def stencil2DTilingLocalMemIdentities(): Unit = {
    LongTestsEnabled()
    // stencil shape
    val n = 3
    val s = 1
    // tile size
    val u = 6
    val v = 4
    // pad parameters
    val l = 1
    val r = 1
    val b = Pad.Boundary.Clamp

    // use non-powers-of-two as input values
    // to enable powers-of-two as valid inputs for Slide step since Pad is not used here
    val input = Array.tabulate(32, 32) { (i, j) => i * 32.0f + j }
    // stencil function: nbh:[3][3] -> [1]
    val f = toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join()

    def lambda(f: Lambda) = {
      fun(
        ArrayType(ArrayType(Float,SizeVar("M")), SizeVar("N")),
        input => f $ input
      )
    }

    // use shorthand notation to avoid huge expressions (partly inspired by BMF)
    val P = Pad(l,r,b)
    val T = Transpose()
    val T_w = TransposeW()
    val J = Join()
    val S_ns = Slide(n,s)
    val S_uv = Slide(u,v)
    def *(f: Lambda) = Map(f)
    def **(f: Lambda) = Map(Map(f))
    def ***(f: Lambda) = Map(Map(Map((f))))

    val gold = MapGlb(1)(MapGlb(0)(f)) o                  // (C) apply stencil function
      Map(Transpose()) o Slide(n,s) o Map(Slide(n,s)) o   // (B) 2d neighborhood creation
        Pad(l,r,b) o Map(Pad(l,r,b))                      // (A) 2d padding

    // same as gold but short
    val goldShort = MapGlb(1)(MapGlb(0)(f)) o             // (C)
      *(T) o S_ns o *(S_ns) o                             // (B)
        P o *(P)                                          // (A)

    // introduce 2d tiles
    val f1 = MapGlb(1)(MapGlb(0)(f)) o                                            // (C)
      *(J) o J o *(T) o ***(T) o **(S_ns) o ***(S_ns) o *(T) o S_uv o *(S_uv) o   // (B) (tiling inclusive)
        P o *(P)                                                                  // (A)

    ////////////////// Promote Two Maps //////////////
    // fuse maps
    val g1 = MapGlb(1)(MapGlb(0)(f) o J) o J o *(T) o
        ***(T) o **(S_ns) o ***(S_ns) o *(T) o S_uv o *(S_uv) o
          P o *(P)

    // apply mapJoin rule twice - introduce MapWrgs and MapLcls
    val g2 = J o MapWrg(1)(MapWrg(0)(J o MapLcl(1)(MapLcl(0)(f)))) o *(T) o
        ***(T) o **(S_ns) o ***(S_ns) o *(T) o S_uv o *(S_uv) o
          P o *(P)

    // split to single maps again
    val g3 = J o **(J) o MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(f)))) o *(T) o
        ***(T) o **(S_ns) o ***(S_ns) o *(T) o S_uv o *(S_uv) o
          P o *(P)

    // the last *T can be moved in front of ****f as T_w -> see 'f3'

    /////////////////// f1 -> f2 ///////////////////////

    // move maps forward to exploit more levels of parallelism
    // functionally *J o * o *T should do the job, however lifts output view require Untile or ...
    val f2 = /* *(J) o J o *(T) o */ Untile2D() o MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(f)))) o // (C) using workgroups
      ***(T) o **(S_ns) o ***(S_ns) o *(T) o S_uv o *(S_uv) o                                   // (B)
        P o *(P)                                                                                // (A)

    // ...TransposeW instead of Transpose after stencil computation
    val f3 = *(J) o J o *(T_w) o MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(f)))) o  // (C)
      ***(T) o **(S_ns) o ***(S_ns) o         // (%)                              // (B) Create neighborhoods in tiles
        *(T) o S_uv o *(S_uv) o                                                   // (B) Create tiles
          P o *(P)                                                                // (A)

    // fuse the expressions (%) from above with the MapWrg's
    val f4 = *(J) o J o *(T_w) o MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(f)) o  // (C)
      *(T) o S_ns o *(S_ns))) o                                                 // (B) Create neighborhoods in tiles
        *(T) o S_uv o *(S_uv) o                                                 // (B) Create tiles
          P o *(P)                                                              // (A)

    // load tiles to local memory
    val f5 = *(J) o J o *(T_w) o MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(f)) o // (C)
      *(T) o S_ns o *(S_ns) o                 // Slide2D n s                   // (B.3) Create neighborhoods in tiles
        toLocal(MapLcl(1)(MapLcl(0)(id))))) o // whole line = id               // (B.2) Load tiles to local memory
        *(T) o S_uv o *(S_uv) o               // Slide2D u v                   // (B.1) Create tiles
          P o *(P)                                                             // (A)

    val (outGold: Array[Float], runtime) = Execute(1,1,32,32,(false,false))(lambda(gold), input)
    val (outGoldShort: Array[Float], runtimeShort) = Execute(1,1,32,32,(false,false))(lambda(goldShort), input)
    val (outF1: Array[Float], _) = Execute(1,1,32,32,(false,false))(lambda(f1), input)
    val (outG1: Array[Float], _) = Execute(1,1,32,32,(false,false))(lambda(g1), input)
    val (outG2: Array[Float], _) = Execute(1,1,32,32,(false,false))(lambda(g2), input)
    val (outG3: Array[Float], _) = Execute(1,1,32,32,(false,false))(lambda(g3), input)
    val (outF2: Array[Float], _) = Execute(1,1,32,32,(false,false))(lambda(f2), input)
    val (outF3: Array[Float], _) = Execute(1,1,32,32,(false,false))(lambda(f3), input)
    val (outF4: Array[Float], _) = Execute(1,1,32,32,(false,false))(lambda(f4), input)
    val (outF5: Array[Float], _) = Execute(1,1,32,32,(false,false))(lambda(f5), input)

    assertArrayEquals(outGold, outGoldShort, 0.1f)
    assertArrayEquals(outGold, outF1, 0.1f)
    assertArrayEquals(outGold, outG1, 0.1f)
    assertArrayEquals(outGold, outG2, 0.1f)
    assertArrayEquals(outGold, outG3, 0.1f)
    assertArrayEquals(outGold, outF2, 0.1f)
    assertArrayEquals(outGold, outF3, 0.1f)
    assertArrayEquals(outGold, outF4, 0.1f)
    assertArrayEquals(outGold, outF5, 0.1f)
  }
}
