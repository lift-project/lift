package opencl.generator.stencil

import lift.arithmetic.{SizeVar, StartFromRange, Var}
import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
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

  val randomData2D = Array.tabulate(1024, 1024) { (i, j) => Random.nextFloat() }
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
      Untile() o MapWrg(1)(MapWrg(0)(fun(tile =>

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
    val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }
    val tiled: Lambda = createCopyTilesLambda(4, 2, 1, 1, BOUNDARY)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (false, false))(tiled, data2D, gaussWeights)
    val gold = Utils.scalaGenerate2DNeighbours(data2D, 4, 2, 4, 2, 1, 1, 1, 1, SCALABOUNDARY).flatten.flatten.flatten

    assertArrayEquals(gold, output, 0.1f)
  }

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Test def tiling2DBiggerTiles(): Unit = {
    val data2D = Array.tabulate(1024, 1024) { (i, j) => i * 24.0f + j }
    val tiled: Lambda = createTiled2DStencil(3, 1, 10, 8, 1, 1, gaussWeights, BOUNDARY)
    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (false, false))(tiled, data2D, gaussWeights)
    val gold = Utils.scalaCompute2DStencil(data2D, 3, 1, 3, 1, 1, 1, 1, 1, gaussWeights, SCALABOUNDARY)

    assertArrayEquals(gold, output, 0.1f)
  }

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Test def tiled2D9PointStencil(): Unit = {
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
      Untile() o MapWrg(1)(MapWrg(0)(fun(tile =>

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

    try {
      val (output: Array[Float], runtime) = Execute(1, 256, 1024, 8192, (false, false))(stencil, input, weights)
      println("Runtime: " + runtime)
    } catch {
      case e: DeviceCapabilityException =>
        Assume.assumeNoException("Device not supported.", e)
    }
  }

  /* **********************************************************
       THREE LEVEL TILING
   ***********************************************************/
  @Test def threeLevelTilingTest(): Unit = {
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

    try {
      val (output: Array[Float], runtime) = Execute(64, 4, 1024, 512, (true, true))(stencil, input, weights)
      println("Runtime: " + runtime)
    } catch {
      case e: DeviceCapabilityException =>
        Assume.assumeNoException("Device not supported.", e)
    }
  }
}
