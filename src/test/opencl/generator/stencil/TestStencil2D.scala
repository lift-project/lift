package opencl.generator.stencil

import apart.arithmetic.{SizeVar, StartFromRange, Var}
import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

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

class TestStencil2D extends TestStencil {

  /* **********************************************************
       SLIDE 2D o PAD 2D
   ***********************************************************/
  def create2DPadSlideLambda(boundary: BoundaryFun,
                             size: Int, step: Int,
                             left: Int, right: Int): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqUnroll(MapSeqUnroll(id)) $ neighbours
          ))
        ) o Slide2D(size, step) o Pad2D(left, right, boundary) $ domain
      }
    )
  }

  def runCombinedPadGroupTest(size: Int, step: Int,
                              left: Int, right: Int,
                              boundary: BoundaryFun,
                              scalaBoundary: (Int, Int) => Int,
                              data: Array[Array[Float]] = data2D): Unit = {
    val gold = Utils.scalaGenerate2DNeighbours(data, size, step, size, step, left, right, left, right, scalaBoundary)
    val goldFlat = gold.flatten.flatten.flatten

    val lambda = create2DPadSlideLambda(boundary, size, step, left, right)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(lambda, data)

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
                   name: String,
                   scalaBoundary: (Int, Int) => Int): Unit = {
    try {
      //val (width, height, input) = readInputImage(lenaPGM)
      // be carefull when choosing small input size because of 'StartsFromRange(100)'
      val width = randomData2D(0).length
      val height = randomData2D.length

      // change input used here
      val input = randomData2D

      val (output: Array[Float], runtime) = Execute(1, 1, width, height, (false, false))(stencil, input, weights)
      println("Runtime: " + runtime)

      //savePGM(name, outputLocation, output.grouped(width).toArray)

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
                   name: String,
                   scalaBoundary: (Int, Int) => Int): Unit = {
    run2DStencil(stencil, size, step, size, step, left, right, left, right, weights, name, scalaBoundary)
  }

  val gaussWeights = Array(
      0.08f, 0.12f, 0.08f,
      0.12f, 0.20f, 0.12f,
      0.08f, 0.12f, 0.08f)

  @Test def gaussianBlur(): Unit = {
    val stencil = createSimple2DStencil(3, 1, 1, 1, gaussWeights, BOUNDARY, 2)
    run2DStencil(stencil, 3, 1, 1, 1, gaussWeights, "gauss.pgm", SCALABOUNDARY)
  }

  @Test def gaussianBlur25PointStencil(): Unit = {
    val weights = Array(
      1, 4, 7, 4, 1,
      4, 16, 26, 16, 4,
      7, 26, 41, 26, 7,
      4, 16, 26, 16, 4,
      1, 4, 7, 4, 1).map(_ * 0.004219409282700422f)
    val stencil = createSimple2DStencil(5, 1, 2, 2, weights, BOUNDARY, 3)
    run2DStencil(stencil, 5, 1, 2, 2, weights, "gauss25.pgm", SCALABOUNDARY)
  }

  @Test def blurX3Point(): Unit = {
    val weights = Array.fill[Float](3)(1.0f)
    val stencil = createSimple2DStencil(1, 1, 3, 1, 0, 0, 1, 1, weights, Pad.Boundary.Wrap, 2)
    run2DStencil(stencil, 1, 1, 3, 1, 0, 0, 1, 1, weights, "notUsed", Utils.scalaWrap)
  }

  @Test def blurY3Point(): Unit = {
    val weights = Array.fill[Float](3)(1.0f)
    val stencil = createSimple2DStencil(3, 1, 1, 1, 1, 1, 0, 0, weights, Pad.Boundary.Wrap, 2)
    run2DStencil(stencil, 3, 1, 1, 1, 1, 1, 0, 0, weights, "notUsed", Utils.scalaWrap)
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
    val data2D = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val tiled: Lambda = createTiled2DStencil(3, 1, 10, 8, 1, 1, gaussWeights, BOUNDARY)
    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (false, false))(tiled, data2D, gaussWeights)
    val gold = Utils.scalaCompute2DStencil(data2D, 3, 1, 3, 1, 1, 1, 1, 1, gaussWeights, SCALABOUNDARY)

    assertArrayEquals(gold, output, 0.1f)
  }

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Test def tiled2D9PointStencil(): Unit = {
    val tiled: Lambda = createTiled2DStencil(3, 1, 4, 2, 1, 1, gaussWeights, BOUNDARY)
    run2DStencil(tiled, 3, 1, 1, 1, gaussWeights, "notUsed", SCALABOUNDARY)
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
    run2DStencil(tiled, 1, 1, 17, 1, 0, 0, 8, 8, weights, "notUsed", Utils.scalaClamp)
  }

}
