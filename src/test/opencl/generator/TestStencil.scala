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
class TestStencil extends TestSlide {
  // Boundary conditions implemented as scala functions for gold versions
  val scalaClamp = (idx: Int, length: Int) => {
    if(idx<0) 0 else if(idx>length-1) length-1 else idx
  }

  val scalaWrap = (idx: Int, length: Int) => {
    (idx % length + length) % length
  }

  val scalaMirror = (idx: Int, length: Int) => {
    val id = (if(idx < 0) -1-idx else idx) % (2*length)
    if(id >= length) length+length-id-1 else id
  }

  /* **********************************************************
      STENCIL TEST SETTINGS
   ***********************************************************/
  override val UNROLL = true
  val randomData = Seq.fill(1024)(Random.nextFloat()).toArray
  val randomData2D = Array.tabulate(1024, 1024) { (i, j) => Random.nextFloat() }
  // currently used for 2D stencils / refactor to run with every boundary condition
  val BOUNDARY = Pad.Boundary.Wrap
  val SCALABOUNDARY: (Int, Int) => Int = scalaWrap

  def createFused1DStencilLambda(weights: Array[Float],
                                 size: Int, step: Int,
                                 left: Int, right: Int): Lambda2 = {
    fun(
      ArrayType(Float, SizeVar("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeqOrMapSeqUnroll(id)) o
              ReduceSeqOrReduceSeqUnroll(fun((acc, y) => {
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
    val (output: Array[Float], runtime) = createGroups1D(f, data)
    compareGoldWithOutput(gold, output, runtime)
  }

  def createPadGroupLambda(boundary: BoundaryFun,
                           size: Int, step: Int,
                           left: Int, right: Int): Lambda1 = {
    fun(
      ArrayType(Float, Var("N", StartFromRange(2))),
      (input) =>
        MapGlb(MapSeqOrMapSeqUnroll(id)) o
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
    val data = Array(0,1,2,3).map(_.toFloat)
    val gold = Array(1,3,6,8).map(_.toFloat)
    val f = createThesisChapter4Example(Pad.Boundary.Clamp, 3,1, 1,1)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(f, data)
    println(output.mkString(","))
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def group3ElementsPadWrap(): Unit = {
    val boundary = Pad.Boundary.Wrap
    val gold = Array(4,0,1, 0,1,2, 1,2,3, 2,3,4, 3,4,0).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3,1, 1,1)
  }

  @Test def group3ElementsPadClamp(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0,0,1, 0,1,2, 1,2,3, 2,3,4, 3,4,4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3,1, 1,1)
  }

  @Test def group3ElementsPadMirror(): Unit = {
    val boundary = Pad.Boundary.Mirror
    val gold = Array(0,0,1, 0,1,2, 1,2,3, 2,3,4, 3,4,4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3,1, 1,1)
  }

  @Test def group3ElementsPadMirrorUnsafe(): Unit = {
    val boundary = Pad.Boundary.MirrorUnsafe
    val gold = Array(0,0,1, 0,1,2, 1,2,3, 2,3,4, 3,4,4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3,1 ,1,1)
  }

  @Test def group5ElementsPadClamp(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(
      0,0,0,1,2,
      0,0,1,2,3,
      0,1,2,3,4,
      1,2,3,4,4,
      2,3,4,4,4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 5,1, 2,2)
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

    val gold = Utils.scalaCompute1DStencil(randomData, 3,1, 1,1, weights, SCALABOUNDARY)
    val stencil = create1DStencilFusedMapReduceLambda(randomData.length, weights, 3,1, 1,1)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def simple5Point1DStencil(): Unit = {
    val weights = Array(1, 2, 3, 2, 1).map(_.toFloat)

    val gold = Utils.scalaCompute1DStencil(randomData, 5,1, 2,2, weights, SCALABOUNDARY)
    val stencil = create1DStencilLambda(weights, 5,1, 2,2)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def createGroupsForOneSidedPadding(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0,0,0, 0,0,1, 0,1,2, 1,2,3, 2,3,4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3,1, 2,0)
  }

  /* **********************************************************
       SLIDE o PAD 2D
   ***********************************************************/
  def create2DPadSlideLambda(boundary: BoundaryFun,
                             size: Int, step: Int,
                             left: Int, right: Int): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqOrMapSeqUnroll(MapSeqOrMapSeqUnroll(id)) $ neighbours
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
    val gold = Utils.scalaGenerate2DNeighbours(data, size, step, size, step, left,right, left, right, scalaBoundary)
    val goldFlat = gold.flatten.flatten.flatten

    val lambda = create2DPadSlideLambda(boundary, size, step, left, right)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(lambda, data)

    compareGoldWithOutput(goldFlat, output, runtime)
  }

  @Ignore
  @Test def groupClampPaddedData2D(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp

    runCombinedPadGroupTest(3,1, 1,1, boundary, scalaBoundary)
  }

  @Ignore // takes ages leads to EOF Exceoption on Fuji
  @Test def groupBigClampPaddedData2D(): Unit = {
    val data2D = Array.tabulate(10, 10) { (i, j) => i * 10.0f + j }
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp

    runCombinedPadGroupTest(5,1, 2,2, boundary, scalaBoundary, data2D)
  }

  @Ignore // Takes ages!!!
  @Test def groupMirrorPaddedData2D(): Unit = {
    val boundary = Pad.Boundary.Mirror
    val scalaBoundary = scalaMirror

    runCombinedPadGroupTest(3,1, 1,1, boundary, scalaBoundary)
  }

  @Ignore
  @Test def groupWrapPaddedData2D(): Unit = {
    val boundary = Pad.Boundary.Wrap
    val scalaBoundary = scalaWrap

    runCombinedPadGroupTest(3,1, 1,1, boundary, scalaBoundary)
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
            toGlobal(MapSeqOrMapSeqUnroll(clamp)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(size1, step1, size2, step2) o Pad2D(top, bottom, left, right, boundary)$ matrix
      })
  }

  def run2DStencil(stencil: Lambda2,
                   size1: Int, step1: Int,
                   size2: Int, step2: Int,
                   top: Int, bottom: Int,
                   left : Int, right: Int,
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

      val gold = Utils.scalaCompute2DStencil(input, size1,step1, size2,step2, top,bottom,left,right, weights, scalaBoundary)
      compareGoldWithOutput(gold, output, runtime)

    } catch {
      case x: Exception => x.printStackTrace()
    }
  }

  def run2DStencil(stencil: Lambda2,
                   size: Int, step: Int,
                   left : Int, right: Int,
                   weights: Array[Float],
                   name: String,
                   scalaBoundary: (Int, Int) => Int): Unit = {
    run2DStencil(stencil, size,step,size,step, left,right,left,right, weights, name, scalaBoundary)
  }

  @Test def gaussianBlur(): Unit = {
    val stencil = createSimple2DStencil(3,1, 1,1, gaussWeights, BOUNDARY,2)
    run2DStencil(stencil, 3,1, 1,1, gaussWeights, "gauss.pgm", SCALABOUNDARY)
  }

  @Ignore // produces EOF exception on fuji
  @Test def gaussianBlur25PointStencil(): Unit = {
    val weights = Array(
      1, 4, 7, 4, 1,
      4, 16, 26, 16, 4,
      7, 26, 41, 26, 7,
      4, 16, 26, 16, 4,
      1, 4, 7, 4, 1).map(_*0.004219409282700422f)
    val stencil = createSimple2DStencil(5,1, 2,2, weights, BOUNDARY,3)
    run2DStencil(stencil, 5,1, 2,2, weights, "gauss25.pgm", SCALABOUNDARY)
  }

  @Test def blurX3Point(): Unit = {
    val weights = Array.fill[Float](3)(1.0f)
    val stencil = createSimple2DStencil(1,1,3,1, 0,0,1,1, weights, Pad.Boundary.Wrap,2)
    run2DStencil(stencil, 1,1,3,1, 0,0,1,1, weights, "notUsed", scalaWrap)
  }

  @Test def blurY3Point(): Unit = {
    val weights = Array.fill[Float](3)(1.0f)
    val stencil = createSimple2DStencil(3,1,1,1, 1,1,0,0, weights, Pad.Boundary.Wrap,2)
    run2DStencil(stencil, 3,1,1,1, 1,1,0,0, weights, "notUsed", scalaWrap)
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

    val gold = Utils.scalaCompute1DStencil(randomData, 3,1, 1,1, weights, SCALABOUNDARY)
    val stencil = create1DMultiInputLambda(randomData.length, weights, 3,1, 1,1)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
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
              toGlobal(MapSeqOrMapSeqUnroll(id)) o
                ReduceSeqOrReduceSeqUnroll(add, 0.0f) o
                MapSeqOrMapSeqUnroll(mult) $
                Zip(weights, neighbourhood)
            })
          ) o Slide(size, step) o MapLcl(toLocal(id)) $ tile

        )) o Slide(tileSize, tileStep) o Pad(left, right, BOUNDARY) $ input
      }
    )
  }

  @Test def tiling1D(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)

    val stencil = createTiled1DStencilLambda(weights, 3,1, 4,2, 1,1)
    val newLambda = create1DStencilLambda(weights, 3,1, 1,1)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    val (gold: Array[Float], _) = Execute(randomData.length)(newLambda, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def tiling1DBiggerTiles(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)

    val stencil = createTiled1DStencilLambda(weights, 3,1, 18,16, 1,1)
    val newLambda = create1DStencilLambda(weights, 3,1, 1,1)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    val (gold: Array[Float], _) = Execute(randomData.length)(newLambda, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
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
      Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

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
      ))) o Slide2D(tileSize1, tileStep1, tileSize2, tileStep2) o Pad2D(top,bottom,left,right, boundary)$ matrix
    }
  )

  def createCopyTilesLambda(size: Int, step: Int,
                            left: Int, right: Int,
                            boundary: Pad.BoundaryFun): Lambda = fun(
    ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
    ArrayType(Float, 9),
    (matrix, weights) => {
      MapWrg(1)(MapWrg(0)(fun( tile =>

        toGlobal(MapLcl(1)(MapLcl(0)(id))) $ tile

      ))) o Slide2D(size, step) o Pad2D(left, right, boundary)$ matrix
    }
  )

  @Test def copyTilesIdentity(): Unit = {
    val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }
    val tiled: Lambda = createCopyTilesLambda(4,2 ,1,1, BOUNDARY)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (false, false))(tiled, data2D, gaussWeights)
    val gold = Utils.scalaGenerate2DNeighbours(data2D, 4,2, 4,2, 1,1,1,1, SCALABOUNDARY).flatten.flatten.flatten

    compareGoldWithOutput(gold, output, runtime)
  }

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Test def tiling2DBiggerTiles(): Unit = {
    val data2D = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val tiled: Lambda = createTiled2DStencil(3,1, 10,8, 1,1, gaussWeights, BOUNDARY)
    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (false, false))(tiled, data2D, gaussWeights)
    val gold = Utils.scalaCompute2DStencil(data2D,3,1,3,1,1,1,1,1, gaussWeights, SCALABOUNDARY)

    compareGoldWithOutput(gold, output, runtime)
  }

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Test def tiled2D9PointStencil(): Unit = {
    val tiled: Lambda = createTiled2DStencil(3,1, 4,2, 1,1, gaussWeights, BOUNDARY)
    run2DStencil(tiled, 3,1, 1,1, gaussWeights, "notUsed", SCALABOUNDARY)
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
      Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

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
        Pad2D(top,bottom,left,right, boundary)$ matrix
    }
  )

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Ignore // falsely classified as not valid because of barriers
  @Test def tiledBlurXTiledLoading(): Unit = {
    val weights = Array(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1).map(_.toFloat)
    val tiled: Lambda = createTiled2DStencilWithTiledLoading(1,1,17,1, 1,1,24,8, 0,0,8,8, weights, Pad.Boundary.Clamp)
    run2DStencil(tiled, 1,1,17,1, 0,0,8,8, weights, "notUsed", scalaClamp)
  }

  /* **********************************************************
     TEMPORAL BLOCKING
 ***********************************************************/
  def createTempAndSpatialBlockingLambda(n: Int, s: Int,
                                         tileSize: Int, tileStep: Int,
                                         l: Int, r: Int, b: Pad.BoundaryFun) = {
    fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        Join() o MapWrg(
          //temporal blocking
          Join() o MapLcl(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o
            Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f)))) o
          //spatial blocking
          Split(2) o
          // create data differently
          Map(Slide(n,s)) o Slide(n+2,1) o Pad(2,2,b) $ input

        //        Join() o MapWrg(
        //          //temporal blocking
        //          Join() o MapLcl(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o
        //            //Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o
        //              Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f)))) o
        //        //spatial blocking
        //        Split(2) o //Slide(2,2) o
        //        //Slide(n,s) o Pad(l,r,b) o
        //          Slide(n,s) o Pad(l,r,b) o
        //            Slide(n,s) o Pad(l,r,b) $ input

        // temp than spatial
        //        MapGlb( fun(tile =>
        //          //temporal blocking
        //          Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o
        //            Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f))) o
        //            Slide(n,s) o /*Pad(l,r,b) o*/
        //            Slide(n,s) /*o Pad(l,r,b)*/ $ tile
        //          // spatial blocking
        //        )) o Slide(tileSize, tileStep) o Pad(l,r,b) $ input
      }
    )
  }

  @Test def tempAndSpatialBlocking1D(): Unit = {
    val weights = Array(1, 1, 1).map(_.toFloat)
    val randomData = Array(0,1,2,3,4,5).map(_.toFloat)
    val length = randomData.length * 2

    val newLambda = create1DStencilLambda(weights, 3,1, 1,1)
    //gold computation
    val (firstIteration: Array[Float], _) = Execute(length, length)(newLambda, randomData, weights)
    //val (gold: Array[Float], runtime3) = Execute(length, length)(newLambda, firstIteration, weights)

    val (secondIteration: Array[Float], _) = Execute(length, length)(newLambda, firstIteration, weights)
    val (_, _) = Execute(length, length)(newLambda, secondIteration, weights)
    //println(gold.mkString(","))

    val stencil = createTempAndSpatialBlockingLambda(3,1, 4,2, 1,1,Pad.Boundary.Clamp)
    val (_, _) = Execute(length,length)(stencil, randomData)
    //println(output.mkString(","))

    //compareGoldWithOutput(gold, output, runtime)
  }

  def createTemporalBlockingUsingTiles1DStencilLambda(weights: Array[Float],
                                                      boundary: Pad.BoundaryFun,
                                                      size: Int, step: Int,
                                                      tileSize: Int, tileStep: Int,
                                                      left: Int, right: Int): Lambda2 = {
    fun(
      ArrayType(Float, SizeVar("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        MapWrg( fun(tile =>
          toGlobal(MapSeqUnroll(id)) o Iterate(2) (fun(localTile =>
            Join() o
              MapLcl(
                fun(neighbourhood => {
                  toLocal(MapSeqUnroll(id)) o
                    ReduceSeqUnroll(fun((acc, y) => {
                      multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
                    }), 0.0f) $
                    Zip(weights, neighbourhood)
                })
              ) o Slide(size, step) $ localTile)) o MapLcl(toLocal(id)) $ tile

        )) o Slide(tileSize, tileStep) o Pad(left, right, boundary) $ input
      }
    )
  }

  def createTemporalBlockingUsingRewriteLambda(b: Pad.BoundaryFun,
                                               n: Int, s: Int,
                                               l: Int, r: Int) = {
    fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {

        //f:      toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)
        //map f:  MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)

        Join() o MapGlb(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o  //first iteration
          Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o   //second iteration
          Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f)))) o  //third iteration
          //Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f))))) o  //fourth iteration
          Slide(n,s) o Pad(l,r,b) o
          Slide(n,s) o Pad(l,r,b) o
          Slide(n,s) o Pad(l,r,b) $ input //o
        //Slide(n,s) o Pad(l,r,b) $ input

        // fused maps - two iterations
        //Join() o
        //MapSeq(
        //          toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o  //f
        //            Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o //map f
        //        Slide(n,s) o Pad(l,r,b) o Slide(n,s) o Pad(l,r,b) $ input

        // before fused map
        //Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o
        //MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o
        //Slide(n,s) o Pad(l,r,b) o Slide(n,s) o Pad(l,r,b) $ input

        //christophes map f: Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f))
        // Pad and Map f vertauscht
        //Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Slide(n,s) o
        //Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o
        //Pad(l,r,b) o Slide(n,s) o Pad(l,r,b) $ input

        // simple two iterations after each other
        //MapSeq(id) o
        //Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Slide(n,s) o Pad(l,r,b) o
        //Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Slide(n,s) o Pad(l,r,b) $ input
      }
    )
  }

  @Test def temporalBlockingUsingRewrite1D(): Unit = {
    val weights = Array(1, 1, 1).map(_.toFloat)
    val randomData = Array(0,1,2,3,4,5).map(_.toFloat)
    val length = randomData.length * 2

    val newLambda = create1DStencilLambda(weights, 3,1, 1,1)
    //gold computation
    val (firstIteration: Array[Float], _) = Execute(length, length)(newLambda, randomData, weights)
    //val (gold: Array[Float], runtime3) = Execute(length, length)(newLambda, firstIteration, weights)

    val (secondIteration: Array[Float], _) = Execute(length, length)(newLambda, firstIteration, weights)
    val (gold: Array[Float], _) = Execute(length, length)(newLambda, secondIteration, weights)

    val stencil = createTemporalBlockingUsingRewriteLambda(BOUNDARY, 3,1, 1,1)
    val (output: Array[Float], runtime) = Execute(length,length)(stencil, randomData)

    compareGoldWithOutput(gold, output, runtime)
  }

  @Ignore // output shrinks because it cant handle padding for multiple iterations
  @Test def temporalBlockingUsingTiles1D(): Unit = {
    val weights = Array(1, 1, 1).map(_.toFloat)
    val randomData = Array(0,1,2,3,4,5).map(_.toFloat)
    val length = randomData.length

    val stencil = createTemporalBlockingUsingTiles1DStencilLambda(weights, Pad.Boundary.Clamp, 3,1, 5,1, 1,1)
    val newLambda = create1DStencilLambda(weights, 3,1, 1,1)
    val (output: Array[Float], runtime) = Execute(length,length)(stencil, randomData, weights)
    val (firstIteration: Array[Float], _) = Execute(length, length)(newLambda, randomData, weights)
    val (gold: Array[Float], _) = Execute(length, length)(newLambda, firstIteration, weights)

    //val (secondIteration: Array[Float], runtime5) = Execute(length, length)(newLambda, firstIteration, weights)
    //val (gold: Array[Float], runtime3) = Execute(length, length)(newLambda, secondIteration, weights)
    compareGoldWithOutput(gold, output, runtime)
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
    val data = Array(0,1,2,3,4,5,6,7).map(_.toFloat)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(lambda, data)
    val gold = Array(0,4,1,5,2,6,3,7).map(_.toFloat)
    println(output.mkString(", "))
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def outputViewGroupTest(): Unit = {
    val lambda = fun(
      ArrayType(Float, SizeVar("N")),
      (input) =>
        MapSeq(MapSeq(id)) o Slide(3,1) o MapSeq(id) $ input
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
    val gold = Array(15.0,12.0,13.0,
      3.0,0.0,1.0,
      7.0,4.0,5.0,
      12.0,13.0,14.0,
      0.0,1.0,2.0,
      4.0,5.0,6.0,
      13.0,14.0,15.0,
      1.0,2.0,3.0,
      5.0,6.0,7.0,
      14.0,15.0,12.0,
      2.0,3.0,0.0,
      6.0,7.0,4.0,
      3.0,0.0,1.0,
      7.0,4.0,5.0,
      11.0,8.0,9.0,
      0.0,1.0,2.0,
      4.0,5.0,6.0,
      8.0,9.0,10.0,
      1.0,2.0,3.0,
      5.0,6.0,7.0,
      9.0,10.0,11.0,
      2.0,3.0,0.0,
      6.0,7.0,4.0,
      10.0,11.0,8.0,
      7.0,4.0,5.0,
      11.0,8.0,9.0,
      15.0,12.0,13.0,
      4.0,5.0,6.0,
      8.0,9.0,10.0,
      12.0,13.0,14.0,
      5.0,6.0,7.0,
      9.0,10.0,11.0,
      13.0,14.0,15.0,
      6.0,7.0,4.0,
      10.0,11.0,8.0,
      14.0,15.0,12.0,
      11.0,8.0,9.0,
      15.0,12.0,13.0,
      3.0,0.0,1.0,
      8.0,9.0,10.0,
      12.0,13.0,14.0,
      0.0,1.0,2.0,
      9.0,10.0,11.0,
      13.0,14.0,15.0,
      1.0,2.0,3.0,
      10.0,11.0,8.0,
      14.0,15.0,12.0,
      2.0,3.0,0.0).map(_.toFloat)
    //output.grouped(3).toArray.map(x => println(x.mkString(",")))
    compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
       CUDA SAMPLE CONVOLUTION
  ***********************************************************/
  @Test def convolutionSimple(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      //ArrayType(ArrayType(Float, 4096), 4096),
      ArrayType(Float, 17*17),
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
        ) o Slide2D(17,1, 17,1) o Pad2D(8,8, 8,8, Pad.Boundary.Clamp)$ matrix
      })

    val weights = Array.fill[Float](17*17)(1.0f)

    // testing
    val input = Array.tabulate(256, 256) { (i, j) => i * 256.0f + j }
    val (output: Array[Float], runtime) = Execute(16, 16, 256, 256, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)

    val gold = Utils.scalaCompute2DStencil(input, 17,1, 17,1, 8,8,8,8, weights, scalaClamp)
    compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 16, 4096, 4096, (true, true))(stencil, input, weights)
  }

  @Test def convolutionTiled(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      //ArrayType(ArrayType(Float, 4096), 4096),
      ArrayType(Float, 17*17),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

          MapLcl(1)(MapLcl(0)(
            // stencil computation
            fun(elem => {
              toGlobal(MapSeqUnroll(id)) o
                ReduceSeq(fun((acc, pair) => {
                  val pixel = Get(pair, 0)
                  val weight = Get(pair, 1)
                  multAndSumUp.apply(acc, pixel, weight)
                }), 0.0f) $ Zip(Join() $ elem, weights)
            })
            // create neighbourhoods in tiles
          )) o Slide2D(17,1, 17,1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(32,16, 32,16) o
          Pad2D(8,8, 8,8, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17*17)(1.0f)

    // testing
    val input = Array.tabulate(256, 256) { (i, j) => i * 256.0f + j }
    val (output: Array[Float], runtime) = Execute(32, 8, 256, 256, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)

    val gold = Utils.scalaCompute2DStencil(input, 17,1, 17,1, 8,8,8,8, weights, scalaClamp)
    compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    // idle threads
    //val (output: Array[Float], runtime) = Execute(32, 32, 4096, 4096, (true, true))(stencil, input, weights)
    // blocked loading to local mem
    //val (output: Array[Float], runtime) = Execute(16, 16, 4096, 4096, (true, true))(stencil, input, weights)
  }

  @Test def blurY(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      //ArrayType(ArrayType(Float, 4096), 4096),
      ArrayType(Float, 17),
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
        ) o Slide2D(17,1, 1,1) o Pad2D(8,8, 0,0, Pad.Boundary.Clamp)$ matrix
      })

    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val (output: Array[Float], runtime) = Execute(16, 16, 1024, 1024, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)

    val gold = Utils.scalaCompute2DStencil(input, 17,1, 1,1, 8,8,0,0, weights, scalaClamp)
    compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 16, 4096, 4096, (true, true))(stencil, input, weights)
  }

  @Test def blurYTiled(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      //ArrayType(ArrayType(Float, 4096), 4096),
      ArrayType(Float, 17),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

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
          )) o Slide2D(17,1, 1,1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(80,64, 1,1) o
          Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val (output: Array[Float], runtime) = Execute(1, 4, 1024, 64, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)

    val gold = Utils.scalaCompute2DStencil(input, 17,1, 1,1, 8,8,0,0, weights, scalaClamp)
    compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], runtime) = Execute(1, 8, 4096, 512, (true, true))(stencil, input, weights)
  }

  @Test def blurYTiled2D(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      //ArrayType(ArrayType(Float, 4096), 4096),
      ArrayType(Float, 17),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

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
          )) o Slide2D(17,1, 1,1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(80,64, 16,16) o
          Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val (output: Array[Float], runtime) = Execute(16, 4, 1024, 64, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)

    val gold = Utils.scalaCompute2DStencil(input, 17,1, 1,1, 8,8,0,0, weights, scalaClamp)
    compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 8, 4096, 512, (true, true))(stencil, input, weights)

    // for generating 3k kernel
    //val input = Array.tabulate(3072, 3072) { (i, j) => i * 3072.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 8, 3072, 384, (true, true))(stencil, input, weights)
  }

  @Test def blurYTiled2DTiledLoading(): Unit = {
    val stencil = fun(
      //ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(ArrayType(Float, Cst(1024)), Cst(1024)),
      //ArrayType(ArrayType(Float, 4096), 4096),
      ArrayType(Float, 17),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

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
          )) o Slide2D(17,1, 1,1) o Join() o
            // load to local memory
            toLocal(MapSeqUnroll(MapLcl(1)(MapLcl(0)(id)))) o Split(8) $ tile
          // split tiles into chunks
        ))) o
          // tiling
          Slide2D(80,64, 16,16) o
          Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val (output: Array[Float], runtime) = Execute(16, 8, 1024, 64, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)

    val gold = Utils.scalaCompute2DStencil(input, 17,1, 1,1, 8,8,0,0, weights, scalaClamp)
    compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 8, 4096, 512, (true, true))(stencil, input, weights)

    // for generating 3k kernel
    //val input = Array.tabulate(3072, 3072) { (i, j) => i * 3072.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 8, 3072, 384, (true, true))(stencil, input, weights)
  }

  @Test def blurYTiled2DTransposed(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      //ArrayType(ArrayType(Float, 4096), 4096),
      ArrayType(Float, 17),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

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
          )) o Slide2D(17,1, 1,1) o
            // transposed load
            Transpose() o
            toLocal(MapLcl(0)(MapLcl(1)(id))) o
            Transpose() $ tile
        ))) o
          // tiling
          Slide2D(80,64, 16,16) o
          Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val (output: Array[Float], runtime) = Execute(16, 4, 1024, 64, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)

    val gold = Utils.scalaCompute2DStencil(input, 17,1, 1,1, 8,8,0,0, weights, scalaClamp)
    compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 8, 4096, 512, (true, true))(stencil, input, weights)

  }

  @Test def blurYTiled2DTiledLoadingTransposed(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      //ArrayType(ArrayType(Float, 4096), 4096),
      ArrayType(Float, 17),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

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
          )) o Slide2D(17,1, 1,1) o
            // transposed load
            Transpose() o
            Map(Join()) o
            // tiled loading
            toLocal(MapLcl(0)(MapSeqUnroll(MapLcl(1)(id)))) o
            // split tile into chunks
            Map(Split(8)) o
            Transpose() $ tile
        ))) o
          // tiling
          Slide2D(80,64, 16,16) o
          Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val (output: Array[Float], runtime) = Execute(16, 4, 1024, 64, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)

    val gold = Utils.scalaCompute2DStencil(input, 17,1, 1,1, 8,8,0,0, weights, scalaClamp)
    compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 8, 4096, 512, (true, true))(stencil, input, weights)

  }

  @Ignore // pad is not the right primitive here, just to try things out
  @Test def blurYTiled2DTransposedPadded(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

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
          )) o Slide2D(17,1, 1,1) o
            // transposed load
            Transpose() o
            toLocal(MapLcl(0)(MapLcl(1)(id))) o
            Transpose() o
            // pad to avoid bank conflicts
            Pad(0,1, Pad.Boundary.Clamp) $ tile
        ))) o
          // tiling
          Slide2D(80,64, 16,16) o
          Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing

    val input = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val (output: Array[Float], runtime) = Execute(16, 4, 1024, 64, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)

    val gold = Utils.scalaCompute2DStencil(input, 17,1, 1,1, 8,8,0,0, weights, scalaClamp)
    compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 8, 4096, 512, (true, true))(stencil, input, weights)

  }

  @Test def blurX(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      //ArrayType(ArrayType(Float, 4096), 4096),
      ArrayType(Float, 17),
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
        ) o Slide2D(1,1, 17,1) o Pad2D(0,0, 8,8, Pad.Boundary.Clamp)$ matrix
      })

    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(256, 256) { (i, j) => i * 256.0f + j }
    val (output: Array[Float], runtime) = Execute(16, 16, 256, 256, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)

    val gold = Utils.scalaCompute2DStencil(input, 1,1, 17,1, 0,0,8,8, weights, scalaClamp)
    compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 16, 4096, 4096, (true, true))(stencil, input, weights)
  }

  @Ignore //fix
  @Test def blurXTiled(): Unit = {
    val stencil = fun(
      //ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(ArrayType(Float, 4096), 4096),
      ArrayType(Float, 17),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

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
          )) o Slide2D(1,1, 17,1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(1,1, 144,128) o
          Pad2D(0,0, 8,8, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    //val input = Array.tabulate(3072, 3072) { (i, j) => i * 3072.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 1, 128, 3072, (true, true))(stencil, input, weights)
    //println("Runtime: " + runtime)

    //val gold = Utils.scalaCompute2DStencil(input, 1,1, 17,1, 0,0,8,8, weights, scalaClamp)
    //compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    val (output: Array[Float], runtime) = Execute(16, 1, 512, 4096, (true, true))(stencil, input, weights)
  }

  @Ignore //fix
  @Test def blurXTiled2D(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      //ArrayType(ArrayType(Float, 4096), 4096),
      ArrayType(Float, 17),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

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
          )) o Slide2D(1,1, 17,1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(4,4, 144,128) o
          Pad2D(0,0, 8,8, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val (output: Array[Float], runtime) = Execute(16, 4, 64, 1024, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)

    //val gold = Utils.scalaCompute2DStencil(input, 1,1, 17,1, 0,0,8,8, weights, scalaClamp)
    //compareGoldWithOutput(gold, output, runtime)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 4, 512, 4096, (true, true))(stencil, input, weights)

    // for generating 3k kernel
    //val input = Array.tabulate(3072, 3072) { (i, j) => i * 3072.0f + j }
    //val (output: Array[Float], runtime) = Execute(16, 8, 3072, 384, (true, true))(stencil, input, weights)
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
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

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
          )) o Slide2D(3,1, 3,1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(10,8, 258,256) $ matrix
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
    var input = Array.tabulate(inputSize, inputSize) { (i, j) => (i-haloSize) * (j-haloSize) * 1.0f }
    input(0) = input(0).map((_*0.0f))
    input(inputSize -1) = input(inputSize -1).map(_*0.0f)
    input = input.transpose
    input(0) = input(0).map(_*0.0f)
    input(inputSize -1) = input(inputSize -1).map(_*0.0f)
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
          fun( wgBlock =>
            MapSeq(MapSeq(
              fun( cacheBlock =>
                MapLcl(1)(MapLcl(0)(
                  fun(elem => {
                    toGlobal(MapSeqUnroll(id)) o
                      ReduceSeqUnroll(fun((acc, pair) => {
                        val pixel = Get(pair, 0)
                        val weight = Get(pair, 1)
                        multAndSumUp.apply(acc, pixel, weight)
                      }), 0.0f) $ Zip(Join() $ elem, weights)
                  })
                )) o Slide2D(15,1, 1,1) $ cacheBlock
              ))) o Slide2D(78,64, 8,8) $ wgBlock
          ))) o Slide2D(526,512, 64,64) o Pad2D(7,7, Pad.Boundary.Clamp) $ matrix
      }
    )

    val weights = Array.fill[Float](15)(1.0f)
    val haloSize = 7
    val outputSize = 8192
    val inputSize = 8192

    // create already padded input array with inner elements (i,j) = i * j
    var input = Array.tabulate(inputSize, inputSize) { (i, j) => i+j * 1.0f }
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
     RODINIA HOTSPOT
 ***********************************************************/
  @Test def rodiniaHotspot(): Unit = {
    // p = powerValue, t = heatNbh; userfun has to compute:
    // out = t[0,0] + c(p + y(t[-1,0] + t[1,0] - 2t[0,0]) +
    //                      x(t[0,-1] + t[0,1] - 2t[0,0]) +
    //                      z(a - t[0,0]));
    // a, c, x, y, z are constants
    val addAmbientTemp = UserFun("addAmbientTemp", Array("x", "y"), "{ return x + y + (0.1f * 1.068e-7f * 80.0f); }", Seq(Float, Float), Float)

    // the two outermost dimensions of A and B have to be the same
    // zip matrices elementwise
    val zip2d = \((A,B) =>
       Map( \(tuple => Zip(tuple._0, tuple._1))) $ Zip(A,B)
    )

    // create 16x16 heat tile and 14x14 power tile
    val createTiles = \((heat, power) =>
      zip2d( Slide2D(16,14) o Pad2D(1,1,Pad.Boundary.Clamp) $ heat,
             Slide2D(14,14) $ power)
    )

    // load into local memory and prepare data for single work-item
    // < < coeff, heat> :: [9] , power >
    val prepareData = \((coeff, tiles) =>
      zip2d(
        // first component
        Map(Map( \(heatNbh =>
          Zip(coeff, Join() $ heatNbh)
        ))) o Slide2D(3,1) o
        toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._0,
        // second component
        toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._1)
    )

    // how to compute output using required data:
    val stencil = fun(requiredData => {
      val coeffHeatTuple = requiredData._0
      val powerValue = requiredData._1

      toGlobal(MapSeq(id)) o
      MapSeq( \(x => addAmbientTemp(powerValue, x))) o
      ReduceSeqUnroll(\((acc, next) =>
       multAndSumUp(acc, next._0, next._1)), 0.0f) $ coeffHeatTuple
    })

    val rodinia = fun(
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(ArrayType(Float, 1036), 1036),
      //ArrayType(ArrayType(Float, 8204), 8204),
      //ArrayType(ArrayType(Float, 8204), 8204),
      ArrayType(Float, 9),
      (heat, power, coeff) => {
        MapWrg(1)(MapWrg(0)( \(tiles =>
          MapLcl(1)(MapLcl(0)(stencil)) o prepareData(coeff) $ tiles)
        )) $ createTiles(heat, power)

      }
    )
    /*
    val stencil = fun(
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(Float, 9),
      (heat, power, coeff) => {
        MapWrg(1)(MapWrg(0)(
          fun(tiles => {
            val powerTile = Get(tiles, 0)
            val heatTile = Get(tiles, 1)
            MapLcl(1)(MapLcl(0)(
              fun(nbhs => {
                val powerValue = Get(nbhs, 0) // Float
                val heatNbh = Get(nbhs, 1)    // [[Float]_3]_3
                toGlobal(MapSeq(id)) o
                  MapSeq( \(x => addAmbientTemp(powerValue, x))) o
                  ReduceSeqUnroll(\((acc, next) =>
                    multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() $ heatNbh, coeff)
              })
            )) o
              Split(14) $
              Zip(Join() o toLocal(MapLcl(1)(MapLcl(0)(id))) $ powerTile,
                  Join() o Slide2D(3,1,3,1) o
                           toLocal(MapLcl(1)(MapLcl(0)(id))) $ heatTile)
          })
        )) o
          Split(74) $
          Zip(Join() o Slide2D(14,14) $ power,
              Join() o Slide2D(16,14) o
                Pad2D(1,1,Pad.Boundary.Wrap) $ heat) //actually its mirror
      }
    )
    */

    val heat = Array.tabulate(1036, 1036) { (i, j) => i * 1036.0f + j }
    val power = Array.tabulate(1036, 1036) { (i, j) => i * 1036.0f + j }
    //val heat = Array.tabulate(8204, 8204) { (i, j) => i * 8204.0f + j }
    //val power = Array.tabulate(8204, 8204) { (i, j) => i * 8204.0f + j }
    val x = 0.1f; val y = 0.1f; val z = 1024000; val c = 1.068e-7f
    val coeff = Array(0, c*y, 0, c*x, c*(-2*y-2*x-z+1), c*x, 0, c*y, 0)

    val (output: Array[Float], runtime) = Execute(16,16, 1184, 1184, (true, true))(rodinia, heat, power, coeff)
    //val (output: Array[Float], runtime) = Execute(16,16, 9376, 9376, (true, true))(rodinia, heat, power, coeff)
    println("Runtime: " + runtime)
  }

  /* **********************************************************
      HOTSPOT 3D RODINIA
  ***********************************************************/
  @Test def rodiniaHotspot3D(): Unit = {
    @Ignore //fix
    //val hotspot = UserFun("hotspot", "tuple", "{ return tuple_0; }", TupleType(Float, ArrayType(ArrayType(Float, 3),3)), Float)
    // segfaults
    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      (input) => {
        MapSeq(MapGlb(1)(MapGlb(0)( \(nbh =>
          toGlobal(MapSeq(id)) o
            ReduceSeq(add, 0.0f) o Join() o Join() $ nbh)
        ))) o Slide3D(3,1) o Pad3D(1,1,1, Pad.Boundary.Clamp) $ input
      }
    )

    // testing
    val input = Array.tabulate(512, 512, 8) { (i, j, k) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(64,4,1, 512,512,1, (true, true))(stencil, input)
    println("Runtime: " + runtime)
    //println(output.mkString(","))
  }

  // rodinia 3d opt1
  @Test def rodiniaHotspot3DOpt1(): Unit = {
    val zip3d = \((A,B,C) =>
       Map(Map( \(tuple => Zip(tuple._0, tuple._1, tuple._2)))) o Map( \(tuple => Zip(tuple._0, tuple._1, tuple._2))) $ Zip(A,B,C)
    )

    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      (input) => {
        MapSeq(MapGlb(1)(MapGlb(0)( \(nbh =>
          toGlobal(MapSeq( \(acc => add(acc, nbh._2)))) o
          MapSeq( \(acc => add(acc, nbh._1))) o
          ReduceSeqUnroll(add, 0.0f) o Join() o Join() $ nbh._0)
        ))) $ zip3d(
          Slide3D(3,1, 3,1, 1,1) o Pad3D(1,1,0, Pad.Boundary.Clamp) $ input,
          input, // leftshift z - upper element
          input) // rightshift z - lower element
      }
    )

    val input = Array.fill(512)(Array.fill(512)(Array.fill(8)(1.0f)))
    val weights = Array.fill(3)(Array.fill(3)(1.0f))
    val (output: Array[Float], runtime) = Execute(64,4,1, 512,512,8, (true, true))(stencil, input)
    println("Runtime: " + runtime)
    //println(output.mkString(","))
  }

  @Test def rodiniaHotspot3DLocalMemory(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      (input) => {
        MapSeq(MapWrg(1)(MapWrg(0)( \(tiles =>
          MapSeq(MapLcl(1)(MapLcl(0)( \(nbh =>
            toGlobal(MapSeq(id)) o
            ReduceSeq(add, 0.0f) o Join() o Join() $ nbh)))) o
            Slide3D(3,1) o
          toLocal(MapSeq(MapLcl(1)(MapLcl(0)(id)))) $ tiles)
        ))) o Slide3D(66,64, 6,4, 10,10) o Pad3D(1,1,1, Pad.Boundary.Clamp) $ input
      }
    )

    // testing
    val input = Array.tabulate(512, 512, 8) { (i, j, k) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(64,4,1, 512,512,1, (true, true))(stencil, input)
    println("Runtime: " + runtime)
    //println(output.mkString(","))
  }

  // 3d simple
  @Test def simple3d(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      (input) => {
        MapSeq(MapGlb(1)(MapGlb(0)(
          toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join() o Join()
        ))) o Slide3D(3,1, 3,1, 3,1) o Pad3D(1,1,1,Pad.Boundary.Clamp) $ input
      }
    )

    val input = Array.fill(512)(Array.fill(512)(Array.fill(8)(1.0f)))
    val (output: Array[Float], runtime) = Execute(64,4,1, 512,512,8, (true, true))(stencil, input)
    println("Runtime: " + runtime)
    //println(output.mkString(","))
  }
}
/*
val stencil = fun(
ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
ArrayType(ArrayType(ArrayType(Float, 3), 3), 3),
(input, weights) => {
  MapWrg(2)(MapWrg(1)(MapWrg(0)( \(nbh =>
    toGlobal(
    MapLcl(2)( \(planes =>
      MapLcl(1)( \(rows =>
        MapLcl(0)( \(elems =>
           (add.apply(elems._0, elems._1))
        )) $ Zip(rows._0, rows._1)
      )) $ Zip(planes._0, planes._1)
    ))) $ Zip(
      toLocal(MapLcl(2)(MapLcl(1)(MapLcl(0)(id)))) $ weights,
      Slide3D(3,1) o
      toLocal(MapLcl(2)(MapLcl(1)(MapLcl(0)(id)))) $ nbh)
  )))) o Slide3D(4,2) /*o Pad3D(1,1,1,Pad.Boundary.Mirror)*/ $ input
}
)
*/
