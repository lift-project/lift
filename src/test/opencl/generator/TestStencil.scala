package opencl.generator

import apart.arithmetic.{?, Var}
import ir.ast._
import opencl.executor._
import opencl.ir.pattern._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}
import opencl.ir._
import ir._
import ir.ast.Pad.BoundaryFun

import scala.collection.immutable.IndexedSeq
import scala.util.Random

object TestStencil {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

/**
  * Contains tests which include the combined usage of the group
  * and pad pattern.
  * Inherits from TestGroup to avoid code duplication
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
  val BOUNDARY = Pad.Boundary.Clamp
  val SCALABOUNDARY: (Int, Int) => Int = scalaClamp

  /*
  /**
    * computes 1D stencil for given array of floats, weights, and neighbourhood description
    *
    * @param data input array
    * @param weights weights applied to each neighbourhood
    * @return result of stencil computation
    */
  def scalaCompute1DStencil(data: Array[Float],
                            size: Int, step: Int,
                            left: Int, right: Int,
                            weights: Array[Float]) = {
    val leftPadding = Array.tabulate(left)(x => data(SCALABOUNDARY((x + 1) * -1, data.length))).reverse
    val rightPadding = Array.tabulate(right)(x => data(SCALABOUNDARY(x + data.length, data.length)))
    val paddedInput = leftPadding ++ data ++ rightPadding

    val neighbourhoodArray = paddedInput.sliding(size, step).toArray
    neighbourhoodArray.map(_.zip(weights).foldLeft(0.0f)((acc, p) => acc + p._1 * p._2))
  }

  def scalaCompute2DStencil(data: Array[Array[Float]],
                            size1: Int, step1: Int,
                            size2: Int, step2: Int,
                            left: Int, right: Int,
                            weights: Array[Float]) = {
    val neighbours = scalaGenerate2DNeighbours(data, size1, step1, size2, step2, left, right)
    val result = neighbours.map(x => x.map(y => y.flatten.zip(weights).foldLeft(0.0f)((acc, p) => acc + p._1 * p._2)))
    def clamp(i: Float) = Math.max(0.0f, i)
    result.flatten.map(clamp(_))
  }

  def scalaGenerate2DNeighbours(data: Array[Array[Float]],
                                size1: Int, step1: Int,
                                size2: Int, step2: Int,
                                left: Int, right: Int,
                                boundary: (Int, Int) => Int = SCALABOUNDARY): Array[Array[Array[Array[Float]]]] = {
    //padding
    val topPadding = Array.tabulate(left)(x => data(boundary((x + 1) * -1, data.length))).reverse
    val bottomPadding = Array.tabulate(right)(x => data(boundary(x + data.length, data.length)))
    val verticalPaddedInput = (topPadding ++ data ++ bottomPadding).transpose
    val leftPadding = Array.tabulate(left)(
      x => verticalPaddedInput(
        boundary((x + 1) * -1, verticalPaddedInput.length))).reverse
    val rightPadding = Array.tabulate(right)(
      x => verticalPaddedInput(
        boundary(x + data.length, verticalPaddedInput.length)))
    val paddedInput = (leftPadding ++ verticalPaddedInput ++ rightPadding).transpose
    //paddedInput.map(x => println(x.mkString(",")))

    //sliding
    val firstSlide = paddedInput.sliding(size1, step1).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(size2, step2).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    neighbours
  }
  */

  /**
    * Create 1D stencil lambda which pads and groups input and afterwards
    * applies the weights to each neighbourhood
    *
    * @param weights weights applied to neighbourhood
    * @return Lambda which computes stencil application
    */
  def create1DStencilLambda(weights: Array[Float], size: Int, step: Int, left: Int, right: Int): Lambda2 = {
    fun(
      ArrayType(Float, Var("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeqOrMapSeqUnroll(id)) o
              ReduceSeqOrReduceSeqUnroll(add, 0.0f) o
              MapSeqOrMapSeqUnroll(mult) $
              Zip(weights, neighbourhood)
          })
        ) o Slide(size, step) o Pad(left, right, BOUNDARY) $ input
      }
    )
  }

  def createTiled1DStencilLambda(weights: Array[Float],
                                 size: Int, step: Int,
                                 tileSize: Int, tileStep: Int,
                                 left: Int, right: Int): Lambda2 = {
    fun(
      ArrayType(Float, Var("N")),
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

  @Test def outputViewGroupTest(): Unit = {
    val lambda = fun(
      ArrayType(Float, Var("N")),
      (input) =>
      MapSeq(MapSeq(id)) o Slide(3,1) o MapSeq(id) $ input
    )
    Compile(lambda)
  }

  def createFused1DStencilLambda(weights: Array[Float],
                                 size: Int, step: Int,
                                 left: Int, right: Int): Lambda2 = {
    fun(
      ArrayType(Float, Var("N")),
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

  // TODO implement iteration for each boundary condition
  /*
  def executeTestForEachBoundaryCondition(neighbours: Array[Int], weights: Array[Float]) = {
    // change function signature to accept additional boundary condition
    val gold = scalaCompute1DStencil(randomData, neighbours, weights)
    val stencil = create1DStencilLambda(weights, neighbours)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
  }
  */

  def createPadGroupLambda(boundary: BoundaryFun,
                           size: Int, step: Int,
                           left: Int, right: Int): Lambda1 = {
    fun(
      ArrayType(Float, Var("N")),
      (input) =>
        MapGlb(MapSeqOrMapSeqUnroll(id)) o
          Slide(size, step) o
          Pad(left, right, boundary)
          $ input
    )
  }

  def testCombinationPadGroup(boundary: BoundaryFun,
                              gold: Array[Float],
                              size: Int, step: Int,
                              left: Int, right: Int): Unit = {
    val f = createPadGroupLambda(boundary, size, step, left, right)
    val (output: Array[Float], runtime) = createGroups1D(f, data)
    compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
      1D STENCILS
   ***********************************************************/
  @Test def groupWrapPaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.Wrap
    val gold = Array(4, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 0).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3,1, 1,1)
  }

  @Test def groupClampPaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3,1, 1,1)
  }

  @Test def groupMirrorPaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.Mirror
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3,1, 1,1)
  }

  @Test def groupMirrorUnsafePaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.MirrorUnsafe
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3,1 ,1,1)
  }

  /* No longer defined
  @Test def groupClampPaddedData(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0, 2, 0, 3, 0, 4, 1, 4, 2, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold)
  }
  */

  @Test def groupBigClampPaddedData(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val neighbours = Array(-2, -1, 0, 1, 2)
    val gold = Array(0,0,0,1,2,
      0,0,1,2,3,
      0,1,2,3,4,
      1,2,3,4,4,
      2,3,4,4,4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 5,1, 2,2)
  }
 /* No longer defined
  @Test def groupMirrorPaddedData(): Unit = {
    val boundary = Pad.Boundary.Mirror
    val gold = Array(1, 2, 0, 3, 0, 4, 1, 4, 2, 3).map(_.toFloat)

    testCombinationPadGroup(boundary, gold)
  }

  @Test def groupWrapPaddedData(): Unit = {
    val boundary = Pad.Boundary.Wrap
    val gold = Array(3, 2, 4, 3, 0, 4, 1, 0, 2, 1).map(_.toFloat)

    testCombinationPadGroup(boundary, gold)
  }
  */

  //todo create scala gold version?
  @Test def tiling1D(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)

    val stencil = createTiled1DStencilLambda(weights, 3,1, 4,2, 1,1)
    val newLambda = create1DStencilLambda(weights, 3,1, 1,1)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    val (gold: Array[Float], runtime2) = Execute(randomData.length)(newLambda, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def createGroupsForOneSidedPadding(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val neighbours = Array(-2, -1, 0)
    val gold = Array(0,0,0, 0,0,1, 0,1,2, 1,2,3, 2,3,4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 3,1, 2,0)
  }

  @Test def simple3Point1DStencil(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)

    val gold = Utils.scalaCompute1DStencil(randomData, 3,1, 1,1, weights, SCALABOUNDARY)
    val stencil = create1DStencilLambda(weights, 3,1, 1,1)
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

 /* **********************************************************
      2D STENCILS
   ***********************************************************/
  def createSimple2DStencil(size: Int, step: Int,
                            left: Int, right: Int,
                            weights: Array[Float], boundary: BoundaryFun): Lambda2 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      ArrayType(Float, weights.length),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqOrMapSeqUnroll(clamp)) o
              ReduceSeqOrReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(size, step) o Pad2D(left, right, boundary)$ matrix
      })
  }

  def create2DPadGroupLambda(boundary: BoundaryFun,
                             size: Int, step: Int,
                             left: Int, right: Int): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqOrMapSeqUnroll(MapSeqOrMapSeqUnroll(id)) $ neighbours
          ))
        ) o Slide2D(size, step) o Pad2D(left, right, boundary) $ domain
      }
    )
  }

  def createTiled2DStencil(size: Int,
                           step: Int,
                           tileSize: Int,
                           tileStep: Int,
                           weights: Array[Float],
                           boundary: Pad.BoundaryFun): Lambda = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      ArrayType(Float, weights.length),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

          MapLcl(1)(MapLcl(0)(
            fun(elem => {
              toGlobal(MapSeqUnroll(clamp)) o
                ReduceSeqUnroll(fun((acc, pair) => {
                  val pixel = Get(pair, 0)
                  val weight = Get(pair, 1)
                  multAndSumUp.apply(acc, pixel, weight)
                }), 0.0f) $ Zip(Join() $ elem, weights)
            })

          )) o Slide2D(size, step) o toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
          //todo fix pad size
        ))) o Slide2D(tileSize, tileStep) o Pad2D(1,1, boundary)$ matrix
      }
  )

  def createCopyTilesLambda(size: Int, step: Int,
                            left: Int, right: Int,
                            boundary: Pad.BoundaryFun): Lambda = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      ArrayType(Float, 9),
      (matrix, weights) => {
        MapWrg(1)(MapWrg(0)(fun( tile =>

         toGlobal(MapLcl(1)(MapLcl(0)(id))) $ tile

        ))) o Slide2D(size, step) o Pad2D(left, right, boundary)$ matrix
      }
  )

  def run2DStencil(stencil: Lambda2,
                   size: Int, step: Int,
                   left : Int, right: Int,
                   weights: Array[Float],
                   name: String,
                   boundary: BoundaryFun): Unit = {
    try {
      //val (width, height, input) = readInputImage(lenaPGM)
      val width = randomData2D(0).length
      val height = randomData2D.length

      val (output: Array[Float], runtime) = Execute(1, 1, width, height, (false, false))(stencil, randomData2D, weights)
      println("Runtime: " + runtime)

      //savePGM(name, outputLocation, output.grouped(width).toArray)

      val gold = Utils.scalaCompute2DStencil(randomData2D, size,step, size,step, left,right, weights, SCALABOUNDARY)
      compareGoldWithOutput(gold, output, runtime)

    } catch {
      case x: Exception => x.printStackTrace()
    }
  }

  @Test def tiled2D9PointStencil(): Unit = {
    val tiled: Lambda = createTiled2DStencil(3,1, 4,2, gaussWeights, BOUNDARY)
    run2DStencil(tiled, 3,1, 1,1, gaussWeights, "notUsed", BOUNDARY)
  }

  def runCombinedPadGroupTest(size: Int, step: Int,
                              left: Int, right: Int,
                              boundary: BoundaryFun,
                              scalaBoundary: (Int, Int) => Int,
                              data: Array[Array[Float]] = data2D): Unit = {
    val gold = Utils.scalaGenerate2DNeighbours(data, size, step, size, step, left,right, scalaBoundary)
    val goldFlat = gold.flatten.flatten.flatten

    val lambda = create2DPadGroupLambda(boundary, size, step, left, right)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(lambda, data)

    compareGoldWithOutput(goldFlat, output, runtime)
  }

  @Ignore
  @Test def groupClampPaddedData2D() = {
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp

    runCombinedPadGroupTest(3,1, 1,1, boundary, scalaBoundary)
  }

  @Ignore // takes ages leads to EOF Exceoption on Fuji
  @Test def groupBigClampPaddedData2D() = {
    val data2D = Array.tabulate(10, 10) { (i, j) => i * 10.0f + j }
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp

    runCombinedPadGroupTest(5,1, 2,2, boundary, scalaBoundary, data2D)
  }

  @Ignore // Takes ages!!!
  @Test def groupMirrorPaddedData2D() = {
    val boundary = Pad.Boundary.Mirror
    val scalaBoundary = scalaMirror

    runCombinedPadGroupTest(3,1, 1,1, boundary, scalaBoundary)
  }

  @Ignore
  @Test def groupWrapPaddedData2D() = {
    val boundary = Pad.Boundary.Wrap
    val scalaBoundary = scalaWrap

    runCombinedPadGroupTest(3,1, 1,1, boundary, scalaBoundary)
  }

  @Test def gaussianBlur(): Unit = {
    val stencil = createSimple2DStencil(3,1, 1,1, gaussWeights, BOUNDARY)
    run2DStencil(stencil, 3,1, 1,1, gaussWeights, "gauss.pgm", BOUNDARY)
  }

  @Test def copyTilesIdentity(): Unit = {
    val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }
    val tiled: Lambda = createCopyTilesLambda(4,2 ,1,1, Pad.Boundary.Clamp)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (false, false))(tiled, data2D, gaussWeights)
    val gold = Utils.scalaGenerate2DNeighbours(data2D, 4,2, 4,2, 1,1, SCALABOUNDARY).flatten.flatten.flatten

    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def sobelFilter(): Unit = {
    val stencil = createSimple2DStencil(3,1, 1,1, gaussWeights, BOUNDARY)
    run2DStencil(stencil, 3,1, 1,1, sobelWeights, "sobel.pgm", BOUNDARY)
  }

  @Ignore //something wrong here
  @Test def gaussianBlur25PointStencil(): Unit = {
    val weights = Array(1, 4, 7, 4, 1,
      4, 16, 26, 16, 4,
      7, 26, 41, 26, 7,
      4, 16, 26, 16, 4,
      1, 4, 7, 4, 1).map(_*0.004219409282700422f)
    val stencil = createSimple2DStencil(5,1, 2,2, weights, BOUNDARY)
    run2DStencil(stencil, 5,1, 2,2, weights, "gauss25.pgm", BOUNDARY)
  }
}
