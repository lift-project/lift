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
class TestStencil extends TestGroup {
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
  // currently used for 2D stencils / refactor to run with every boundary condition
  val BOUNDARY = Pad.Boundary.Wrap
  val SCALABOUNDARY: (Int, Int) => Int = scalaWrap

  /**
    * Creates a single neighbourhood for an element in a given array
    * and performs boundary handling if needed
    *
    * @param data input array
    * @param relIndices neighbourhood specification
    * @param idx current element
    * @return array of neighbourhood elements
    */
  def scalaGather1DNeighboursForSpecificElement(data: Array[Float],
                                                relIndices: Array[Int],
                                                idx: Int,
                                                boundary: (Int, Int) => Int = SCALABOUNDARY): Array[Float] = {
    relIndices.map(x => {
      val newIdx = boundary(idx + x, data.length)
      data(newIdx)
    })
  }

  def scala2DNeighbours(data: Array[Array[Float]],
                        l1: Int, c1: Int, r1: Int,
                        l2: Int, c2: Int, r2: Int,
                        r: Int,
                        c: Int,
                        boundary: (Int, Int) => Int = SCALABOUNDARY) = {
    /*todo fix
    val nrRows = data.length
    val nrColumns = data(0).length

    relRows.flatMap(x => {
      var newR = boundary(r + x, nrRows)

      relColumns.map(y => {
        var newC = boundary(c + y, nrRows)
        data(newR)(newC)
      })
    })
    */
    data.flatten
  }

  /**
    * computes 1D stencil for given array of floats, weights, and neighbourhood description
    *
    * @param data input array
    * @param weights weights applied to each neighbourhood
    * @return result of stencil computation
    */
  def scalaCompute1DStencil(data: Array[Float], leftHalo: Int, center: Int, rightHalo: Int, weights: Array[Float]) = {
    //todo find implementation
    /*
    val neighbourhoodArray = data.indices.map(
      x => scalaGather1DNeighboursForSpecificElement(data, neighbours, x))
    neighbourhoodArray.map(_.zip(weights).foldLeft(0.0f)((acc, p) => acc + p._1 * p._2)).toArray
    */
    data
  }

  def scala2DStencil(data: Array[Array[Float]],
                     l1: Int, c1: Int, r1: Int,
                     l2: Int, c2: Int, r2: Int,
                     weights: Array[Float]) = {
    val nrRows = data.length
    val nrColumns = data(0).length

    val neighbours: IndexedSeq[Array[Float]] = (0 until nrRows).flatMap(row =>
      (0 until nrColumns).map(column =>
        scala2DNeighbours(data, l1, c1, r1, l2, c2, r2, row, column))
    )

    val clamp = (x: Float) => if(x < 0.0f) 0.0f else x

    neighbours.map(_.zip(weights).foldLeft(0.0f)((acc, p) => acc + p._1 * p._2)).toArray.map(clamp(_))
  }

  /**
    * Create 1D stencil lambda which pads and groups input and afterwards
    * applies the weights to each neighbourhood
    *
    * @param weights weights applied to neighbourhood
    * @return Lambda which computes stencil application
    */
  def create1DStencilLambda(weights: Array[Float], leftHalo: Int, center: Int, rightHalo: Int): Lambda2 = {
    fun(
      ArrayType(Float, Var("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        val padOffset = Math.max(leftHalo, rightHalo)
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeqOrMapSeqUnroll(id)) o
              ReduceSeqOrReduceSeqUnroll(add, 0.0f) o
              MapSeqOrMapSeqUnroll(mult) $
              Zip(weights, neighbourhood)
          })
        ) o Group(leftHalo, center, rightHalo) o Pad(padOffset, BOUNDARY) $ input
      }
    )
  }

  def createTiled1DStencilLambda(weights: Array[Float], leftHalo: Int, center: Int, rightHalo: Int, tileSize: Int): Lambda2 = {
    fun(
      ArrayType(Float, Var("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        val padOffset = Math.max(leftHalo, rightHalo)
        MapWrg(fun(tile =>
          MapLcl(
          fun(neighbourhood => {
            toGlobal(MapSeqOrMapSeqUnroll(id)) o
              ReduceSeqOrReduceSeqUnroll(add, 0.0f) o
              MapSeqOrMapSeqUnroll(mult) $
              Zip(weights, neighbourhood)
          })
        ) o Group(leftHalo, center, rightHalo) o MapLcl(toLocal(id)) $ tile

        )) o Group(leftHalo, tileSize, rightHalo) o Pad(padOffset, BOUNDARY) $ input
      }
    )
  }

  @Test def outputViewGroupTest(): Unit = {
    val lambda = fun(
      ArrayType(Float, Var("N")),
      (input) =>
      MapSeq(MapSeq(id)) o Group(1,1,1) o MapSeq(id) $ input
    )
    Compile(lambda)
  }

  def createFused1DStencilLambda(weights: Array[Float], leftHalo: Int, center: Int, rightHalo: Int): Lambda2 = {
    fun(
      ArrayType(Float, Var("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        val padOffset = Math.max(leftHalo, rightHalo)
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeqOrMapSeqUnroll(id)) o
              ReduceSeqOrReduceSeqUnroll(fun((acc, y) => {
                multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
              }), 0.0f) $
              Zip(weights, neighbourhood)
          })
        ) o Group(leftHalo, center, rightHalo) o Pad(padOffset, BOUNDARY) $ input
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

  def createPadGroupLambda(boundary: BoundaryFun, leftHalo: Int, center: Int, rightHalo: Int): Lambda1 = {
    fun(
      ArrayType(Float, Var("N")),
      (input) =>
        MapGlb(MapSeqOrMapSeqUnroll(id)) o
          Group(leftHalo, center, rightHalo) o
          Pad(Math.max(leftHalo, rightHalo), boundary)
          $ input
    )
  }

  def testCombinationPadGroup(boundary: BoundaryFun,
                              gold: Array[Float],
                              leftHalo: Int,
                              center: Int,
                              rightHalo: Int): Unit = {
    val f = createPadGroupLambda(boundary, leftHalo, center, rightHalo)
    val (output: Array[Float], runtime) = createGroups1D(f, data)
    compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
      1D STENCILS
   ***********************************************************/
  @Test def groupWrapPaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.Wrap
    val gold = Array(4, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 0).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 1,1,1)
  }

  @Test def groupClampPaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 1,1,1)
  }

  @Test def groupMirrorPaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.Mirror
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 1,1,1)
  }

  @Test def groupMirrorUnsafePaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.MirrorUnsafe
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, 1,1,1)
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

    testCombinationPadGroup(boundary, gold, 2,1,2)
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

    val stencil = createTiled1DStencilLambda(weights, 1,1,1, 2)
    val newLambda = create1DStencilLambda(weights, 1,1,1)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    val (gold: Array[Float], runtime2) = Execute(randomData.length)(newLambda, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def simple3Point1DStencil(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)

    val gold = scalaCompute1DStencil(randomData, 1,1,1, weights)
    val stencil = create1DStencilLambda(weights, 1,1,1)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def simple5Point1DStencil(): Unit = {
    val weights = Array(1, 2, 3, 2, 1).map(_.toFloat)

    val gold = scalaCompute1DStencil(randomData, 2,1,2, weights)
    val stencil = create1DStencilLambda(weights, 2,1,2)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

 /* **********************************************************
      2D STENCILS
   ***********************************************************/
  def createSimple2DStencil(leftHalo: Int, center: Int, rightHalo: Int, weights: Array[Float], boundary: BoundaryFun): Lambda2 = {
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
        ) o Group2D(leftHalo, center, rightHalo) o Pad2D(Math.max(leftHalo, rightHalo), boundary)$ matrix
      })
  }

  def create2DPadGroupLambda(boundary: BoundaryFun, leftHalo: Int, center: Int, rightHalo: Int): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqOrMapSeqUnroll(MapSeqOrMapSeqUnroll(id)) $ neighbours
          ))
        ) o Group2D(leftHalo, center, rightHalo) o Pad2D(Math.max(leftHalo, rightHalo), boundary) $ domain
      }
    )
  }

  def createTiled2DStencil(leftHalo: Int,
                           center: Int,
                           rightHalo: Int,
                           tileSize: Int,
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

          )) o Group2D(leftHalo, center, rightHalo) o toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o Group2D(leftHalo, tileSize / 2, rightHalo) o Pad2D(1, boundary)$ matrix
      }
  )

  def createCopyTilesLambda(boundary: Pad.BoundaryFun): Lambda = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      ArrayType(Float, 9),
      (matrix, weights) => {
        MapWrg(1)(MapWrg(0)(fun( tile =>

         toGlobal(MapLcl(1)(MapLcl(0)(id))) $ tile


        ))) o Group2D(1, 2, 1) o Pad2D(1, boundary)$ matrix
      }
  )

  def run2DStencil(stencil: Lambda2,
                   leftHalo: Int,
                   center: Int,
                   rightHalo: Int,
                   weights: Array[Float],
                   name: String,
                   boundary: BoundaryFun): Unit = {
    try {
      val (width, height, input) = readInputImage(lenaPGM)

      val (output: Array[Float], runtime) = Execute(1, 1, width, height, (false, false))(stencil, input, weights)
      println("Runtime: " + runtime)

      savePGM(name, outputLocation, output.grouped(width).toArray)

      val gold = scala2DStencil(input, leftHalo, center, rightHalo, leftHalo, center, rightHalo, weights)
      compareGoldWithOutput(gold, output, runtime)

    } catch {
      case x: Exception => x.printStackTrace()
    }
  }

  def runCombinedPadGroupTest(leftHalo: Int,
                              center: Int,
                              rightHalo: Int,
                              boundary: BoundaryFun,
                              scalaBoundary: (Int, Int) => Int,
                              data: Array[Array[Float]] = data2D): Unit = {
    val nrRows = data.length
    val nrColumns = data(0).length
    val gold = (0 until nrRows).flatMap(r =>
      (0 until nrColumns).map(c =>
        scala2DNeighbours(data, leftHalo, center, rightHalo, leftHalo, center, rightHalo, r, c, scalaBoundary))
    )

    val lambda = create2DPadGroupLambda(boundary, leftHalo, center, rightHalo)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(lambda, data)
    //compareGoldWithOutput(gold.flatten.toArray, output, runtime)
  }

  @Ignore
  @Test def groupClampPaddedData2D() = {
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp

    runCombinedPadGroupTest(1,1,1, boundary, scalaBoundary)
  }

  @Ignore // takes ages leads to EOF Exceoption on Fuji
  @Test def groupBigClampPaddedData2D() = {
    val data2D = Array.tabulate(10, 10) { (i, j) => i * 10.0f + j }
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp

    runCombinedPadGroupTest(2,1,2, boundary, scalaBoundary, data2D)
  }

  @Ignore // Takes ages!!!
  @Test def groupMirrorPaddedData2D() = {
    val boundary = Pad.Boundary.Mirror
    val scalaBoundary = scalaMirror

    runCombinedPadGroupTest(1,1,1, boundary, scalaBoundary)
  }

  @Ignore
  @Test def groupWrapPaddedData2D() = {
    val boundary = Pad.Boundary.Wrap
    val scalaBoundary = scalaWrap

    runCombinedPadGroupTest(1,1,1, boundary, scalaBoundary)
  }

  @Test def gaussianBlur(): Unit = {
    val stencil = createSimple2DStencil(1,1,1, gaussWeights, BOUNDARY)
    val goldF = run2DStencil(stencil, 1,1,1, gaussWeights, "gauss.pgm", BOUNDARY)

  }

  @Test def tiled2D9PointStencil(): Unit = {
    val tiled: Lambda = createTiled2DStencil(1,1,1, 4, gaussWeights, Pad.Boundary.Wrap)
    val nontiled: Lambda2 = createSimple2DStencil(1,1,1, gaussWeights, Pad.Boundary.Wrap)
    val (width, height, input) = readInputImage(lenaPGM)

    val (output: Array[Float], runtime) = Execute(2, 2, width, height, (false, false))(tiled, input, gaussWeights)
    val (gold: Array[Float], _) = Execute(2, 2, width, height, (false, false))(nontiled, input, gaussWeights)
    println("Runtime: " + runtime)

    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def copyTilesIdentity(): Unit = {
     val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }
     val tiled: Lambda = createCopyTilesLambda(Pad.Boundary.Clamp)

     val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (false, false))(tiled, data2D, gaussWeights)
    //todo create gold version
  }

  @Test def sobelFilter(): Unit = {
    val stencil = createSimple2DStencil(1,1,1, gaussWeights, BOUNDARY)
    run2DStencil(stencil, 1,1,1, sobelWeights, "sobel.pgm", BOUNDARY)
  }

  @Test def gaussianBlur25PointStencil(): Unit = {
    val weights = Array(1, 4, 7, 4, 1,
      4, 16, 26, 16, 4,
      7, 26, 41, 26, 7,
      4, 16, 26, 16, 4,
      1, 4, 7, 4, 1).map(_*0.004219409282700422f)
    val stencil = createSimple2DStencil(2,1,2, weights, BOUNDARY)
    run2DStencil(stencil, 2,1,2, weights, "gauss25.pgm", BOUNDARY)
  }
}
