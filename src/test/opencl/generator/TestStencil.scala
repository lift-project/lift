package opencl.generator

import apart.arithmetic.Var
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
  override val UNROLL = false
  val randomData = Seq.fill(1024)(Random.nextFloat()).toArray
  // currently used for 2D stencils / refactor to run with every boundary condition
  val BOUNDARY = Pad.Boundary.Clamp
  val SCALABOUNDARY: (Int, Int) => Int = scalaClamp

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
                        relRows: Array[Int],
                        relColumns: Array[Int],
                        r: Int,
                        c: Int,
                        boundary: (Int, Int) => Int = SCALABOUNDARY) = {
    val nrRows = data.length
    val nrColumns = data(0).length

    relRows.flatMap(x => {
      var newR = boundary(r + x, nrRows)

      relColumns.map(y => {
        var newC = boundary(c + y, nrRows)
        data(newR)(newC)
      })
    })
  }

  /**
    * computes 1D stencil for given array of floats, weights, and neighbourhood description
    *
    * @param data input array
    * @param neighbours specification of neighbourhood
    * @param weights weights applied to each neighbourhood
    * @return result of stencil computation
    */
  def scalaCompute1DStencil(data: Array[Float], neighbours: Array[Int], weights: Array[Float]) = {
    val neighbourhoodArray = data.indices.map(
      x => scalaGather1DNeighboursForSpecificElement(data, neighbours, x))
    neighbourhoodArray.map(_.zip(weights).foldLeft(0.0f)((acc, p) => acc + p._1 * p._2)).toArray
  }

  def scala2DStencil(data: Array[Array[Float]],
                     relRows: Array[Int],
                     relColumns: Array[Int],
                     weights: Array[Float]) = {
    val nrRows = data.length
    val nrColumns = data(0).length

    val neighbours: IndexedSeq[Array[Float]] = (0 until nrRows).flatMap(r =>
      (0 until nrColumns).map(c =>
        scala2DNeighbours(data, relRows, relColumns, r, c))
    )

    val clamp = (x: Float) => if(x < 0.0f) 0.0f else x

    neighbours.map(_.zip(weights).foldLeft(0.0f)((acc, p) => acc + p._1 * p._2)).toArray.map(clamp(_))
  }

  /**
    * Create 1D stencil lambda which pads and groups input and afterwards
    * applies the weights to each neighbourhood
    *
    * @param weights weights applied to neighbourhood
    * @param neighbours specification of neighbourhood
    * @return Lambda which computes stencil application
    */
  def create1DStencilLambda(weights: Array[Float], neighbours: Array[Int]): Lambda2 = {
    fun(
      ArrayType(Float, Var("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        val padOffset = neighbours.map(Math.abs).max
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeqOrMapSeqUnroll(id)) o
              ReduceSeq(add, 0.0f) o
              MapSeqOrMapSeqUnroll(mult) $
              Zip(weights, neighbourhood)
          })
        ) o Group(neighbours) o Pad(padOffset, BOUNDARY) $ input
      }
    )
  }

  /**
    * Computes 1D stencil
    *
    * @param weights weights applied to neighbourhood
    * @param neighbours specification of neighbourhood
    * @return result array
    */
  def computeStencil(weights: Array[Float], neighbours: Array[Int]): (Array[Float], Double) = {
    val stencil = create1DStencilLambda(weights, neighbours)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    (output, runtime)
  }

  // TODO implement iteration for each boundary condition
  def executeTestForEachBoundaryCondition(neighbours: Array[Int], weights: Array[Float]) = {
    // change function signature to accept additional boundary condition
    val gold = scalaCompute1DStencil(randomData, neighbours, weights)
    val (output: Array[Float], runtime: Double) = computeStencil(weights, neighbours)
    compareGoldWithOutput(gold, output, runtime)
  }

  def createPadGroupLambda(boundary: BoundaryFun, neighbours: Array[Int]): Lambda1 = {
    fun(
      ArrayType(Float, Var("N")),
      (input) =>
        MapGlb(MapSeq(id)) o
          Group(neighbours) o
          Pad(neighbours.map(Math.abs).max, boundary)
          $ input
    )
  }

  def testCombinationPadGroup(boundary: BoundaryFun, gold: Array[Float]): Unit = {
    val neighbours = Array(-2, 2)
    val f = createPadGroupLambda(boundary, neighbours)
    val (output: Array[Float], runtime) = createGroups1D(f, data)
    compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
      1D STENCILS
   ***********************************************************/
  @Test def groupClampPaddedData(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0, 2, 0, 3, 0, 4, 1, 4, 2, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold)
  }

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

  @Test def simple3Point1DStencil(): Unit = {
    val weights = Array(1, 2, 1).map(_.toFloat)
    val neighbours = Array(-1, 0, 1)

    val gold = scalaCompute1DStencil(randomData, neighbours, weights)
    val (output: Array[Float], runtime: Double) = computeStencil(weights, neighbours)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def simple5Point1DStencil(): Unit = {
    val weights = Array(1, 2, 3, 2, 1).map(_.toFloat)
    val neighbours = Array(-2, -1, 0, 1, 2)

    val gold = scalaCompute1DStencil(randomData, neighbours, weights)
    val (output: Array[Float], runtime: Double) = computeStencil(weights, neighbours)
    compareGoldWithOutput(gold, output, runtime)
  }

 /* **********************************************************
      2D STENCILS
   ***********************************************************/
  def createSimpleStencil(neighbours: Array[Int], weights: Array[Float], boundary: BoundaryFun): Lambda2 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      ArrayType(Float, weights.length),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeq(clamp)) o
              ReduceSeq(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Group2D(neighbours) o Pad2D(neighbours.map(Math.abs).max, boundary)$ matrix
      })
  }

  def create2DPadGroupLambda(boundary: BoundaryFun, neighbours: Array[Int]): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqOrMapSeqUnroll(MapSeqOrMapSeqUnroll(id)) $ neighbours
          ))
        ) o Group2D(neighbours) o Pad2D(neighbours.map(Math.abs).max, boundary) $ domain
      }
    )
  }

  def run2DStencil(neighbours: Array[Int],
                   weights: Array[Float],
                   name: String,
                   boundary: BoundaryFun): Unit = {
    try {
      val (width, height, input) = readInputImage(lenaPGM)
      val f = createSimpleStencil(neighbours, weights, boundary)

      val (output: Array[Float], runtime) = Execute(1, 1, width, height, (false, false))(f, input, weights)
      println("Runtime: " + runtime)

      savePGM(name, outputLocation, output.grouped(width).toArray)

      val gold = scala2DStencil(input, neighbours, neighbours, weights)
      compareGoldWithOutput(gold, output, runtime)

    } catch {
      case x: Exception => x.printStackTrace()
    }
  }

  def runCombinedPadGroupTest(neighbours: Array[Int], boundary: BoundaryFun, scalaBoundary: (Int, Int) => Int): Unit = {
    val nrRows = data2D.length
    val nrColumns = data2D(0).length
    val gold = (0 until nrRows).flatMap(r =>
      (0 until nrColumns).map(c =>
        scala2DNeighbours(data2D, neighbours, neighbours, r, c, scalaBoundary))
    )

    val lambda = create2DPadGroupLambda(boundary, neighbours)
    val (output: Array[Float], runtime) = Execute(data2D.length, data2D.length)(lambda, data2D)
    compareGoldWithOutput(gold.flatten.toArray, output, runtime)
  }

  @Test def groupClampPaddedData2D() = {
    val neighbours = Array(-1, 0, 1)
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp

    runCombinedPadGroupTest(neighbours, boundary, scalaBoundary)
  }

  @Test def groupMirrorPaddedData2D() = {
    val neighbours = Array(-1, 0, 1)
    val boundary = Pad.Boundary.Mirror
    val scalaBoundary = scalaMirror

    runCombinedPadGroupTest(neighbours, boundary, scalaBoundary)
  }

  @Test def groupWrapPaddedData2D() = {
    val neighbours = Array(-1, 0, 1)
    val boundary = Pad.Boundary.Wrap
    val scalaBoundary = scalaWrap

    runCombinedPadGroupTest(neighbours, boundary, scalaBoundary)
  }

  @Test def gaussianBlur(): Unit = {
    val neighbours = Array(-1, 0, 1)
    run2DStencil(neighbours, gaussWeights, "gauss.pgm", BOUNDARY)
  }

  @Test def sobelFilter(): Unit = {
    val neighbours = Array(-1, 0, 1)
    run2DStencil(neighbours, sobelWeights, "sobel.pgm", BOUNDARY)
  }
}
