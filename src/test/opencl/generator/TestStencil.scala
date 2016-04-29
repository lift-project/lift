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
              ReduceSeqOrReduceSeqUnroll(add, 0.0f) o
              MapSeqOrMapSeqUnroll(mult) $
              Zip(weights, neighbourhood)
          })
        ) o Group(neighbours) o Pad(padOffset, BOUNDARY) $ input
      }
    )
  }

  def createFused1DStencilLambda(weights: Array[Float], neighbours: Array[Int]): Lambda2 = {
    fun(
      ArrayType(Float, Var("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        val padOffset = neighbours.map(Math.abs).max
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeqOrMapSeqUnroll(id)) o
              ReduceSeqOrReduceSeqUnroll(fun((acc, y) => {
                multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
              }), 0.0f) $
              Zip(weights, neighbourhood)
          })
        ) o Group(neighbours) o Pad(padOffset, BOUNDARY) $ input
      }
    )
  }

  // TODO implement iteration for each boundary condition
  def executeTestForEachBoundaryCondition(neighbours: Array[Int], weights: Array[Float]) = {
    // change function signature to accept additional boundary condition
    val gold = scalaCompute1DStencil(randomData, neighbours, weights)
    val stencil = create1DStencilLambda(weights, neighbours)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

  def createPadGroupLambda(boundary: BoundaryFun, neighbours: Array[Int]): Lambda1 = {
    fun(
      ArrayType(Float, Var("N")),
      (input) =>
        MapGlb(MapSeqOrMapSeqUnroll(id)) o
          Group(neighbours) o
          Pad(neighbours.map(Math.abs).max, boundary)
          $ input
    )
  }

  def testCombinationPadGroup(boundary: BoundaryFun,
                              gold: Array[Float],
                              neighbours: Array[Int] = Array(-2, 2)): Unit = {
    val f = createPadGroupLambda(boundary, neighbours)
    val (output: Array[Float], runtime) = createGroups1D(f, data)
    compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
      1D STENCILS
   ***********************************************************/
  @Test def groupWrapPaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.Wrap
    val gold = Array(4, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 0).map(_.toFloat)
    val neighbours = Array(-1, 0, 1)

    testCombinationPadGroup(boundary, gold, neighbours)
  }

  @Test def groupClampPaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)
    val neighbours = Array(-1, 0, 1)

    testCombinationPadGroup(boundary, gold, neighbours)
  }

  @Test def groupMirrorPaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.Mirror
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)
    val neighbours = Array(-1, 0, 1)

    testCombinationPadGroup(boundary, gold, neighbours)
  }

  @Test def groupMirrorUnsafePaddedData3Point(): Unit = {
    val boundary = Pad.Boundary.MirrorUnsafe
    val gold = Array(0, 0, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 4).map(_.toFloat)
    val neighbours = Array(-1, 0, 1)

    testCombinationPadGroup(boundary, gold, neighbours)
  }

  @Test def groupClampPaddedData(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val gold = Array(0, 2, 0, 3, 0, 4, 1, 4, 2, 4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold)
  }

  @Test def groupBigClampPaddedData(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val neighbours = Array(-2, -1, 0, 1, 2)
    val gold = Array(0,0,0,1,2,
      0,0,1,2,3,
      0,1,2,3,4,
      1,2,3,4,4,
      2,3,4,4,4).map(_.toFloat)

    testCombinationPadGroup(boundary, gold, neighbours)
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
    val stencil = create1DStencilLambda(weights, neighbours)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def simple5Point1DStencil(): Unit = {
    val weights = Array(1, 2, 3, 2, 1).map(_.toFloat)
    val neighbours = Array(-2, -1, 0, 1, 2)

    val gold = scalaCompute1DStencil(randomData, neighbours, weights)
    val stencil = create1DStencilLambda(weights, neighbours)
    val (output: Array[Float], runtime) = Execute(randomData.length)(stencil, randomData, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

 /* **********************************************************
      2D STENCILS
   ***********************************************************/
  def createSimple2DStencil(neighbours: Array[Int], weights: Array[Float], boundary: BoundaryFun): Lambda2 = {
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

  def run2DStencil(stencil: Lambda2,
                   neighbours: Array[Int],
                   weights: Array[Float],
                   name: String,
                   boundary: BoundaryFun): Unit = {
    try {
      val (width, height, input) = readInputImage(lenaPGM)

      val (output: Array[Float], runtime) = Execute(1, 1, width, height, (false, false))(stencil, input, weights)
      println("Runtime: " + runtime)

      savePGM(name, outputLocation, output.grouped(width).toArray)

      val gold = scala2DStencil(input, neighbours, neighbours, weights)
      compareGoldWithOutput(gold, output, runtime)

    } catch {
      case x: Exception => x.printStackTrace()
    }
  }

  def runCombinedPadGroupTest(neighbours: Array[Int],
                              boundary: BoundaryFun,
                              scalaBoundary: (Int, Int) => Int,
                              data: Array[Array[Float]] = data2D): Unit = {
    val nrRows = data.length
    val nrColumns = data(0).length
    val gold = (0 until nrRows).flatMap(r =>
      (0 until nrColumns).map(c =>
        scala2DNeighbours(data, neighbours, neighbours, r, c, scalaBoundary))
    )

    val lambda = create2DPadGroupLambda(boundary, neighbours)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(lambda, data)
    compareGoldWithOutput(gold.flatten.toArray, output, runtime)
  }

  @Ignore
  @Test def groupClampPaddedData2D() = {
    val neighbours = Array(-1, 0, 1)
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp

    runCombinedPadGroupTest(neighbours, boundary, scalaBoundary)
  }

  @Ignore // takes ages leads to EOF Exceoption on Fuji
  @Test def groupBigClampPaddedData2D() = {
    val neighbours = Array(-2 , -1, 0, 1, 2)
    val data2D = Array.tabulate(10, 10) { (i, j) => i * 10.0f + j }
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp

    runCombinedPadGroupTest(neighbours, boundary, scalaBoundary, data2D)
  }

  @Ignore // Takes ages!!!
  @Test def groupMirrorPaddedData2D() = {
    val neighbours = Array(-1, 0, 1)
    val boundary = Pad.Boundary.Mirror
    val scalaBoundary = scalaMirror

    runCombinedPadGroupTest(neighbours, boundary, scalaBoundary)
  }

  @Ignore
  @Test def groupWrapPaddedData2D() = {
    val neighbours = Array(-1, 0, 1)
    val boundary = Pad.Boundary.Wrap
    val scalaBoundary = scalaWrap

    runCombinedPadGroupTest(neighbours, boundary, scalaBoundary)
  }

  @Test def gaussianBlur(): Unit = {
    val neighbours = Array(-1, 0, 1)
    val stencil = createSimple2DStencil(neighbours, gaussWeights, BOUNDARY)
    run2DStencil(stencil, neighbours, gaussWeights, "gauss.pgm", BOUNDARY)
  }

  @Test def sobelFilter(): Unit = {
    val neighbours = Array(-1, 0, 1)
    val stencil = createSimple2DStencil(neighbours, gaussWeights, BOUNDARY)
    run2DStencil(stencil, neighbours, sobelWeights, "sobel.pgm", BOUNDARY)
  }

  @Test def gaussianBlur25PointStencil(): Unit = {
    val neighbours = Array(-2, -1, 0, 1, 2)
    val weights = Array(1, 4, 7, 4, 1,
      4, 16, 26, 16, 4,
      7, 26, 41, 26, 7,
      4, 16, 26, 16, 4,
      1, 4, 7, 4, 1).map(_*0.004219409282700422f)
    val stencil = createSimple2DStencil(neighbours, weights, BOUNDARY)
    run2DStencil(stencil, neighbours, weights, "gauss25.pgm", BOUNDARY)
  }

   /* **********************************************************
      ITERATIVE 1D STENCILS
   ***********************************************************/
  def createIterative1DStencilLambda(weights: Array[Float],
                                     neighbours: Array[Int],
                                     i: Int,
                                     boundary: BoundaryFun = BOUNDARY): Lambda2 = {
    fun(
      ArrayType(Float, Var("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        val padOffset = neighbours.map(Math.abs).max
        Iterate(i)(
          MapGlb(
           fun(neighbourhood => {
             toGlobal(MapSeqOrMapSeqUnroll(id)) o
               ReduceSeqOrReduceSeqUnroll(add, 0.0f) o
               MapSeqOrMapSeqUnroll(mult) $
               Zip(weights, neighbourhood)
           })
         ) o Group(neighbours) o Pad(padOffset, boundary)
        ) $ input
      }
    )
  }

  @Ignore //do not iterate over mapglb!
  @Test def iterative3Point1DStencil(): Unit = {
    val data = Array(1,1,1,1,1).map(_.toFloat)
    val weights = Array(1, 1, 1).map(_.toFloat)
    val neighbours = Array(-1, 0, 1)

    val gold = Array(27, 27, 27, 27, 27).map(_.toFloat)
    val stencil = createIterative1DStencilLambda(weights, neighbours, 3)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(stencil, data, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Ignore //do not iterate over mapglb
  @Test def iterative5Point1DStencil(): Unit = {
    val data = Array(1,1,1,1,1).map(_.toFloat)
    val weights = Array(1, 1, 1, 1, 1).map(_.toFloat)
    val neighbours = Array(-2, -1, 0, 1, 2)

    val gold = Array(25, 25, 25, 25, 25).map(_.toFloat)
    val stencil = createIterative1DStencilLambda(weights, neighbours, 2)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(stencil, data, weights)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Ignore
  @Test def heatDiffusion1D(): Unit = {
    val data = Array.fill[Float](32)(0.0f)
    data(1023) = 100.0f

    // arbitrary value describing some properties of the physical material
    val alpha = 0.24f
    val beta = 1 - 2*alpha
    val weights = Array(alpha, beta, alpha)

    val neighbours = Array(-1, 0, 1)

    //val gold = Array(27, 27, 27, 27, 27).map(_.toFloat)
    // should pass constant boundary condition
    val stencil = createIterative1DStencilLambda(weights, neighbours, 2, Pad.Boundary.Clamp)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(stencil, data, weights)
    println(output.mkString(","))
    //compareGoldWithOutput(gold, output, runtime)
  }

   /* **********************************************************
      ITERATIVE 2D STENCILS
   ***********************************************************/
  def createIterative2DStencil(neighbours: Array[Int],
                               weights: Array[Float],
                               boundary: BoundaryFun,
                               i: Int): Lambda2 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      ArrayType(Float, weights.length),
      (matrix, weights) => {
        Iterate(i)(
        MapGlb(1)(Join() o
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeq(clamp)) o
              ReduceSeqOrReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Group2D(neighbours) o Pad2D(neighbours.map(Math.abs).max, boundary))$ matrix
      })
  }

  @Ignore // you usually do not iterate over mapglbid
  @Test def iterativeGaussianBlur(): Unit = {
    val neighbours = Array(-2, -1, 0, 1, 2)
    val weights = Array(1, 4, 7, 4, 1,
      4, 16, 26, 16, 4,
      7, 26, 41, 26, 7,
      4, 16, 26, 16, 4,
      1, 4, 7, 4, 1).map(_*0.004219409282700422f)
    val stencil = createIterative2DStencil(neighbours, weights, BOUNDARY, 4)
    run2DStencil(stencil, neighbours, weights, "iterativeGauss25.pgm", BOUNDARY)
  }

   /* **********************************************************
      DEBUGGING
   ***********************************************************/
  @Ignore
  @Test def debugGroupPad2D() = {
    val neighbours = Array(-1, 0, 1)
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp
    val data = data2D
    val paddingSize = neighbours.map(math.abs).max

    //scala
     /*
    val nrRows = data.length
    val nrColumns = data(0).length
    val gold = (0 until nrRows).flatMap(r =>
      (0 until nrColumns).map(c =>
        scala2DNeighbours(data, neighbours, neighbours, r, c, scalaBoundary))
    )
    */

     val dim = 4//Var("N")

    //lift
    val lambda = fun(
      //ArrayType(ArrayType(Float, Var("M")), Var("N")),
      ArrayType(ArrayType(Float, dim), dim),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqOrMapSeqUnroll(MapSeqOrMapSeqUnroll(id)) $ neighbours
          ))
        // Group2D
        ) o Map(
          Map(
            Transpose()
          ) o Group(neighbours) o Transpose()
        ) o Group(neighbours) o
        // Pad2D
          //Transpose() o
          //Pad(paddingSize, boundary) o
          //Transpose() o
          Pad(paddingSize, boundary) $ domain
      }
    )

     val code = Compile(lambda, 4, 4, ?, dim, dim, ?, scala.collection.immutable.Map())

    val (output: Array[Float], runtime) = Execute(data.length, data.length, data.length, data.length, (false, false))(code, lambda, data)
    //compareGoldWithOutput(gold.flatten.toArray, output, runtime)
  }
}
