package opencl.generator

import java.io.FileInputStream
import java.util.Scanner

import apart.arithmetic.{ArithExpr, Cst, Var, Lookup}
import ir.ArrayType
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}


object TestGroup {
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

class TestGroup {

  val UNROLL = true
  val data = Array.tabulate(5)(_ * 1.0f)
  val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }

  val ReduceSeqOrReduceSeqUnroll = (g: FunDecl, init: Expr) =>
    if (UNROLL)
      ReduceSeqUnroll(g, init)
    else
      ReduceSeq(g, init)

  val MapSeqOrMapSeqUnroll = (g: FunDecl) =>
    if (UNROLL)
      MapSeqUnroll(g)
    else
      MapSeq(g)

  val gaussWeights = Array(0f, 0.12f, 0.08f,
      0.12f, 0.20f, 0.12f,
      0.08f, 0.12f, 0.08f)
  val sobelWeights = Array(-1, -2, -1,
      0, 0, 0,
      1, 2, 1).map(_.toFloat)


  @Test
  def lookupSimplfication(): Unit = {
    val table: Seq[ArithExpr] = Array(0,1,2).map(Cst(_))
    val lookup = Lookup(table, Cst(1), 0)
    assertEquals(table.apply(1), lookup)
  }

  /**
    * Creates simple 1D Group Lambda = Map(Map(id) o Group $ data
    *
    * @param indices specifies how to group the array
    * @return Lambda which groups input using relative indices
    */
  def createSimple1DGroupLambda(indices: Array[Int]): Lambda1 = {
    fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(MapSeqOrMapSeqUnroll(id)) o Group(indices) $ domain
    )
  }

  /**
    * Creates simple 2D Group Lambda = Map(Map(lambda x.Map(Map(id(x)) o Group2D $ data
    *
    * @param indices specifies how to group the array
    * @return Lambda which groups input using relative indices
    */
  def createSimple2DGroupLambda(indices: Array[Int]): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqOrMapSeqUnroll(MapSeqOrMapSeqUnroll(id)) $ neighbours
          ))
        ) o Group2D(indices) $ domain
      }
    )
  }

  /**
    * Creates asymmetric 2D Group Lambda depending on two sets of relative indices
    * specifying the relative columns and rows of interest
    *
    * @param relRows relative row indices
    * @param relCols relative column indices
    * @return Lambda which groups input using relative indices
    */
  def createAsymmetric2DGroupLambda(relRows: Array[Int], relCols: Array[Int]): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeq(MapSeqOrMapSeqUnroll(id)) $ neighbours
          ))
        ) o Group2D(relRows, relCols) $ domain
      }
    )
  }

  /* // Different Syntax
  def createSimple2DGroupLambda2(indices: Array[Int]): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      (domain) => {
        domain :>> Group2D(indices) :>> MapGlb(1)(
          MapGlb(0)(
             MapSeq(
               MapSeq(id)
             )
          )
        )
      }
    )
  }
  */

  /**
    * Executes a given lambda and creates output
    *
    * @param lambda executes the given lambda and applies it to data
    * @return grouped data
    */
  def createGroups1D(lambda: Lambda1, data: Array[Float]): (Array[Float], Double) = {
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(lambda, data)
    (output, runtime)
  }

  def createGroup2D(lambda: Lambda1, data2D: Array[Array[Float]]): (Array[Float], Double) = {
    val (output: Array[Float], runtime) = Execute(data2D.length, data2D.length)(lambda, data2D)
    (output, runtime)
  }

  /**
    * Asserts that gold version equals calculated output
    *
    * @param gold    expected result
    * @param output  calculated result
    * @param runtime runtime
    */
  def compareGoldWithOutput(gold: Array[Float], output: Array[Float], runtime: Double): Unit = {
    println("runtime = " + runtime)
    //println(output.mkString(", "))
    assertArrayEquals(gold, output, 0.00001f)
  }

  /* **********************************************************
      GROUP 1D
   ***********************************************************/
  @Test def checkInformationLossRight(): Unit = {
    val gold = Array(1,2,3,4).map(_.toFloat)
    val indices = Array(1)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(indices), data)
    println(output.mkString(","))
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def checkInformationLossLeft(): Unit = {
    val gold = Array(0,1,2,3).map(_.toFloat)
    val indices = Array(-1)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def checkInformationLossLeftBig(): Unit = {
    val gold = Array(0,1,1,2,2,3).map(_.toFloat)
    val indices = Array(-2,-1)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def checkInformationLossRighBig(): Unit = {
    val gold = Array(1,2,2,3,3,4).map(_.toFloat)
    val indices = Array(1,2)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupLeftCurrentRight(): Unit = {
    val gold = Array(0, 1, 2, 1, 2, 3, 2, 3, 4).map(_.toFloat)
    val indices = Array(-1, 0, 1)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupCurrentWithTwoRightNeighbours(): Unit = {
    val gold = Array(0, 1, 2, 1, 2, 3, 2, 3, 4).map(_.toFloat)
    val indices = Array(0, 1, 2)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupCurrentWithTwoLeftNeighbours(): Unit = {
    val gold = Array(0, 1, 2, 1, 2, 3, 2, 3, 4).map(_.toFloat)
    val indices = Array(-2, -1, 0)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupIdentity(): Unit = {
    val indices = Array(0)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(data, output, runtime)
  }

  @Test def groupLeftGapTwoToTheRight(): Unit = {
    val gold = Array(0, 3, 1, 4).map(_.toFloat)
    val indices = Array(-1, 2)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupTwoLeftTwoRight(): Unit = {
    val data = Array(0, 1, 2, 3, 4, 5).map(_.toFloat)
    val gold = Array(0, 1, 3, 4, 1, 2, 4, 5).map(_.toFloat)
    val indices = Array(-2, -1, 1, 2)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
      GROUP 2D
   ***********************************************************/
  /**
    * Should never fail because it only uses functional compositions of "1D group primitive"
    */
  @Test def group2DIdentity(): Unit = {
    val indices = Array(0)

    val (output: Array[Float], runtime: Double) = createGroup2D(createSimple2DGroupLambda(indices), data2D)
    compareGoldWithOutput(data2D.flatten, output, runtime)
  }

  /**
    * Since the group primitive does not care about "out-of-boundary" elements
    * there is only one possible group to create.
    */
  @Test def group9PointNeighbourhoodIdentity(): Unit = {
    val data2D = Array.tabulate(3, 3) { (i, j) => i * 3.0f + j }
    val indices = Array(-1, 0, 1)

    val (output: Array[Float], runtime: Double) = createGroup2D(createSimple2DGroupLambda(indices), data2D)
    compareGoldWithOutput(data2D.flatten, output, runtime)
  }

  /**
    * Grouping a 2D data structure results in a 4D data structure.
    * Creating a 9 Point Neighbourhood of a 4x4 Array results in a
    * 2x2x3x3 Array. Each gold version below corresponds to a
    * Neighbourhood of one element of the input array
    */
  @Test def group9PointNeighbourhood(): Unit = {
    /* */
    val goldTopLeft = Array.tabulate(3, 3) { (i, j) => i * 3.0f + j + i }.flatten
    val goldTopRight = Array.tabulate(3, 3) { (i, j) => i * 3.0f + 1 + j + i }.flatten
    val goldBottomLeft = goldTopLeft.map(_ + 4) // Top arrays are already flat
    val goldBottomRight = goldTopRight.map(_ + 4)
    val gold = goldTopLeft ++ goldTopRight ++ goldBottomLeft ++ goldBottomRight
    val indices = Array(-1, 0, 1)

    val (output: Array[Float], runtime: Double) = createGroup2D(createSimple2DGroupLambda(indices), data2D)
    compareGoldWithOutput(gold, output, runtime)
  }

  /**
    * Applies a 1D group to a 2D data structure.
    * Grouping left and right neighbours
    */
  @Test def group2DRelColsOnly(): Unit = {
    val gold = Array(0, 1, 2,
      1, 2, 3,
      4, 5, 6,
      5, 6, 7,
      8, 9, 10,
      9, 10, 11,
      12, 13, 14,
      13, 14, 15).map(_.toFloat)
    val relCols = Array(-1, 0, 1)
    val relRows = Array(0)

    val (output: Array[Float], runtime: Double) = createGroup2D(createAsymmetric2DGroupLambda(relRows, relCols), data2D)
    compareGoldWithOutput(gold, output, runtime)
  }

  /**
    * Applyies a 1D group to a 2D data structure.
    * Group top and bottom neighbours
    */
  @Test def group2DRelRowsOnly(): Unit = {
    val gold = Array(0, 4, 8,
      1, 5, 9,
      2, 6, 10,
      3, 7, 11,
      4, 8, 12,
      5, 9, 13,
      6, 10, 14,
      7, 11, 15).map(_.toFloat)
    val relRows = Array(-1, 0, 1)
    val relCols = Array(0)

    val (output: Array[Float], runtime: Double) = createGroup2D(createAsymmetric2DGroupLambda(relRows, relCols), data2D)
    compareGoldWithOutput(gold, output, runtime)
  }

  /**
    * Creates an assymetric grouping of a 2D data structure
    */
  @Test def group2DAssymetric(): Unit = {
    val data2D = Array.tabulate(5, 5) { (i, j) => i * 5.0f + j }
    val gold = Array(0, 3, 15, 18,
      1, 4, 16, 19,
      5, 8, 20, 23,
      6, 9, 21, 24).map(_.toFloat)
    val relRows = Array(-2, 1)
    val relCols = Array(-1, 2)

    val (output: Array[Float], runtime: Double) = createGroup2D(createAsymmetric2DGroupLambda(relRows, relCols), data2D)
    compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
   2D STENCILS WITHOUT PADDING
  ***********************************************************/
  val outputLocation = "../../../../../../Downloads/pgm"
  val lenaPGM = "../../../../../../Downloads/pgm/lena.ascii.pgm"

  /**
    * ensures that resulting pixel value will be greater than 0
    */
  val clamp = UserFun("my_clamp", "i", "{ return (i < 0) ? 0 : i;  }",
    Float, Float)

  /**
    * saves an array of floats as grayscale ascii .pgm image
    *
    * @param name     name of result file
    * @param location specifies where image should be stored
    * @param img      array of pixels
    */
  def savePGM(name: String, location: String, img: Array[Array[Float]]) = {
    val out = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(location, name)))
    out.write(
      s"""|P2
          |${img.length} ${img.head.length}
          |255
          |${img.map(_.map(x => (x /* * 255.0f*/).toInt).mkString("\n")).mkString("\n")}
      """.stripMargin)
    out.close()
  }

  /**
    * Stores the pixel values of the input image in an array of floats
    *
    * @param name name and path of the input picture
    * @return array of floats containing pixel values
    */
  def readInputImage(name: String): (Int, Int, Array[Array[Float]]) = {
    val in = new FileInputStream(lenaPGM)
    val scanner = new Scanner(in, "ASCII")
    scanner.useDelimiter("""\s+#.+\s+|\s+""".r.pattern)
    scanner.nextLine()
    val width = scanner.nextInt()
    val height = scanner.nextInt()
    val max = scanner.nextInt()

    val input = Array.tabulate(width, height)((r, c) => scanner.nextInt()).map(_.map(_/*/ max*/.toFloat))
    scanner.close()
    (width, height, input)
  }

  /**
    * Create lambda expression for simple stencil application without padding
    *
    * @param neighbours describing neighbours used for symmetricc 2D group creation
    * @param weights    weights used in the stencil
    * @return lambda expression that calculates the stencil
    */
  def createSimpleStencilWithoutPad(neighbours: Array[Int], weights: Array[Float]): Lambda2 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      ArrayType(Float, weights.length),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeq(clamp)) o
              ReduceSeqOrReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Group2D(neighbours) $ matrix
      })
  }

  /**
    * Runs simple stencil 2D stencil application and stores result as .pgm image
    *
    * @param neighbours array of related indices to build symmetric 2D groups
    * @param weights    weights applied to each group
    * @param name       name of the resulting picture
    */
  def runSimple2DStencilWithoutPadding(neighbours: Array[Int], weights: Array[Float], name: String): Unit = {
    try {
      val (width, height, input) = readInputImage(lenaPGM)
      val f = createSimpleStencilWithoutPad(neighbours, weights)

      val (output: Array[Float], runtime) = Execute(1, 1, width, height, (false, false))(f, input, weights)
      println("Runtime: " + runtime)

      val outOfBoundElementsX = Math.abs(Math.min(0, neighbours.min)) + Math.max(0, neighbours.max)
      savePGM(name, outputLocation, output.grouped(width - outOfBoundElementsX).toArray)

    } catch {
      case x: Exception => x.printStackTrace()
    }
  }

  /**
    * Computation of gaussian blur using a .pgm file
    * Uses no padding which causes the output picture to be smaller than the input
    */
  @Test def gaussianBlurNoPad(): Unit = {
    val neighbours = Array(-1, 0, 1)
    runSimple2DStencilWithoutPadding(neighbours, gaussWeights, "gaussNoPad.pgm")
  }

  /**
    * Computation of sobel filter using a .pgm file
    * Uses no padding which causes the output picture to be smaller than the input
    */
  @Test def sobelFilterNoPad(): Unit = {
    val neighbours = Array(-1, 0, 1)
    runSimple2DStencilWithoutPadding(neighbours, sobelWeights, "sobelNoPad.pgm")
  }
}
