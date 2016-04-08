package opencl.generator

import apart.arithmetic.Var
import ir.ArrayType
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, MapSeqUnroll}
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

/**
  * Created by Bastian Hagedorn on 04.04.16.
  */
class TestGroup {

  val UNROLL = true
  val data = Array.tabulate(5)(_*1.0f)
  val data2D = Array.tabulate(4,4) { (i,j) => i * 4.0f + j }

  /**
    * Creates simple 1D Group Lambda = Map(Map(id) o Group $ data
    *
    * @param indices specifies how to group the array
    * @return Lambda which groups input using relative indices
    */
  def createSimple1DGroupLambda(indices: Array[Int]): Lambda1 = {
    if(UNROLL)
      fun(
        ArrayType(Float, Var("N")),
        (domain) => MapGlb(new MapSeqUnroll(id)) o Group(indices) $ domain
      )
    else
      fun(
        ArrayType(Float, Var("N")),
        (domain) => MapGlb(MapSeq(id)) o Group(indices) $ domain
      )
  }

  /**
    * Creates simple 2D Group Lambda = Map(Map(lambda x.Map(Map(id(x)) o Group2D $ data
    *
    * @param indices specifies how to group the array
    * @return Lambda which groups input using relative indices
    */
  def createSimple2DGroupLambda(indices: Array[Int]): Lambda1 = {
    if(UNROLL)
      fun(
        ArrayType(ArrayType(Float, Var("M")), Var("N")),
       (domain) => {
         MapGlb(1)(
           MapGlb(0)(fun(neighbours =>
             new MapSeqUnroll(new MapSeqUnroll(id)) $ neighbours
           ))
         ) o Group2D(indices) $ domain
       }
     )
    else
      fun(
        ArrayType(ArrayType(Float, Var("M")), Var("N")),
       (domain) => {
         MapGlb(1)(
           MapGlb(0)(fun(neighbours =>
             MapSeq(MapSeq(id)) $ neighbours
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
    if(UNROLL)
      fun(
        ArrayType(ArrayType(Float, Var("M")), Var("N")),
        (domain) => {
          MapGlb(1)(
            MapGlb(0)(fun(neighbours =>
             new MapSeqUnroll(new MapSeqUnroll(id)) $ neighbours
           ))
          ) o Group2D(relRows, relCols) $ domain
       }
     )
    else
      fun(
        ArrayType(ArrayType(Float, Var("M")), Var("N")),
        (domain) => {
          MapGlb(1)(
            MapGlb(0)(fun(neighbours =>
             MapSeq(MapSeq(id)) $ neighbours
           ))
          ) o Group2D(relRows, relCols) $ domain
       }
     )
  }

  /*
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
  def createGroup1D(lambda: Lambda1, data: Array[Float]): (Array[Float], Double) = {
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(lambda, data)
    (output, runtime)
  }

  def create2DGroup(lambda: Lambda1, data2D: Array[Array[Float]]): (Array[Float], Double) = {
    val (output: Array[Float], runtime) = Execute(data2D.length, data2D.length)(lambda, data2D)
    (output, runtime)
  }

  /**
    * Asserts that gold version equals calculated output
    * @param gold expected result
    * @param output calculated result
    * @param runtime runtime
    */
  def compareGoldWithOutput(gold: Array[Float], output: Array[Float], runtime: Double): Unit = {
    println("runtime = " + runtime)
    println(output.mkString(", "))
    assertArrayEquals(gold, output, 0.00001f)
  }

  /* **********************************************************
      GROUP 1D
   ***********************************************************/
  @Test def groupLeftCurrentRight(): Unit = {
    val gold = Array(0.0f, 1.0f, 2.0f, 1.0f, 2.0f, 3.0f, 2.0f, 3.0f, 4.0f)
    val indices = Array(-1, 0 , 1)

    val (output: Array[Float], runtime: Double) = createGroup1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupCurrentWithTwoRightNeighbours(): Unit = {
    val gold = Array(0.0f, 1.0f, 2.0f, 1.0f, 2.0f, 3.0f, 2.0f, 3.0f, 4.0f)
    val indices = Array(0, 1, 2)

    val (output: Array[Float], runtime: Double) = createGroup1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupCurrentWithTwoLeftNeighbours(): Unit = {
    val gold = Array(0.0f, 1.0f, 2.0f, 1.0f, 2.0f, 3.0f, 2.0f, 3.0f, 4.0f)
    val indices = Array(-2, -1 ,0)

    val (output: Array[Float], runtime: Double) = createGroup1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupIdentity(): Unit = {
    val indices = Array(0)

    val (output: Array[Float], runtime: Double) = createGroup1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(data, output, runtime)
  }

  @Test def groupLeftGapTwoToTheRight(): Unit = {
    val gold = Array(0.0f, 3.0f, 1.0f, 4.0f)
    val indices = Array(-1, 2)

    val (output: Array[Float], runtime: Double) = createGroup1D(createSimple1DGroupLambda(indices), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupTwoLeftTwoRight(): Unit = {
    val data = Array(0.0f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val gold = Array(0.0f, 1.0f, 3.0f, 4.0f, 1.0f, 2.0f, 4.0f, 5.0f)
    val indices = Array(-2, -1, 1, 2)

    val (output: Array[Float], runtime: Double) = createGroup1D(createSimple1DGroupLambda(indices), data)
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

     val (output: Array[Float], runtime: Double) = create2DGroup(createSimple2DGroupLambda(indices), data2D)
     compareGoldWithOutput(data2D.flatten, output, runtime)
   }

  /**
    * Since the group primitive does not care about "out-of-boundary" elements
    * there is only one possible group to create.
    */
  @Test def group9PointNeighbourhoodIdentity(): Unit = {
    val data2D = Array.tabulate(3,3) { (i,j) => i * 4.0f + j }
    val indices = Array(-1,0,1)

    val (output: Array[Float], runtime: Double) = create2DGroup(createSimple2DGroupLambda(indices), data2D)
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
    val goldTopLeft = Array.tabulate(3,3) { (i,j) => i * 3.0f + j + i }.flatten
    val goldTopRight = Array.tabulate(3,3) { (i,j) => i * 3.0f + 1 + j + i }.flatten
    val goldBottomLeft = goldTopLeft.map(_+4) // Top arrays are already flat
    val goldBottomRight = goldTopRight.map(_+4)
    val gold = goldTopLeft ++ goldTopRight ++ goldBottomLeft ++ goldBottomRight
    val indices = Array(-1, 0 , 1)

    val (output: Array[Float], runtime: Double) = create2DGroup(createSimple2DGroupLambda(indices), data2D)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def group2DRelColsOnly(): Unit = {
    val gold = Array(0.0f, 1.0f, 2.0f,
                     1.0f, 2.0f, 3.0f,
                     4.0f, 5.0f, 6.0f,
                     5.0f, 6.0f, 7.0f,
                     8.0f, 9.0f, 10.0f,
                     9.0f, 10.0f, 11.0f,
                     12.0f, 13.0f, 14.0f,
                     13.0f, 14.0f, 15.0f)
    val relCols = Array(-1,0,1)
    val relRows = Array(0)

    val (output: Array[Float], runtime: Double) = create2DGroup(createAsymmetric2DGroupLambda(relRows, relCols), data2D)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def group2DRelRowsOnly(): Unit = {
    val gold = Array(0.0f, 4.0f, 8.0f,
                     1.0f, 5.0f, 9.0f,
                     2.0f, 6.0f, 10.0f,
                     3.0f, 7.0f, 11.0f,
                     4.0f, 8.0f, 12.0f,
                     5.0f, 9.0f, 13.0f,
                     6.0f, 10.0f, 14.0f,
                     7.0f, 11.0f, 15.0f)
    val relRows = Array(-1,0,1)
    val relCols = Array(0)

    val (output: Array[Float], runtime: Double) = create2DGroup(createAsymmetric2DGroupLambda(relRows, relCols), data2D)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def group2DAssymetric(): Unit = {
    val data2D = Array.tabulate(5,5) { (i,j) => i * 5.0f + j }
    val gold = Array(0.0f, 3.0f, 15.0f, 18.0f,
                     1.0f, 4.0f, 16.0f, 19.0f,
                     5.0f, 8.0f, 20.0f, 23.0f,
                     6.0f, 9.0f, 21.0f, 24.0f)
    val relRows = Array(-2,1)
    val relCols = Array(-1,2)

    val (output: Array[Float], runtime: Double) = create2DGroup(createAsymmetric2DGroupLambda(relRows, relCols), data2D)
    compareGoldWithOutput(gold, output, runtime)
  }
}
