package opencl.generator

import apart.arithmetic.Var
import ir.ArrayType
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq}
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

  val data = Array.tabulate(5)(_*1.0f)

  /**
    * Creates simple Lambda = Nested Map(Map(id) o Group $ data
    *
    * @param indices specifies how to group the array
    * @return Lambda which groups input using relative indices
    */
  def CreateSimple1DGroupLambda(indices: Array[Int]): Lambda1 = {
    fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(MapSeq(id)) o Group(indices) $ domain
    )
  }


  def CreateSimple2DGroupLambda(indices: Array[Int]): Lambda1 = {
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

  def CreateSimple2DGroupLambda2(indices: Array[Int]): Lambda1 = {
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

  /**
    * Executes the simpleGroupLambda and creates output
    *
    * @param indices specifies how to group the array
    * @return grouped data
    */
  def createGroup(indices: Array[Int]): (Array[Float], Double) = {
    val groupFun = CreateSimple1DGroupLambda(indices)
    val (output: Array[Float], runtime) = Execute(data.length, 10)(groupFun, data)
    (output, runtime)
  }

  /**
    * Asserts that gold version equals calculated output
 *
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

    val (output: Array[Float], runtime: Double) = createGroup(indices)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupCurrentWithTwoRightNeighbours(): Unit = {
    val gold = Array(0.0f, 1.0f, 2.0f, 1.0f, 2.0f, 3.0f, 2.0f, 3.0f, 4.0f)
    val indices = Array(0, 1, 2)

    val (output: Array[Float], runtime: Double) = createGroup(indices)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupCurrentWithTwoLeftNeighbours(): Unit = {
    val gold = Array(0.0f, 1.0f, 2.0f, 1.0f, 2.0f, 3.0f, 2.0f, 3.0f, 4.0f)
    val indices = Array(-2, -1 ,0)

    val (output: Array[Float], runtime: Double) = createGroup(indices)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def groupIdentity(): Unit = {
    val indices = Array(0)

    val (output: Array[Float], runtime: Double) = createGroup(indices)
    compareGoldWithOutput(data, output, runtime)
  }

  @Test def groupLeftGapTwoToTheRight(): Unit = {
    val gold = Array(0.0f, 3.0f, 1.0f, 4.0f)
    val indices = Array(-1, 2)

    val (output: Array[Float], runtime: Double) = createGroup(indices)
    compareGoldWithOutput(gold, output, runtime)
  }

   /* **********************************************************
      GROUP 2D
   ***********************************************************/
}
