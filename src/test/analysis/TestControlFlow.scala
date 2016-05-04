package analysis

import apart.arithmetic.{ArithExpr, Cst, Var}
import ir._
import ir.ast._
import opencl.generator.get_global_size
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, toGlobal, toPrivate}
import org.junit.Assert._
import org.junit.Test

class TestControlFlow {

  val N = Var("N")
  val globalSize0 = new get_global_size(0)
  val globalSize1 = new get_global_size(1)

  val globalsOne = Array[ArithExpr](1024,1,1)
  val localsOne = Array[ArithExpr](32,1,1)
  val valueMapOne = collection.immutable.Map[ArithExpr, ArithExpr](N -> 1024)

  val globalsLessThanOne = Array[ArithExpr](2048,1,1)
  val localsLessThanOne = Array[ArithExpr](32,1,1)
  val valueMapLessThanOne = collection.immutable.Map[ArithExpr, ArithExpr](N -> 1024)

  @Test
  def simple(): Unit = {
    val f = fun(
     ArrayType(Float, N),
      x => MapGlb(id) $ x
    )

    val counts = ControlFlow(f)

   // for-loop
    assertEquals(Cst(1), counts.getForStatements)
    assertEquals(Cst(0), counts.getIfStatements)
  }

  @Test
  def simpleExactlyOne(): Unit = {
     val f = fun(
     ArrayType(Float, N),
      x => MapGlb(id) $ x
    )

    val counts = ControlFlow(f, localsOne, globalsOne, valueMapOne)

    // One iter per thread
    // just a single statement
    assertEquals(Cst(0), counts.getForStatements)
    assertEquals(Cst(0), counts.getIfStatements)
  }

  @Test
  def simpleLessThanOne(): Unit = {
     val f = fun(
     ArrayType(Float, N),
      x => MapGlb(id) $ x
    )

    val counts = ControlFlow(f, localsLessThanOne, globalsLessThanOne, valueMapLessThanOne)

    // if statement
    assertEquals(Cst(0), counts.getForStatements)
    assertEquals(Cst(1), counts.getIfStatements)
  }

  @Test
  def nestedTwo(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, 16), N),
      x => MapGlb(MapSeq(id) o MapSeq(id)) $ x
    )

    val counts = ControlFlow(f)

    // for-loops
    assertEquals(Cst(1) + 2*N/^globalSize0, counts.getForStatements)
    assertEquals(Cst(0), counts.getIfStatements)
  }

  @Test
  def nestedFused(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, 16), N),
      x => MapGlb(MapSeq(id o id)) $ x
    )

    val counts = ControlFlow(f)

    // for-loops
    assertEquals(Cst(1) + N/^globalSize0, counts.getForStatements)
    assertEquals(Cst(0), counts.getIfStatements)
  }

  @Test
  def nestedOne(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, 16), N),
      x => MapGlb(MapSeq(id)) $ x
    )

    val counts = ControlFlow(f)

    // for-loops
    assertEquals(Cst(1) + N/^globalSize0, counts.getForStatements)
    assertEquals(Cst(0), counts.getIfStatements)
  }

  @Test
  def unrolling(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, 16), N),
      x => MapGlb(toGlobal(MapSeq(id)) o toPrivate(MapSeq(id))) $ x
    )

    val counts = ControlFlow(f)

    // for-loops
    assertEquals(Cst(1), counts.getForStatements)
    assertEquals(Cst(0), counts.getIfStatements)
  }
}