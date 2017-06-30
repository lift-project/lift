package analysis

import ir._
import ir.ast._
import lift.arithmetic.{ArithExpr, Cst, SizeVar}
import opencl.generator.{NDRange, get_global_size}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, toGlobal, toPrivate}
import org.junit.Assert._
import org.junit.Test

class TestControlFlow {

  val N = SizeVar("N")
  val globalSize0 = get_global_size(0)
  val globalSize1 = get_global_size(1)

  val globalsLessThanOne = NDRange(2048,1,1)
  val globalsOne = NDRange(1024,1,1)
  val locals = NDRange(32,1,1)
  val valueMap = collection.immutable.Map[ArithExpr, ArithExpr](N -> 1024)

  @Test
  def simple(): Unit = {
    val f = fun(
     ArrayTypeWSWC(Float, N),
      x => MapGlb(id) $ x
    )

    val counts = ControlFlow(f)

   // for-loop
    assertEquals(N /^ globalSize0, counts.getForBranches())
    assertEquals(Cst(1), counts.getForStatements())
    assertEquals(Cst(0), counts.getIfStatements())
  }

  @Test
  def simpleExactlyOne(): Unit = {
     val f = fun(
     ArrayTypeWSWC(Float, N),
      x => MapGlb(id) $ x
    )

    val counts = ControlFlow(f, locals, globalsOne, valueMap)

    // One iter per thread
    // just a single statement
    assertEquals(Cst(0), counts.getForBranches())
    assertEquals(Cst(0), counts.getForStatements())
    assertEquals(Cst(0), counts.getIfStatements())
  }

  @Test
  def simpleLessThanOne(): Unit = {
     val f = fun(
     ArrayTypeWSWC(Float, N),
      x => MapGlb(id) $ x
    )

    val counts = ControlFlow(f, locals, globalsLessThanOne, valueMap)

    // if statement
    assertEquals(Cst(0), counts.getForBranches())
    assertEquals(Cst(0), counts.getForStatements())
    assertEquals(Cst(1), counts.getIfStatements())
  }

  @Test
  def nestedTwo(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 16), N),
      x => MapGlb(MapSeq(id) o MapSeq(id)) $ x
    )

    val counts = ControlFlow(f)

    // for-loops
    assertEquals(N/^globalSize0 + 32*N/^globalSize0, counts.getForBranches())
    assertEquals(Cst(1) + 2*N/^globalSize0, counts.getForStatements())
    assertEquals(Cst(0), counts.getIfStatements())
  }

  @Test
  def nestedFused(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 16), N),
      x => MapGlb(MapSeq(id o id)) $ x
    )

    val counts = ControlFlow(f)

    // for-loops

    assertEquals(N/^globalSize0 + 16*N/^globalSize0, counts.getForBranches())
    assertEquals(Cst(1) + N/^globalSize0, counts.getForStatements())
    assertEquals(Cst(0), counts.getIfStatements())
  }

  @Test
  def nestedOne(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 16), N),
      x => MapGlb(MapSeq(id)) $ x
    )

    val counts = ControlFlow(f)

    // for-loops
    assertEquals(N/^globalSize0 + 16*N/^globalSize0, counts.getForBranches())
    assertEquals(Cst(1) + N/^globalSize0, counts.getForStatements())
    assertEquals(Cst(0), counts.getIfStatements())
  }

  @Test
  def unrolling(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 16), N),
      x => MapGlb(toGlobal(MapSeq(id)) o toPrivate(MapSeq(id))) $ x
    )

    val counts = ControlFlow(f)

    // for-loops
    assertEquals(N/^globalSize0, counts.getForBranches())
    assertEquals(Cst(1), counts.getForStatements())
    assertEquals(Cst(0), counts.getIfStatements())
  }
}
