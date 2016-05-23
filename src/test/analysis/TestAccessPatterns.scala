package analysis

import apart.arithmetic.ArithExpr._
import apart.arithmetic._
import ir._
import ir.ast.{Map => _, _}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq}
import org.junit.Assert._
import org.junit._

class TestAccessPatterns {

  private def findRead(lambda: Lambda) = {
    Expr.visitWithState(None: Option[Expr])(lambda.body, {
      case (FunCall(_:UserFun, arg), _) => Some(arg)
      case (_, read) => read
    }).get
  }

  private def isReadCoalesced(lambda: Lambda) = {
    val (reads, _) = AccessPatterns(lambda)()
    val view = findRead(lambda)

    reads(view) == CoalescedPattern
  }

  val N = SizeVar("N")

  @Test
  def coalescedGlb1(): Unit = {
    val f = fun(ArrayType(Float, N),
      x => MapGlb(0)(id) $ x
    )

    assertTrue(isReadCoalesced(f))
  }

 @Test
  def coalescedGlb2(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) $ x
    )

   assertTrue(isReadCoalesced(f))
  }

  @Test
  def coalescedGlb3(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, N), N),
      x => MapGlb(0)(MapGlb(1)(id)) o Transpose() $ x
    )

    assertTrue(isReadCoalesced(f))
  }

  @Test
  def coalescedGlb4(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, N), N),
      x => MapGlb(0)(MapSeq(id)) o Transpose() $ x
    )

    assertTrue(isReadCoalesced(f))
  }

 @Test
  def coalescedGlb5(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) o Gather(ReorderWithStride(16)) $ x
    )

   assertTrue(isReadCoalesced(f))
  }

  @Test
  def coalescedGlb6(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, N), N),
      x => MapGlb(0)(MapGlb(1)(id)) o Transpose() o Gather(ReorderWithStride(16)) $ x
    )

    assertTrue(isReadCoalesced(f))
  }

  @Test
  def notCoalescedGlb1(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, N), N),
      x => MapGlb(0)(MapGlb(1)(id)) $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def notCoalescedGlb2(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) o Transpose() $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def notCoalescedGlb3(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, N), N),
      x => MapGlb(0)(MapSeq(id)) $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def notCoalescedGlb4(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id) o Gather(ReorderWithStride(16))) $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def notCoalescedGlb5(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, N), N),
      x => MapGlb(0)(MapGlb(1)(id)) o Gather(ReorderWithStride(16)) o Transpose() $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def strideGlb(): Unit = {
    val f = fun(ArrayType(Float, N),
      x => MapGlb(0)(id) o Gather(ReorderWithStride(16)) $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  // TODO: MapWrg + MapLcl
}
