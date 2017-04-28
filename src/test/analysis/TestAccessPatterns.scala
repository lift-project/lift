package analysis

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapLcl, MapSeq, MapWrg}
import org.junit.Assert._
import org.junit._

class TestAccessPatterns {

  private def findRead(lambda: Lambda) = {
    Expr.visitWithState(None: Option[Expr])(lambda.body, {
      case (FunCall(_:UserFun, arg), _) => Some(arg)
      case (FunCall(_:VectorizeUserFun, arg), _) => Some(arg)
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
    val f = fun(ArrayTypeWSWC(Float, N),
      x => MapGlb(0)(id) $ x
    )

    assertTrue(isReadCoalesced(f))
  }

 @Test
  def coalescedGlb2(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) $ x
    )

   assertTrue(isReadCoalesced(f))
  }

  @Test
  def coalescedGlb3(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(0)(MapGlb(1)(id)) o Transpose() $ x
    )

    assertTrue(isReadCoalesced(f))
  }

  @Test
  def coalescedGlb4(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(0)(MapSeq(id)) o Transpose() $ x
    )

    assertTrue(isReadCoalesced(f))
  }

 @Test
  def coalescedGlb5(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) o Gather(ReorderWithStride(16)) $ x
    )

   assertTrue(isReadCoalesced(f))
  }

  @Test
  def coalescedGlb6(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(0)(MapGlb(1)(id)) o Transpose() o Gather(ReorderWithStride(16)) $ x
    )

    assertTrue(isReadCoalesced(f))
  }

  @Test
  def notCoalescedGlb1(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(0)(MapGlb(1)(id)) $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def notCoalescedGlb2(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) o Transpose() $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def notCoalescedGlb3(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(0)(MapSeq(id)) $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def notCoalescedGlb4(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id) o Gather(ReorderWithStride(16))) $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def notCoalescedGlb5(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(0)(MapGlb(1)(id)) o Gather(ReorderWithStride(16)) o Transpose() $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def strideGlb(): Unit = {
    val f = fun(ArrayTypeWSWC(Float, N),
      x => MapGlb(0)(id) o Gather(ReorderWithStride(16)) $ x
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def simpleVectorised(): Unit = {
    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(VectorizeUserFun(4, id)) o asVector(4) $ _
    )

    assertTrue(isReadCoalesced(f))
  }

  @Test
  def stridedVectorised(): Unit = {
    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(VectorizeUserFun(4, id)) o Gather(ReorderWithStride(32)) o asVector(4) $ _
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def simpleWrgVectorised(): Unit = {
    val f = \(ArrayType(ArrayType(Float, N), N),
      MapWrg(MapLcl(VectorizeUserFun(4, id)) o asVector(4)) $ _
    )

    assertTrue(isReadCoalesced(f))
  }

  @Test
  def stridedWrgVectorised(): Unit = {
    val f = \(ArrayType(ArrayType(Float, N), N),
      MapWrg(
        MapLcl(VectorizeUserFun(4, id)) o Gather(ReorderWithStride(32)) o asVector(4)
      ) $ _
    )

    assertFalse(isReadCoalesced(f))
  }

    @Test
    def simpleWrg2DVectorised(): Unit = {
      val f = \(ArrayType(ArrayType(ArrayType(ArrayType(Float, 32), 32), N), N),
      MapWrg(1)(
        MapWrg(0)(
          MapLcl(1)(
            MapLcl(0)(VectorizeUserFun(4, id)) o asVector(4)
          )
        )
      ) $ _
    )

    assertTrue(isReadCoalesced(f))
  }

  @Test
    def simpleWrg2DVectorisedNotCoalesced(): Unit = {
      val f = \(ArrayType(ArrayType(ArrayType(ArrayType(Float, 32), 32), N), N),
      MapWrg(1)(
        MapWrg(0)(
          MapLcl(0)(
            MapLcl(1)(VectorizeUserFun(4, id)) o asVector(4)
          )
        )
      ) $ _
    )

    assertFalse(isReadCoalesced(f))
  }

  @Test
  def stridedWrg2DVectorised(): Unit = {
    val f = \(ArrayType(ArrayType(ArrayType(ArrayType(Float, 32), 32), N), N),
      MapWrg(1)(
        MapWrg(0)(
          MapLcl(1)(
            MapLcl(0)(VectorizeUserFun(4, id)) o Gather(reverse) o asVector(4)
          )
        )
      ) $ _
    )

    assertFalse(isReadCoalesced(f))
  }

  // TODO: MapWrg + MapLcl
}
