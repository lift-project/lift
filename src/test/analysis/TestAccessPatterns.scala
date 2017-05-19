package analysis

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern._
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

  private def findWrite(lambda: Lambda) = {
    Expr.visitWithState(None: Option[Expr])(lambda.body, {
      case (call@FunCall(_:UserFun, _), _) => Some(call)
      case (call@FunCall(_:VectorizeUserFun, _), _) => Some(call)
      case (_, read) => read
    }).get
  }

  private def isWriteCoalesced(lambda: Lambda) = {
    val (_, writes) = AccessPatterns(lambda)()
    val view = findWrite(lambda)

    writes(view) == CoalescedPattern
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

  @Test
  def writeCoalescedGlb1(): Unit = {
    val f = fun(ArrayTypeWSWC(Float, N),
      x => MapGlb(0)(id) $ x
    )

    assertTrue(isWriteCoalesced(f))
  }

 @Test
  def writeCoalescedGlb2(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) $ x
    )

   assertTrue(isWriteCoalesced(f))
  }

  @Test
  def writeCoalescedGlb3(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => TransposeW() o MapGlb(0)(MapGlb(1)(id)) $ x
    )

    assertTrue(isWriteCoalesced(f))
  }

  @Test
  def writeCoalescedGlb4(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => TransposeW() o MapGlb(0)(MapSeq(id)) $ x
    )

    assertTrue(isWriteCoalesced(f))
  }

 @Test
  def writeCoalescedGlb5(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) o Gather(ReorderWithStride(16)) $ x
    )

   assertTrue(isWriteCoalesced(f))
  }

  @Test
  def writeCoalescedGlb6(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => Scatter(ReorderWithStride(32)) o TransposeW() o MapGlb(0)(MapGlb(1)(id)) $ x
    )

    assertTrue(isWriteCoalesced(f))
  }

  @Test
  def writeNotCoalescedGlb1(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(0)(MapGlb(1)(id)) $ x
    )

    assertFalse(isWriteCoalesced(f))
  }

  @Test
  def writeNotCoalescedGlb2(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => TransposeW() o MapGlb(1)(MapGlb(0)(id)) $ x
    )

    assertFalse(isWriteCoalesced(f))
  }

  @Test
  def writeNotCoalescedGlb3(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(0)(MapSeq(id)) $ x
    )

    assertFalse(isWriteCoalesced(f))
  }

  @Test
  def writeNotCoalescedGlb4(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(1)(Scatter(ReorderWithStride(32)) o MapGlb(0)(id)) $ x
    )

    assertFalse(isWriteCoalesced(f))
  }

  @Test
  def writeNotCoalescedGlb5(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(0)(MapGlb(1)(id)) o Gather(ReorderWithStride(16)) o Transpose() $ x
    )

    assertFalse(isWriteCoalesced(f))
  }

  @Test
  def writeStrideGlb(): Unit = {
    val f = fun(ArrayTypeWSWC(Float, N),
      x => Scatter(ReorderWithStride(32)) o MapGlb(0)(id) $ x
    )

    assertFalse(isWriteCoalesced(f))
  }

  @Test
  def writeSimpleVectorised(): Unit = {
    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(VectorizeUserFun(4, id)) o asVector(4) $ _
    )

    assertTrue(isWriteCoalesced(f))
  }

  @Test
  def writeStridedVectorised(): Unit = {
    val f = \(ArrayTypeWSWC(Float, N),
      Scatter(ReorderWithStride(32)) o MapGlb(VectorizeUserFun(4, id)) o asVector(4) $ _
    )

    assertFalse(isWriteCoalesced(f))
  }

  @Test
  def writeSimpleWrgVectorised(): Unit = {
    val f = \(ArrayType(ArrayType(Float, N), N),
      MapWrg(MapLcl(VectorizeUserFun(4, id)) o asVector(4)) $ _
    )

    assertTrue(isWriteCoalesced(f))
  }

  @Test
  def writeStridedWrgVectorised(): Unit = {
    val f = \(ArrayType(ArrayType(Float, N), N),
      MapWrg(
        Scatter(ReorderWithStride(32)) o MapLcl(VectorizeUserFun(4, id)) o asVector(4)
      ) $ _
    )

    assertFalse(isWriteCoalesced(f))
  }

    @Test
    def writeSimpleWrg2DVectorised(): Unit = {
      val f = \(ArrayType(ArrayType(ArrayType(ArrayType(Float, 32), 32), N), N),
      MapWrg(1)(
        MapWrg(0)(
          MapLcl(1)(
            MapLcl(0)(VectorizeUserFun(4, id)) o asVector(4)
          )
        )
      ) $ _
    )

    assertTrue(isWriteCoalesced(f))
  }

  @Test
    def writeSimpleWrg2DVectorisedNotCoalesced(): Unit = {
      val f = \(ArrayType(ArrayType(ArrayType(ArrayType(Float, 32), 32), N), N),
      MapWrg(1)(
        MapWrg(0)(
          MapLcl(0)(
            MapLcl(1)(VectorizeUserFun(4, id)) o asVector(4)
          )
        )
      ) $ _
    )

    assertFalse(isWriteCoalesced(f))
  }

  @Test
  def writeStridedWrg2DVectorised(): Unit = {
    val f = \(ArrayType(ArrayType(ArrayType(ArrayType(Float, 32), 32), N), N),
      MapWrg(1)(
        MapWrg(0)(
          MapLcl(1)(
            Scatter(reverse) o MapLcl(0)(VectorizeUserFun(4, id)) o asVector(4)
          )
        )
      ) $ _
    )

    assertFalse(isWriteCoalesced(f))
  }

  @Test
  def issue99(): Unit = {
    val factory = (variables: Seq[ArithExpr]) => {
      val v_M_0 = variables(0)
      val v_N_1 = variables(1)

      val idfloat = UserFun("idfloat", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
      val idTuple2_float_float = UserFun("idTuple2_float_float", Array("x"), """|{ return x; }""".stripMargin, Seq(TupleType(Float, Float)), TupleType(Float, Float))
      val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
      val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
      fun(ArrayType(ArrayType(Float, v_M_0), v_N_1), ArrayType(Float, 9),(p_0, p_1) => FunCall(MapWrg(0)(fun((p_2) => FunCall(MapLcl(0)(fun((p_3) => FunCall(toGlobal(fun((p_4) => FunCall(MapSeq(fun((p_5) => FunCall(idfloat, p_5))), p_4))), FunCall(MapSeq(fun((p_6) => FunCall(toLocal(fun((p_7) => FunCall(idfloat, p_7))), p_6))), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(fun((p_10) => FunCall(fun((p_11) => FunCall(add, p_8, FunCall(mult, FunCall(Get(0), p_11), FunCall(Get(1), p_11)))), p_10)), FunCall(toPrivate(fun((p_12) => FunCall(idTuple2_float_float, p_12))), p_9)))), FunCall(idfloat, Value("0.0f", Float)), FunCall(Zip(2), p_1, FunCall(Join(), p_3))))))), FunCall(Transpose(), p_2)))), FunCall(Slide(3,1), FunCall(Map(fun((p_13) => FunCall(Slide(3,1), FunCall(Pad(1,1,Pad.Boundary.Clamp), p_13)))), FunCall(Pad(1,1,Pad.Boundary.Clamp), p_0)))))
    }

    val M = SizeVar("M")
    val N = SizeVar("N")

    val f = factory(Seq(M, N))

    val gold = AccessPatternCollection(Seq(Some(UnknownPattern), Some(UnknownPattern)))
    val patterns = AccessPatterns(f)

    val tupleConstructorPattern =
      patterns.getReadPatterns.filter(_._1.mem.isInstanceOf[OpenCLMemoryCollection])

    assertEquals(1, tupleConstructorPattern.size)
    assertEquals(gold, tupleConstructorPattern.values.head)
  }
}
