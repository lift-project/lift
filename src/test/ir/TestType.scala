package ir

import lift.arithmetic._
import opencl.ir._
import org.junit.Assert._
import org.junit.Test

class TestType {

  @Test(expected = classOf[TypeException])
  def halfLength(): Unit = {
    ArrayTypeWSWC(Float, Cst(1)/^Cst(2))
  }

  @Test(expected = classOf[TypeException])
  def negative(): Unit = {
    ArrayTypeWSWC(Float, Cst(-1))
  }

  @Test(expected = classOf[TypeException])
  def nonInt(): Unit = {
    ArrayTypeWSWC(Float, Cst(5)/^Cst(2))
  }

  @Test
  def equality(): Unit = {
    val t1 = ArrayTypeWSWC(ArrayTypeWSWC(Float,32),32)
    val t2 = ArrayTypeWSWC(ArrayTypeWSWC(Float,32),32)
    assertEquals(t1, t2)

    assertNotEquals(t1, ArrayTypeWC(ArrayTypeWSWC(Float, 32), 32))
    assertNotEquals(t1, ArrayTypeWS(ArrayTypeWSWC(Float, 32), 32))
    assertNotEquals(t1, ArrayTypeWSWC(ArrayTypeWS(Float, 32), 32))
    assertNotEquals(t1, ArrayTypeWSWC(ArrayTypeWC(Float, 32), 32))
  }

  @Test
  def tupleAlignment(): Unit = {
    val tests = Seq(
      (TupleType(Float4, Float4), (16, 2)),
      (TupleType(Bool, Float4), (16, 2)),
      (TupleType(Int, TupleType(Bool, Float)), (4, 3)),
      (TupleType(Float, TupleType(Float2, Int)), (8, 3)),
      (TupleType(TupleType(Bool, Bool), TupleType(Int2, Bool)), (8, 4))
    )

    tests.foreach({
      case (tt, (expectedBaseSize, expectedNb)) =>
        val (baseSize, nb) = tt.alignment
        assertEquals(s"Base size of $tt", expectedBaseSize, baseSize.eval)
        assertEquals(s"Nb of $tt", expectedNb, nb.eval)
    })
  }
}
