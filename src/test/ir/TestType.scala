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
  }
}
