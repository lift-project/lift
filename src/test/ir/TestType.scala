package ir

import apart.arithmetic._
import opencl.ir._
import org.junit.Test
import org.junit.Assert._

class TestType {

  @Test(expected = classOf[TypeException])
  def halfLength(): Unit = {
    ArrayType(Float, Cst(1)/^Cst(2))
  }

  @Test(expected = classOf[TypeException])
  def negative(): Unit = {
    ArrayType(Float, Cst(-1))
  }

  @Test(expected = classOf[TypeException])
  def nonInt(): Unit = {
    ArrayType(Float, Cst(5)/^Cst(2))
  }

  @Test
  def equality(): Unit = {
    val t1 = ArrayType(ArrayType(Float,32),32)
    val t2 = ArrayType(ArrayType(Float,32),32)
    assertEquals(t1, t2)
  }
}
