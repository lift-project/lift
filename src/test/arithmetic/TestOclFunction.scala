package arithmetic

import apart.arithmetic.{Cst, RangeAdd}
import opencl.generator.{get_global_size, get_local_id, get_local_size}
import org.junit.Test
import org.junit.Assert._

class TestOclFunction {

  @Test
  def equality(): Unit = {
    val g0 = get_global_size(0)
    val g1 = get_global_size(1)
    assertFalse(g0.equals(g1))
  }

  @Test
  def numValues(): Unit = {
    val range = RangeAdd(get_local_id(0), 1, get_local_size(0))

    val min = range.numVals.min
    val max = range.numVals.max

    assertEquals(Cst(0), min)
    assertEquals(Cst(1), max)
  }

}
