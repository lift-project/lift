package arithmetic

import apart.arithmetic.{Cst, RangeAdd}
import opencl.generator.{get_global_size, get_local_id, get_local_size}
import org.junit.Assert._
import org.junit.{Ignore, Test}

class TestOclFunction {

  @Test
  def equality(): Unit = {
    val g0 = get_global_size(0)
    val g1 = get_global_size(1)
    assertFalse(g0.equals(g1))
  }

  @Ignore
  @Test
  def numValues(): Unit = {
    // TODO: Also see issue #62
    val range = RangeAdd(get_local_id(0), 1, get_local_size(0))

    val min = range.numVals.min
    val max = range.numVals.max

    assertEquals(Cst(0), min)
    assertEquals(Cst(1), max)
  }

}
