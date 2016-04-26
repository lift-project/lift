package arithmetic

import opencl.generator.get_global_size
import org.junit.Test
import org.junit.Assert._

class TestOclFunction {

  @Test
  def equality(): Unit = {
    val g0 = new get_global_size(0)
    val g1 = new get_global_size(1)
    assertFalse(g0.equals(g1))
  }

}
