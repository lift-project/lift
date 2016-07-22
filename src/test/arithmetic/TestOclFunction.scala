package arithmetic

import apart.arithmetic._
import opencl.generator._
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
  def substitution(): Unit = {
    val M = SizeVar("M")
    val g0 = get_global_size(0, RangeAdd(0,M,1))

    val valueMap = Map[ArithExpr, ArithExpr](M -> 1024)
    val gold = RangeAdd(0,1024,1)

    val substituted = ArithExpr.substitute(g0, valueMap)

    assertTrue(substituted.isInstanceOf[OclFunction])
    val g1 = substituted.asInstanceOf[OclFunction]
    assertEquals(g0.name, g1.name)
    assertEquals(g0.param, g1.param)
    assertEquals(gold, g1.range)
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

  @Test
  def regression0(): Unit = {
    val v_N_5 = Var("N", StartFromRange(1))

    val v_wg_id_71 = Var("wg_id", RangeAdd(get_group_id(1, RangeAdd(0,16,1)),16,16) )
    val v_wg_id_72 = Var("wg_id", RangeAdd(get_group_id(0, RangeAdd(0,8,1)),8,8))
    val v_l_id_94 = Var("l_id", RangeAdd(get_local_id(1, RangeAdd(0,8,1)),8,8))
    val v_i_96 = Var("i", RangeAdd(0,8,1))
    val v_i_97 = Var("i", RangeAdd(0,4,1))
    val v_l_id_95 = Var("l_id", RangeAdd(get_local_id(0, RangeAdd(0,32,1)),32,32))

    val replacements = Map[ArithExpr, ArithExpr](
      v_i_97 -> 3,
      v_l_id_94 -> get_local_id(1, RangeAdd(0,8,1)),
      v_l_id_95 -> get_local_id(0, RangeAdd(0,32,1)),
      v_i_96 -> 7
    )

    val gold = 64*v_wg_id_71*v_N_5+128*v_wg_id_72+8*v_l_id_94*v_N_5+v_N_5*v_i_96+32*v_i_97+v_l_id_95

    val actual = (899 + (4 * get_local_id(0, RangeAdd(0, 32, 1)))) % 128 / 4 + (64 * v_wg_id_71 * v_N_5) + (8 * get_local_id(1, RangeAdd(0, 8, 1)) * v_N_5) + (128 * v_wg_id_72) + ((899 + (4 * get_local_id(0, RangeAdd(0, 32, 1)))) / 128 * v_N_5) + ((((899 + (4 * get_local_id(0, RangeAdd(0, 32, 1)))) % 128) % 4) * 32)

    assertEquals(ArithExpr.substitute(gold, replacements), actual)
  }

}
