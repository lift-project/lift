package arithmetic

import apart.arithmetic.{Cst, RangeAdd, Sign, ceil}
import opencl.generator.get_group_id
import org.junit.Assert._
import org.junit.Test


/**
  * @author cdubach
  */
class TestArith {

  @Test
  def minCeilGrpId(): Unit = {
    val grpId = get_group_id(0,new RangeAdd(0,131072,1))
    val e = ceil(grpId)
    val min_e = e.min
    assertEquals(min_e, Cst(0))
  }

  @Test
  def gripIdSign1(): Unit = {
    val grpId = get_group_id(0,new RangeAdd(0,131072,1))
    val s = grpId.sign
    assertEquals(s, Sign.Positive)
  }

  @Test
  def gripIdSign2(): Unit = {
    val grpId = get_group_id(0,new RangeAdd(0,131072,1))
    val s = (-1 * grpId).sign
    assertEquals(s, Sign.Negative)
  }

  @Test
  def gripIdSign3(): Unit = {
    val grpId = get_group_id(0,new RangeAdd(0,131072,1))
    val s = (-1 * grpId /^ 131072).sign
    assertEquals(s, Sign.Negative)
  }



  @Test
  def minCeilGrpIdandMore(): Unit = {
    val grpId = get_group_id(0,new RangeAdd(0,131072,1))
    val e = ceil((512 - grpId) /^ 131072)
    val min_e = e.min
    assertEquals(min_e, Cst(0))
  }

}
