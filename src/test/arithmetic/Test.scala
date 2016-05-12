package arithmetic

import apart.arithmetic._
import opencl.generator.{get_group_id, get_local_id}
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

  @Test
  def RangeAddMinMax(): Unit = {
    val ra = RangeAdd(get_local_id(0, RangeAdd(0,128,1)), 128, 128)
    val min = ra.min.min
    val max = ra.max.max
    assert(min == Cst(0))
    assert(max == Cst(127))
  }

  @Test def simplificationDiv(): Unit = {
    val N = SizeVar("N")
    val v = Var("v", RangeAdd(0,N,1))
    val result = v/N
    assertEquals(Cst(0),result)
  }

  @Test def simplificationMod(): Unit = {
    val N = SizeVar("N")
    val v = Var("v", RangeAdd(0,N/4,1))
    val result = v%(N/4)
    assertEquals(Cst(0),result)
  }

  @Test def isPositive(): Unit = {
    val N = Var("N",StartFromRange(2))
    val sign = (N-1).sign
    assertEquals(Sign.Positive, sign)
  }

}
