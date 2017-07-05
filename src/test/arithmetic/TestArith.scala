package arithmetic

import lift.arithmetic._
import opencl.generator.{get_global_id, get_global_size, get_group_id, get_local_id}
import org.junit.Assert._
import org.junit.Test


/**
  * @author cdubach
  */
class TestArith {

  @Test
  def minCeilGrpId(): Unit = {
    val grpId = get_group_id(0, new RangeAdd(0, 131072, 1))
    val e = ceil(grpId)
    val min_e = e.min
    assertEquals(min_e, Cst(0))
  }

  @Test
  def gripIdSign1(): Unit = {
    val grpId = get_group_id(0, new RangeAdd(0, 131072, 1))
    val s = grpId.sign
    assertEquals(s, Sign.Positive)
  }

  @Test
  def gripIdSign2(): Unit = {
    val grpId = get_group_id(0, new RangeAdd(0, 131072, 1))
    val s = (-1 * grpId).sign
    assertEquals(s, Sign.Negative)
  }

  @Test
  def gripIdSign3(): Unit = {
    val grpId = get_group_id(0, new RangeAdd(0, 131072, 1))
    val s = (-1 * grpId /^ 131072).sign
    assertEquals(s, Sign.Negative)
  }


  @Test
  def minCeilGrpIdandMore(): Unit = {
    val grpId = get_group_id(0, new RangeAdd(0, 131072, 1))
    val e = ceil((512 - grpId) /^ 131072)
    val min_e = e.min
    assertEquals(min_e, Cst(0))
  }

  @Test
  def RangeAddMinMax(): Unit = {
    val ra = RangeAdd(get_local_id(0, RangeAdd(0, 128, 1)), 128, 128)
    val min = ra.min.min
    val max = ra.max.max
    assert(min == Cst(0))
    assert(max == Cst(127))
  }

  @Test def simplificationDiv(): Unit = {
    val N = SizeVar("N")
    val v = Var("v", RangeAdd(0, N, 1))
    val result = v / N
    assertEquals(Cst(0), result)
  }

  @Test def simplificationMod(): Unit = {
    val N = SizeVar("N")
    val v = Var("v", RangeAdd(0, N / 4, 1))
    val result = v % (N / 4)
    assertEquals(v, result)
  }

  /*@Test def isPositive(): Unit = {
    val N = Var("N",StartFromRange(2))
    val sign = (N-1).sign
    assertEquals(Sign.Positive, sign)
  }*/

  @Test def TestIsSmaller(): Unit = {
    val expr1: ArithExpr = ?
    val expr2: ArithExpr = 1 + Var("a")
    val ret = ArithExpr.isSmaller(expr1, expr2)
    assertTrue(ret.isEmpty /* answer should be "I don't know" */)
  }

  @Test
  def sortList() : Unit = {

    val v_P_2 : ArithExpr = Var("P", StartFromRange(1))
    val v_N_1 : ArithExpr = Var("N", StartFromRange(1))
    val v_O_3 : ArithExpr = Var("O", StartFromRange(1))
    val v_gl_id_25 : ArithExpr = Var(RangeAdd(get_global_id(25,RangeAdd(0,2,1)),6,2))
    val v_gl_id_27 : ArithExpr = Var(RangeAdd(get_global_id(27,RangeAdd(0,2,1)),6,2))
    val v_gl_id_26 : ArithExpr = Var(RangeAdd(get_global_id(26,RangeAdd(0,2,1)),6,2))
    val listToSort : List[ArithExpr] =List((4*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3)))) / (3)), (2*v_N_1*v_O_3*v_gl_id_25), (2*v_N_1*v_P_2*v_gl_id_25), (2*v_O_3*v_P_2*((v_gl_id_26+((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3)))) / (3))) / (v_N_1)), (v_N_1*v_O_3*v_P_2*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3))) % (3))), (2*v_O_3*((((2+v_P_2+((3+v_P_2) % ((2+v_P_2))))) / ((6+(3*v_P_2)))+((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2)))) % (v_O_3)))) / (v_O_3)), (2*v_O_3*(((2+v_P_2+((3+v_P_2) % ((2+v_P_2))))) / ((2+v_P_2)) % (3))), (4*v_N_1*v_gl_id_25), (4*v_N_1*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3))) % (3))), (2*v_O_3*v_P_2*v_gl_id_25), (2*v_N_1*v_O_3*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3))) % (3))), (2*v_N_1*v_P_2*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3))) % (3))), (2*v_O_3*v_P_2*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3))) % (3))), (4*v_O_3*v_gl_id_25), (4*v_O_3*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3))) % (3))), (4*v_P_2*v_gl_id_25), (2*v_O_3*v_gl_id_26), (2*v_P_2*((((2+v_P_2+((3+v_P_2) % ((2+v_P_2))))) / ((6+(3*v_P_2)))+((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2)))) % (v_O_3)))) / (v_O_3)), (2*v_O_3*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3)))) / (3)), (2*v_P_2*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3)))) / (3)), (4*v_O_3*((v_gl_id_26+((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3)))) / (3))) / (v_N_1)), (2*v_P_2*v_gl_id_26), (4*v_P_2*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3))) % (3))), (2*v_P_2*(((2+v_P_2+((3+v_P_2) % ((2+v_P_2))))) / ((2+v_P_2)) % (3))), (8*v_gl_id_25), (4*((((2+v_P_2+((3+v_P_2) % ((2+v_P_2))))) / ((6+(3*v_P_2)))+((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2)))) % (v_O_3)))) / (v_O_3)), (8*((v_gl_id_26+((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3)))) / (3))) / (v_N_1)), (8*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3))) % (3))), (v_N_1*v_O_3*v_P_2*v_gl_id_25), (v_O_3*v_P_2*v_gl_id_26), (v_O_3*v_P_2*((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3)))) / (3)), (4*v_P_2*((v_gl_id_26+((((v_gl_id_27+((3+v_P_2)) / ((6+(3*v_P_2))))) / (v_O_3)+(((3+v_P_2)) / ((2+v_P_2)) % (3)))) / (3))) / (v_N_1)), (4*v_gl_id_26))
//     List(Cst(6), (2*v_N_2*v_O_1*v_gl_id_44), (2*v_N_2*v_P_0*v_gl_id_44), (2*v_O_1*v_P_0*v_gl_id_44), (2*v_O_1*v_P_0*((v_gl_id_45+(v_gl_id_46) / ((3*v_O_1)))) / (v_N_2)), (2*v_O_1*v_P_0*((v_gl_id_46) / (v_O_1) % (3))), (4*v_O_1*v_gl_id_44), (2*v_O_1*v_gl_id_45), (2*v_O_1*(v_gl_id_46) / ((3*v_O_1))), (2*v_O_1), (2*v_P_0), (2*v_P_0*v_gl_id_45), (4*v_P_0*v_gl_id_44), (v_O_1*v_P_0*v_gl_id_45), (2*v_P_0*(v_gl_id_46) / ((3*v_O_1))), (4*v_O_1*((v_gl_id_45+(v_gl_id_46) / ((3*v_O_1)))) / (v_N_2)), (4*v_P_0*((v_gl_id_46) / (v_O_1) % (3))), (4*v_gl_id_45), (8*v_gl_id_44), (8*((v_gl_id_45+(v_gl_id_46) / ((3*v_O_1)))) / (v_N_2)), (v_N_2*v_O_1*v_P_0*v_gl_id_44), (2*v_N_2*v_O_1*((v_gl_id_46) / (v_O_1) % (3))), (2*v_N_2*v_P_0*((v_gl_id_46) / (v_O_1) % (3))), (v_N_2*v_O_1*v_P_0*((v_gl_id_46) / (v_O_1) % (3))), (4*v_N_2*v_gl_id_44), (4*v_P_0*((v_gl_id_45+(v_gl_id_46) / ((3*v_O_1)))) / (v_N_2)), (4*v_N_2*((v_gl_id_46) / (v_O_1) % (3))), (4*v_O_1*((v_gl_id_46) / (v_O_1) % (3))), (v_O_1*v_P_0*(v_gl_id_46) / ((3*v_O_1))), (v_O_1*v_P_0), (4*(v_gl_id_46) / ((3*v_O_1))), (8*((v_gl_id_46) / (v_O_1) % (3))))
//    listToSort.sortWith(ArithExpr.sort)// with SimplifiedExpr
    Sum(listToSort.sortWith(ArithExpr.sort))

  }

}
