package analysis

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.generator.get_global_size
import opencl.ir._
import opencl.ir.ast._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

class TestFunctionCounts {

  private val N = SizeVar("N")
  private val globalSize0 = get_global_size(0)

  @Test
  def simple(): Unit = {

    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(plusOne) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(plusOne))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(plusOne))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(plusOne))
    assertEquals(Cst(0), functionCounts.getAddMultCount())
    assertEquals(Cst(0), functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def vectorised(): Unit = {

    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(VectorizeUserFun(4, plusOne)) o asVector(4) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(Cst(0), functionCounts.getFunctionCount(plusOne))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedCount(plusOne))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getTotalCount(plusOne))
    assertEquals(Cst(0), functionCounts.getAddMultCount())
    assertEquals(Cst(0), functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def builtin(): Unit = {
    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(\(a => fma(a, a, a))) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(fma))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(fma))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(fma))
    assertEquals(Cst(0), functionCounts.getAddMultCount())
    assertEquals(Cst(0), functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def addMult(): Unit = {

    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(\(a => add(mult(a, a), a))) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(add))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(add))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(add))

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(mult))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(mult))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(mult))

    assertEquals(N /^ globalSize0, functionCounts.getAddMultCount())
    assertEquals(Cst(0), functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def addMult2(): Unit = {

    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(\(a => add(a, mult(a, a)))) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(add))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(add))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(add))

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(mult))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(mult))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(mult))

    assertEquals(N /^ globalSize0, functionCounts.getAddMultCount())
    assertEquals(Cst(0), functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def addMultVec(): Unit = {

    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(\(a =>
        VectorizeUserFun(4, add)(VectorizeUserFun(4, mult)(a, a), a)
      )) o asVector(4) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(Cst(0), functionCounts.getFunctionCount(add))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedCount(add))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getTotalCount(add))

    assertEquals(Cst(0), functionCounts.getFunctionCount(mult))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedCount(mult))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getTotalCount(mult))

    assertEquals(Cst(0), functionCounts.getAddMultCount())
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def addMultVec2(): Unit = {

    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(\(a =>
        VectorizeUserFun(4, add)(a, VectorizeUserFun(4, mult)(a, a))
      )) o asVector(4) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(Cst(0), functionCounts.getFunctionCount(add))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedCount(add))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getTotalCount(add))

    assertEquals(Cst(0), functionCounts.getFunctionCount(mult))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedCount(mult))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getTotalCount(mult))

    assertEquals(Cst(0), functionCounts.getAddMultCount())
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def differentObject(): Unit = {

    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(plusOne) $ _
    )

    val x = UserFun("plusOne", "x", "{ return x+1; }", Float, Float)
    val functionCounts = FunctionCounts(f)

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(x))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(x))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(x))
    assertEquals(Cst(0), functionCounts.getAddMultCount())
    assertEquals(Cst(0), functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def arithExprIssue1(): Unit = {
    val factory = (variables: Seq[ArithExpr]) => {
      val v_M_0 = variables(0)
      val v_K_1 = variables(1)
      val v_N_2 = variables(2)
      val v__3 = variables(3)
      val v__4 = variables(4)
      val v__5 = variables(5)
      val v__6 = variables(6)
      val v__7 = variables(7)

      fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1), ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1), (p_0, p_1) => FunCall(Join(), FunCall(MapWrg(1)(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(MapWrg(0)(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_4))))))), FunCall(TransposeW(), FunCall(toGlobal(fun((p_8) => FunCall(MapSeq(fun((p_9) => FunCall(MapLcl(1)(fun((p_10) => FunCall(MapLcl(0)(fun((p_11) => FunCall(MapSeq(fun((p_12) => FunCall(MapSeq(fun((p_13) => FunCall(id, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))), FunCall(ReduceSeq(fun((p_14, p_15) => FunCall(fun((p_16) => FunCall(MapLcl(1)(fun((p_17) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_18) => FunCall(MapSeq(fun((p_19) => p_19)), FunCall(ReduceSeq(fun((p_20, p_21) => FunCall(fun((p_22) => FunCall(MapSeq(fun((p_23) => FunCall(MapSeq(fun((p_24) => FunCall(add, FunCall(Get(0), p_24), FunCall(mult, FunCall(Get(1), p_23), FunCall(Get(1), p_24))))), FunCall(Zip(2), FunCall(Get(0), p_23), FunCall(Get(1), p_22))))), FunCall(Zip(2), p_20, FunCall(Get(0), p_22)))), FunCall(toPrivate(fun((p_25) => FunCall(fun((p_26) => FunCall(Tuple(2), FunCall(MapSeq(fun((p_27) => FunCall(id, p_27))), FunCall(Get(0), p_26)), FunCall(MapSeq(fun((p_28) => FunCall(id, p_28))), FunCall(Get(1), p_26)))), p_25))), p_21)))), FunCall(Get(0), p_18), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p_17)), FunCall(Transpose(), FunCall(Get(1), p_18))))))), FunCall(Zip(2), FunCall(Get(0), p_17), FunCall(Split(v__4), FunCall(Gather(ReorderWithStride(v__3 / v__4)), FunCall(Transpose(), FunCall(Get(1), p_16))))))))), FunCall(Zip(2), p_14, FunCall(Split(v__5), FunCall(Transpose(), FunCall(Get(0), p_16)))))), FunCall(toLocal(fun((p_29) => FunCall(fun((p_30) => FunCall(Unzip(), FunCall(MapLcl(1)(fun((p_31) => FunCall(Tuple(2), FunCall(MapLcl(0)(fun((p_32) => FunCall(id, p_32))), FunCall(Get(0), p_31)), FunCall(MapLcl(0)(fun((p_33) => FunCall(id, p_33))), FunCall(Get(1), p_31))))), FunCall(Zip(2), FunCall(Get(0), p_30), FunCall(Get(1), p_30))))), p_29))), p_15)))), FunCall(MapLcl(1)(fun((p_34) => FunCall(MapLcl(0)(fun((p_35) => FunCall(MapSeq(fun((p_36) => FunCall(MapSeq(fun((p_37) => FunCall(id, p_37))), p_36))), p_35))), p_34))), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3 * 1 /^ v__4), v__6 * 1 /^ v__5))), FunCall(Zip(2), p_2, p_3))))))))), FunCall(Transpose(), FunCall(Map(fun((p_38) => FunCall(Transpose(), p_38))), FunCall(Split(v__7), FunCall(Map(fun((p_39) => FunCall(Split(v__3), p_39))), p_1))))))))), FunCall(Transpose(), FunCall(Map(fun((p_40) => FunCall(Transpose(), p_40))), FunCall(Split(v__7), FunCall(Map(fun((p_41) => FunCall(Split(v__6), p_41))), p_0)))))))
    }

    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")

    val f = factory(Seq[ArithExpr](v_M_0, v_K_1, v_N_2,128,4,8, 64 ,8))

    val localSize = Array[ArithExpr](32, 8, 1)
    val globalSize = Array[ArithExpr](2048 / 4, 2048 / 8, 1)
    val valueMap = collection.immutable.Map[ArithExpr, ArithExpr](
      v_M_0 -> 2048, v_N_2 -> 2048, v_K_1 -> 512)

    val functionCounts = FunctionCounts(f, localSize, globalSize, valueMap)

    val addCount = functionCounts.getFunctionCount(add, exact = true)
    val multCount = functionCounts.getFunctionCount(mult, exact = true)

    assertEquals(16384, addCount.eval)
    assertEquals(16384, multCount.eval)
  }
}
