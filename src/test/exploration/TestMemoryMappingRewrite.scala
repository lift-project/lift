package exploration

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.LongTestsEnabled
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.EnabledMappings
import rewriting.utils.DumpToFile

class TestMemoryMappingRewrite {

  LongTestsEnabled()

  private val K = SizeVar("K")
  private val M = SizeVar("M")
  private val N = SizeVar("N")

  private val v__2 = SizeVar("")
  private val v__3 = SizeVar("")
  private val v__4 = SizeVar("")
  private val v__5 = SizeVar("")
  private val v__6 = SizeVar("")
  private val v__7 = SizeVar("")

  val enabledMappings = EnabledMappings(global0 = true, global01 = true, global10 = false, global012 = false, global210 = false, group0 = true, group01 = false, group10 = true)

  val mmTATiled2DBlocked = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_4))))))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_9, p_10) => FunCall(Map(fun((p_11) => FunCall(Join(), FunCall(Map(fun((p_12) => FunCall(ReduceSeq(fun((p_14, p_15) => FunCall(Map(fun((p_16) => FunCall(Map(fun((p_17) => FunCall(add, FunCall(Get(0), p_17), FunCall(mult, FunCall(Get(1), p_16), FunCall(Get(1), p_17))))), FunCall(Zip(2), FunCall(Get(0), p_16), FunCall(Get(1), p_15))))), FunCall(Zip(2), p_14, FunCall(Get(0), p_15))))), FunCall(Get(0), p_12), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p_11)), FunCall(Transpose(), FunCall(Get(1), p_12)))))), FunCall(Zip(2), FunCall(Get(0), p_11), FunCall(Split(v__4), FunCall(Gather(ReorderWithStride(v__3 / v__4)), FunCall(Transpose(), FunCall(Get(1), p_10))))))))), FunCall(Zip(2), p_9, FunCall(Split(v__5), FunCall(Transpose(), FunCall(Get(0), p_10))))))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3*1/^v__4), v__6*1/^v__5)), FunCall(Zip(2), p_2, p_3)))))))), FunCall(Transpose(), FunCall(Map(fun((p_22) => FunCall(Transpose(), p_22))), FunCall(Split(v__7), FunCall(Map(fun((p_23) => FunCall(Split(v__3), p_23))), p_1))))))))), FunCall(Transpose(), FunCall(Map(fun((p_24) => FunCall(Transpose(), p_24))), FunCall(Split(v__7), FunCall(Map(fun((p_25) => FunCall(Split(v__6), p_25))), p_0)))))))

  val mmTBMali = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), (p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), p_4))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_6, p_7) => FunCall(Map(fun((p_8) => FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(Reduce(fun((p_10, p_11) => FunCall(add, p_10, p_11))), FunCall(Get(0), p_9), FunCall(asScalar(), FunCall(Map(fun((p_12) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_12), FunCall(Get(1), p_12)))), FunCall(Zip(2), FunCall(asVector(4), FunCall(Get(1), p_8)), FunCall(asVector(4), FunCall(Get(1), p_9)))))))), FunCall(Zip(2), FunCall(Get(0), p_8), FunCall(Transpose(), FunCall(Get(1), p_7))))))), FunCall(Zip(2), p_6, FunCall(Transpose(), FunCall(Get(0), p_7)))))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4)), FunCall(Zip(2), FunCall(Split(v__5), FunCall(Transpose(), p_2)), FunCall(Split(v__5), FunCall(Transpose(), p_3))))))))), FunCall(Split(v__3), p_1)))))), FunCall(Split(v__4), p_0))))

  val gemvAmd = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), ArrayTypeWSWC(Float, M), ArrayTypeWSWC(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, p_6, p_3), FunCall(mult, FunCall(Get(1), p_5), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(add, p_7, p_8))), Value("0.0f", Float), FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(ReduceSeq(fun((p_11, p_12) => FunCall(add, p_11, FunCall(mult, FunCall(Get(0), p_12), FunCall(Get(1), p_12))))), Value("0.0f", Float), p_9))), FunCall(Split(M*1/^v__2), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Zip(2), p_1, FunCall(Get(0), p_5)))))))))), FunCall(Zip(2), p_0, p_2)))

  def getHash(lambda: Lambda): String =
    DumpToFile.Sha256Hash(DumpToFile.dumpLambdaToMethod(lambda))

  @Test
  def mmKepler(): Unit = {

    val keplerGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(MapWrg(1)(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(MapWrg(0)(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_4))))))), FunCall(TransposeW(), FunCall(toGlobal(fun((p_8) => FunCall(MapSeq(fun((p_9) => FunCall(MapLcl(1)(fun((p_10) => FunCall(MapLcl(0)(fun((p_11) => FunCall(MapSeq(fun((p_12) => FunCall(MapSeq(fun((p_13) => FunCall(idfloat, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))), FunCall(ReduceSeq(fun((p_14, p_15) => FunCall(fun((p_16) => FunCall(fun((p_17) => FunCall(MapLcl(1)(fun((p_18) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_19) => FunCall(MapSeq(fun((p_20) => p_20)), FunCall(ReduceSeq(fun((p_21, p_22) => FunCall(fun((p_23) => FunCall(fun((p_24) => FunCall(MapSeq(fun((p_25) => FunCall(MapSeq(fun((p_26) => FunCall(add, FunCall(Get(0), p_26), FunCall(mult, FunCall(Get(1), p_25), FunCall(Get(1), p_26))))), FunCall(Zip(2), FunCall(Get(0), p_25), FunCall(Get(1), p_24))))), FunCall(Zip(2), p_21, FunCall(Get(0), p_24)))), p_23)), FunCall(toPrivate(fun((p_27) => FunCall(Tuple(2), FunCall(MapSeq(fun((p_28) => FunCall(idfloat, p_28))), FunCall(Get(0), p_27)), FunCall(MapSeq(fun((p_29) => FunCall(idfloat, p_29))), FunCall(Get(1), p_27))))), p_22)))), FunCall(Get(0), p_19), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p_18)), FunCall(Transpose(), FunCall(Get(1), p_19))))))), FunCall(Zip(2), FunCall(Get(0), p_18), FunCall(Split(v__4), FunCall(Gather(ReorderWithStride(v__3 / v__4)), FunCall(Transpose(), FunCall(Get(1), p_17))))))))), FunCall(Zip(2), p_14, FunCall(Split(v__5), FunCall(Transpose(), FunCall(Get(0), p_17)))))), FunCall(toLocal(fun((p_30) => FunCall(Unzip(), FunCall(MapLcl(1)(fun((p_31) => FunCall(Tuple(2), FunCall(MapLcl(0)(fun((p_32) => FunCall(idfloat, p_32))), FunCall(Get(0), p_31)), FunCall(MapLcl(0)(fun((p_33) => FunCall(idfloat, p_33))), FunCall(Get(1), p_31))))), FunCall(Zip(2), FunCall(Get(0), p_30), FunCall(Get(1), p_30)))))), p_16))), p_15))), FunCall(MapLcl(1)(fun((p_34) => FunCall(MapLcl(0)(fun((p_35) => FunCall(MapSeq(fun((p_36) => FunCall(MapSeq(fun((p_37) => FunCall(idfloat, p_37))), p_36))), p_35))), p_34))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3 * 1 /^ v__4), v__6 * 1 /^ v__5))), FunCall(Zip(2), p_2, p_3))))))))), FunCall(Transpose(), FunCall(Map(fun((p_38) => FunCall(Transpose(), p_38))), FunCall(Split(v__7), FunCall(Map(fun((p_39) => FunCall(Split(v__3), p_39))), p_1))))))))), FunCall(Transpose(), FunCall(Map(fun((p_40) => FunCall(Transpose(), p_40))), FunCall(Split(v__7), FunCall(Map(fun((p_41) => FunCall(Split(v__6), p_41))), p_0)))))))
    val keplerHash = getHash(keplerGold)

    val mapped = MemoryMappingRewrite.lowerLambda(mmTATiled2DBlocked, enabledMappings)
    assertTrue(mapped.exists(getHash(_) == keplerHash))
  }

  @Test
  def mmHawaii(): Unit = {

    val hawaiiGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(MapWrg(1)(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(MapWrg(0)(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_4))))))), FunCall(TransposeW(), FunCall(toGlobal(fun((p_8) => FunCall(MapSeq(fun((p_9) => FunCall(MapLcl(1)(fun((p_10) => FunCall(MapLcl(0)(fun((p_11) => FunCall(MapSeq(fun((p_12) => FunCall(MapSeq(fun((p_13) => FunCall(idfloat, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))), FunCall(ReduceSeq(fun((p_14, p_15) => FunCall(fun((p_16) => FunCall(fun((p_17) => FunCall(MapLcl(1)(fun((p_18) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_19) => FunCall(MapSeq(fun((p_20) => p_20)), FunCall(ReduceSeq(fun((p_21, p_22) => FunCall(fun((p_23) => FunCall(fun((p_24) => FunCall(MapSeq(fun((p_25) => FunCall(MapSeq(fun((p_26) => FunCall(add, FunCall(Get(0), p_26), FunCall(mult, FunCall(Get(1), p_25), FunCall(Get(1), p_26))))), FunCall(Zip(2), FunCall(Get(0), p_25), FunCall(Get(1), p_24))))), FunCall(Zip(2), p_21, FunCall(Get(0), p_24)))), FunCall(toLocal(fun((p_27) => FunCall(Tuple(2), FunCall(MapSeq(fun((p_28) => FunCall(idfloat, p_28))), FunCall(Get(0), p_27)), FunCall(MapSeq(fun((p_29) => FunCall(idfloat, p_29))), FunCall(Get(1), p_27))))), p_23))), FunCall(toPrivate(fun((p_30) => FunCall(Tuple(2), FunCall(Get(0), p_30), FunCall(MapSeq(fun((p_31) => FunCall(idfloat, p_31))), FunCall(Get(1), p_30))))), p_22)))), FunCall(Get(0), p_19), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p_18)), FunCall(Transpose(), FunCall(Get(1), p_19))))))), FunCall(Zip(2), FunCall(Get(0), p_18), FunCall(Split(v__4), FunCall(Gather(ReorderWithStride(v__3 / v__4)), FunCall(Transpose(), FunCall(Get(1), p_17))))))))), FunCall(Zip(2), p_14, FunCall(Split(v__5), FunCall(Transpose(), FunCall(Get(0), p_17)))))), p_16)), p_15))), FunCall(MapLcl(1)(fun((p_32) => FunCall(MapLcl(0)(fun((p_33) => FunCall(MapSeq(fun((p_34) => FunCall(MapSeq(fun((p_35) => FunCall(idfloat, p_35))), p_34))), p_33))), p_32))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3 * 1 /^ v__4), v__6 * 1 /^ v__5))), FunCall(Zip(2), p_2, p_3))))))))), FunCall(Transpose(), FunCall(Map(fun((p_36) => FunCall(Transpose(), p_36))), FunCall(Split(v__7), FunCall(Map(fun((p_37) => FunCall(Split(v__3), p_37))), p_1))))))))), FunCall(Transpose(), FunCall(Map(fun((p_38) => FunCall(Transpose(), p_38))), FunCall(Split(v__7), FunCall(Map(fun((p_39) => FunCall(Split(v__6), p_39))), p_0)))))))
    val hawaiiHash = getHash(hawaiiGold)

    val mapped = MemoryMappingRewrite.lowerLambda(mmTATiled2DBlocked, enabledMappings)
    assertTrue(mapped.exists(getHash(_) == hawaiiHash))
  }

  @Test
  def mmVectorLoads(): Unit = {

    val vectorLoadsGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(MapWrg(1)(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(MapWrg(0)(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_4))))))), FunCall(TransposeW(), FunCall(toGlobal(fun((p_8) => FunCall(MapSeq(fun((p_9) => FunCall(MapLcl(1)(fun((p_10) => FunCall(MapLcl(0)(fun((p_11) => FunCall(MapSeq(fun((p_12) => FunCall(MapSeq(fun((p_13) => FunCall(idfloat, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))), FunCall(ReduceSeq(fun((p_14, p_15) => FunCall(fun((p_16) => FunCall(fun((p_17) => FunCall(MapLcl(1)(fun((p_18) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_19) => FunCall(MapSeq(fun((p_20) => p_20)), FunCall(ReduceSeq(fun((p_21, p_22) => FunCall(fun((p_23) => FunCall(fun((p_24) => FunCall(MapSeq(fun((p_25) => FunCall(MapSeq(fun((p_26) => FunCall(add, FunCall(Get(0), p_26), FunCall(mult, FunCall(Get(1), p_25), FunCall(Get(1), p_26))))), FunCall(Zip(2), FunCall(Get(0), p_25), FunCall(Get(1), p_24))))), FunCall(Zip(2), p_21, FunCall(Get(0), p_24)))), p_23)), FunCall(toPrivate(fun((p_27) => FunCall(Tuple(2), FunCall(MapSeq(fun((p_28) => FunCall(idfloat, p_28))), FunCall(Get(0), p_27)), FunCall(MapSeq(fun((p_29) => FunCall(idfloat, p_29))), FunCall(Get(1), p_27))))), p_22)))), FunCall(Get(0), p_19), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p_18)), FunCall(Transpose(), FunCall(Get(1), p_19))))))), FunCall(Zip(2), FunCall(Get(0), p_18), FunCall(Split(v__4), FunCall(Gather(ReorderWithStride(v__3 / v__4)), FunCall(Transpose(), FunCall(Get(1), p_17))))))))), FunCall(Zip(2), p_14, FunCall(Split(v__5), FunCall(Transpose(), FunCall(Get(0), p_17)))))), FunCall(toLocal(fun((p_30) => FunCall(Unzip(), FunCall(MapLcl(1)(fun((p_31) => FunCall(Tuple(2), FunCall(MapLcl(0)(fun((p_32) => FunCall(idfloat, p_32))), FunCall(Get(0), p_31)), FunCall(MapLcl(0)(fun((p_33) => FunCall(idfloat, p_33))), FunCall(Get(1), p_31))))), FunCall(Zip(2), FunCall(Get(0), p_30), FunCall(Get(1), p_30)))))), p_16))), p_15))), FunCall(MapLcl(1)(fun((p_34) => FunCall(MapLcl(0)(fun((p_35) => FunCall(MapSeq(fun((p_36) => FunCall(MapSeq(fun((p_37) => FunCall(idfloat, p_37))), p_36))), p_35))), p_34))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3 * 1 /^ v__4), v__6 * 1 /^ v__5))), FunCall(Zip(2), p_2, p_3))))))))), FunCall(Transpose(), FunCall(Map(fun((p_38) => FunCall(Transpose(), p_38))), FunCall(Split(v__7), FunCall(Map(fun((p_39) => FunCall(Split(v__3), p_39))), p_1))))))))), FunCall(Transpose(), FunCall(Map(fun((p_40) => FunCall(Transpose(), p_40))), FunCall(Split(v__7), FunCall(Map(fun((p_41) => FunCall(Split(v__6), p_41))), p_0)))))))
    val vectorLoadsHash = getHash(vectorLoadsGold)

    val mapped = MemoryMappingRewrite.lowerLambda(mmTATiled2DBlocked, enabledMappings)
    assertTrue(mapped.exists(getHash(_) == vectorLoadsHash))
  }

  @Test
  def mmMali(): Unit = {

    val maliGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),(p_0, p_1) => FunCall(Join(), FunCall(MapGlb(0)(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(MapGlb(1)(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), p_4))), FunCall(TransposeW(), FunCall(toGlobal(fun((p_5) => FunCall(MapSeq(fun((p_6) => FunCall(MapSeq(fun((p_7) => FunCall(MapSeq(fun((p_8) => FunCall(idfloat, p_8))), p_7))), p_6))), p_5))), FunCall(ReduceSeq(fun((p_9, p_10) => FunCall(fun((p_11) => FunCall(fun((p_12) => FunCall(MapSeq(fun((p_13) => FunCall(Join(), FunCall(MapSeq(fun((p_14) => FunCall(MapSeq(fun((p_15) => p_15)), FunCall(ReduceSeq(fun((p_16, p_17) => FunCall(fun((p_18) => FunCall(fun((p_19) => FunCall(add, p_16, p_19)), p_18)), p_17))), FunCall(Get(0), p_14), FunCall(asScalar(), FunCall(MapSeq(fun((p_20) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_20), FunCall(Get(1), p_20)))), FunCall(Zip(2), FunCall(asVector(4), FunCall(Get(1), p_13)), FunCall(asVector(4), FunCall(Get(1), p_14))))))))), FunCall(Zip(2), FunCall(Get(0), p_13), FunCall(Transpose(), FunCall(Get(1), p_12))))))), FunCall(Zip(2), p_9, FunCall(Transpose(), FunCall(Get(0), p_12))))), p_11)), p_10))), FunCall(MapSeq(fun((p_21) => FunCall(MapSeq(fun((p_22) => FunCall(idfloat, p_22))), p_21))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4))), FunCall(Zip(2), FunCall(Split(v__5), FunCall(Transpose(), p_2)), FunCall(Split(v__5), FunCall(Transpose(), p_3)))))))))), FunCall(Split(v__3), p_1)))))), FunCall(Split(v__4), p_0))))
    val maliHash = getHash(maliGold)

    val mapped = MemoryMappingRewrite.lowerLambda(mmTBMali, enabledMappings)
    assertTrue(mapped.exists(getHash(_) == maliHash))
  }

  @Test
  def gemv(): Unit = {

    val gemvAmdGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), ArrayTypeWSWC(Float, M), ArrayTypeWSWC(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) =>
      FunCall(MapWrg(0)(fun((p_5) =>
          FunCall(MapSeq(fun((p_7) =>
            FunCall(toGlobal(add),
              FunCall(toLocal(fun((p_8, p_9) =>
                FunCall(mult, p_8, p_9))), p_7, p_3),
              FunCall(toLocal(fun((p_10, p_11) =>
                FunCall(mult, p_10, p_11))),
                FunCall(Get(1), p_5), p_4)))),
          FunCall(MapSeq(fun((p_12) =>
            FunCall(toLocal(fun((p_13) =>
              FunCall(idfloat, p_13))), p_12))),
            FunCall(ReduceSeq(fun((p_14, p_15) =>
              FunCall(fun((p_16) =>
                FunCall(fun((p_17) =>
                  FunCall(add, p_14, p_17)), p_16)), p_15))),
              FunCall(idfloat, Value("0.0f", Float)),
              FunCall(Join(),
                FunCall(MapLcl(0)(fun((p_18) =>
                  FunCall(MapSeq(fun((p_19) =>
                    FunCall(toLocal(fun((p_20) =>
                      FunCall(idfloat, p_20))), p_19))),
                    FunCall(ReduceSeq(fun((p_21, p_22) =>
                      FunCall(fun((p_23) =>
                        FunCall(fun((p_24) =>
                          FunCall(add, p_21,
                            FunCall(mult,
                              FunCall(Get(0), p_24),
                              FunCall(Get(1), p_24)))), p_23)), p_22))),
                      FunCall(idfloat, Value("0.0f", Float)), p_18)))),
                  FunCall(Split(M * 1 /^ v__2),
                    FunCall(Gather(ReorderWithStride(v__2)),
                      FunCall(Zip(2), p_1,
                        FunCall(Get(0), p_5))))))))))),
        FunCall(Zip(2), p_0, p_2)))
    val gemvAmdHash = getHash(gemvAmdGold)

    val mapped = MemoryMappingRewrite.lowerLambda(gemvAmd, enabledMappings)
    assertTrue(mapped.exists(getHash(_) == gemvAmdHash))
  }

  @Test
  def unrolledReduce(): Unit = {
    val highLevel = fun(ArrayTypeWSWC(Float, N),(p_0) => FunCall(Join(), FunCall(Map(fun((p_1) => FunCall(Reduce(fun((p_2, p_3) => FunCall(add, p_2, p_3))), Value("0.0f", Float), p_1))), FunCall(Slide(3,1), FunCall(Pad(1,1,Pad.Boundary.Clamp), p_0)))))
    val gold = fun(ArrayTypeWSWC(Float, N),(p_0) => FunCall(Join(), FunCall(MapGlb(0)(fun((p_1) => FunCall(toGlobal(fun((p_2) => FunCall(MapSeq(fun((p_3) => FunCall(idfloat, p_3))), p_2))), FunCall(MapSeq(fun((p_4) => p_4)), FunCall(ReduceSeqUnroll(fun((p_5, p_6) => FunCall(fun((p_7) => FunCall(fun((p_8) => FunCall(add, p_5, p_8)), p_7)), p_6))), FunCall(idfloat, Value("0.0f", Float)), p_1))))), FunCall(Slide(3,1), FunCall(Pad(1,1,Pad.Boundary.Clamp), p_0)))))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(highLevel, enabledMappings, unroll = true)
    println(mapped)
    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def kmeans(): Unit = {

    val v_P_0 = SizeVar("P")
    val v_F_1 = SizeVar("F")
    val v_C_2 = SizeVar("C")

    val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
    val currentDistance = UserFun("currentDistance", Array("x", "y"), """|{ return (x - y) * (x - y); }""".stripMargin, Seq(Float, Float), Float)
    val test = UserFun("test", Array("dist", "tuple"), """|{float min_dist = tuple._0;int i          = tuple._1;int index      = tuple._2;if (dist < min_dist) {  Tuple t = {dist, i + 1, i};  return t;} else {  Tuple t = {min_dist, i + 1, index};  return t;}}""".stripMargin, Seq(Float, TupleType(Float, Int, Int)), TupleType(Float, Int, Int))
    val select_ = UserFun("select_", Array("tuple"), """|{ return tuple._2; }""".stripMargin, Seq(TupleType(Float, Int, Int)), Int)

    val idfloat = UserFun("idfloat", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
    val idTuple_float_int_int = UserFun("idTuple3_float_int_int", Array("x"), """|{ return x; }""".stripMargin, Seq(TupleType(Float, Int, Int)), TupleType(Float, Int, Int))

    val start = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_P_0), v_F_1), ArrayTypeWSWC(ArrayTypeWSWC(Float, v_F_1), v_C_2),(p_0, p_1) => FunCall(Map(fun((p_2) => FunCall(Map(fun((p_3) => FunCall(Map(fun((p_4) => FunCall(select_, p_4))), p_3))), FunCall(ReduceSeq(fun((p_5, p_6) => FunCall(Map(fun((p_7) => FunCall(test, FunCall(Get(0), p_7), FunCall(Get(1), p_7)))), FunCall(Zip(2), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(add, p_8, FunCall(currentDistance, FunCall(Get(0), p_9), FunCall(Get(1), p_9))))), Value("0.0f", Float), FunCall(Zip(2), p_2, p_6)), p_5)))), Value("{3.40282347e+38, 0, 0}", ArrayTypeWSWC(TupleType(Float, Int, Int), 1)), p_1)))), FunCall(Transpose(), p_0)))

    val kmeansGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_P_0), v_F_1), ArrayTypeWSWC(ArrayTypeWSWC(Float, v_F_1), v_C_2),(p_0, p_1) =>
      FunCall(MapGlb(0)(fun((p_2) =>
        FunCall(MapSeq(fun((p_3) =>
            FunCall(MapSeq(fun((p_5) =>
              FunCall(toGlobal(select_), p_5))), p_3))),
          FunCall(ReduceSeq(fun((p_6, p_7) =>
            FunCall(fun((p_8) =>
              FunCall(fun((p_9) =>
                FunCall(MapSeq(fun((p_10) =>
                  FunCall(test,
                    FunCall(Get(0), p_10),
                    FunCall(Get(1), p_10)))),
                  FunCall(Zip(2),
                    FunCall(MapSeq(fun((p_11) => p_11)),
                      FunCall(ReduceSeq(fun((p_12, p_13) =>
                        FunCall(fun((p_14) =>
                          FunCall(fun((p_15) =>
                            FunCall(add, p_12,
                              FunCall(currentDistance,
                                FunCall(Get(0), p_15),
                                FunCall(Get(1), p_15)))), p_14)), p_13))),
                        FunCall(idfloat, Value("0.0f", Float)),
                        FunCall(Zip(2), p_2, p_9))), p_6))), p_8)), p_7))),
            FunCall(MapSeq(fun((p_16) =>
              FunCall(idTuple_float_int_int, p_16))), Value("{3.40282347e+38, 0, 0}", ArrayTypeWSWC(TupleType(Float, Int, Int), 1))), p_1)))),
        FunCall(Transpose(), p_0)))
    val kmeansHash = getHash(kmeansGold)

    val mapped = MemoryMappingRewrite.lowerLambda(start, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == kmeansHash))
  }

}
