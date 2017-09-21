package exploration.mapping

import exploration.MemoryMappingRewrite
import ir._
import ir.ast._
import lift.arithmetic.{Cst, Pow, SizeVar}
import opencl.executor.LongTestsEnabled
import opencl.ir.pattern._
import opencl.ir.{add, idfloat, _}
import org.junit.Assert._
import org.junit.Test
import rewriting.EnabledMappings
import rewriting.utils.Utils.getHash

class TestMemoryMappingRewrite {

  LongTestsEnabled()

  private val X = SizeVar("X")
  private val K = SizeVar("K")
  private val M = SizeVar("M")
  private val N = SizeVar("N")

  private val v__1 = SizeVar("")
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

  @Test
  def mmKepler(): Unit = {

    val keplerGold = fun(ArrayType(ArrayType(Float, M), K), ArrayType(ArrayType(Float, N), K),(p_0, p_1) =>
      FunCall(Join(),
        FunCall(MapWrg(1)(fun((p_2) =>
          FunCall(TransposeW(),
            FunCall(Join(),
              FunCall(MapWrg(0)(fun((p_3) =>
                FunCall(TransposeW(),
                  FunCall(Join(),
                    FunCall(Map(fun((p_4) =>
                      FunCall(Map(fun((p_5) =>
                        FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))),
                        FunCall(TransposeW(),
                          FunCall(Join(),
                            FunCall(Map(fun((p_6) =>
                              FunCall(TransposeW(),
                                FunCall(Map(fun((p_7) =>
                                  FunCall(TransposeW(), p_7))),
                                  FunCall(TransposeW(), p_6))))),
                              FunCall(TransposeW(), p_4))))))),
                      FunCall(TransposeW(),
                        FunCall(toGlobal(fun((p_8) =>
                          FunCall(MapSeq(fun((p_9) =>
                            FunCall(MapLcl(1)(fun((p_10) =>
                              FunCall(MapLcl(0)(fun((p_11) =>
                                FunCall(MapSeq(fun((p_12) =>
                                  FunCall(MapSeq(fun((p_13) =>
                                    FunCall(idfloat, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))),
                          FunCall(ReduceSeq(fun((p_14, p_15) =>
                            FunCall(fun((p_16) =>
                              FunCall(MapLcl(1)(fun((p_17) =>
                                FunCall(Join(),
                                  FunCall(MapLcl(0)(fun((p_18) =>
                                    FunCall(ReduceSeq(fun((p_19, p_20) =>
                                      FunCall(fun((p_21) =>
                                        FunCall(MapSeq(fun((p_22) =>
                                          FunCall(MapSeq(fun((p_23) =>
                                            FunCall(add,
                                              FunCall(Get(0), p_23),
                                              FunCall(toPrivate(fun((p_24, p_25) =>
                                                FunCall(mult, p_24, p_25))),
                                                FunCall(Get(1), p_22),
                                                FunCall(Get(1), p_23))))),
                                            FunCall(Zip(2),
                                              FunCall(Get(0), p_22),
                                              FunCall(Get(1), p_21))))),
                                          FunCall(Zip(2), p_19,
                                            FunCall(Get(0), p_21)))),
                                        FunCall(toPrivate(fun((p_26) =>
                                          FunCall(Tuple(2),
                                            FunCall(MapSeq(fun((p_27) =>
                                              FunCall(idfloat, p_27))),
                                              FunCall(Get(0), p_26)),
                                            FunCall(MapSeq(fun((p_28) =>
                                              FunCall(idfloat, p_28))),
                                              FunCall(Get(1), p_26))))), p_20)))),
                                      FunCall(Get(0), p_18),
                                      FunCall(Zip(2),
                                        FunCall(Transpose(),
                                          FunCall(Get(1), p_17)),
                                        FunCall(Transpose(),
                                          FunCall(Get(1), p_18)))))),
                                    FunCall(Zip(2),
                                      FunCall(Get(0), p_17),
                                      FunCall(Split(v__4),
                                        FunCall(Gather(ReorderWithStride(v__3 / v__4)),
                                          FunCall(Transpose(),
                                            FunCall(Get(1), p_16))))))))),
                                FunCall(Zip(2), p_14,
                                  FunCall(Split(v__5),
                                    FunCall(Transpose(),
                                      FunCall(Get(0), p_16)))))),
                              FunCall(toLocal(fun((p_29) =>
                                FunCall(Unzip(),
                                  FunCall(MapLcl(1)(fun((p_30) =>
                                    FunCall(Tuple(2),
                                      FunCall(MapLcl(0)(fun((p_31) =>
                                        FunCall(idfloat, p_31))),
                                        FunCall(Get(0), p_30)),
                                      FunCall(MapLcl(0)(fun((p_32) =>
                                        FunCall(idfloat, p_32))),
                                        FunCall(Get(1), p_30))))),
                                    FunCall(Zip(2),
                                      FunCall(Get(0), p_29),
                                      FunCall(Get(1), p_29)))))), p_15)))),
                            FunCall(MapLcl(1)(fun((p_33) =>
                              FunCall(MapLcl(0)(fun((p_34) =>
                                FunCall(MapSeq(fun((p_35) =>
                                  FunCall(MapSeq(fun((p_36) =>
                                    FunCall(idfloat, p_36))), p_35))), p_34))), p_33))), Value("0.0f", ArrayType(ArrayType(ArrayType(ArrayType(Float, v__4), v__5), v__3*1/^v__4), v__6*1/^v__5))),
                            FunCall(Zip(2), p_2, p_3))))))))),
                FunCall(Transpose(),
                  FunCall(Map(fun((p_37) =>
                    FunCall(Transpose(), p_37))),
                    FunCall(Split(v__7),
                      FunCall(Map(fun((p_38) =>
                        FunCall(Split(v__3), p_38))), p_1))))))))),
          FunCall(Transpose(),
            FunCall(Map(fun((p_39) =>
              FunCall(Transpose(), p_39))),
              FunCall(Split(v__7),
                FunCall(Map(fun((p_40) =>
                  FunCall(Split(v__6), p_40))), p_0)))))))
    val keplerHash = getHash(keplerGold)

    val mapped = MemoryMappingRewrite.lowerLambda(mmTATiled2DBlocked, enabledMappings)
    assertTrue(mapped.exists(getHash(_) == keplerHash))
  }

  @Test
  def mmHawaii(): Unit = {

    val hawaiiGold = fun(ArrayType(ArrayType(Float, M), K), ArrayType(ArrayType(Float, N), K),(p_0, p_1) =>
      FunCall(Join(),
        FunCall(MapWrg(1)(fun((p_2) =>
          FunCall(TransposeW(),
            FunCall(Join(),
              FunCall(MapWrg(0)(fun((p_3) =>
                FunCall(TransposeW(),
                  FunCall(Join(),
                    FunCall(Map(fun((p_4) =>
                      FunCall(Map(fun((p_5) =>
                        FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))),
                        FunCall(TransposeW(),
                          FunCall(Join(),
                            FunCall(Map(fun((p_6) =>
                              FunCall(TransposeW(),
                                FunCall(Map(fun((p_7) =>
                                  FunCall(TransposeW(), p_7))),
                                  FunCall(TransposeW(), p_6))))),
                              FunCall(TransposeW(), p_4))))))),
                      FunCall(TransposeW(),
                        FunCall(toGlobal(fun((p_8) =>
                          FunCall(MapSeq(fun((p_9) =>
                            FunCall(MapLcl(1)(fun((p_10) =>
                              FunCall(MapLcl(0)(fun((p_11) =>
                                FunCall(MapSeq(fun((p_12) =>
                                  FunCall(MapSeq(fun((p_13) =>
                                    FunCall(idfloat, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))),
                          FunCall(ReduceSeq(fun((p_14, p_15) =>
                            FunCall(MapLcl(1)(fun((p_16) =>
                              FunCall(Join(),
                                FunCall(MapLcl(0)(fun((p_17) =>
                                  FunCall(ReduceSeq(fun((p_18, p_19) =>
                                    FunCall(fun((p_20) =>
                                      FunCall(fun((p_21) =>
                                        FunCall(MapSeq(fun((p_22) =>
                                          FunCall(MapSeq(fun((p_23) =>
                                            FunCall(add,
                                              FunCall(Get(0), p_23),
                                              FunCall(toPrivate(fun((p_24, p_25) =>
                                                FunCall(mult, p_24, p_25))),
                                                FunCall(Get(1), p_22),
                                                FunCall(Get(1), p_23))))),
                                            FunCall(Zip(2),
                                              FunCall(Get(0), p_22),
                                              FunCall(Get(1), p_21))))),
                                          FunCall(Zip(2), p_18,
                                            FunCall(Get(0), p_21)))),
                                        FunCall(toLocal(fun((p_26) =>
                                          FunCall(Tuple(2),
                                            FunCall(MapSeq(fun((p_27) =>
                                              FunCall(idfloat, p_27))),
                                              FunCall(Get(0), p_26)),
                                            FunCall(MapSeq(fun((p_28) =>
                                              FunCall(idfloat, p_28))),
                                              FunCall(Get(1), p_26))))), p_20))),
                                      FunCall(toPrivate(fun((p_29) =>
                                        FunCall(Tuple(2),
                                          FunCall(Get(0), p_29),
                                          FunCall(MapSeq(fun((p_30) =>
                                            FunCall(idfloat, p_30))),
                                            FunCall(Get(1), p_29))))), p_19)))),
                                    FunCall(Get(0), p_17),
                                    FunCall(Zip(2),
                                      FunCall(Transpose(),
                                        FunCall(Get(1), p_16)),
                                      FunCall(Transpose(),
                                        FunCall(Get(1), p_17)))))),
                                  FunCall(Zip(2),
                                    FunCall(Get(0), p_16),
                                    FunCall(Split(v__4),
                                      FunCall(Gather(ReorderWithStride(v__3 / v__4)),
                                        FunCall(Transpose(),
                                          FunCall(Get(1), p_15))))))))),
                              FunCall(Zip(2), p_14,
                                FunCall(Split(v__5),
                                  FunCall(Transpose(),
                                    FunCall(Get(0), p_15))))))),
                            FunCall(MapLcl(1)(fun((p_31) =>
                              FunCall(MapLcl(0)(fun((p_32) =>
                                FunCall(MapSeq(fun((p_33) =>
                                  FunCall(MapSeq(fun((p_34) =>
                                    FunCall(idfloat, p_34))), p_33))), p_32))), p_31))), Value("0.0f", ArrayType(ArrayType(ArrayType(ArrayType(Float, v__4), v__5), v__3*1/^v__4), v__6*1/^v__5))),
                            FunCall(Zip(2), p_2, p_3))))))))),
                FunCall(Transpose(),
                  FunCall(Map(fun((p_35) =>
                    FunCall(Transpose(), p_35))),
                    FunCall(Split(v__7),
                      FunCall(Map(fun((p_36) =>
                        FunCall(Split(v__3), p_36))), p_1))))))))),
          FunCall(Transpose(),
            FunCall(Map(fun((p_37) =>
              FunCall(Transpose(), p_37))),
              FunCall(Split(v__7),
                FunCall(Map(fun((p_38) =>
                  FunCall(Split(v__6), p_38))), p_0)))))))
    val hawaiiHash = getHash(hawaiiGold)

    val mapped = MemoryMappingRewrite.lowerLambda(mmTATiled2DBlocked, enabledMappings)
    assertTrue(mapped.exists(getHash(_) == hawaiiHash))
  }

  @Test
  def mmVectorLoads(): Unit = {

    val vectorLoadsGold = fun(ArrayType(ArrayType(Float, M), K), ArrayType(ArrayType(Float, N), K),(p_0, p_1) =>
      FunCall(Join(),
        FunCall(MapWrg(1)(fun((p_2) =>
          FunCall(TransposeW(),
            FunCall(Join(),
              FunCall(MapWrg(0)(fun((p_3) =>
                FunCall(TransposeW(),
                  FunCall(Join(),
                    FunCall(Map(fun((p_4) =>
                      FunCall(Map(fun((p_5) =>
                        FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))),
                        FunCall(TransposeW(),
                          FunCall(Join(),
                            FunCall(Map(fun((p_6) =>
                              FunCall(TransposeW(),
                                FunCall(Map(fun((p_7) =>
                                  FunCall(TransposeW(), p_7))),
                                  FunCall(TransposeW(), p_6))))),
                              FunCall(TransposeW(), p_4))))))),
                      FunCall(TransposeW(),
                        FunCall(toGlobal(fun((p_8) =>
                          FunCall(MapSeq(fun((p_9) =>
                            FunCall(MapLcl(1)(fun((p_10) =>
                              FunCall(MapLcl(0)(fun((p_11) =>
                                FunCall(MapSeq(fun((p_12) =>
                                  FunCall(MapSeq(fun((p_13) =>
                                    FunCall(idfloat, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))),
                          FunCall(ReduceSeq(fun((p_14, p_15) =>
                            FunCall(fun((p_16) =>
                              FunCall(MapLcl(1)(fun((p_17) =>
                                FunCall(Join(),
                                  FunCall(MapLcl(0)(fun((p_18) =>
                                    FunCall(ReduceSeq(fun((p_19, p_20) =>
                                      FunCall(fun((p_21) =>
                                        FunCall(MapSeq(fun((p_22) =>
                                          FunCall(MapSeq(fun((p_23) =>
                                            FunCall(add,
                                              FunCall(Get(0), p_23),
                                              FunCall(toPrivate(fun((p_24, p_25) =>
                                                FunCall(mult, p_24, p_25))),
                                                FunCall(Get(1), p_22),
                                                FunCall(Get(1), p_23))))),
                                            FunCall(Zip(2),
                                              FunCall(Get(0), p_22),
                                              FunCall(Get(1), p_21))))),
                                          FunCall(Zip(2), p_19,
                                            FunCall(Get(0), p_21)))),
                                        FunCall(toPrivate(fun((p_26) =>
                                          FunCall(Tuple(2),
                                            FunCall(MapSeq(fun((p_27) =>
                                              FunCall(idfloat, p_27))),
                                              FunCall(Get(0), p_26)),
                                            FunCall(MapSeq(fun((p_28) =>
                                              FunCall(idfloat, p_28))),
                                              FunCall(Get(1), p_26))))), p_20)))),
                                      FunCall(Get(0), p_18),
                                      FunCall(Zip(2),
                                        FunCall(Transpose(),
                                          FunCall(Get(1), p_17)),
                                        FunCall(Transpose(),
                                          FunCall(Get(1), p_18)))))),
                                    FunCall(Zip(2),
                                      FunCall(Get(0), p_17),
                                      FunCall(Split(v__4),
                                        FunCall(Gather(ReorderWithStride(v__3 / v__4)),
                                          FunCall(Transpose(),
                                            FunCall(Get(1), p_16))))))))),
                                FunCall(Zip(2), p_14,
                                  FunCall(Split(v__5),
                                    FunCall(Transpose(),
                                      FunCall(Get(0), p_16)))))),
                              FunCall(toLocal(fun((p_29) =>
                                FunCall(Unzip(),
                                  FunCall(MapLcl(1)(fun((p_30) =>
                                    FunCall(Tuple(2),
                                      FunCall(MapLcl(0)(fun((p_31) =>
                                        FunCall(idfloat, p_31))),
                                        FunCall(Get(0), p_30)),
                                      FunCall(MapLcl(0)(fun((p_32) =>
                                        FunCall(idfloat, p_32))),
                                        FunCall(Get(1), p_30))))),
                                    FunCall(Zip(2),
                                      FunCall(Get(0), p_29),
                                      FunCall(Get(1), p_29)))))), p_15)))),
                            FunCall(MapLcl(1)(fun((p_33) =>
                              FunCall(MapLcl(0)(fun((p_34) =>
                                FunCall(MapSeq(fun((p_35) =>
                                  FunCall(MapSeq(fun((p_36) =>
                                    FunCall(idfloat, p_36))), p_35))), p_34))), p_33))), Value("0.0f", ArrayType(ArrayType(ArrayType(ArrayType(Float, v__4), v__5), v__3*1/^v__4), v__6*1/^v__5))),
                            FunCall(Zip(2), p_2, p_3))))))))),
                FunCall(Transpose(),
                  FunCall(Map(fun((p_37) =>
                    FunCall(Transpose(), p_37))),
                    FunCall(Split(v__7),
                      FunCall(Map(fun((p_38) =>
                        FunCall(Split(v__3), p_38))), p_1))))))))),
          FunCall(Transpose(),
            FunCall(Map(fun((p_39) =>
              FunCall(Transpose(), p_39))),
              FunCall(Split(v__7),
                FunCall(Map(fun((p_40) =>
                  FunCall(Split(v__6), p_40))), p_0)))))))
    val vectorLoadsHash = getHash(vectorLoadsGold)

    val mapped = MemoryMappingRewrite.lowerLambda(mmTATiled2DBlocked, enabledMappings)
    assertTrue(mapped.exists(getHash(_) == vectorLoadsHash))
  }

  @Test
  def mmMali(): Unit = {

    val maliGold = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N),(p_0, p_1) =>
      FunCall(Join(),
        FunCall(MapGlb(0)(fun((p_2) =>
          FunCall(TransposeW(),
            FunCall(Join(),
              FunCall(MapGlb(1)(fun((p_3) =>
                FunCall(TransposeW(),
                  FunCall(Map(fun((p_4) =>
                    FunCall(TransposeW(), p_4))),
                    FunCall(TransposeW(),
                      FunCall(toGlobal(fun((p_5) =>
                        FunCall(MapSeq(fun((p_6) =>
                          FunCall(MapSeq(fun((p_7) =>
                            FunCall(MapSeq(fun((p_8) =>
                              FunCall(idfloat, p_8))), p_7))), p_6))), p_5))),
                        FunCall(ReduceSeq(fun((p_9, p_10) =>
                              FunCall(MapSeq(fun((p_13) =>
                                FunCall(Join(),
                                  FunCall(MapSeq(fun((p_14) =>
                                      FunCall(ReduceSeq(fun((p_16, p_17) =>
                                            FunCall(add, p_16, p_17))),
                                        FunCall(Get(0), p_14),
                                        FunCall(asScalar(),
                                          FunCall(MapSeq(fun((p_20) =>
                                            FunCall(VectorizeUserFun(4,mult),
                                              FunCall(Get(0), p_20),
                                              FunCall(Get(1), p_20)))),
                                            FunCall(Zip(2),
                                              FunCall(asVector(4),
                                                FunCall(Get(1), p_13)),
                                              FunCall(asVector(4),
                                                FunCall(Get(1), p_14)))))))),
                                    FunCall(Zip(2),
                                      FunCall(Get(0), p_13),
                                      FunCall(Transpose(),
                                        FunCall(Get(1), p_10))))))),
                                FunCall(Zip(2), p_9,
                                  FunCall(Transpose(),
                                    FunCall(Get(0), p_10)))))),
                          FunCall(MapSeq(fun((p_21) =>
                            FunCall(MapSeq(fun((p_22) =>
                              FunCall(idfloat, p_22))), p_21))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4))),
                          FunCall(Zip(2),
                            FunCall(Split(v__5),
                              FunCall(Transpose(), p_2)),
                            FunCall(Split(v__5),
                              FunCall(Transpose(), p_3)))))))))),
                FunCall(Split(v__3), p_1)))))),
          FunCall(Split(v__4), p_0))))
    val maliHash = getHash(maliGold)

    val mapped = MemoryMappingRewrite.lowerLambda(mmTBMali, enabledMappings)
    assertTrue(mapped.exists(getHash(_) == maliHash))
  }

  @Test
  def gemv(): Unit = {

    val gemvAmdGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), ArrayTypeWSWC(Float, M), ArrayTypeWSWC(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) =>
      FunCall(MapWrg(0)(fun((p_5) =>
        FunCall(toGlobal(fun((p_6) =>
          FunCall(MapSeq(fun((p_7) =>
            FunCall(add,
              FunCall(toPrivate(fun((p_8, p_9) =>
                FunCall(mult, p_8, p_9))), p_7, p_3),
              FunCall(toPrivate(fun((p_10, p_11) =>
                FunCall(mult, p_10, p_11))),
                FunCall(Get(1), p_5), p_4)))), p_6))),
          FunCall(MapSeq(fun((p_12) =>
            FunCall(toLocal(fun((p_13) =>
              FunCall(idfloat, p_13))), p_12))),
            FunCall(ReduceSeq(fun((p_14, p_15) =>
                  FunCall(add, p_14, p_15))),
              FunCall(idfloat, Value("0.0f", Float)),
              FunCall(Join(),
                FunCall(MapLcl(0)(fun((p_18) =>
                  FunCall(MapSeq(fun((p_19) =>
                    FunCall(toLocal(fun((p_20) =>
                      FunCall(idfloat, p_20))), p_19))),
                    FunCall(ReduceSeq(fun((p_21, p_22) =>
                          FunCall(add, p_21,
                            FunCall(toPrivate(mult),
                              FunCall(Get(0), p_22),
                              FunCall(Get(1), p_22))))),
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
    val highLevel = fun(ArrayType(Float, N),(p_0) => FunCall(Join(), FunCall(Map(fun((p_1) => FunCall(Reduce(fun((p_2, p_3) => FunCall(add, p_2, p_3))), Value("0.0f", Float), p_1))), FunCall(Slide(3,1), FunCall(Pad(1,1,Pad.Boundary.Clamp), p_0)))))
    val gold = fun(ArrayType(Float, N),(p_0) =>
      FunCall(Join(),
        FunCall(MapGlb(0)(fun((p_1) =>
          FunCall(toGlobal(fun((p_2) =>
            FunCall(MapSeq(fun((p_3) =>
              FunCall(idfloat, p_3))), p_2))),
              FunCall(ReduceSeqUnroll(fun((p_5, p_6) =>
                    FunCall(add, p_5, p_6))),
                FunCall(idfloat, Value("0.0f", Float)), p_1)))),
          FunCall(Slide(3,1),
            FunCall(Pad(1,1,Pad.Boundary.Clamp), p_0)))))
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
          FunCall(toGlobal(fun((p_4) =>
            FunCall(MapSeq(fun((p_5) =>
              FunCall(select_, p_5))), p_4))), p_3))),
          FunCall(ReduceSeq(fun((p_6, p_7) =>
                FunCall(MapSeq(fun((p_10) =>
                  FunCall(test,
                    FunCall(Get(0), p_10),
                    FunCall(Get(1), p_10)))),
                  FunCall(Zip(2),
                      FunCall(ReduceSeq(fun((p_12, p_13) =>
                            FunCall(add, p_12,
                              FunCall(toPrivate(currentDistance),
                                FunCall(Get(0), p_13),
                                FunCall(Get(1), p_13))))),
                        FunCall(idfloat, Value("0.0f", Float)),
                        FunCall(Zip(2), p_2, p_7)), p_6)))),
            FunCall(MapSeq(fun((p_16) =>
              FunCall(idTuple_float_int_int, p_16))), Value("{3.40282347e+38, 0, 0}", ArrayTypeWSWC(TupleType(Float, Int, Int), 1))), p_1)))),
        FunCall(Transpose(), p_0)))
    val kmeansHash = getHash(kmeansGold)

    val mapped = MemoryMappingRewrite.lowerLambda(start, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == kmeansHash))
  }

  @Test
  def gemvClblast(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), FunCall(Map(fun((p_6) => FunCall(Map(fun((p_7) => FunCall(add, FunCall(mult, FunCall(Get(1), p_7), p_3), FunCall(mult, FunCall(Get(1), FunCall(Get(0), p_7)), p_4)))), FunCall(Zip(2), p_5, p_6)))), FunCall(Transpose(), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(Join(), FunCall(Map(fun((p_10) => FunCall(PartRed(fun((p_11, p_12) => FunCall(add, p_11, p_12))), FunCall(Get(0), p_10), FunCall(Get(1), p_10)))), FunCall(Zip(2), p_8, p_9))))), Value("0.0f", ArrayType(Float, v__2)), FunCall(Transpose(), FunCall(Map(fun((p_13) => FunCall(Split(v__3), FunCall(Join(), p_13)))), FunCall(TransposeW(), FunCall(Map(fun((p_14) => FunCall(Map(fun((p_15) => FunCall(Map(fun((p_16) => FunCall(mult, FunCall(Get(0), p_16), FunCall(Get(1), p_16)))), FunCall(Zip(2), FunCall(Get(0), p_14), p_15)))), FunCall(Get(1), p_14)))), FunCall(Zip(2), FunCall(Split(v__4), p_1), FunCall(Transpose(), FunCall(Map(fun((p_17) => FunCall(Split(v__4), FunCall(Get(0), p_17)))), p_5)))))))))))))), FunCall(Split(v__2), FunCall(Zip(2), p_0, p_2)))))
    val gold = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(MapWrg(0)(fun((p_5) => FunCall(TransposeW(), FunCall(MapSeq(fun((p_6) => FunCall(toGlobal(fun((p_7) => FunCall(MapLcl(0)(fun((p_8) => FunCall(add, FunCall(toPrivate(fun((p_9, p_10) => FunCall(mult, p_9, p_10))), FunCall(Get(1), p_8), p_3), FunCall(toPrivate(fun((p_11, p_12) => FunCall(mult, p_11, p_12))), FunCall(Get(1), FunCall(Get(0), p_8)), p_4)))), p_7))), FunCall(Zip(2), p_5, p_6)))), FunCall(ReduceSeq(fun((p_13, p_14) => FunCall(fun((p_15) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_16) => FunCall(ReduceSeq(fun((p_17, p_18) => FunCall(add, p_17, FunCall(toPrivate(fun((p_19, p_20) => FunCall(mult, p_19, p_20))), FunCall(Get(0), p_18), FunCall(Get(1), p_18))))), FunCall(Get(0), p_16), FunCall(Zip(2), FunCall(Get(0), p_15), FunCall(Get(1), p_16))))), FunCall(Zip(2), p_13, FunCall(Get(1), p_15))))), FunCall(toLocal(fun((p_21) => FunCall(Tuple(2), FunCall(MapLcl(0)(fun((p_22) => FunCall(idfloat, p_22))), FunCall(Get(0), p_21)), FunCall(Get(1), p_21)))), p_14)))), FunCall(MapLcl(0)(fun((p_23) => FunCall(idfloat, p_23))), Value("0.0f", ArrayType(Float, v__2))), FunCall(Zip(2), FunCall(Split(v__3), p_1), FunCall(Transpose(), FunCall(Map(fun((p_24) => FunCall(Split(v__3), FunCall(Get(0), p_24)))), p_5)))))))), FunCall(Split(v__2), FunCall(Zip(2), p_0, p_2)))))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def gemvPartialReduceWithReorderNoRace(): Unit = {
    val gold = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(MapWrg(0)(fun((p_5) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_6) => FunCall(toGlobal(fun((p_7) => FunCall(MapSeq(fun((p_8) => FunCall(add, FunCall(toPrivate(fun((p_9, p_10) => FunCall(mult, p_9, p_10))), p_8, p_3), FunCall(toPrivate(fun((p_11, p_12) => FunCall(mult, p_11, p_12))), FunCall(Get(1), p_5), p_4)))), p_7))), FunCall(ReduceSeq(fun((p_13, p_14) => FunCall(add, p_13, p_14))), FunCall(idfloat, Value("0.0f", Float)), p_6)))), FunCall(Split(v__2), FunCall(Join(), FunCall(MapLcl(0)(fun((p_15) => FunCall(MapSeq(fun((p_16) => FunCall(toLocal(fun((p_17) => FunCall(idfloat, p_17))), p_16))), FunCall(ReduceSeq(fun((p_18, p_19) => FunCall(add, p_18, FunCall(toPrivate(fun((p_20, p_21) => FunCall(mult, p_20, p_21))), FunCall(Get(0), p_19), FunCall(Get(1), p_19))))), FunCall(idfloat, Value("0.0f", Float)), p_15)))), FunCall(Split( M * Pow(v__2, Cst(-1) )), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Zip(2), p_1, FunCall(Get(0), p_5))))))))))), FunCall(Zip(2), p_0, p_2)))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(gemvAmd, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def mriqIntroduceReuse(): Unit = {
    val mapFun = UserFun("mapFun", Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag"),
      """|{
         |    #define PIx2 6.2831853071795864769252867665590058f
         |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
         |    Tuple2_float_float bla = { PhiMag * cos(expArg), PhiMag * sin(expArg) };
         |    return  bla;
         |}""".stripMargin, Seq(Float, Float, Float, Float, Float, Float, Float), TupleType(Float, Float))

    val reduceFun = UserFun("reduceFun", Array("x", "y"),
      """|{
         | x._0 += y._0;
         | x._1 += y._1;
         | return x;
         |        }""".stripMargin, Seq(TupleType(Float, Float), TupleType(Float, Float)), TupleType(Float, Float))

    val idTuple4_float_float_float_float = UserFun("idTuple4_float_float_float_float", Array("x"), """|{ return x; }""".stripMargin, Seq(TupleType(Float, Float, Float, Float)), TupleType(Float, Float, Float, Float))
    val idTuple2_float_float = UserFun("idTuple2_float_float", Array("x"), """|{ return x; }""".stripMargin, Seq(TupleType(Float, Float)), TupleType(Float, Float))

    val f = fun(ArrayType(Float, X), ArrayType(Float, X), ArrayType(Float, X), ArrayType(TupleType(Float, Float, Float, Float), K),(p_0, p_1, p_2, p_3) => FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_5, p_6) => FunCall(Join(), FunCall(Map(fun((p_7) => FunCall(PartRed(fun((p_8, p_9) => FunCall(reduceFun, p_8, p_9))), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))), FunCall(Zip(2), p_5, p_6))))), Value("{ 0.0f, 0.0f}", ArrayType(TupleType(Float, Float), v__2)), FunCall(Transpose(), FunCall(Map(fun((p_10) => FunCall(Split(v__3), p_10))), FunCall(Map(fun((p_11) => FunCall(Join(), p_11))), FunCall(TransposeW(), FunCall(Map(fun((p_12) => FunCall(Map(fun((p_13) => FunCall(Map(fun((p_14) => FunCall(mapFun, FunCall(Get(0), p_13), FunCall(Get(1), p_13), FunCall(Get(2), p_13), FunCall(Get(0), p_14), FunCall(Get(1), p_14), FunCall(Get(2), p_14), FunCall(Get(3), p_14)))), p_12))), p_4))), FunCall(Split(v__4), p_3)))))))))), FunCall(Split(v__2), FunCall(Zip(3), p_0, p_1, p_2)))))
    val gold = fun(ArrayType(Float, X), ArrayType(Float, X), ArrayType(Float, X), ArrayType(TupleType(Float, Float, Float, Float), K),(p_0, p_1, p_2, p_3) => FunCall(Join(), FunCall(MapWrg(0)(fun((p_4) => FunCall(TransposeW(), FunCall(toGlobal(fun((p_5) => FunCall(MapSeq(fun((p_6) => FunCall(MapLcl(0)(fun((p_7) => FunCall(idTuple2_float_float, p_7))), p_6))), p_5))), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(fun((p_10) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_11) => FunCall(ReduceSeq(fun((p_12, p_13) => FunCall(reduceFun, p_12, FunCall(toPrivate(fun((p_14, p_15, p_16, p_17, p_18, p_19, p_20) => FunCall(mapFun, p_14, p_15, p_16, p_17, p_18, p_19, p_20))), FunCall(Get(0), FunCall(Get(1), p_11)), FunCall(Get(1), FunCall(Get(1), p_11)), FunCall(Get(2), FunCall(Get(1), p_11)), FunCall(Get(0), p_13), FunCall(Get(1), p_13), FunCall(Get(2), p_13), FunCall(Get(3), p_13))))), FunCall(Get(0), p_11), p_10))), FunCall(Zip(2), p_8, p_4)))), FunCall(toLocal(fun((p_21) => FunCall(MapLcl(0)(fun((p_22) => FunCall(idTuple4_float_float_float_float, p_22))), p_21))), p_9)))), FunCall(MapLcl(0)(fun((p_23) => FunCall(idTuple2_float_float, p_23))), Value("{ 0.0f, 0.0f}", ArrayType(TupleType(Float, Float), v__2))), FunCall(Split(v__3), p_3)))))), FunCall(Split(v__2), FunCall(Zip(3), p_0, p_1, p_2)))))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def mriqPartialReduceWithReorderNoRace(): Unit = {
    val idTuple2_float_float = UserFun("idTuple2_float_float", Array("x"), """|{ return x; }""".stripMargin, Seq(TupleType(Float, Float)), TupleType(Float, Float))
    val reduceFun = UserFun("reduceFun", Array("x", "y"),
      """|{
         | x._0 += y._0;
         | x._1 += y._1;
         | return x;
         |        }""".stripMargin, Seq(TupleType(Float, Float), TupleType(Float, Float)), TupleType(Float, Float))

    val mapFun = UserFun("mapFun", Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag"),
      """|{
         |    #define PIx2 6.2831853071795864769252867665590058f
         |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
         |    Tuple2_float_float bla = { PhiMag * cos(expArg), PhiMag * sin(expArg) };
         |    return  bla;
         |}""".stripMargin, Seq(Float, Float, Float, Float, Float, Float, Float), TupleType(Float, Float))


    val f = fun(ArrayType(Float, X), ArrayType(Float, X), ArrayType(Float, X), ArrayType(TupleType(Float, Float, Float, Float), K),(p_0, p_1, p_2, p_3) => FunCall(Map(fun((p_4) => FunCall(Reduce(fun((p_5, p_6) => FunCall(reduceFun, p_5, p_6))), Value("{ 0.0f, 0.0f}", TupleType(Float, Float)), FunCall(Join(), FunCall(Map(fun((p_7) => FunCall(PartRed(fun((p_8, p_9) => FunCall(reduceFun, p_8, p_9))), Value("{ 0.0f, 0.0f}", TupleType(Float, Float)), FunCall(Map(fun((p_10) => FunCall(mapFun, FunCall(Get(0), p_4), FunCall(Get(1), p_4), FunCall(Get(2), p_4), FunCall(Get(0), p_10), FunCall(Get(1), p_10), FunCall(Get(2), p_10), FunCall(Get(3), p_10)))), p_7)))), FunCall(Split( K * Pow(v__2, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__2)), p_3))))))), FunCall(Zip(3), p_0, p_1, p_2)))
    val gold = fun(ArrayType(Float, X), ArrayType(Float, X), ArrayType(Float, X), ArrayType(TupleType(Float, Float, Float, Float), K),(p_0, p_1, p_2, p_3) => FunCall(MapWrg(0)(fun((p_4) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_5) => FunCall(toGlobal(fun((p_6) => FunCall(MapSeq(fun((p_7) => FunCall(idTuple2_float_float, p_7))), p_6))), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(reduceFun, p_8, p_9))), FunCall(idTuple2_float_float, Value("{ 0.0f, 0.0f}", TupleType(Float, Float))), p_5)))), FunCall(Split(v__2), FunCall(Join(), FunCall(MapLcl(0)(fun((p_10) => FunCall(MapSeq(fun((p_11) => FunCall(toLocal(fun((p_12) => FunCall(idTuple2_float_float, p_12))), p_11))), FunCall(ReduceSeq(fun((p_13, p_14) => FunCall(reduceFun, p_13, FunCall(toPrivate(fun((p_15, p_16, p_17, p_18, p_19, p_20, p_21) => FunCall(mapFun, p_15, p_16, p_17, p_18, p_19, p_20, p_21))), FunCall(Get(0), p_4), FunCall(Get(1), p_4), FunCall(Get(2), p_4), FunCall(Get(0), p_14), FunCall(Get(1), p_14), FunCall(Get(2), p_14), FunCall(Get(3), p_14))))), FunCall(idTuple2_float_float, Value("{ 0.0f, 0.0f}", TupleType(Float, Float))), p_10)))), FunCall(Split( K * Pow(v__2, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__2)), p_3))))))))), FunCall(Zip(3), p_0, p_1, p_2)))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def gesummvIntroduceReuse(): Unit = {
    val idTuple2_float_float = UserFun("idTuple2_float_float", Array("x"), """|{ return x; }""".stripMargin, Seq(TupleType(Float, Float)), TupleType(Float, Float))

    val f = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), FunCall(Map(fun((p_6) => FunCall(Map(fun((p_7) => FunCall(add, FunCall(mult, FunCall(Get(0), p_7), p_3), FunCall(mult, FunCall(Get(1), p_7), p_4)))), p_6))), FunCall(Transpose(), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(Join(), FunCall(Map(fun((p_10) => FunCall(PartRed(fun((p_11, p_12) => FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_11), FunCall(Get(0), p_12)), FunCall(add, FunCall(Get(1), p_11), FunCall(Get(1), p_12))))), FunCall(Get(0), p_10), FunCall(Get(1), p_10)))), FunCall(Zip(2), p_8, p_9))))), Value("{ 0.0f, 0.0f }", ArrayType(TupleType(Float, Float), v__2)), FunCall(Transpose(), FunCall(Map(fun((p_13) => FunCall(Split(v__3), FunCall(Join(), p_13)))), FunCall(TransposeW(), FunCall(Map(fun((p_14) => FunCall(Map(fun((p_15) => FunCall(Map(fun((p_16) => FunCall(Tuple(2), FunCall(mult, FunCall(Get(0), p_16), FunCall(Get(1), p_16)), FunCall(mult, FunCall(Get(2), p_16), FunCall(Get(1), p_16))))), FunCall(Zip(3), FunCall(Get(0), p_15), FunCall(Get(1), p_14), FunCall(Get(1), p_15))))), FunCall(Zip(2), FunCall(Get(0), p_14), FunCall(Get(2), p_14))))), FunCall(Zip(3), FunCall(Transpose(), FunCall(Map(fun((p_17) => FunCall(Split(v__4), FunCall(Get(0), p_17)))), p_5)), FunCall(Split(v__4), p_2), FunCall(Transpose(), FunCall(Map(fun((p_18) => FunCall(Split(v__4), FunCall(Get(1), p_18)))), p_5)))))))))))))), FunCall(Split(v__2), FunCall(Zip(2), p_0, p_1))))))
    val gold = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Join(), FunCall(MapWrg(0)(fun((p_5) => FunCall(TransposeW(), FunCall(MapSeq(fun((p_6) => FunCall(toGlobal(fun((p_7) => FunCall(MapLcl(0)(fun((p_8) => FunCall(add, FunCall(toPrivate(fun((p_9, p_10) => FunCall(mult, p_9, p_10))), FunCall(Get(0), p_8), p_3), FunCall(toPrivate(fun((p_11, p_12) => FunCall(mult, p_11, p_12))), FunCall(Get(1), p_8), p_4)))), p_7))), p_6))), FunCall(ReduceSeq(fun((p_13, p_14) => FunCall(fun((p_15) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_16) => FunCall(ReduceSeq(fun((p_17, p_18) => FunCall(idTuple2_float_float, FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_17), FunCall(toPrivate(fun((p_19, p_20) => FunCall(mult, p_19, p_20))), FunCall(Get(0), p_18), FunCall(Get(1), p_18))), FunCall(add, FunCall(Get(1), p_17), FunCall(toPrivate(fun((p_21, p_22) => FunCall(mult, p_21, p_22))), FunCall(Get(2), p_18), FunCall(Get(1), p_18))))))), FunCall(Get(0), p_16), FunCall(Zip(3), FunCall(Get(1), p_16), FunCall(Get(1), p_15), FunCall(Get(2), p_16))))), FunCall(Zip(3), p_13, FunCall(Get(0), p_15), FunCall(Get(2), p_15))))), FunCall(toLocal(fun((p_23) => FunCall(Tuple(3), FunCall(Get(0), p_23), FunCall(MapLcl(0)(fun((p_24) => FunCall(idfloat, p_24))), FunCall(Get(1), p_23)), FunCall(Get(2), p_23)))), p_14)))), FunCall(MapLcl(0)(fun((p_25) => FunCall(idTuple2_float_float, p_25))), Value("{ 0.0f, 0.0f }", ArrayType(TupleType(Float, Float), v__2))), FunCall(Zip(3), FunCall(Transpose(), FunCall(Map(fun((p_26) => FunCall(Split(v__3), FunCall(Get(0), p_26)))), p_5)), FunCall(Split(v__3), p_2), FunCall(Transpose(), FunCall(Map(fun((p_27) => FunCall(Split(v__3), FunCall(Get(1), p_27)))), p_5)))))))), FunCall(Split(v__2), FunCall(Zip(2), p_0, p_1))))))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def gesummvPartialReduceWithReorderNoRace(): Unit = {
    val idTuple2_float_float = UserFun("idTuple2_float_float", Array("x"), """|{ return x; }""".stripMargin, Seq(TupleType(Float, Float)), TupleType(Float, Float))

    val f = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, FunCall(Get(0), p_6), p_3), FunCall(mult, FunCall(Get(1), p_6), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_7), FunCall(Get(0), p_8)), FunCall(add, FunCall(Get(1), p_7), FunCall(Get(1), p_8))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)), FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(PartRed(fun((p_10, p_11) => FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_10), FunCall(Get(0), p_11)), FunCall(add, FunCall(Get(1), p_10), FunCall(Get(1), p_11))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)), p_9))), FunCall(Split( K * Pow(v__2, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Scatter(ReorderWithStride(v__2)), FunCall(Join(), FunCall(Map(fun((p_12) => FunCall(Map(fun((p_13) => FunCall(Tuple(2), FunCall(mult, FunCall(Get(0), p_13), FunCall(Get(1), p_13)), FunCall(mult, FunCall(Get(2), p_13), FunCall(Get(1), p_13))))), p_12))), FunCall(Split( K * Pow(v__2, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Zip(3), FunCall(Get(0), p_5), p_2, FunCall(Get(1), p_5))))))))))))))), FunCall(Zip(2), p_0, p_1))))
    val gold = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(MapWrg(0)(fun((p_5) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_6) => FunCall(toGlobal(fun((p_7) => FunCall(MapSeq(fun((p_8) => FunCall(add, FunCall(toPrivate(fun((p_9, p_10) => FunCall(mult, p_9, p_10))), FunCall(Get(0), p_8), p_3), FunCall(toPrivate(fun((p_11, p_12) => FunCall(mult, p_11, p_12))), FunCall(Get(1), p_8), p_4)))), p_7))), FunCall(ReduceSeq(fun((p_13, p_14) => FunCall(idTuple2_float_float, FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_13), FunCall(Get(0), p_14)), FunCall(add, FunCall(Get(1), p_13), FunCall(Get(1), p_14)))))), FunCall(idTuple2_float_float, Value("{ 0.0f, 0.0f }", TupleType(Float, Float))), p_6)))), FunCall(Split(v__2), FunCall(Join(), FunCall(MapLcl(0)(fun((p_15) => FunCall(MapSeq(fun((p_16) => FunCall(toLocal(fun((p_17) => FunCall(idTuple2_float_float, p_17))), p_16))), FunCall(ReduceSeq(fun((p_18, p_19) => FunCall(idTuple2_float_float, FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_18), FunCall(toPrivate(fun((p_20, p_21) => FunCall(mult, p_20, p_21))), FunCall(Get(0), p_19), FunCall(Get(1), p_19))), FunCall(add, FunCall(Get(1), p_18), FunCall(toPrivate(fun((p_22, p_23) => FunCall(mult, p_22, p_23))), FunCall(Get(2), p_19), FunCall(Get(1), p_19))))))), FunCall(idTuple2_float_float, Value("{ 0.0f, 0.0f }", TupleType(Float, Float))), p_15)))), FunCall(Split( K * Pow(v__2, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Zip(3), FunCall(Get(0), p_5), p_2, FunCall(Get(1), p_5))))))))))), FunCall(Zip(2), p_0, p_1))))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def nbodyIntroduceReuse(): Unit = {
    val calcAcc = UserFun("calcAcc", Array("p1", "p2", "deltaT", "espSqr"),
      """|{
         |  float4 r;
         |  r.xyz = p2.xyz - p1.xyz ;
         |  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
         |  float invDist = 1.0f / sqrt(distSqr + espSqr);
         |  float invDistCube = invDist * invDist * invDist;
         |  float s = invDistCube * p2.w;
         |  float4 res;
         |  res.xyz = s * r.xyz;
         |  return res;
         |}
         | """.stripMargin, Seq(VectorType(Float, 4), VectorType(Float, 4), Float, Float), VectorType(Float, 4))

    val update = UserFun("update", Array("pos", "vel", "deltaT", "acceleration"),
      """|{
         |  float4 newPos;
         |  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
         |  newPos.w = pos.w;
         |  float4 newVel;
         |  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
         |  newVel.w = vel.w;
         |  Tuple t = {newPos, newVel};
         |  return t;
         |}
         |      """.stripMargin, Seq(VectorType(Float, 4), VectorType(Float, 4), Float, VectorType(Float, 4)), TupleType(VectorType(Float, 4), VectorType(Float, 4)))

    val idfloat4 = UserFun("idfloat4", Array("x"), """|{ return x; }""".stripMargin, Seq(VectorType(Float, 4)), VectorType(Float, 4))

    val f = fun(ArrayType(VectorType(Float, 4), N), ArrayType(VectorType(Float, 4), N), Float, Float,(p_0, p_1, p_2, p_3) => FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(update, FunCall(Get(0), FunCall(Get(0), p_6)), FunCall(Get(1), FunCall(Get(0), p_6)), p_3, FunCall(Get(1), p_6)))), FunCall(Zip(2), p_4, p_5)))), FunCall(Transpose(), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_7, p_8) => FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(PartRed(fun((p_10, p_11) => FunCall(VectorizeUserFun(Cst(4),add), p_10, p_11))), FunCall(Get(0), p_9), FunCall(Get(1), p_9)))), FunCall(Zip(2), p_7, p_8))))), Value("0.0f", ArrayType(VectorType(Float, 4), v__1)), FunCall(Transpose(), FunCall(Map(fun((p_12) => FunCall(Split(v__2), FunCall(Join(), p_12)))), FunCall(TransposeW(), FunCall(Map(fun((p_13) => FunCall(Map(fun((p_14) => FunCall(Map(fun((p_15) => FunCall(calcAcc, FunCall(Get(0), p_14), p_15, p_3, p_2))), p_13))), p_4))), FunCall(Split(v__3), p_0)))))))))))), FunCall(Split(v__1), FunCall(Zip(2), p_0, p_1)))))
    val gold = fun(ArrayType(VectorType(Float, 4), N), ArrayType(VectorType(Float, 4), N), Float, Float,(p_0, p_1, p_2, p_3) => FunCall(Join(), FunCall(MapWrg(0)(fun((p_4) => FunCall(TransposeW(), FunCall(MapSeq(fun((p_5) => FunCall(toGlobal(fun((p_6) => FunCall(MapLcl(0)(fun((p_7) => FunCall(update, FunCall(Get(0), FunCall(Get(0), p_7)), FunCall(Get(1), FunCall(Get(0), p_7)), p_3, FunCall(Get(1), p_7)))), p_6))), FunCall(Zip(2), p_4, p_5)))), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(fun((p_10) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_11) => FunCall(ReduceSeq(fun((p_12, p_13) => FunCall(VectorizeUserFun(Cst(4),add), p_12, FunCall(toPrivate(fun((p_14, p_15, p_16, p_17) => FunCall(calcAcc, p_14, p_15, p_16, p_17))), FunCall(Get(0), FunCall(Get(1), p_11)), p_13, p_3, p_2)))), FunCall(Get(0), p_11), p_10))), FunCall(Zip(2), p_8, p_4)))), FunCall(toLocal(fun((p_18) => FunCall(MapLcl(0)(fun((p_19) => FunCall(idfloat4, p_19))), p_18))), p_9)))), FunCall(MapLcl(0)(fun((p_20) => FunCall(idfloat4, p_20))), Value("0.0f", ArrayType(VectorType(Float, 4), v__1))), FunCall(Split(v__2), p_0)))))), FunCall(Split(v__1), FunCall(Zip(2), p_0, p_1)))))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def nbodyPartialReduceWithReorderNoRace(): Unit = {
    val calcAcc = UserFun("calcAcc", Array("p1", "p2", "deltaT", "espSqr"),
      """|{
         |  float4 r;
         |  r.xyz = p2.xyz - p1.xyz ;
         |  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
         |  float invDist = 1.0f / sqrt(distSqr + espSqr);
         |  float invDistCube = invDist * invDist * invDist;
         |  float s = invDistCube * p2.w;
         |  float4 res;
         |  res.xyz = s * r.xyz;
         |  return res;
         |}
         | """.stripMargin, Seq(VectorType(Float, 4), VectorType(Float, 4), Float, Float), VectorType(Float, 4))

    val update = UserFun("update", Array("pos", "vel", "deltaT", "acceleration"),
      """|{
         |  float4 newPos;
         |  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
         |  newPos.w = pos.w;
         |  float4 newVel;
         |  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
         |  newVel.w = vel.w;
         |  Tuple t = {newPos, newVel};
         |  return t;
         |}
         |      """.stripMargin, Seq(VectorType(Float, 4), VectorType(Float, 4), Float, VectorType(Float, 4)), TupleType(VectorType(Float, 4), VectorType(Float, 4)))

    val idfloat4 = UserFun("idfloat4", Array("x"), """|{ return x; }""".stripMargin, Seq(VectorType(Float, 4)), VectorType(Float, 4))

    val f = fun(ArrayType(VectorType(Float, 4), N), ArrayType(VectorType(Float, 4), N), Float, Float,(p_0, p_1, p_2, p_3) => FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(update, FunCall(Get(0), p_4), FunCall(Get(1), p_4), p_3, p_5))), FunCall(Reduce(fun((p_6, p_7) => FunCall(VectorizeUserFun(Cst(4),add), p_6, p_7))), Value("0.0f", VectorType(Float, 4)), FunCall(Join(), FunCall(Map(fun((p_8) => FunCall(PartRed(fun((p_9, p_10) => FunCall(VectorizeUserFun(Cst(4),add), p_9, p_10))), Value("0.0f", VectorType(Float, 4)), p_8))), FunCall(Split( N * Pow(v__1, Cst(-1) )), FunCall(Gather(ReorderWithStride(v__1)), FunCall(Scatter(ReorderWithStride(v__1)), FunCall(Join(), FunCall(Map(fun((p_11) => FunCall(Map(fun((p_12) => FunCall(calcAcc, FunCall(Get(0), p_4), p_12, p_3, p_2))), p_11))), FunCall(Split( N * Pow(v__1, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__1)), p_0))))))))))))), FunCall(Zip(2), p_0, p_1)))
    val gold = fun(ArrayType(VectorType(Float, 4), N), ArrayType(VectorType(Float, 4), N), Float, Float,(p_0, p_1, p_2, p_3) => FunCall(MapWrg(0)(fun((p_4) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_5) => FunCall(toGlobal(fun((p_6) => FunCall(MapSeq(fun((p_7) => FunCall(update, FunCall(Get(0), p_4), FunCall(Get(1), p_4), p_3, p_7))), p_6))), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(VectorizeUserFun(Cst(4),add), p_8, p_9))), FunCall(idfloat4, Value("0.0f", VectorType(Float, 4))), p_5)))), FunCall(Split(v__1), FunCall(Join(), FunCall(MapLcl(0)(fun((p_10) => FunCall(MapSeq(fun((p_11) => FunCall(toLocal(fun((p_12) => FunCall(idfloat4, p_12))), p_11))), FunCall(ReduceSeq(fun((p_13, p_14) => FunCall(VectorizeUserFun(Cst(4),add), p_13, FunCall(toPrivate(fun((p_15, p_16, p_17, p_18) => FunCall(calcAcc, p_15, p_16, p_17, p_18))), FunCall(Get(0), p_4), p_14, p_3, p_2)))), FunCall(idfloat4, Value("0.0f", VectorType(Float, 4))), p_10)))), FunCall(Split( N * Pow(v__1, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__1)), p_0))))))))), FunCall(Zip(2), p_0, p_1)))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

}
