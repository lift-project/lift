package exploration.mapping

import exploration.MemoryMappingRewrite
import ir._
import ir.ast._
import lift.arithmetic._
import opencl.executor.LongTestsEnabled
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.utils.Utils.getHash

class MappingMm {

  LongTestsEnabled()

  private val mmTATiled2DBlocked = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_4))))))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_9, p_10) => FunCall(Map(fun((p_11) => FunCall(Join(), FunCall(Map(fun((p_12) => FunCall(ReduceSeq(fun((p_14, p_15) => FunCall(Map(fun((p_16) => FunCall(Map(fun((p_17) => FunCall(add, FunCall(Get(0), p_17), FunCall(mult, FunCall(Get(1), p_16), FunCall(Get(1), p_17))))), FunCall(Zip(2), FunCall(Get(0), p_16), FunCall(Get(1), p_15))))), FunCall(Zip(2), p_14, FunCall(Get(0), p_15))))), FunCall(Get(0), p_12), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p_11)), FunCall(Transpose(), FunCall(Get(1), p_12)))))), FunCall(Zip(2), FunCall(Get(0), p_11), FunCall(Split(v__4), FunCall(Gather(ReorderWithStride(v__3 / v__4)), FunCall(Transpose(), FunCall(Get(1), p_10))))))))), FunCall(Zip(2), p_9, FunCall(Split(v__5), FunCall(Transpose(), FunCall(Get(0), p_10))))))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3*1/^v__4), v__6*1/^v__5)), FunCall(Zip(2), p_2, p_3)))))))), FunCall(Transpose(), FunCall(Map(fun((p_22) => FunCall(Transpose(), p_22))), FunCall(Split(v__7), FunCall(Map(fun((p_23) => FunCall(Split(v__3), p_23))), p_1))))))))), FunCall(Transpose(), FunCall(Map(fun((p_24) => FunCall(Transpose(), p_24))), FunCall(Split(v__7), FunCall(Map(fun((p_25) => FunCall(Split(v__6), p_25))), p_0)))))))

  private val mmTBMali = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), (p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), p_4))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_6, p_7) => FunCall(Map(fun((p_8) => FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(Reduce(fun((p_10, p_11) => FunCall(add, p_10, p_11))), FunCall(Get(0), p_9), FunCall(asScalar(), FunCall(Map(fun((p_12) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_12), FunCall(Get(1), p_12)))), FunCall(Zip(2), FunCall(asVector(4), FunCall(Get(1), p_8)), FunCall(asVector(4), FunCall(Get(1), p_9)))))))), FunCall(Zip(2), FunCall(Get(0), p_8), FunCall(Transpose(), FunCall(Get(1), p_7))))))), FunCall(Zip(2), p_6, FunCall(Transpose(), FunCall(Get(0), p_7)))))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4)), FunCall(Zip(2), FunCall(Split(v__5), FunCall(Transpose(), p_2)), FunCall(Split(v__5), FunCall(Transpose(), p_3))))))))), FunCall(Split(v__3), p_1)))))), FunCall(Split(v__4), p_0))))

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
                                          FunCall(Tuple(2),
                                            FunCall(MapSeq(fun((p_27) =>
                                              FunCall(toPrivate(idfloat), p_27))),
                                              FunCall(Get(0), p_20)),
                                            FunCall(MapSeq(fun((p_28) =>
                                              FunCall(toPrivate(idfloat), p_28))),
                                              FunCall(Get(1), p_20)))))),
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
                                FunCall(Unzip(),
                                  FunCall(MapLcl(1)(fun((p_30) =>
                                    FunCall(Tuple(2),
                                      FunCall(MapLcl(0)(fun((p_31) =>
                                        FunCall(toLocal(idfloat), p_31))),
                                        FunCall(Get(0), p_30)),
                                      FunCall(MapLcl(0)(fun((p_32) =>
                                        FunCall(toLocal(idfloat), p_32))),
                                        FunCall(Get(1), p_30))))),
                                    FunCall(Zip(2),
                                      FunCall(Get(0), p_15),
                                      FunCall(Get(1), p_15))))))),
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
                                          FunCall(Tuple(2),
                                            FunCall(MapSeq(fun((p_27) =>
                                              FunCall(toLocal(idfloat), p_27))),
                                              FunCall(Get(0), p_19)),
                                            FunCall(MapSeq(fun((p_28) =>
                                              FunCall(toLocal(idfloat), p_28))),
                                              FunCall(Get(1), p_19)))))),
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

    val vectorLoadsGold = fun(
      ArrayType(ArrayType(Float, M), K),
      ArrayType(ArrayType(Float, N), K),
      (p_0, p_1) =>
        FunCall(Join(),
          FunCall(MapWrg(1)(fun((p_2) =>
            FunCall(TransposeW(),
              FunCall(Join(),
                FunCall(MapWrg(0)(fun((p_3) =>
                  FunCall(TransposeW(),
                    FunCall(Join(),
                      FunCall(Map(fun((p_4) =>
                        FunCall(Map(fun((p_5) =>
                          FunCall(Scatter(ReorderWithStride(v__5 / v__7)), p_5))),
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
                                          FunCall(Tuple(2),
                                            FunCall(asScalar(),
                                              FunCall(MapSeq(fun((p_26) =>
                                                FunCall(toPrivate(fun((p_27) =>
                                                  FunCall(VectorizeUserFun(Cst(4),idfloat), p_27))), p_26))),
                                                FunCall(asVector(4),
                                                  FunCall(Get(0), p_20)))),
                                            FunCall(asScalar(),
                                              FunCall(MapSeq(fun((p_28) =>
                                                FunCall(toPrivate(fun((p_29) =>
                                                  FunCall(VectorizeUserFun(Cst(4),idfloat), p_29))), p_28))),
                                                FunCall(asVector(4),
                                                  FunCall(Get(1), p_20)))))))),
                                        FunCall(Get(0), p_18),
                                        FunCall(Zip(2),
                                          FunCall(Transpose(),
                                            FunCall(Get(1), p_17)),
                                          FunCall(Transpose(),
                                            FunCall(Get(1), p_18)))))),
                                      FunCall(Zip(2),
                                        FunCall(Get(0), p_17),
                                        FunCall(Split(v__7),
                                          FunCall(Gather(ReorderWithStride(v__5 / v__7)),
                                            FunCall(Transpose(),
                                              FunCall(Get(1), p_16))))))))),
                                  FunCall(Zip(2), p_14,
                                    FunCall(Split(v__6),
                                      FunCall(Transpose(),
                                        FunCall(Get(0), p_16)))))),
                                FunCall(Unzip(),
                                  FunCall(MapLcl(1)(fun((p_30) =>
                                    FunCall(Tuple(2),
                                      FunCall(asScalar(),
                                        FunCall(MapLcl(0)(fun((p_31) =>
                                          FunCall(toLocal(fun((p_32) =>
                                            FunCall(VectorizeUserFun(Cst(4),idfloat), p_32))), p_31))),
                                          FunCall(asVector(4),
                                            FunCall(Get(0), p_30)))),
                                      FunCall(asScalar(),
                                        FunCall(MapLcl(0)(fun((p_33) =>
                                          FunCall(toLocal(fun((p_34) =>
                                            FunCall(VectorizeUserFun(Cst(4),idfloat), p_34))), p_33))),
                                          FunCall(asVector(4),
                                            FunCall(Get(1), p_30))))))),
                                    FunCall(Zip(2),
                                      FunCall(Get(0), p_15),
                                      FunCall(Get(1), p_15))))))),
                              FunCall(MapLcl(1)(fun((p_35) =>
                                FunCall(MapLcl(0)(fun((p_36) =>
                                  FunCall(MapSeq(fun((p_37) =>
                                    FunCall(MapSeq(fun((p_38) =>
                                      FunCall(idfloat, p_38))), p_37))), p_36))), p_35))), Value("0.0f", ArrayType(ArrayType(ArrayType(ArrayType(Float, v__7), v__6), v__5*1/^v__7), v__3*1/^v__6))),
                              FunCall(Zip(2), p_2, p_3))))))))),
                  FunCall(Transpose(),
                    FunCall(Map(fun((p_39) =>
                      FunCall(Transpose(), p_39))),
                      FunCall(Split(v__4),
                        FunCall(Map(fun((p_40) =>
                          FunCall(Split(v__5), p_40))), p_1))))))))),
            FunCall(Transpose(),
              FunCall(Map(fun((p_41) =>
                FunCall(Transpose(), p_41))),
                FunCall(Split(v__4),
                  FunCall(Map(fun((p_42) =>
                    FunCall(Split(v__3), p_42))), p_0)))))))
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
}
