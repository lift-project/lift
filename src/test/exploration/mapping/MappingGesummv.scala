package exploration.mapping
import exploration.MemoryMappingRewrite
import ir._
import ir.ast._
import lift.arithmetic.simplifier.SimplifyPow
import lift.arithmetic.{Cst, Pow}
import opencl.executor.LongTestsEnabled
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.utils.Utils.getHash

class MappingGesummv {

  LongTestsEnabled()

  @Test
  def gesummvIntroduceReuse(): Unit = {

    val f = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) =>
      FunCall(Join(),
        FunCall(Join(),
          FunCall(Map(fun((p_5) =>
            FunCall(TransposeW(),
              FunCall(Map(fun((p_6) =>
                FunCall(Map(fun((p_7) =>
                  FunCall(add,
                    FunCall(mult,
                      FunCall(Get(0), p_7), p_3),
                    FunCall(mult,
                      FunCall(Get(1), p_7), p_4)))), p_6))),
                FunCall(Transpose(),
                  FunCall(TransposeW(),
                    FunCall(ReduceSeq(fun((p_8, p_9) =>
                      FunCall(Join(),
                        FunCall(Map(fun((p_10) =>
                          FunCall(PartRed(fun((p_11, p_12) =>
                            FunCall(Tuple(2),
                              FunCall(add,
                                FunCall(Get(0), p_11),
                                FunCall(Get(0), p_12)),
                              FunCall(add,
                                FunCall(Get(1), p_11),
                                FunCall(Get(1), p_12))))),
                            FunCall(Get(0), p_10),
                            FunCall(Get(1), p_10)))),
                          FunCall(Zip(2), p_8, p_9))))), Value("{ 0.0f, 0.0f }", ArrayType(TupleType(Float, Float), v__2)),
                      FunCall(Transpose(),
                        FunCall(Map(fun((p_13) =>
                          FunCall(Split(v__3),
                            FunCall(Join(), p_13)))),
                          FunCall(TransposeW(),
                            FunCall(Map(fun((p_14) =>
                              FunCall(Map(fun((p_15) =>
                                FunCall(Map(fun((p_16) =>
                                  FunCall(Tuple(2),
                                    FunCall(mult,
                                      FunCall(Get(0), p_16),
                                      FunCall(Get(1), p_16)),
                                    FunCall(mult,
                                      FunCall(Get(2), p_16),
                                      FunCall(Get(1), p_16))))),
                                  FunCall(Zip(3),
                                    FunCall(Get(0), p_15),
                                    FunCall(Get(1), p_14),
                                    FunCall(Get(1), p_15))))),
                                FunCall(Zip(2),
                                  FunCall(Get(0), p_14),
                                  FunCall(Get(2), p_14))))),
                              FunCall(Zip(3),
                                FunCall(Transpose(),
                                  FunCall(Map(fun((p_17) =>
                                    FunCall(Split(v__4),
                                      FunCall(Get(0), p_17)))), p_5)),
                                FunCall(Split(v__4), p_2),
                                FunCall(Transpose(),
                                  FunCall(Map(fun((p_18) =>
                                    FunCall(Split(v__4),
                                      FunCall(Get(1), p_18)))), p_5)))))))))))))),
            FunCall(Split(v__2),
              FunCall(Zip(2), p_0, p_1))))))

    val gold = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) =>
      FunCall(Join(),
        FunCall(Join(),
          FunCall(MapWrg(0)(fun((p_5) =>
            FunCall(TransposeW(),
              FunCall(MapSeq(fun((p_6) =>
                  FunCall(MapLcl(0)(fun((p_8) =>
                    FunCall(toGlobal(add),
                      FunCall(toPrivate(fun((p_9, p_10) =>
                        FunCall(mult, p_9, p_10))),
                        FunCall(Get(0), p_8), p_3),
                      FunCall(toPrivate(fun((p_11, p_12) =>
                        FunCall(mult, p_11, p_12))),
                        FunCall(Get(1), p_8), p_4)))), p_6))),
                FunCall(ReduceSeq(fun((p_13, p_14) =>
                  FunCall(fun((p_15) =>
                    FunCall(Join(),
                      FunCall(MapLcl(0)(fun((p_16) =>
                        FunCall(ReduceSeq(fun((p_17, p_18) =>
                          FunCall(idTuple2_float_float,
                            FunCall(Tuple(2),
                              FunCall(add,
                                FunCall(Get(0), p_17),
                                FunCall(toPrivate(fun((p_19, p_20) =>
                                  FunCall(mult, p_19, p_20))),
                                  FunCall(Get(0), p_18),
                                  FunCall(Get(1), p_18))),
                              FunCall(add,
                                FunCall(Get(1), p_17),
                                FunCall(toPrivate(fun((p_21, p_22) =>
                                  FunCall(mult, p_21, p_22))),
                                  FunCall(Get(2), p_18),
                                  FunCall(Get(1), p_18))))))),
                          FunCall(Get(0), p_16),
                          FunCall(Zip(3),
                            FunCall(Get(1), p_16),
                            FunCall(Get(1), p_15),
                            FunCall(Get(2), p_16))))),
                        FunCall(Zip(3), p_13,
                          FunCall(Get(0), p_15),
                          FunCall(Get(2), p_15))))),
                      FunCall(Tuple(3),
                        FunCall(Get(0), p_14),
                        FunCall(MapLcl(0)(fun((p_24) =>
                          FunCall(toLocal(idfloat), p_24))),
                          FunCall(Get(1), p_14)),
                        FunCall(Get(2), p_14))))),
                  FunCall(MapLcl(0)(fun((p_25) =>
                    FunCall(idTuple2_float_float, p_25))), Value("{ 0.0f, 0.0f }", ArrayType(TupleType(Float, Float), v__2))),
                  FunCall(Zip(3),
                    FunCall(Transpose(),
                      FunCall(Map(fun((p_26) =>
                        FunCall(Split(v__3),
                          FunCall(Get(0), p_26)))), p_5)),
                    FunCall(Split(v__3), p_2),
                    FunCall(Transpose(),
                      FunCall(Map(fun((p_27) =>
                        FunCall(Split(v__3),
                          FunCall(Get(1), p_27)))), p_5)))))))),
            FunCall(Split(v__2),
              FunCall(Zip(2), p_0, p_1))))))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def gesummvPartialReduceWithReorderNoRace(): Unit = {

    val f = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) =>
      FunCall(Join(),
        FunCall(Map(fun((p_5) =>
          FunCall(Map(fun((p_6) =>
            FunCall(add,
              FunCall(mult,
                FunCall(Get(0), p_6), p_3),
              FunCall(mult,
                FunCall(Get(1), p_6), p_4)))),
            FunCall(Reduce(fun((p_7, p_8) =>
              FunCall(Tuple(2),
                FunCall(add,
                  FunCall(Get(0), p_7),
                  FunCall(Get(0), p_8)),
                FunCall(add,
                  FunCall(Get(1), p_7),
                  FunCall(Get(1), p_8))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)),
              FunCall(Join(),
                FunCall(Map(fun((p_9) =>
                  FunCall(PartRed(fun((p_10, p_11) =>
                    FunCall(Tuple(2),
                      FunCall(add,
                        FunCall(Get(0), p_10),
                        FunCall(Get(0), p_11)),
                      FunCall(add,
                        FunCall(Get(1), p_10),
                        FunCall(Get(1), p_11))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)), p_9))),
                  FunCall(Split( K * SimplifyPow(v__2, Cst(-1)) ),
                    FunCall(Gather(ReorderWithStride(v__2)),
                      FunCall(Scatter(ReorderWithStride(v__2)),
                        FunCall(Join(),
                          FunCall(Map(fun((p_12) =>
                            FunCall(Map(fun((p_13) =>
                              FunCall(Tuple(2),
                                FunCall(mult,
                                  FunCall(Get(0), p_13),
                                  FunCall(Get(1), p_13)),
                                FunCall(mult,
                                  FunCall(Get(2), p_13),
                                  FunCall(Get(1), p_13))))), p_12))),
                            FunCall(Split( K * SimplifyPow(v__2, Cst(-1)) ),
                              FunCall(Gather(ReorderWithStride(v__2)),
                                FunCall(Zip(3),
                                  FunCall(Get(0), p_5), p_2,
                                  FunCall(Get(1), p_5))))))))))))))),
          FunCall(Zip(2), p_0, p_1))))

    val gold = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) =>
      FunCall(Join(),
        FunCall(MapWrg(0)(fun((p_5) =>
          FunCall(Join(),
            FunCall(MapLcl(0)(fun((p_6) =>
                FunCall(MapSeq(fun((p_8) =>
                  FunCall(toGlobal(add),
                    FunCall(toPrivate(fun((p_9, p_10) =>
                      FunCall(mult, p_9, p_10))),
                      FunCall(Get(0), p_8), p_3),
                    FunCall(toPrivate(fun((p_11, p_12) =>
                      FunCall(mult, p_11, p_12))),
                      FunCall(Get(1), p_8), p_4)))),
                FunCall(ReduceSeq(fun((p_13, p_14) =>
                  FunCall(idTuple2_float_float,
                    FunCall(Tuple(2),
                      FunCall(add,
                        FunCall(Get(0), p_13),
                        FunCall(Get(0), p_14)),
                      FunCall(add,
                        FunCall(Get(1), p_13),
                        FunCall(Get(1), p_14)))))),
                  FunCall(idTuple2_float_float, Value("{ 0.0f, 0.0f }", TupleType(Float, Float))), p_6)))),
              FunCall(Split(v__2),
                FunCall(Join(),
                  FunCall(MapLcl(0)(fun((p_15) =>
                    FunCall(MapSeq(fun((p_16) =>
                      FunCall(toLocal(fun((p_17) =>
                        FunCall(idTuple2_float_float, p_17))), p_16))),
                      FunCall(ReduceSeq(fun((p_18, p_19) =>
                        FunCall(idTuple2_float_float,
                          FunCall(Tuple(2),
                            FunCall(add,
                              FunCall(Get(0), p_18),
                              FunCall(toPrivate(fun((p_20, p_21) =>
                                FunCall(mult, p_20, p_21))),
                                FunCall(Get(0), p_19),
                                FunCall(Get(1), p_19))),
                            FunCall(add,
                              FunCall(Get(1), p_18),
                              FunCall(toPrivate(fun((p_22, p_23) =>
                                FunCall(mult, p_22, p_23))),
                                FunCall(Get(2), p_19),
                                FunCall(Get(1), p_19))))))),
                        FunCall(idTuple2_float_float, Value("{ 0.0f, 0.0f }", TupleType(Float, Float))), p_15)))),
                    FunCall(Split( K * Pow(v__2, Cst(-1)) ),
                      FunCall(Gather(ReorderWithStride(v__2)),
                        FunCall(Zip(3),
                          FunCall(Get(0), p_5), p_2,
                          FunCall(Get(1), p_5))))))))))),
          FunCall(Zip(2), p_0, p_1))))

    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }
}
