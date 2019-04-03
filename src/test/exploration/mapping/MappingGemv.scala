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

class MappingGemv {

  LongTestsEnabled()

  private val gemvAmd = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), ArrayTypeWSWC(Float, M), ArrayTypeWSWC(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, p_6, p_3), FunCall(mult, FunCall(Get(1), p_5), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(add, p_7, p_8))), Value("0.0f", Float), FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(ReduceSeq(fun((p_11, p_12) => FunCall(add, p_11, FunCall(mult, FunCall(Get(0), p_12), FunCall(Get(1), p_12))))), Value("0.0f", Float), p_9))), FunCall(Split(M*1/^v__2), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Zip(2), p_1, FunCall(Get(0), p_5)))))))))), FunCall(Zip(2), p_0, p_2)))

  @Test
  def gemvPartialReduceWithReorderNoRace(): Unit = {
    val gold = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) =>
      FunCall(MapWrg(0)(fun((p_5) =>
        FunCall(Join(),
          FunCall(MapLcl(0)(fun((p_6) =>
              FunCall(MapSeq(fun((p_8) =>
                FunCall(toGlobal(add),
                  FunCall(toPrivate(fun((p_9, p_10) =>
                    FunCall(mult, p_9, p_10))), p_8, p_3),
                  FunCall(toPrivate(fun((p_11, p_12) =>
                    FunCall(mult, p_11, p_12))),
                    FunCall(Get(1), p_5), p_4)))),
              FunCall(ReduceSeq(fun((p_13, p_14) =>
                FunCall(add, p_13, p_14))),
                FunCall(idfloat, Value("0.0f", Float)), p_6)))),
            FunCall(Split(v__2),
              FunCall(Join(),
                FunCall(MapLcl(0)(fun((p_15) =>
                  FunCall(MapSeq(fun((p_16) =>
                    FunCall(toLocal(fun((p_17) =>
                      FunCall(idfloat, p_17))), p_16))),
                    FunCall(ReduceSeq(fun((p_18, p_19) =>
                      FunCall(add, p_18,
                        FunCall(toPrivate(fun((p_20, p_21) =>
                          FunCall(mult, p_20, p_21))),
                          FunCall(Get(0), p_19),
                          FunCall(Get(1), p_19))))),
                      FunCall(idfloat, Value("0.0f", Float)), p_15)))),
                  FunCall(Split( M * SimplifyPow(v__2, Cst(-1) )),
                    FunCall(Gather(ReorderWithStride(v__2)),
                      FunCall(Zip(2), p_1,
                        FunCall(Get(0), p_5))))))))))),
        FunCall(Zip(2), p_0, p_2)))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(gemvAmd, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def gemvClblast(): Unit = {
    val f = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) =>
      FunCall(Join(),
        FunCall(Map(fun((p_5) =>
          FunCall(TransposeW(),
            FunCall(Map(fun((p_6) =>
              FunCall(Map(fun((p_7) =>
                FunCall(add,
                  FunCall(mult,
                    FunCall(Get(1), p_7), p_3),
                  FunCall(mult,
                    FunCall(Get(1),
                      FunCall(Get(0), p_7)), p_4)))),
                FunCall(Zip(2), p_5, p_6)))),
              FunCall(Transpose(),
                FunCall(TransposeW(),
                  FunCall(ReduceSeq(fun((p_8, p_9) =>
                    FunCall(Join(),
                      FunCall(Map(fun((p_10) =>
                        FunCall(PartRed(fun((p_11, p_12) =>
                          FunCall(add, p_11, p_12))),
                          FunCall(Get(0), p_10),
                          FunCall(Get(1), p_10)))),
                        FunCall(Zip(2), p_8, p_9))))), Value("0.0f", ArrayType(Float, v__2)),
                    FunCall(Transpose(),
                      FunCall(Map(fun((p_13) =>
                        FunCall(Split(v__3),
                          FunCall(Join(), p_13)))),
                        FunCall(TransposeW(),
                          FunCall(Map(fun((p_14) =>
                            FunCall(Map(fun((p_15) =>
                              FunCall(Map(fun((p_16) =>
                                FunCall(mult,
                                  FunCall(Get(0), p_16),
                                  FunCall(Get(1), p_16)))),
                                FunCall(Zip(2),
                                  FunCall(Get(0), p_14), p_15)))),
                              FunCall(Get(1), p_14)))),
                            FunCall(Zip(2),
                              FunCall(Split(v__4), p_1),
                              FunCall(Transpose(),
                                FunCall(Map(fun((p_17) =>
                                  FunCall(Split(v__4),
                                    FunCall(Get(0), p_17)))), p_5)))))))))))))),
          FunCall(Split(v__2),
            FunCall(Zip(2), p_0, p_2)))))

    val gold = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) =>
      FunCall(Join(),
        FunCall(MapWrg(0)(fun((p_5) =>
          FunCall(TransposeW(),
            FunCall(MapSeq(fun((p_6) =>
                FunCall(MapLcl(0)(fun((p_8) =>
                  FunCall(toGlobal(add),
                    FunCall(toPrivate(fun((p_9, p_10) =>
                      FunCall(mult, p_9, p_10))),
                      FunCall(Get(1), p_8), p_3),
                    FunCall(toPrivate(fun((p_11, p_12) =>
                      FunCall(mult, p_11, p_12))),
                      FunCall(Get(1),
                        FunCall(Get(0), p_8)), p_4)))),
                FunCall(Zip(2), p_5, p_6)))),
              FunCall(ReduceSeq(fun((p_13, p_14) =>
                FunCall(fun((p_15) =>
                  FunCall(Join(),
                    FunCall(MapLcl(0)(fun((p_16) =>
                      FunCall(ReduceSeq(fun((p_17, p_18) =>
                        FunCall(add, p_17,
                          FunCall(toPrivate(fun((p_19, p_20) =>
                            FunCall(mult, p_19, p_20))),
                            FunCall(Get(0), p_18),
                            FunCall(Get(1), p_18))))),
                        FunCall(Get(0), p_16),
                        FunCall(Zip(2),
                          FunCall(Get(0), p_15),
                          FunCall(Get(1), p_16))))),
                      FunCall(Zip(2), p_13,
                        FunCall(Get(1), p_15))))),
                    FunCall(Tuple(2),
                      FunCall(MapLcl(0)(fun((p_22) =>
                        FunCall(toLocal(idfloat), p_22))),
                        FunCall(Get(0), p_14)),
                      FunCall(Get(1), p_14))))),
                FunCall(MapLcl(0)(fun((p_23) =>
                  FunCall(idfloat, p_23))), Value("0.0f", ArrayType(Float, v__2))),
                FunCall(Zip(2),
                  FunCall(Split(v__3), p_1),
                  FunCall(Transpose(),
                    FunCall(Map(fun((p_24) =>
                      FunCall(Split(v__3),
                        FunCall(Get(0), p_24)))), p_5)))))))),
          FunCall(Split(v__2),
            FunCall(Zip(2), p_0, p_2)))))

    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)
    assertTrue(mapped.exists(getHash(_) == goldHash))
  }
}
