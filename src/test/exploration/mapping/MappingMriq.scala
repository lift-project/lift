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

class MappingMriq {

  LongTestsEnabled()

  private val mapFun = UserFun("mapFun", Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag"),
    """|{
       |    #define PIx2 6.2831853071795864769252867665590058f
       |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
       |    Tuple2_float_float bla = { PhiMag * cos(expArg), PhiMag * sin(expArg) };
       |    return  bla;
       |}""".stripMargin, Seq(Float, Float, Float, Float, Float, Float, Float), TupleType(Float, Float))

  private val reduceFun = UserFun("reduceFun", Array("x", "y"),
    """|{
       | x._0 += y._0;
       | x._1 += y._1;
       | return x;
       |        }""".stripMargin, Seq(TupleType(Float, Float), TupleType(Float, Float)), TupleType(Float, Float))

  private val idTuple4_float_float_float_float = UserFun("idTuple4_float_float_float_float", Array("x"), """|{ return x; }""".stripMargin, Seq(TupleType(Float, Float, Float, Float)), TupleType(Float, Float, Float, Float))

  @Test
  def mriqIntroduceReuse(): Unit = {
    val f = fun(ArrayType(Float, X), ArrayType(Float, X), ArrayType(Float, X), ArrayType(TupleType(Float, Float, Float, Float), K),(p_0, p_1, p_2, p_3) =>
      FunCall(Join(),
        FunCall(Map(fun((p_4) =>
          FunCall(TransposeW(),
            FunCall(ReduceSeq(fun((p_5, p_6) =>
              FunCall(Join(),
                FunCall(Map(fun((p_7) =>
                  FunCall(PartRed(fun((p_8, p_9) =>
                    FunCall(reduceFun, p_8, p_9))),
                    FunCall(Get(0), p_7),
                    FunCall(Get(1), p_7)))),
                  FunCall(Zip(2), p_5, p_6))))), Value("{ 0.0f, 0.0f}", ArrayType(TupleType(Float, Float), v__2)),
              FunCall(Transpose(),
                FunCall(Map(fun((p_10) =>
                  FunCall(Split(v__3), p_10))),
                  FunCall(Map(fun((p_11) =>
                    FunCall(Join(), p_11))),
                    FunCall(TransposeW(),
                      FunCall(Map(fun((p_12) =>
                        FunCall(Map(fun((p_13) =>
                          FunCall(Map(fun((p_14) =>
                            FunCall(mapFun,
                              FunCall(Get(0), p_13),
                              FunCall(Get(1), p_13),
                              FunCall(Get(2), p_13),
                              FunCall(Get(0), p_14),
                              FunCall(Get(1), p_14),
                              FunCall(Get(2), p_14),
                              FunCall(Get(3), p_14)))), p_12))), p_4))),
                        FunCall(Split(v__4), p_3)))))))))),
          FunCall(Split(v__2),
            FunCall(Zip(3), p_0, p_1, p_2)))))

    val gold = fun(ArrayType(Float, X), ArrayType(Float, X), ArrayType(Float, X), ArrayType(TupleType(Float, Float, Float, Float), K),(p_0, p_1, p_2, p_3) =>
      FunCall(Join(),
        FunCall(MapWrg(0)(fun((p_4) =>
          FunCall(TransposeW(),
            FunCall(toGlobal(fun((p_5) =>
              FunCall(MapSeq(fun((p_6) =>
                FunCall(MapLcl(0)(fun((p_7) =>
                  FunCall(idTuple2_float_float, p_7))), p_6))), p_5))),
              FunCall(ReduceSeq(fun((p_8, p_9) =>
                FunCall(fun((p_10) =>
                  FunCall(Join(),
                    FunCall(MapLcl(0)(fun((p_11) =>
                      FunCall(ReduceSeq(fun((p_12, p_13) =>
                        FunCall(reduceFun, p_12,
                          FunCall(toPrivate(fun((p_14, p_15, p_16, p_17, p_18, p_19, p_20) =>
                            FunCall(mapFun, p_14, p_15, p_16, p_17, p_18, p_19, p_20))),
                            FunCall(Get(0),
                              FunCall(Get(1), p_11)),
                            FunCall(Get(1),
                              FunCall(Get(1), p_11)),
                            FunCall(Get(2),
                              FunCall(Get(1), p_11)),
                            FunCall(Get(0), p_13),
                            FunCall(Get(1), p_13),
                            FunCall(Get(2), p_13),
                            FunCall(Get(3), p_13))))),
                        FunCall(Get(0), p_11), p_10))),
                      FunCall(Zip(2), p_8, p_4)))),
                    FunCall(MapLcl(0)(fun((p_22) =>
                      FunCall(toLocal(idTuple4_float_float_float_float), p_22))), p_9)))),
                FunCall(MapLcl(0)(fun((p_23) =>
                  FunCall(idTuple2_float_float, p_23))), Value("{ 0.0f, 0.0f}", ArrayType(TupleType(Float, Float), v__2))),
                FunCall(Split(v__3), p_3)))))),
          FunCall(Split(v__2),
            FunCall(Zip(3), p_0, p_1, p_2)))))

    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def mriqPartialReduceWithReorderNoRace(): Unit = {
    val f = fun(ArrayType(Float, X), ArrayType(Float, X), ArrayType(Float, X), ArrayType(TupleType(Float, Float, Float, Float), K),(p_0, p_1, p_2, p_3) =>
      FunCall(Map(fun((p_4) =>
        FunCall(Reduce(fun((p_5, p_6) =>
          FunCall(reduceFun, p_5, p_6))), Value("{ 0.0f, 0.0f}", TupleType(Float, Float)),
          FunCall(Join(),
            FunCall(Map(fun((p_7) =>
              FunCall(PartRed(fun((p_8, p_9) =>
                FunCall(reduceFun, p_8, p_9))), Value("{ 0.0f, 0.0f}", TupleType(Float, Float)),
                FunCall(Map(fun((p_10) =>
                  FunCall(mapFun,
                    FunCall(Get(0), p_4),
                    FunCall(Get(1), p_4),
                    FunCall(Get(2), p_4),
                    FunCall(Get(0), p_10),
                    FunCall(Get(1), p_10),
                    FunCall(Get(2), p_10),
                    FunCall(Get(3), p_10)))), p_7)))),
              FunCall(Split( K * SimplifyPow(v__2, Cst(-1)) ),
                FunCall(Gather(ReorderWithStride(v__2)), p_3))))))),
        FunCall(Zip(3), p_0, p_1, p_2)))

    val gold = fun(ArrayType(Float, X), ArrayType(Float, X), ArrayType(Float, X), ArrayType(TupleType(Float, Float, Float, Float), K),(p_0, p_1, p_2, p_3) =>
      FunCall(MapWrg(0)(fun((p_4) =>
        FunCall(Join(),
          FunCall(MapLcl(0)(fun((p_5) =>
            FunCall(toGlobal(fun((p_6) =>
              FunCall(MapSeq(fun((p_7) =>
                FunCall(idTuple2_float_float, p_7))), p_6))),
              FunCall(ReduceSeq(fun((p_8, p_9) =>
                FunCall(reduceFun, p_8, p_9))),
                FunCall(idTuple2_float_float, Value("{ 0.0f, 0.0f}", TupleType(Float, Float))), p_5)))),
            FunCall(Split(v__2),
              FunCall(Join(),
                FunCall(MapLcl(0)(fun((p_10) =>
                  FunCall(MapSeq(fun((p_11) =>
                    FunCall(toLocal(fun((p_12) =>
                      FunCall(idTuple2_float_float, p_12))), p_11))),
                    FunCall(ReduceSeq(fun((p_13, p_14) =>
                      FunCall(reduceFun, p_13,
                        FunCall(toPrivate(fun((p_15, p_16, p_17, p_18, p_19, p_20, p_21) =>
                          FunCall(mapFun, p_15, p_16, p_17, p_18, p_19, p_20, p_21))),
                          FunCall(Get(0), p_4),
                          FunCall(Get(1), p_4),
                          FunCall(Get(2), p_4),
                          FunCall(Get(0), p_14),
                          FunCall(Get(1), p_14),
                          FunCall(Get(2), p_14),
                          FunCall(Get(3), p_14))))),
                      FunCall(idTuple2_float_float, Value("{ 0.0f, 0.0f}", TupleType(Float, Float))), p_10)))),
                  FunCall(Split( K * SimplifyPow(v__2, Cst(-1)) ),
                    FunCall(Gather(ReorderWithStride(v__2)), p_3))))))))),
        FunCall(Zip(3), p_0, p_1, p_2)))

    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }
}
