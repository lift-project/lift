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

class MappingNbody {

  LongTestsEnabled()

  private val calcAcc = UserFun("calcAcc", Array("p1", "p2", "deltaT", "espSqr"),
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

  private val update = UserFun("update", Array("pos", "vel", "deltaT", "acceleration"),
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
       |""".stripMargin, Seq(VectorType(Float, 4), VectorType(Float, 4), Float, VectorType(Float, 4)), TupleType(VectorType(Float, 4), VectorType(Float, 4)))

  private val idfloat4 = UserFun("idfloat4", Array("x"), """|{ return x; }""".stripMargin, Seq(VectorType(Float, 4)), VectorType(Float, 4))

  @Test
  def nbodyIntroduceReuse(): Unit = {
    val f = fun(ArrayType(VectorType(Float, 4), N), ArrayType(VectorType(Float, 4), N), Float, Float,(p_0, p_1, p_2, p_3) =>
      FunCall(Join(),
        FunCall(Map(fun((p_4) =>
          FunCall(TransposeW(),
            FunCall(Map(fun((p_5) =>
              FunCall(Map(fun((p_6) =>
                FunCall(update,
                  FunCall(Get(0),
                    FunCall(Get(0), p_6)),
                  FunCall(Get(1),
                    FunCall(Get(0), p_6)), p_3,
                  FunCall(Get(1), p_6)))),
                FunCall(Zip(2), p_4, p_5)))),
              FunCall(Transpose(),
                FunCall(TransposeW(),
                  FunCall(ReduceSeq(fun((p_7, p_8) =>
                    FunCall(Join(),
                      FunCall(Map(fun((p_9) =>
                        FunCall(PartRed(fun((p_10, p_11) =>
                          FunCall(VectorizeUserFun(Cst(4),add), p_10, p_11))),
                          FunCall(Get(0), p_9),
                          FunCall(Get(1), p_9)))),
                        FunCall(Zip(2), p_7, p_8))))), Value("0.0f", ArrayType(VectorType(Float, 4), v__1)),
                    FunCall(Transpose(),
                      FunCall(Map(fun((p_12) =>
                        FunCall(Split(v__2),
                          FunCall(Join(), p_12)))),
                        FunCall(TransposeW(),
                          FunCall(Map(fun((p_13) =>
                            FunCall(Map(fun((p_14) =>
                              FunCall(Map(fun((p_15) =>
                                FunCall(calcAcc,
                                  FunCall(Get(0), p_14), p_15, p_3, p_2))), p_13))), p_4))),
                            FunCall(Split(v__3), p_0)))))))))))),
          FunCall(Split(v__1),
            FunCall(Zip(2), p_0, p_1)))))

    val gold = fun(ArrayType(VectorType(Float, 4), N), ArrayType(VectorType(Float, 4), N), Float, Float,(p_0, p_1, p_2, p_3) =>
      FunCall(Join(),
        FunCall(MapWrg(0)(fun((p_4) =>
          FunCall(TransposeW(),
            FunCall(MapSeq(fun((p_5) =>
                FunCall(MapLcl(0)(fun((p_7) =>
                  FunCall(toGlobal(update),
                    FunCall(Get(0),
                      FunCall(Get(0), p_7)),
                    FunCall(Get(1),
                      FunCall(Get(0), p_7)), p_3,
                    FunCall(Get(1), p_7)))),
                FunCall(Zip(2), p_4, p_5)))),
              FunCall(ReduceSeq(fun((p_8, p_9) =>
                FunCall(fun((p_10) =>
                  FunCall(Join(),
                    FunCall(MapLcl(0)(fun((p_11) =>
                      FunCall(ReduceSeq(fun((p_12, p_13) =>
                        FunCall(VectorizeUserFun(Cst(4),add), p_12,
                          FunCall(toPrivate(fun((p_14, p_15, p_16, p_17) =>
                            FunCall(calcAcc, p_14, p_15, p_16, p_17))),
                            FunCall(Get(0),
                              FunCall(Get(1), p_11)), p_13, p_3, p_2)))),
                        FunCall(Get(0), p_11), p_10))),
                      FunCall(Zip(2), p_8, p_4)))),
                    FunCall(MapLcl(0)(fun((p_19) =>
                      FunCall(toLocal(idfloat4), p_19))), p_9)))),
                FunCall(MapLcl(0)(fun((p_20) =>
                  FunCall(idfloat4, p_20))), Value("0.0f", ArrayType(VectorType(Float, 4), v__1))),
                FunCall(Split(v__2), p_0)))))),
          FunCall(Split(v__1),
            FunCall(Zip(2), p_0, p_1)))))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def nbodyPartialReduceWithReorderNoRace(): Unit = {
    val f = fun(ArrayType(VectorType(Float, 4), N), ArrayType(VectorType(Float, 4), N), Float, Float,(p_0, p_1, p_2, p_3) =>
      FunCall(Map(fun((p_4) =>
        FunCall(Map(fun((p_5) =>
          FunCall(update,
            FunCall(Get(0), p_4),
            FunCall(Get(1), p_4), p_3, p_5))),
          FunCall(Reduce(fun((p_6, p_7) =>
            FunCall(VectorizeUserFun(Cst(4),add), p_6, p_7))), Value("0.0f", VectorType(Float, 4)),
            FunCall(Join(),
              FunCall(Map(fun((p_8) =>
                FunCall(PartRed(fun((p_9, p_10) =>
                  FunCall(VectorizeUserFun(Cst(4),add), p_9, p_10))), Value("0.0f", VectorType(Float, 4)), p_8))),
                FunCall(Split( N * SimplifyPow(v__1, Cst(-1) )),
                  FunCall(Gather(ReorderWithStride(v__1)),
                    FunCall(Scatter(ReorderWithStride(v__1)),
                      FunCall(Join(),
                        FunCall(Map(fun((p_11) =>
                          FunCall(Map(fun((p_12) =>
                            FunCall(calcAcc,
                              FunCall(Get(0), p_4), p_12, p_3, p_2))), p_11))),
                          FunCall(Split( N * SimplifyPow(v__1, Cst(-1)) ),
                            FunCall(Gather(ReorderWithStride(v__1)), p_0))))))))))))),
        FunCall(Zip(2), p_0, p_1)))

    val gold = fun(ArrayType(VectorType(Float, 4), N), ArrayType(VectorType(Float, 4), N), Float, Float,(p_0, p_1, p_2, p_3) =>
      FunCall(MapWrg(0)(fun((p_4) =>
        FunCall(Join(),
          FunCall(MapLcl(0)(fun((p_5) =>
              FunCall(MapSeq(fun((p_7) =>
                FunCall(toGlobal(update),
                  FunCall(Get(0), p_4),
                  FunCall(Get(1), p_4), p_3, p_7))),
              FunCall(ReduceSeq(fun((p_8, p_9) =>
                FunCall(VectorizeUserFun(Cst(4),add), p_8, p_9))),
                FunCall(idfloat4, Value("0.0f", VectorType(Float, 4))), p_5)))),
            FunCall(Split(v__1),
              FunCall(Join(),
                FunCall(MapLcl(0)(fun((p_10) =>
                  FunCall(MapSeq(fun((p_11) =>
                    FunCall(toLocal(fun((p_12) =>
                      FunCall(idfloat4, p_12))), p_11))),
                    FunCall(ReduceSeq(fun((p_13, p_14) =>
                      FunCall(VectorizeUserFun(Cst(4),add), p_13,
                        FunCall(toPrivate(fun((p_15, p_16, p_17, p_18) =>
                          FunCall(calcAcc, p_15, p_16, p_17, p_18))),
                          FunCall(Get(0), p_4), p_14, p_3, p_2)))),
                      FunCall(idfloat4, Value("0.0f", VectorType(Float, 4))), p_10)))),
                  FunCall(Split( N * SimplifyPow(v__1, Cst(-1)) ),
                    FunCall(Gather(ReorderWithStride(v__1)), p_0))))))))),
        FunCall(Zip(2), p_0, p_1)))

    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(f, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == goldHash))
  }
}
