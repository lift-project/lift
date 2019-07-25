package exploration.algorithmic

import exploration.HighLevelRewrite
import ir._
import ir.ast._
import lift.arithmetic._
import lift.arithmetic.simplifier.SimplifyPow
import opencl.executor.LongTestsEnabled
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.{AfterClass, BeforeClass, Test}
import rewriting.rules.Rule

object AlgorithmicNbody {
  private val calcAcc =
    UserFun("calcAcc", Array("p1", "p2", "deltaT", "espSqr"),
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
         | """.stripMargin,
      Seq(Float4, Float4, Float, Float), Float4)

  private val update =
    UserFun("update", Array("pos", "vel", "deltaT", "acceleration"),
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
      """.stripMargin,
      Seq(Float4, Float4, Float, Float4), TupleType(Float4, Float4))

  private val nbody = fun(
    ArrayType(Float4, N),
    ArrayType(Float4, N),
    Float,
    Float,
    (pos, vel, espSqr, deltaT) =>
      Map(fun(p1 =>
        Map(fun(acceleration =>
          update(Get(p1, 0), Get(p1, 1), deltaT, acceleration)
        )) o Reduce(VectorizeUserFun(4, add), Value("0.0f", Float4)
        ) o Map(\(p2 =>
          calcAcc(Get(p1,0), p2, deltaT, espSqr)
        )) $ pos
      )) $ Zip(pos, vel)
  )

  private val rewriter = new HighLevelRewrite(4, 2, 4)
  private var rewrittenLambdas: Seq[(Lambda, Seq[Rule])] = Seq()

  @BeforeClass
  def before(): Unit =
    if (LongTestsEnabled.areEnabled) rewrittenLambdas = rewrite(rewriter, nbody)

  @AfterClass
  def after(): Unit = rewrittenLambdas = Seq()

}

class AlgorithmicNbody {

  LongTestsEnabled()
  import AlgorithmicNbody._

  @Test
  def partialReduceWithReorder(): Unit = {
    val partialReduceWithReorderGold = fun(ArrayType(VectorType(Float, 4), N), ArrayType(VectorType(Float, 4), N), Float, Float,(p_0, p_1, p_2, p_3) => FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(update, FunCall(Get(0), p_4), FunCall(Get(1), p_4), p_3, p_5))), FunCall(Reduce(fun((p_6, p_7) => FunCall(VectorizeUserFun(Cst(4),add), p_6, p_7))), Value("0.0f", VectorType(Float, 4)), FunCall(Join(), FunCall(Map(fun((p_8) => FunCall(PartRed(fun((p_9, p_10) => FunCall(VectorizeUserFun(Cst(4),add), p_9, p_10))), Value("0.0f", VectorType(Float, 4)), p_8))), FunCall(Split( N * SimplifyPow(v__1, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__1)), FunCall(Scatter(ReorderWithStride(v__1)), FunCall(Join(), FunCall(Map(fun((p_11) => FunCall(Map(fun((p_12) => FunCall(calcAcc, FunCall(Get(0), p_4), p_12, p_3, p_2))), p_11))), FunCall(Split( N * SimplifyPow(v__1, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__1)), p_0))))))))))))), FunCall(Zip(2), p_0, p_1)))

    checkExists(partialReduceWithReorderGold, rewrittenLambdas)
    checkDistance(partialReduceWithReorderGold)
  }

  @Test
  def introduceReuse(): Unit = {
    val introduceReuseGold = fun(ArrayType(VectorType(Float, 4), N), ArrayType(VectorType(Float, 4), N), Float, Float,(p_0, p_1, p_2, p_3) => FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(update, FunCall(Get(0), FunCall(Get(0), p_6)), FunCall(Get(1), FunCall(Get(0), p_6)), p_3, FunCall(Get(1), p_6)))), FunCall(Zip(2), p_4, p_5)))), FunCall(Transpose(), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_7, p_8) => FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(PartRed(fun((p_10, p_11) => FunCall(VectorizeUserFun(Cst(4),add), p_10, p_11))), FunCall(Get(0), p_9), FunCall(Get(1), p_9)))), FunCall(Zip(2), p_7, p_8))))), Value("0.0f", ArrayType(VectorType(Float, 4), v__1)), FunCall(Transpose(), FunCall(Map(fun((p_12) => FunCall(Split(v__2), FunCall(Join(), p_12)))), FunCall(TransposeW(), FunCall(Map(fun((p_13) => FunCall(Map(fun((p_14) => FunCall(Map(fun((p_15) => FunCall(calcAcc, FunCall(Get(0), p_14), p_15, p_3, p_2))), p_13))), p_4))), FunCall(Split(v__3), p_0)))))))))))), FunCall(Split(v__1), FunCall(Zip(2), p_0, p_1)))))

    checkExists(introduceReuseGold, rewrittenLambdas)
    checkDistance(introduceReuseGold)
  }
}
