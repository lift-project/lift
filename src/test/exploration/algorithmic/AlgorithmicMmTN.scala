package exploration.algorithmic

import exploration.HighLevelRewrite
import ir._
import ir.ast._
import opencl.executor.LongTestsEnabled
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.Test
import rewriting.macrorules.ReuseRules

class AlgorithmicMmTN {

  LongTestsEnabled()

  private val mmTransposedB = fun(
    ArrayType(ArrayType(Float, K), M),
    ArrayType(ArrayType(Float, K), N),
    (A, B) => {
      Map(fun(aRow =>
        Map(fun(bCol =>
          Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
        )) $ B
      )) $ A
    })

  private val rewriter = new HighLevelRewrite(4, 2, 5)
  private val rewrittenLambdas = rewriter(mmTransposedB)

  @Test
  def vectorised(): Unit = {
    val vectorisedGold = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N),(p_0, p_1) => FunCall(Map(fun((p_2) => FunCall(Map(fun((p_3) => FunCall(Reduce(fun((p_4, p_5) => FunCall(add, p_4, p_5))), Value("0.0f", Float), FunCall(asScalar(), FunCall(PartRed(fun((p_6, p_7) => FunCall(VectorizeUserFun(4,add), p_6, p_7))), Value("0.0f", VectorType(Float, 4)), FunCall(asVector(4), FunCall(asScalar(), FunCall(Map(fun((p_8) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))), FunCall(Zip(2), FunCall(asVector(4), p_2), FunCall(asVector(4), p_3)))))))))), p_1))), p_0))

    checkExists(vectorisedGold, rewrittenLambdas)
    checkDistance(vectorisedGold)
  }

  @Test
  def partiallyVectorisedTiled(): Unit = {
    val partiallyVectorisedTiledGold = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), p_4))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_5, p_6) => FunCall(Map(fun((p_7) => FunCall(Join(), FunCall(Map(fun((p_8) => FunCall(PartRed(fun((p_9, p_10) => FunCall(add, p_9, p_10))), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))), FunCall(Zip(2), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))))), FunCall(Zip(2), p_5, p_6)))), Value("0.0f", ArrayType(ArrayType(Float, v__3), v__4)), FunCall(Transpose(), FunCall(Map(fun((p_11) => FunCall(Transpose(), FunCall(Map(fun((p_12) => FunCall(Split(v__5), p_12))), p_11)))), FunCall(Map(fun((p_13) => FunCall(TransposeW(), p_13))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_14) => FunCall(TransposeW(), FunCall(Map(fun((p_15) => FunCall(TransposeW(), FunCall(Map(fun((p_16) => FunCall(asScalar(), FunCall(Map(fun((p_17) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_17), FunCall(Get(1), p_17)))), FunCall(Zip(2), FunCall(asVector(4), p_15), FunCall(asVector(4), p_16)))))), FunCall(Transpose(), FunCall(Get(1), p_14)))))), FunCall(Transpose(), FunCall(Get(0), p_14)))))), FunCall(Zip(2), FunCall(Split(v__6), FunCall(Transpose(), p_2)), FunCall(Split(v__6), FunCall(Transpose(), p_3))))))))))))))), FunCall(Split(v__3), p_1)))))), FunCall(Split(v__4), p_0))))

    checkExists(partiallyVectorisedTiledGold, rewrittenLambdas)
    checkDistance(partiallyVectorisedTiledGold)
  }

}
