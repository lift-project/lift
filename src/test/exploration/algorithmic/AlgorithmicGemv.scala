package exploration.algorithmic

import exploration.HighLevelRewrite
import ir._
import ir.ast._
import opencl.executor.LongTestsEnabled
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.Test

object AlgorithmicGemv {
  private val gemv = fun(
    ArrayType(ArrayType(Float, M), N),
    ArrayType(Float, M),
    ArrayType(Float, N),
    Float,
    Float,
    (matrix, vectorX, vectorY, alpha, beta) => {
      Map(fun(t =>
        Map(fun(x =>
          add(
            mult(x, alpha),
            mult(Get(t, 1), beta)
          )
        )) o
          Reduce(add, 0.0f) o
          Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(vectorX, Get(t, 0))
      )) $ Zip(matrix, vectorY)
    })

  private val rewriter = new HighLevelRewrite(4, 2, 5)
  private val rewrittenLambdas = rewriter(gemv)
}

class AlgorithmicGemv {

  LongTestsEnabled()
  import AlgorithmicGemv._

  @Test
  def partialReduceWithReorder(): Unit = {
    val partialReduceWithReorderGold = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, p_6, p_3), FunCall(mult, FunCall(Get(1), p_5), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(add, p_7, p_8))), Value("0.0f", Float), FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(PartRed(fun((p_10, p_11) => FunCall(add, p_10, p_11))), Value("0.0f", Float), p_9))), FunCall(Split(M*1/^v__2), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Scatter(ReorderWithStride(v__2)), FunCall(Join(), FunCall(Map(fun((p_12) => FunCall(Map(fun((p_13) => FunCall(mult, FunCall(Get(0), p_13), FunCall(Get(1), p_13)))), p_12))), FunCall(Split(M*1/^v__2), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Zip(2), p_1, FunCall(Get(0), p_5))))))))))))))), FunCall(Zip(2), p_0, p_2)))

    checkExists(partialReduceWithReorderGold, rewrittenLambdas)
    checkDistance(partialReduceWithReorderGold)
  }

  @Test
  def vectorised(): Unit = {
    val vectorisedGold = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, p_6, p_3), FunCall(mult, FunCall(Get(1), p_5), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(add, p_7, p_8))), Value("0.0f", Float), FunCall(asScalar(), FunCall(PartRed(fun((p_9, p_10) => FunCall(VectorizeUserFun(4,add), p_9, p_10))), Value("0.0f", VectorType(Float, 4)), FunCall(asVector(4), FunCall(asScalar(), FunCall(Map(fun((p_11) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_11), FunCall(Get(1), p_11)))), FunCall(Zip(2), FunCall(asVector(4), p_1), FunCall(asVector(4), FunCall(Get(0), p_5)))))))))))), FunCall(Zip(2), p_0, p_2)))

    checkExists(vectorisedGold, rewrittenLambdas)
    checkDistance(vectorisedGold)
  }

  @Test
  def introduceReuse(): Unit = {
    val introduceReuseGold = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), FunCall(Map(fun((p_6) => FunCall(Map(fun((p_7) => FunCall(add, FunCall(mult, FunCall(Get(1), p_7), p_3), FunCall(mult, FunCall(Get(1), FunCall(Get(0), p_7)), p_4)))), FunCall(Zip(2), p_5, p_6)))), FunCall(Transpose(), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(Join(), FunCall(Map(fun((p_10) => FunCall(PartRed(fun((p_11, p_12) => FunCall(add, p_11, p_12))), FunCall(Get(0), p_10), FunCall(Get(1), p_10)))), FunCall(Zip(2), p_8, p_9))))), Value("0.0f", ArrayType(Float, v__2)), FunCall(Transpose(), FunCall(Map(fun((p_13) => FunCall(Split(v__3), FunCall(Join(), p_13)))), FunCall(TransposeW(), FunCall(Map(fun((p_14) => FunCall(Map(fun((p_15) => FunCall(Map(fun((p_16) => FunCall(mult, FunCall(Get(0), p_16), FunCall(Get(1), p_16)))), FunCall(Zip(2), FunCall(Get(0), p_14), p_15)))), FunCall(Get(1), p_14)))), FunCall(Zip(2), FunCall(Split(v__4), p_1), FunCall(Transpose(), FunCall(Map(fun((p_17) => FunCall(Split(v__4), FunCall(Get(0), p_17)))), p_5)))))))))))))), FunCall(Split(v__2), FunCall(Zip(2), p_0, p_2)))))

    checkExists(introduceReuseGold, rewrittenLambdas)
    checkDistance(introduceReuseGold)
  }
}
