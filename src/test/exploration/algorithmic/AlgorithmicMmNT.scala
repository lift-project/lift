package exploration.algorithmic

import exploration.HighLevelRewrite
import ir._
import ir.ast._
import opencl.executor.LongTestsEnabled
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.Test

class AlgorithmicMmNT {

  LongTestsEnabled()

  private val mmTransposedA = fun(
    ArrayType(ArrayType(Float, M), K),
    ArrayType(ArrayType(Float, N), K),
    (A, B) =>
      Map(fun(aRow =>
        Map(fun(bCol =>
          Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
        )) o Transpose() $ B
      )) o Transpose() $ A
    )

  private val rewriter = new HighLevelRewrite(4, 2, 5)
  private val rewrittenLambdas = rewriter(mmTransposedA)

  @Test
  def tilingWithTwoDBlocking(): Unit = {
    val twoDGold = fun(ArrayType(ArrayType(Float, M), K), ArrayType(ArrayType(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_4))), FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_5)))))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(Map(fun((p_10) => FunCall(Join(), FunCall(Map(fun((p_11) => FunCall(PartRed(fun((p_12, p_13) => FunCall(Map(fun((p_14) => FunCall(Map(fun((p_15) => FunCall(add, FunCall(Get(0), p_15), FunCall(Get(1), p_15)))), FunCall(Zip(2), FunCall(Get(0), p_14), FunCall(Get(1), p_14))))), FunCall(Zip(2), p_12, p_13)))), FunCall(Get(0), p_11), FunCall(Get(1), p_11)))), FunCall(Zip(2), FunCall(Get(0), p_10), FunCall(Get(1), p_10)))))), FunCall(Zip(2), p_8, p_9)))), Value("0.0f", ArrayType(ArrayType(ArrayType(ArrayType(Float, v__4), v__5), v__3*1/^v__4), v__6*1/^v__5)), FunCall(Transpose(), FunCall(Map(fun((p_16) => FunCall(Transpose(), FunCall(Map(fun((p_17) => FunCall(Split(v__7), FunCall(Transpose(), FunCall(Map(fun((p_18) => FunCall(Transpose(), p_18))), FunCall(Transpose(), p_17)))))), FunCall(Split(v__4), FunCall(Transpose(), FunCall(Map(fun((p_19) => FunCall(Gather(ReorderWithStride(v__3 / v__4)), p_19))), p_16))))))), FunCall(Split(v__5), FunCall(Map(fun((p_20) => FunCall(TransposeW(), p_20))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_21) => FunCall(TransposeW(), FunCall(Map(fun((p_22) => FunCall(TransposeW(), FunCall(Scatter(ReorderWithStride(v__3 / v__8)), p_22)))), FunCall(Join(), FunCall(Map(fun((p_23) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_24) => FunCall(TransposeW(), FunCall(Map(fun((p_25) => FunCall(TransposeW(), p_25))), FunCall(TransposeW(), FunCall(Map(fun((p_26) => FunCall(Map(fun((p_27) => FunCall(Map(fun((p_28) => FunCall(mult, p_27, p_28))), FunCall(Get(1), p_26)))), FunCall(Get(0), p_26)))), FunCall(Zip(2), FunCall(Transpose(), p_23), FunCall(Transpose(), p_24)))))))), FunCall(Split(v__8), FunCall(Gather(ReorderWithStride(v__3 / v__8)), FunCall(Transpose(), FunCall(Get(1), p_21))))))))), FunCall(Split(v__9), FunCall(Transpose(), FunCall(Get(0), p_21))))))))), FunCall(Zip(2), FunCall(Split(v__10), FunCall(Transpose(), p_2)), FunCall(Split(v__10), FunCall(Transpose(), p_3)))))))))))))))))), FunCall(Split(v__3), FunCall(Transpose(), p_1))))))), FunCall(Split(v__6), FunCall(Transpose(), p_0)))))

    checkExists(twoDGold, rewrittenLambdas)
    checkDistance(twoDGold)
  }

  @Test
  def plainTiling(): Unit = {
    val plainTilingGold = fun(ArrayType(ArrayType(Float, M), K), ArrayType(ArrayType(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), p_4))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_5, p_6) => FunCall(Map(fun((p_7) => FunCall(Join(), FunCall(Map(fun((p_8) => FunCall(PartRed(fun((p_9, p_10) => FunCall(add, p_9, p_10))), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))), FunCall(Zip(2), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))))), FunCall(Zip(2), p_5, p_6)))), Value("0.0f", ArrayType(ArrayType(Float, v__3), v__4)), FunCall(Transpose(), FunCall(Map(fun((p_11) => FunCall(Transpose(), FunCall(Map(fun((p_12) => FunCall(Split(v__5), p_12))), p_11)))), FunCall(Map(fun((p_13) => FunCall(TransposeW(), p_13))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_14) => FunCall(TransposeW(), FunCall(Map(fun((p_15) => FunCall(TransposeW(), FunCall(Map(fun((p_16) => FunCall(Map(fun((p_17) => FunCall(mult, FunCall(Get(0), p_17), FunCall(Get(1), p_17)))), FunCall(Zip(2), p_15, p_16)))), FunCall(Transpose(), FunCall(Get(1), p_14)))))), FunCall(Transpose(), FunCall(Get(0), p_14)))))), FunCall(Zip(2), FunCall(Split(v__6), FunCall(Transpose(), p_2)), FunCall(Split(v__6), FunCall(Transpose(), p_3))))))))))))))), FunCall(Split(v__3), FunCall(Transpose(), p_1))))))), FunCall(Split(v__4), FunCall(Transpose(), p_0)))))

    checkExists(plainTilingGold, rewrittenLambdas)
    checkDistance(plainTilingGold)
  }

  @Test
  def tilingWithOneDBlocking(): Unit = {
    val oneDGold = fun(ArrayType(ArrayType(Float, M), K), ArrayType(ArrayType(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), p_5))), FunCall(TransposeW(), p_4))))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_6, p_7) => FunCall(Map(fun((p_8) => FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(PartRed(fun((p_10, p_11) => FunCall(Map(fun((p_12) => FunCall(add, FunCall(Get(0), p_12), FunCall(Get(1), p_12)))), FunCall(Zip(2), p_10, p_11)))), FunCall(Get(0), p_9), FunCall(Get(1), p_9)))), FunCall(Zip(2), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))))), FunCall(Zip(2), p_6, p_7)))), Value("0.0f", ArrayType(ArrayType(ArrayType(Float, v__3), v__4), v__5*1/^v__3)), FunCall(Transpose(), FunCall(Map(fun((p_13) => FunCall(Transpose(), FunCall(Map(fun((p_14) => FunCall(Split(v__6), FunCall(Transpose(), p_14)))), FunCall(Transpose(), p_13))))), FunCall(Split(v__3), FunCall(Map(fun((p_15) => FunCall(TransposeW(), p_15))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_16) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_17) => FunCall(Map(fun((p_18) => FunCall(TransposeW(), p_18))), FunCall(TransposeW(), FunCall(Map(fun((p_19) => FunCall(TransposeW(), FunCall(Map(fun((p_20) => FunCall(Map(fun((p_21) => FunCall(mult, p_21, FunCall(Get(1), p_20)))), FunCall(Get(0), p_20)))), FunCall(Zip(2), FunCall(Transpose(), p_17), p_19))))), FunCall(Transpose(), FunCall(Get(1), p_16))))))), FunCall(Split(v__7), FunCall(Transpose(), FunCall(Get(0), p_16)))))))), FunCall(Zip(2), FunCall(Split(v__8), FunCall(Transpose(), p_2)), FunCall(Split(v__8), FunCall(Transpose(), p_3))))))))))))))))), FunCall(Split(v__4), FunCall(Transpose(), p_1))))))), FunCall(Split(v__5), FunCall(Transpose(), p_0)))))

    checkExists(oneDGold, rewrittenLambdas)
    checkDistance(oneDGold)
  }
}
