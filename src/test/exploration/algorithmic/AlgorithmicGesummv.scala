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

object AlgorithmicGesummv {

  private def mvAlpha = fun(
    ArrayType(ArrayType(Float, K), N),
    ArrayType(Float, K),
    Float,
    (matrix, vector, alpha) =>
      Join() o
        Map(fun(row =>
          Map(fun(x => mult(x, alpha))) o
            Reduce(add, 0.0f) o Map(fun(y => mult(y._0, y._1))) $ Zip(row, vector)
        )) $ matrix
  )

  private def vecAdd = fun(
    ArrayType(Float, K),
    ArrayType(Float, K),
    (a,b) => Map(fun(x => add(x._0, x._1))) $ Zip(a, b)
  )

  private val gesummv = fun(
    ArrayType(ArrayType(Float, K), N),
    ArrayType(ArrayType(Float, K), N),
    ArrayType(Float, K),
    Float,
    Float,
    (A, B, x, alpha, beta) =>
      vecAdd(mvAlpha(A, x, alpha), mvAlpha(B, x, beta))
  )

  private val rewriter = new HighLevelRewrite(4, 2, 4)
  private var rewrittenLambdas: Seq[(Lambda, Seq[Rule])] = Seq()

  @BeforeClass
  def before(): Unit =
    if (LongTestsEnabled.areEnabled) rewrittenLambdas = rewrite(rewriter, gesummv)

  @AfterClass
  def after(): Unit = rewrittenLambdas = Seq()
}

class AlgorithmicGesummv {

  LongTestsEnabled()
  import AlgorithmicGesummv._

  @Test
  def introduceReuse(): Unit = {
    val introduceReuseGold = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), FunCall(Map(fun((p_6) => FunCall(Map(fun((p_7) => FunCall(add, FunCall(mult, FunCall(Get(0), p_7), p_3), FunCall(mult, FunCall(Get(1), p_7), p_4)))), p_6))), FunCall(Transpose(), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(Join(), FunCall(Map(fun((p_10) => FunCall(PartRed(fun((p_11, p_12) => FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_11), FunCall(Get(0), p_12)), FunCall(add, FunCall(Get(1), p_11), FunCall(Get(1), p_12))))), FunCall(Get(0), p_10), FunCall(Get(1), p_10)))), FunCall(Zip(2), p_8, p_9))))), Value("{ 0.0f, 0.0f }", ArrayType(TupleType(Float, Float), v__2)), FunCall(Transpose(), FunCall(Map(fun((p_13) => FunCall(Split(v__3), FunCall(Join(), p_13)))), FunCall(TransposeW(), FunCall(Map(fun((p_14) => FunCall(Map(fun((p_15) => FunCall(Map(fun((p_16) => FunCall(Tuple(2), FunCall(mult, FunCall(Get(0), p_16), FunCall(Get(1), p_16)), FunCall(mult, FunCall(Get(2), p_16), FunCall(Get(1), p_16))))), FunCall(Zip(3), FunCall(Get(0), p_15), FunCall(Get(1), p_14), FunCall(Get(1), p_15))))), FunCall(Zip(2), FunCall(Get(0), p_14), FunCall(Get(2), p_14))))), FunCall(Zip(3), FunCall(Transpose(), FunCall(Map(fun((p_17) => FunCall(Split(v__4), FunCall(Get(0), p_17)))), p_5)), FunCall(Split(v__4), p_2), FunCall(Transpose(), FunCall(Map(fun((p_18) => FunCall(Split(v__4), FunCall(Get(1), p_18)))), p_5)))))))))))))), FunCall(Split(v__2), FunCall(Zip(2), p_0, p_1))))))

    checkExists(introduceReuseGold, rewrittenLambdas)
    checkDistance(introduceReuseGold)
  }

  @Test
  def basicFused(): Unit = {
    val basicFusedGold = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, FunCall(Get(0), p_6), p_3), FunCall(mult, FunCall(Get(1), p_6), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_7), FunCall(Get(0), p_8)), FunCall(add, FunCall(Get(1), p_7), FunCall(Get(1), p_8))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)), FunCall(Map(fun((p_9) => FunCall(Tuple(2), FunCall(mult, FunCall(Get(0), p_9), FunCall(Get(1), p_9)), FunCall(mult, FunCall(Get(2), p_9), FunCall(Get(1), p_9))))), FunCall(Zip(3), FunCall(Get(0), p_5), p_2, FunCall(Get(1), p_5))))))), FunCall(Zip(2), p_0, p_1))))

    checkExists(basicFusedGold, rewrittenLambdas)
    checkDistance(basicFusedGold)
  }

  @Test
  def partialReduceWithReorder(): Unit = {
    val partialReduceWithReorder = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, FunCall(Get(0), p_6), p_3), FunCall(mult, FunCall(Get(1), p_6), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_7), FunCall(Get(0), p_8)), FunCall(add, FunCall(Get(1), p_7), FunCall(Get(1), p_8))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)), FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(PartRed(fun((p_10, p_11) => FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_10), FunCall(Get(0), p_11)), FunCall(add, FunCall(Get(1), p_10), FunCall(Get(1), p_11))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)), p_9))), FunCall(Split( K * SimplifyPow(v__2, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Scatter(ReorderWithStride(v__2)), FunCall(Join(), FunCall(Map(fun((p_12) => FunCall(Map(fun((p_13) => FunCall(Tuple(2), FunCall(mult, FunCall(Get(0), p_13), FunCall(Get(1), p_13)), FunCall(mult, FunCall(Get(2), p_13), FunCall(Get(1), p_13))))), p_12))), FunCall(Split(K * SimplifyPow(v__2, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Zip(3), FunCall(Get(0), p_5), p_2, FunCall(Get(1), p_5))))))))))))))), FunCall(Zip(2), p_0, p_1))))

    checkExists(partialReduceWithReorder, rewrittenLambdas)
    checkDistance(partialReduceWithReorder)
  }
}
