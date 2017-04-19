package rewriting

import exploration.HighLevelRewrite
import ir.ArrayTypeWSWC
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestRewriteMatrixVector {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    Executor.shutdown()
  }
}

class TestRewriteMatrixVector {

  val N = SizeVar("N")
  val M = SizeVar("M")

  @Test
  def gemvAMD(): Unit = {

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, M),
      ArrayTypeWSWC(Float,N),
      Float,
      Float,
      (matrix, vectorX, vectorY, alpha, beta) => {
        Map(fun( t =>
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

    // Algorithmic rewrite
    val f1 = Rewrite.applyRuleAtId(f, 5, Rules.partialReduce)
    val f2 = Rewrite.applyRuleAtId(f1, 6, Rules.partialReduceReorder(128))
    val f3 = Rewrite.applyRuleAtId(f2, 8, Rules.reorderBothSidesWithStride(128))
    val f4 = Rewrite.applyRuleAtId(f3, 7, Rules.gatherScatterId)
    val f5 = Rewrite.applyRuleAtId(f4, 7, Rules.splitJoin(M/^128))
    val f6 = Rewrite.applyRuleAtId(f5, 6, Rules.partialReduceSplitJoin(M/^128))
    val f7 = Rewrite.applyRuleAtId(f6, 8, Rules.splitJoinId)
    val f8 = Rewrite.applyRuleAtId(f7, 7, Rules.mapFusion)
    val f9 = Rewrite.applyRuleAtId(f8, 14, Rules.partialReduceToReduce)
    val f10 = Rewrite.applyRuleAtId(f9, 14, MacroRules.reduceMapFusion)

    // Lower to OpenCL
    val f11 = Lower.lowerPartialReduces(f10)
    val f12 = Lower.lowerReduces(f11)
    val f13 = Rewrite.applyRuleAtId(f12, 34, Rules.implementIdAsDeepCopy)
    val f14 = Rewrite.applyRuleAtId(f13, 27, Rules.implementIdAsDeepCopy)
    val f15 = Lower.lowerNextLevelWithRule(f14, Rules.mapWrg(0))
    val f16 = Lower.lowerNextLevelWithRule(f15, Rules.mapLcl(0))
    val f17 = Rewrite.applyRuleAtId(f16, 15, Rules.localMemory)
    val f18 = Rewrite.applyRuleAtId(f17, 5, Rules.localMemory)
    val f19 = Rewrite.applyRuleAtId(f18, 45, Rules.localMemory)
    val f20 = Rewrite.applyRuleAtId(f19, 41, Rules.localMemory)
    val f21 = Rewrite.applyRuleAtId(f20, 4, Rules.globalMemory)

    val inputSize = 4096

    val matrix = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val vectorX = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val vectorY = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f
    val beta = 1.5f
    val gold = Utils.matrixVector(matrix, vectorX, vectorY, alpha, beta)
    val (local, global) = InferNDRange(f21, matrix, vectorX, vectorY, alpha, beta)

    val (output: Array[Float], _) =
      Execute(local(0).eval, global(0).eval)(f21, matrix, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, output,0.0f)
    assertTrue(HighLevelRewrite.filterByDistance(f11))
  }

  @Test
  def gemvAMDMacro(): Unit = {

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, M),
      ArrayTypeWSWC(Float, N),
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

    val f1 = Rewrite.applyRuleAtId(f, 5, MacroRules.partialReduceWithReorder)

    val f2 = SimplifyAndFuse(f1)
    assertTrue(HighLevelRewrite.filterByDistance(f2))
    Lower.mapCombinations(f2)
  }

  @Test
  def gemvVectorised(): Unit = {

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, M),
      ArrayTypeWSWC(Float, N),
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

    val f1 = Rewrite.applyRuleAtId(f, 6, Rules.vectorizeMapZip(4))
    val f2 = Rewrite.applyRuleAtId(f1, 5, MacroRules.vectorizeReduce(4))
    val f3 = Rewrite.applyRuleAtId(f2, 7, Rules.partialReduceToReduce)
    val f4 = SimplifyAndFuse(f3)
    assertTrue(HighLevelRewrite.filterByDistance(f4))
  }

}
