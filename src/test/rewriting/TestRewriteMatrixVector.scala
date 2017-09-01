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

  private val N = SizeVar("N")
  private val M = SizeVar("M")

  private val inputSize = 4096

  private val matrix = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
  private val vectorX = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  private val vectorY = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  private val alpha = 2.5f
  private val beta = 1.5f
  private val gold = Utils.matrixVector(matrix, vectorX, vectorY, alpha, beta)

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

  @Test
  def gemvCLBlast(): Unit = {

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

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(64))
    val f2 = Rewrite.applyRuleAtId(f1, 9, Rules.partialReduce)
    val f3 = Rewrite.applyRuleAtId(f2, 10, Rules.partialReduceSplitJoin(64))
    val f4 = Rewrite.applyRuleAtId(f3, 6, Rules.mapFission2)
    val f5 = Rewrite.applyRuleAtId(f4, 8, MacroRules.mapFissionAtPosition(2))
    val f6 = Rewrite.applyRuleAtId(f5, 22, Rules.reduceSeq)
    val f7 = Rewrite.applyRuleAtId(f6, 8, Rules.mapReducePartialReduce)
    val f8 = Rewrite.applyRuleAtId(f7, 14, Rules.splitJoin(64))
    val f10 = Rewrite.applyRuleAtId(f8, 15, Rules.splitIntoZip)
    val f11 = Rewrite.applyRuleAtId(f10, 11, MacroRules.mapMapInterchange)
    val f13 = Rewrite.applyRuleAtId(f11, 6, MacroRules.mapMapInterchange)

    val l0 = Lower.lowerPartialReduces(f13)
    val l1 = Lower.lowerReduces(l0)
    val l2 = SimplifyAndFuse(l1)
    val l3 = Rewrite.applyRulesUntilCannot(l2, Seq(Rules.dropId, Rules.removeEmptyMap))
    val l4 = Lower.lowerNextLevelWithRule(l3, Rules.mapWrg)
    val l5 = Rewrite.applyRuleAtId(l4, 7, Rules.mapSeq)
    val l6 = Lower.lastWriteToGlobal(l5)
    val l7 = Lower.lowerNextLevelWithRule(l6, Rules.mapLcl)
    val l8 = Rewrite.applyRuleAtId(l7, 8, Rules.addIdForCurrentValueInReduce)
    val l9 = Rewrite.applyRuleAtId(l8, 23, Rules.implementOneLevelOfId)
    val l11 = Rewrite.applyRuleAtId(l9, 24, Rules.dropId)
    val l12 = Rewrite.applyRuleAtId(l11, 26, Rules.implementIdAsDeepCopy)
    val l13 = Rewrite.applyRuleAtId(l12, 26, Rules.mapLcl)
    val l14 = Rewrite.applyRuleAtId(l13, 26, Rules.localMemory)
    val l15 = Rewrite.applyRuleAtId(l14, 66, Rules.privateMemory)
    val l16 = Rewrite.applyRuleAtId(l15, 61, Rules.privateMemory)

    val (local, global) = InferNDRange(l16)
    val code = Compile(l16, local, global)

    val (output: Array[Float], _) = Execute()(code, l16, matrix, vectorX, vectorY, alpha, beta)

    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.001f)
  }

}
