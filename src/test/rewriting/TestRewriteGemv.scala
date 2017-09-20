package rewriting

import exploration.HighLevelRewrite
import ir.ArrayType
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import org.junit.Assert._
import org.junit.Test
import rewriting.macrorules.{MacroRules, ReuseRules}
import rewriting.rules._

object TestRewriteGemv extends TestWithExecutor

class TestRewriteGemv {

  private val N = SizeVar("N")
  private val M = SizeVar("M")

  private val inputSize = 4096

  private val matrix = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
  private val vectorX = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  private val vectorY = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  private val alpha = 2.5f
  private val beta = 1.5f
  private val gold = Utils.matrixVector(matrix, vectorX, vectorY, alpha, beta)

  private def f = fun(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(Float, M),
      ArrayType(Float,N),
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

  private val group0Mapping = EnabledMappings(
    global0 = false, global01 = false, global10 = false,
    global012 = false, global210 = false,
    group0 = true, group01 = false, group10 = false)

  private val global0Mapping = EnabledMappings(
    global0 = true, global01 = false, global10 = false,
    global012 = false, global210 = false,
    group0 = false, group01 = false, group10 = false)

  @Test
  def gemvAMD(): Unit = {

    // Algorithmic rewrite
    val f1 = Rewrite.applyRuleAtId(f, 5, ReduceRules.partialReduce)
    val f2 = Rewrite.applyRuleAtId(f1, 6, ReduceRules.partialReduceReorder(128))
    val f3 = Rewrite.applyRuleAtId(f2, 8, Rules.reorderBothSidesWithStride(128))
    val f4 = Rewrite.applyRuleAtId(f3, 7, SimplificationRules.gatherScatterId)
    val f5 = Rewrite.applyRuleAtId(f4, 7, Rules.splitJoin(M/^128))
    val f6 = Rewrite.applyRuleAtId(f5, 6, ReduceRules.partialReduceSplitJoin(M/^128))
    val f7 = Rewrite.applyRuleAtId(f6, 8, SimplificationRules.splitJoinId)
    val f8 = Rewrite.applyRuleAtId(f7, 7, FusionRules.mapFusion)
    val f9 = Rewrite.applyRuleAtId(f8, 14, ReduceRules.partialReduceToReduce)
    val f10 = Rewrite.applyRuleAtId(f9, 14, MacroRules.reduceMapFusion)

    // Lower to OpenCL
    val f11 = Lower.lowerPartialReduces(f10)
    val f12 = Lower.lowerReduces(f11)
    val f13 = Rewrite.applyRuleAtId(f12, 34, CopyRules.implementIdAsDeepCopy)
    val f14 = Rewrite.applyRuleAtId(f13, 27, CopyRules.implementIdAsDeepCopy)
    val f15 = Lower.lowerNextLevelWithRule(f14, OpenCLRules.mapWrg(0))
    val f16 = Lower.lowerNextLevelWithRule(f15, OpenCLRules.mapLcl(0))
    val f17 = Rewrite.applyRuleAtId(f16, 15, OpenCLRules.localMemory)
    val f18 = Rewrite.applyRuleAtId(f17, 5, OpenCLRules.localMemory)
    val f19 = Rewrite.applyRuleAtId(f18, 45, OpenCLRules.localMemory)
    val f20 = Rewrite.applyRuleAtId(f19, 41, OpenCLRules.localMemory)
    val f21 = Rewrite.applyRuleAtId(f20, 4, OpenCLRules.globalMemory)

    val (local, global) = InferNDRange(f21, matrix, vectorX, vectorY, alpha, beta)

    val (output: Array[Float], _) =
      Execute(local(0).eval, global(0).eval)(f21, matrix, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, output,0.0f)
    assertTrue(HighLevelRewrite.filterByDistance(f11))
  }

  @Test
  def gemvAMDMacro(): Unit = {

    val f1 = Rewrite.applyRuleAtId(f, 5, MacroRules.partialReduceWithReorder(128))
    val f2 = SimplifyAndFuse(f1)

    assertTrue(HighLevelRewrite.filterByDistance(f2))

    val lowered = Lower.mapCombinations(f2, group0Mapping).head

    val l0 = Rewrite.applyRuleAtId(lowered, 14, CopyRules.addIdAfterReduce)
    val l1 = Rewrite.applyRuleAtId(l0, 14, OpenCLRules.localMemory)
    val l2 = Rewrite.applyRuleAtId(l1, 28, CopyRules.implementIdAsDeepCopy)
    val l3 = Rewrite.applyRuleAtId(l2, 5, CopyRules.addIdAfterReduce)
    val l4 = Rewrite.applyRuleAtId(l3, 5, OpenCLRules.localMemory)
    val l5 = Rewrite.applyRuleAtId(l4, 38, CopyRules.implementIdAsDeepCopy)
    val l6 = Rewrite.applyRuleUntilCannot(l5, MacroRules.userFunCompositionToPrivate)

    val (output: Array[Float], _) =
      Execute()(l6, matrix, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def partialReduceWithReorderNoRace(): Unit = {
    val f0 = Rewrite.applyRuleAtId(f, 5, MacroRules.partialReduceWithReorder(128))

    // TODO: Apply in lowering.
    val f2 = Lower.pushReduceDeeper(f0)
    val lowered = Lower.mapCombinations(f2, group0Mapping).head

    val l0 = Rewrite.applyRuleUntilCannot(lowered, MacroRules.userFunCompositionToPrivate)
    val l1 = Rewrite.applyRuleAtId(l0, 15, CopyRules.addIdAfterReduce)
    val l2 = Rewrite.applyRuleAtId(l1, 30, CopyRules.implementIdAsDeepCopy)
    val l3 = Rewrite.applyRuleAtId(l2, 30, OpenCLRules.localMemory)

    val (output: Array[Float], _) =
      Execute()(l3, matrix, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def gemvVectorised(): Unit = {

    val f1 = Rewrite.applyRuleAtId(f, 6, OpenCLRules.vectorizeMapZip(4))
    val f2 = Rewrite.applyRuleAtId(f1, 5, MacroRules.vectorizeReduce(4))
    val f3 = Rewrite.applyRuleAtId(f2, 7, ReduceRules.partialReduceToReduce)
    val f4 = SimplifyAndFuse(f3)
    assertTrue(HighLevelRewrite.filterByDistance(f4))

    val lowered = Lower.mapCombinations(f3, global0Mapping).head

    val l0 = Rewrite.applyRuleUntilCannot(lowered, MacroRules.userFunCompositionToPrivate)

    val (output: Array[Float], _) = Execute()(l0, matrix, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def gemvCLBlast(): Unit = {

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(64))

    val f13 = Rewrite.applyRuleAtId(f1, 6, MacroRules.interchange)
    val f5 = Rewrite.applyRuleAtId(f13, 9, ReuseRules.introduceReuseFromMap(64))
    val f11 = Rewrite.applyRuleAtId(f5, 12, ReuseRules.introduceReuseFromMap(64))

    val lowered = Lower.mapCombinations(f11, group0Mapping).head

    val l8 = Rewrite.applyRuleAtId(lowered, 8, CopyRules.addIdForCurrentValueInReduce)
    val l9 = Rewrite.applyRuleAtId(l8, 23, CopyRules.implementOneLevelOfId)
    val l11 = Rewrite.applyRuleAtId(l9, 24, SimplificationRules.dropId)
    val l12 = Rewrite.applyRuleAtId(l11, 26, CopyRules.implementIdAsDeepCopy)
    val l14 = Rewrite.applyRuleAtId(l12, 26, OpenCLRules.localMemory)
    val l13 = Lower.lowerNextLevelWithRule(l14, OpenCLRules.mapLcl)

    val l15 = Rewrite.applyRuleUntilCannot(l13, MacroRules.userFunCompositionToPrivate)

    val (local, global) = InferNDRange(l15)
    val code = Compile(l15, local, global)

    val (output: Array[Float], _) = Execute()(code, l15, matrix, vectorX, vectorY, alpha, beta)

    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def gemvCLBlastFromMapping(): Unit = {

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(64))

    val f13 = Rewrite.applyRuleAtId(f1, 6, MacroRules.interchange)
    val f5 = Rewrite.applyRuleAtId(f13, 9, ReuseRules.introduceReuseFromMap(64))
    val f11 = Rewrite.applyRuleAtId(f5, 12, ReuseRules.introduceReuseFromMap(64))

    val lowered = Lower.mapCombinations(f11, group0Mapping).head

    val l8 = Rewrite.applyRuleAtId(lowered, 8, CopyRules.addIdForCurrentValueInReduce)
    val mapping0 = Rewrite.applyRuleAtId(l8, 23, OpenCLRules.localMemory)
    val mapping1 = Rewrite.applyRuleAtId(mapping0, 25, CopyRules.implementOneLevelOfId)
    val mapping2 = Rewrite.applyRuleAtId(mapping1, 29, CopyRules.implementIdAsDeepCopy)
    val mapping3 = Rewrite.applyRuleAtId(mapping2, 26, SimplificationRules.dropId)
    val mapping4 = Lower.lowerNextLevelWithRule(mapping3, OpenCLRules.mapLcl)
    val mapping5 = Rewrite.applyRuleUntilCannot(mapping4, MacroRules.userFunCompositionToPrivate)

    val (output: Array[Float], _) = Execute()(mapping5, matrix, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, output, 0.001f)
  }
}
