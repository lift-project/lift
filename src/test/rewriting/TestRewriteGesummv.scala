package rewriting

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import org.junit.Assert._
import org.junit.Test
import rewriting.macrorules.{MacroRules, ReuseRules}
import rewriting.rules._

object TestRewriteGesummv extends TestWithExecutor

class TestRewriteGesummv {

  private val K = SizeVar("K")
  private val N = SizeVar("N")

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

  private def f0 = fun(
    ArrayType(ArrayType(Float, K), N),
    ArrayType(ArrayType(Float, K), N),
    ArrayType(Float, K),
    Float,
    Float,
    (A, B, x, alpha, beta) =>
      vecAdd(mvAlpha(A, x, alpha), mvAlpha(B, x, beta))
  )

  private val n = 128

  private val alpha = 2.0f
  private val beta = 1.5f
  private val x = Array.fill(n)(util.Random.nextInt(5).toFloat)
  private val A = Array.fill(n, n)(util.Random.nextInt(5).toFloat)
  private val B = Array.fill(n, n)(util.Random.nextInt(5).toFloat)

  private val tmp1Gold = Utils.matrixVector(A, x, alpha)
  private val tmp2Gold = Utils.matrixVector(B, x, beta)
  private val yGold = (tmp1Gold, tmp2Gold).zipped.map(_+_)

  private val mappings = EnabledMappings(
    global0 = false, global01 = false, global10 = false,
    global012 = false, global210 = false,
    group0 = true, group01 = false, group10 = false)

  @Test
  def simpleFusion(): Unit = {

    val f1 = Rewrite.applyRuleAtId(f0, 0, Rules.splitJoin(1))
    val f2 = Rewrite.applyRuleAtId(f1, 1, Rules.splitIntoZip)
    val f3 = Rewrite.applyRuleAtId(f2, 25, SimplificationRules.splitJoinId)
    val f4 = Rewrite.applyRuleAtId(f3, 3, SimplificationRules.splitJoinId)
    val f5 = Rewrite.applyRuleAtId(f4, 2, FusionRules.mapFusionInZip)
    val f6 = Rewrite.applyRuleAtId(f5, 1, FusionRules.mapFusion)
    val f7 = Rewrite.applyRuleAtId(f6, 5, FusionRules.fuseZipTuple)
    val f8 = Rewrite.applyRuleAtId(f7, 6, FusionRules.mapFusionInZip)
    val f9 = Rewrite.applyRuleAtId(f8, 7, FusionRules.reduceFusionInZip)
    val f10 = Rewrite.applyRuleAtId(f9, 8, FusionRules.mapFusionInZip)
    val f11 = Rewrite.applyRuleAtId(f10, 5, FusionRules.mapFusion)
    val f12 = Rewrite.applyRuleAtId(f11, 6, MacroRules.reduceMapFusion)

    // Not strictly necessary, but makes it look nicer
    val f14 = Rewrite.applyRuleAtId(f12, 17, SimplificationRules.tupleInline)

    val f15 = Lower.lowerNextLevelWithRule(f14, OpenCLRules.mapGlb)
    val f16 = Lower.lowerNextLevelWithRule(f15, OpenCLRules.mapSeq)

    val f17 = Rewrite.applyRuleAtId(f16, 5, OpenCLRules.globalMemory)

    // Won't write to accumulator without this
    val f18 = Rewrite.applyRuleAtId(f17, 17, CopyRules.tupleToStruct)

    val (y: Array[Float], _) = Execute(n)(f18, A, B, x, alpha, beta)

    assertArrayEquals(yGold, y, 0.001f)
  }

  @Test
  def automaticFusion(): Unit = {

    val f1 = SimplifyAndFuse.withoutPreventingFurtherOptimisation(f0)
    val f2 = Rewrite.applyRuleUntilCannot(f1, MacroRules.reduceMapFusion)

    val numConcreteMapsAndReduces = Expr.visitWithState(0)(f2.body, {
      case (FunCall(map: AbstractMap, _), a) if map.f.body.isConcrete => a+1
      case (FunCall(map: AbstractPartRed, _, _), a) if map.f.body.isConcrete => a+1
      case (_, a) => a
    })

    assertEquals(3, numConcreteMapsAndReduces)

    val f15 = Lower.lowerNextLevelWithRule(f2, OpenCLRules.mapGlb)
    val f16 = Lower.lowerNextLevelWithRule(f15, OpenCLRules.mapSeq)

    val f17 = Rewrite.applyRuleAtId(f16, 5, OpenCLRules.globalMemory)

    // Won't write to accumulator without this
    val f18 = Rewrite.applyRuleAtId(f17, 27, CopyRules.tupleToStruct)

    val (y: Array[Float], _) = Execute(n)(f18, A, B, x, alpha, beta)

    assertArrayEquals(yGold, y, 0.001f)
  }

  @Test
  def fuseAndOptimise(): Unit = {

    val f1 = SimplifyAndFuse.withoutPreventingFurtherOptimisation(f0)

    val f2 = Rewrite.applyRuleAtId(f1, 1, Rules.splitJoin(64))
    val f3 = Rewrite.applyRuleAtId(f2, 7, MacroRules.interchange)
    val f4 = Rewrite.applyRuleAtId(f3, 10, ReuseRules.introduceReuseFromMap(64))
    val f5 = Rewrite.applyRuleAtId(f4, 13, ReuseRules.introduceReuseFromMap(64))

    val lowered = Lower.mapCombinations(f5, mappings).head

    val l0 = Rewrite.applyRuleAtId(lowered, 9, CopyRules.addIdForCurrentValueInReduce)
    val l1 = Rewrite.applyRuleAtId(l0, 30, CopyRules.implementOneLevelOfId)
    val l2 = Rewrite.applyRuleAtId(l1, 37, SimplificationRules.dropId)
    val l3 = Rewrite.applyRuleAtId(l2, 34, CopyRules.implementIdAsDeepCopy)
    val l4 = Rewrite.applyRuleAtId(l3, 31, SimplificationRules.dropId)
    val l7 = Rewrite.applyRuleAtId(l4, 33, OpenCLRules.localMemory)
    val l8 = Lower.lowerNextLevelWithRule(l7, OpenCLRules.mapLcl)

    val l9 = Rewrite.applyRuleAtId(l8, 82, MacroRules.userFunCompositionToPrivate)

    val finalExpr = l9
    val (local, global) = InferNDRange(finalExpr)

    val code = Compile(finalExpr, local, global)

    val (y: Array[Float], _) = Execute()(code, finalExpr, A, B, x, alpha, beta)

    assertArrayEquals(yGold, y, 0.001f)
  }

  @Test
  def partialReduceWithReorder(): Unit = {
    val f1 = SimplifyAndFuse.withoutPreventingFurtherOptimisation(f0)

    val g2 = Rewrite.applyRuleAtId(f1, 6, MacroRules.partialReduceWithReorder(128))

    val lowered = Lower.mapCombinations(g2, mappings).head

    val l1 = Rewrite.applyRuleAtId(lowered, 62, MacroRules.userFunCompositionToPrivate)
    val l2 = Rewrite.applyRuleAtId(l1, 17, CopyRules.addIdAfterReduce)
    val l3 = Rewrite.applyRuleAtId(l2, 6, CopyRules.addIdAfterReduce)
    val l4 = Rewrite.applyRuleAtId(l3, 64, CopyRules.implementIdAsDeepCopy)
    val l5 = Rewrite.applyRuleAtId(l4, 64, OpenCLRules.localMemory)
    val l6 = Rewrite.applyRuleAtId(l5, 48, CopyRules.implementIdAsDeepCopy)
    val l7 = Rewrite.applyRuleAtId(l6, 48, OpenCLRules.localMemory)

    val (y: Array[Float], _) = Execute()(l7, A, B, x, alpha, beta)

    assertArrayEquals(yGold, y, 0.001f)
  }

}
