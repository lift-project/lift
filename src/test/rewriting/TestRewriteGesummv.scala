package rewriting

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import org.junit.Assert._
import org.junit.Test

object TestRewriteGesummv extends TestWithExecutor

class TestRewriteGesummv {

  private val K = SizeVar("K")
  private val N = SizeVar("N")

  private def mvAlpha = fun(
    ArrayType(ArrayTypeWSWC(Float, K), N),
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
    ArrayType(ArrayTypeWSWC(Float, K), N),
    ArrayType(ArrayTypeWSWC(Float, K), N),
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

  @Test
  def simpleFusion(): Unit = {

    val f1 = Rewrite.applyRuleAtId(f0, 0, Rules.splitJoin(1))
    val f2 = Rewrite.applyRuleAtId(f1, 1, Rules.splitIntoZip)
    val f3 = Rewrite.applyRuleAtId(f2, 25, Rules.splitJoinId)
    val f4 = Rewrite.applyRuleAtId(f3, 3, Rules.splitJoinId)
    val f5 = Rewrite.applyRuleAtId(f4, 2, Rules.mapFusionInZip)
    val f6 = Rewrite.applyRuleAtId(f5, 1, Rules.mapFusion)
    val f7 = Rewrite.applyRuleAtId(f6, 5, Rules.fuseZipTuple)
    val f8 = Rewrite.applyRuleAtId(f7, 6, Rules.mapFusionInZip)
    val f9 = Rewrite.applyRuleAtId(f8, 7, Rules.reduceFusionInZip)
    val f10 = Rewrite.applyRuleAtId(f9, 8, Rules.mapFusionInZip)
    val f11 = Rewrite.applyRuleAtId(f10, 5, Rules.mapFusion)
    val f12 = Rewrite.applyRuleAtId(f11, 6, MacroRules.reduceMapFusion)

    // Not strictly necessary, but makes it look nicer
    val f14 = Rewrite.applyRuleAtId(f12, 17, Rules.tupleInline)

    // Still uses x twice. Flatten zips and get rid of duplicates?
    // Could issue only one load. Would it make a difference?
    // Would it be easier to rewrite and optimise later? Probably.

    val f15 = Lower.lowerNextLevelWithRule(f14, Rules.mapGlb)
    val f16 = Lower.lowerNextLevelWithRule(f15, Rules.mapSeq)

    val f17 = Rewrite.applyRuleAtId(f16, 5, Rules.globalMemory)

    // Won't write to accumulator without this
    val f18 = Rewrite.applyRuleAtId(f17, 17, Rules.tupleToStruct)

    val (y: Array[Float], _) = Execute(n)(f18, A, B, x, alpha, beta)

    assertArrayEquals(yGold, y, 0.001f)
  }

  @Test
  def automaticFusion(): Unit = {

    val f1 = SimplifyAndFuse.withoutPreventingFurtherOptimisation(f0)
    val f2 = Rewrite.applyRulesUntilCannot(f1, Seq(MacroRules.reduceMapFusion))

    val numConcreteMapsAndReduces = Expr.visitWithState(0)(f2.body, {
      case (FunCall(map: AbstractMap, _), a) if map.f.body.isConcrete => a+1
      case (FunCall(map: AbstractPartRed, _, _), a) if map.f.body.isConcrete => a+1
      case (_, a) => a
    })

    assertEquals(3, numConcreteMapsAndReduces)

    val f14 = Rewrite.applyRuleAtId(f2, 17, Rules.tupleInline)

    // Still uses x twice. Flatten zips and get rid of duplicates?
    // Could issue only one load. Would it make a difference?

    val f15 = Lower.lowerNextLevelWithRule(f14, Rules.mapGlb)
    val f16 = Lower.lowerNextLevelWithRule(f15, Rules.mapSeq)

    val f17 = Rewrite.applyRuleAtId(f16, 5, Rules.globalMemory)

    // Won't write to accumulator without this
    val f18 = Rewrite.applyRuleAtId(f17, 17, Rules.tupleToStruct)

    val (y: Array[Float], _) = Execute(n)(f18, A, B, x, alpha, beta)

    assertArrayEquals(yGold, y, 0.001f)
  }

  @Test
  def fuseAndOptimise(): Unit = {

    val f1 = SimplifyAndFuse.withoutPreventingFurtherOptimisation(f0)

    val g = Rewrite.applyRuleUntilCannot(f1, Rules.flattenZips)

    val f2 = Rewrite.applyRuleAtId(g, 1, Rules.splitJoin(64))
    val f3 = Rewrite.applyRuleAtId(f2, 7, MacroRules.interchange)
    val f4 = Rewrite.applyRuleAtId(f3, 10, MacroRules.introduceReuseFromMap(64))
    val f5 = Rewrite.applyRuleAtId(f4, 13, MacroRules.introduceReuseFromMap(64))

    val mappings = EnabledMappings(
      global0 = false, global01 = false, global10 = false,
      global012 = false, global210 = false,
      group0 = true, group01 = false, group10 = false)

    val lowered = Lower.mapCombinations(f5, mappings).head

    // TODO: Get rid of the second copy of `x` in local memory

    val l0 = Rewrite.applyRuleAtId(lowered, 9, Rules.addIdForCurrentValueInReduce)
    val l1 = Rewrite.applyRuleAtId(l0, 32, Rules.implementOneLevelOfId)
    val l2 = Rewrite.applyRuleAtId(l1, 42, Rules.dropId)
    val l3 = Rewrite.applyRuleAtId(l2, 39, Rules.implementIdAsDeepCopy)
    val l4 = Rewrite.applyRuleAtId(l3, 36, Rules.dropId)
    val l5 = Rewrite.applyRuleAtId(l4, 33, Rules.implementIdAsDeepCopy)
    val l6 = Rewrite.applyRuleAtId(l5, 40, Rules.localMemory)
    val l7 = Rewrite.applyRuleAtId(l6, 33, Rules.localMemory)
    val l8 = Lower.lowerNextLevelWithRule(l7, Rules.mapLcl)

    // TODO: Private memory for last temp results

    val finalExpr = l8
    val (local, global) = InferNDRange(finalExpr)

    val code = Compile(finalExpr, local, global)

    val (y: Array[Float], _) = Execute()(code, finalExpr, A, B, x, alpha, beta)

    assertArrayEquals(yGold, y, 0.001f)
  }

}
