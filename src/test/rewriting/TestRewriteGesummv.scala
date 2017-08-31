package rewriting

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor, Utils}
import opencl.ir._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import rewriting.utils.NumberPrinter

object TestRewriteGesummv {
  @BeforeClass
  def before(): Unit = Executor.loadAndInit()

  @AfterClass
  def after(): Unit = Executor.shutdown()
}

class TestRewriteGesummv {

  private val K = SizeVar("K")
  private val N = SizeVar("N")

  private def mvAlpha = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(Float, K),
    Float,
    (matrix, vector, alpha) =>
      Join() o
        Map(fun(row =>
          Map(fun(x => mult(x, alpha))) o
            Reduce(add, 0.0f) o Map(fun(y => mult(y._0, y._1))) $ Zip(row, vector)
        )) $ matrix
  )

  private val vecAdd = fun(
    ArrayTypeWSWC(Float, K),
    ArrayTypeWSWC(Float, K),
    (a,b) => Map(fun(x => add(x._0, x._1))) $ Zip(a, b)
  )

  private val f0 = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(Float, K),
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
    val f13 = Rewrite.applyRuleAtId(f12, 47, Rules.tupleInline)
    val f14 = Rewrite.applyRuleAtId(f13, 17, Rules.tupleInline)

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
  def automaticFusion(): Unit = {

    val f1 = SimplifyAndFuse.withoutPreventingFurtherOptimisation(f0)
    val f2 = Rewrite.applyRulesUntilCannot(f1, Seq(MacroRules.reduceMapFusion))

    val numConcreteMapsAndReduces = Expr.visitWithState(0)(f2.body, {
      case (FunCall(map: AbstractMap, _), a) if map.f.body.isConcrete => a+1
      case (FunCall(map: AbstractPartRed, _, _), a) if map.f.body.isConcrete => a+1
      case (_, a) => a
    })

    assertEquals(3, numConcreteMapsAndReduces)

    val f13 = Rewrite.applyRuleAtId(f2, 47, Rules.tupleInline)
    val f14 = Rewrite.applyRuleAtId(f13, 17, Rules.tupleInline)

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

    println(NumberPrinter(f1))
  }

}
