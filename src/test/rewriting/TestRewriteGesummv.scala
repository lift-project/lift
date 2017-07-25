package rewriting

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor, Utils}
import opencl.ir._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestRewriteGesummv {
  @BeforeClass
  def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    Executor.shutdown()
  }
}

class TestRewriteGesummv {

  @Test
  def simpleFusion(): Unit = {

    val K = SizeVar("K")
    val N = SizeVar("N")

    def mvAlpha = fun(
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

    val vecAdd = fun(
      ArrayTypeWSWC(Float, K),
      ArrayTypeWSWC(Float, K),
      (a,b) => Map(fun(x => add(x._0, x._1))) $ Zip(a, b)
    )

    TypeChecker(mvAlpha)
    TypeChecker(vecAdd)

    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      ArrayTypeWSWC(Float, K),
      Float,
      Float,
      (A, B, x, alpha, beta) =>
      vecAdd(mvAlpha(A, x, alpha), mvAlpha(B, x, beta))
    )

    val f1 = Rewrite.applyRuleAtId(f0, 0, Rules.splitJoin(1))
    val f2 = Rewrite.applyRuleAtId(f1, 1, Rules.splitZip)
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

    val n = 128

    val alpha = 2.0f
    val beta = 1.5f
    val x = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val A = Array.fill(n, n)(util.Random.nextInt(5).toFloat)
    val B = Array.fill(n, n)(util.Random.nextInt(5).toFloat)

    val tmp1Gold = Utils.matrixVector(A, x, alpha)
    val tmp2Gold = Utils.matrixVector(B, x, beta)
    val yGold = (tmp1Gold, tmp2Gold).zipped.map(_+_)

    val (y, _) = Execute(n)[Array[Float]](f18, A, B, x, alpha, beta)

    assertArrayEquals(yGold, y, 0.001f)
  }

}
