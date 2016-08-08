package rewriting

import apart.arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.ir._
import org.junit.Test
import rewriting.utils.NumberPrinter


class TestRewriteGesummv {

  @Test
  def simpleFusion(): Unit = {

    val K = SizeVar("K")
    val N = SizeVar("N")

    def mvAlpha = fun(
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

    val vecAdd = fun(
      ArrayType(Float, K),
      ArrayType(Float, K),
      (a,b) => Map(fun(x => add(x._0, x._1))) $ Zip(a, b)
    )

    TypeChecker(mvAlpha)
    TypeChecker(vecAdd)

    val f0 = fun(
      ArrayType(ArrayType(Float, K), N),
      ArrayType(ArrayType(Float, K), N),
      ArrayType(Float, K),
      Float,
      Float,
      (A, B, x, alpha, beta) =>
      vecAdd(mvAlpha(A, x, alpha), mvAlpha(B, x, beta))
    )

    TypeChecker(f0)

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

    // TODO: Tuple call inlined...
    val f12 = Rewrite.applyRuleAtId(f11, 6, MacroRules.reduceMapFusion)

    TypeChecker(f12)
  }

}
