package rewriting.rules

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.ir._
import org.junit.Assert._
import org.junit.Test
import rewriting.Rewrite
import rewriting.macrorules.ReuseRules
import rewriting.utils.DumpToFile.dumpLambdaToString

class SequencesForMacroRules {

  private val N = SizeVar("N")

  @Test
  def tileMapMap(): Unit = {

    val f0 = \(
      ArrayType(ArrayType(Float, N), N),
      Map(Map(plusOne)) $ _
    )

    // Sequence
    val f1 = Rewrite.applyRuleAtId(f0, 0, Rules.splitJoin)
    val f2 = Rewrite.applyRuleAtId(f1, 4, InterchangeRules.transposeBothSides)
    val f3 = Rewrite.applyRuleAtId(f2, 5, Rules.splitJoin)
    val f4 = Rewrite.applyRuleAtId(f3, 10, InterchangeRules.transposeBothSides)

    // Macro-rule
    val g = Rewrite.applyRuleAtId(f0, 0, ReuseRules.tileMapMap)

    assertEquals(TypeChecker(g), TypeChecker(f4))
    assertEquals(dumpLambdaToString(g, printNonFixedVarIds = false), dumpLambdaToString(f4, printNonFixedVarIds = false))
  }

  @Test
  def oneDimBlocking(): Unit = {

    val f0 = \(
      ArrayType(ArrayType(ArrayType(Float, N), N), N),
      Map(Map(Map(plusOne))) $ _
    )

    // Sequence
    val f1 = Rewrite.applyRuleAtId(f0, 0, Rules.splitJoin)
    val f2 = Rewrite.applyRuleAtId(f1, 4, InterchangeRules.transposeBothSides)
    val f3 = Rewrite.applyRuleAtId(f2, 8, InterchangeRules.transposeBothSides)

    // Macro-rule
    val g = Rewrite.applyRuleAtId(f0, 0, ReuseRules.apply1DRegisterBlocking)

    assertEquals(TypeChecker(g), TypeChecker(f3))
    assertEquals(dumpLambdaToString(g, printNonFixedVarIds = false), dumpLambdaToString(f3, printNonFixedVarIds = false))
  }

  @Test
  def twoDimBlocking(): Unit = {
    val f0 = \(
      ArrayType(ArrayType(ArrayType(Float, N), N), N),
      Map(Map(Map(plusOne))) $ _
    )

    // Sequence
    val f1 = Rewrite.applyRuleAtId(f0, 0, ReuseRules.tileMapMap)
    val f2 = Rewrite.applyRuleAtId(f1, 14, InterchangeRules.transposeBothSides)

    val f3 = Rewrite.applyRuleAtId(f2, 11, FissionRules.mapFission) // Prepare for next rule
    val f4 = Rewrite.applyRuleAtId(f3, 12, FissionRules.mapFission) // Prepare for next rule

    val f5 = Rewrite.applyRuleAtId(f4, 12, InterchangeRules.transposeBothSides)

    // Macro-rule
    val g = Rewrite.applyRuleAtId(f0, 0, ReuseRules.apply2DRegisterBlockingNoReorder)

    assertEquals(TypeChecker(g), TypeChecker(f5))
    assertEquals(dumpLambdaToString(g, printNonFixedVarIds = false), dumpLambdaToString(f5, printNonFixedVarIds = false))
  }
}
