package exploration

import ir.TypeChecker
import ir.ast.Lambda

object SimplifyAndFuse {

  def apply(lambda: Lambda) = simplifyAndFuse(lambda)

  def simplifyAndFuse(lambda: Lambda): Lambda = {

    simplify(lambda)

  }

  def simplify(lambda: Lambda): Lambda = {
    TypeChecker.check(lambda.body)

    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, simplificationRules)

    if (allRulesAt.isEmpty) {

      val appliedEnablingRule = tryToEnableMoreSimplifications(lambda)

       val enabledMost = appliedEnablingRule.fold(lambda, 0)((a, b) => if (a._2 > b._2) a else b)

      if (enabledMost._2 != 0) {
        simplify(enabledMost._1)
      } else {

        val appliedOneMore = appliedEnablingRule.flatMap(l => tryToEnableMoreSimplifications(l._1))

        val bestRule = appliedOneMore.fold(lambda, 0)((a, b) => if (a._2 > b._2) a else b)

        if (bestRule._2 != 0) {
          simplify(bestRule._1)
        } else {
          fuse(lambda)
        }
      }

    } else {
      val ruleAt = allRulesAt.head
      simplify(Rewrite.applyRuleAtId(lambda, ruleAt._2, ruleAt._1))
    }
  }

  def tryToEnableMoreSimplifications(lambda: Lambda): Seq[(Lambda, Int)] = {
    val enablingRules =
      Seq(
        Rules.mapTransposeTransposeMapTranspose,
        MacroRules.transposeMapMapTranspose,
        Rules.mapSplitTranspose,
        Rules.transposeMapSplit
      )

    val tryNow = Rewrite.rewrite(lambda, enablingRules, 1)

    val simplificationsForExpr =
      tryNow.map(Rewrite.listAllPossibleRewritesForRules(_, simplificationRules).length)

    tryNow zip simplificationsForExpr
  }

  def fuse(lambda: Lambda): Lambda = {
    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, fusionRules)

    TypeChecker.check(lambda.body)

    if (allRulesAt.isEmpty)
      lambda
    else {
      val ruleAt = allRulesAt.head
      simplifyAndFuse(Rewrite.applyRuleAtId(lambda, ruleAt._2, ruleAt._1))
    }
  }
}
