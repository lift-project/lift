package exploration

import ir.TypeChecker
import ir.ast.Lambda

object SimplifyAndFuse {

  def apply(lambda: Lambda) = simplify(lambda)

  def simplify(lambda: Lambda): Lambda = {
    TypeChecker.check(lambda.body)

    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, simplificationRules)

    if (allRulesAt.isEmpty) {

      val enabledMost = tryToEnableMoreSimplifications(lambda, 2)

      if (enabledMost._2 != 0) {
        simplify(enabledMost._1)
      } else {
        fuse(lambda)
      }

    } else {
      val ruleAt = allRulesAt.head
      simplify(Rewrite.applyRuleAtId(lambda, ruleAt._2, ruleAt._1))
    }
  }

  def tryToEnableMoreSimplifications(lambda: Lambda, maxRulesToApply: Int): (Lambda, Int) = {
    if (maxRulesToApply < 1) {
      (lambda, 0)
    } else {
      val appliedEnablingRule = applyOneEnablingRule(lambda)

      val enabledMost = getTheBestRule(lambda, appliedEnablingRule)

      if (enabledMost._2 != 0) {
        enabledMost
      } else {
        val appliedOneMore =
          appliedEnablingRule.map(l => tryToEnableMoreSimplifications(l._1, maxRulesToApply - 1))

        getTheBestRule(lambda, appliedOneMore)
      }
    }
  }

  def getTheBestRule(lambda: Lambda, candidates: Seq[(Lambda, Int)]): (Lambda, Int) =
    candidates.fold(lambda, 0)((a, b) => if (a._2 > b._2) a else b)

  def applyOneEnablingRule(lambda: Lambda): Seq[(Lambda, Int)] = {
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
      simplify(Rewrite.applyRuleAtId(lambda, ruleAt._2, ruleAt._1))
    }
  }
}
