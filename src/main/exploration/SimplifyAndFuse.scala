package exploration

import ir.TypeChecker
import ir.ast.Lambda

object SimplifyAndFuse {

  /**
   * Try to simplify a lambda by eliminating sequences of operations that have
   * no effect and fuse all maps and reduces that can be fused.
   *
   * @param lambda The lambda to simplify
   * @return A lambda where possible simplifications and fusions have been performed
   */
  def apply(lambda: Lambda) = simplify(lambda)

  def simplify(lambda: Lambda): Lambda = {
    TypeChecker.check(lambda.body)

    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, simplificationRules)

    if (allRulesAt.isEmpty) {

      val enabledMost = tryToEnableMoreSimplifications(lambda, 2)

      if (enabledMost._2 != 0)
        simplify(enabledMost._1)
      else
        fuse(lambda)

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

  /**
   * Applies enabling rules where possible.
   *
   * @param lambda The lambda where to apply the rules
   * @return A sequence of lambdas paired with the number of simplification
   *         rules that can be applied in it
   */
  def applyOneEnablingRule(lambda: Lambda): Seq[(Lambda, Int)] = {
    val enablingRules =
      Seq(
        Rules.mapTransposeTransposeMapTranspose,
        MacroRules.transposeMapMapTranspose,
        MacroRules.mapSplitTranspose,
        MacroRules.transposeMapSplit,
        Rules.splitTranspose/*,
        MacroRules.mapTransposeSplit*/ // apply if nothing else works? infinite loop
      )

    val tryNow = Rewrite.rewrite(lambda, enablingRules, 1)

    val possibleSimplifications =
      tryNow.map(Rewrite.listAllPossibleRewritesForRules(_, simplificationRules).length)

    tryNow zip possibleSimplifications
  }

  /**
   * Apply a fusion rule to the lambda if possible and then try to simplify.
   * If no fusion rules apply, returns the lambda.
   *
   * @param lambda The lambda where to apply a fusion rule
   * @return
   */
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
