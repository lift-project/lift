package exploration

import ir.TypeChecker
import ir.ast.{Expr, Lambda}

object SimplifyAndFuse {

  /**
   * Try to simplify a lambda by eliminating sequences of operations that have
   * no effect and fuse all maps and reduces that can be fused.
   *
   * @param lambda The lambda to simplify
   * @return A lambda where possible simplifications and fusions have been performed
   */
  def apply(lambda: Lambda) = simplify(lambda)

  var cantUndo = List[Expr]()

  def simplify(lambda: Lambda): Lambda = {

    val fused = fuseAll(lambda)

    TypeChecker.check(fused.body)

    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(fused, simplificationRules)

    if (allRulesAt.isEmpty) {

      val enabledMost = tryToEnableMoreSimplifications(fused, 4)

      if (enabledMost._2 != 0) {
        simplify(enabledMost._1)
      }
      else {

        val rewrites = Rewrite.listAllPossibleRewrites(fused, Rules.splitTranspose)
        if (rewrites.nonEmpty) {

          val bestTry = rewrites.map(ruleAt => {
            val applyRuleAtId = Rewrite.applyRuleAtId(fused, ruleAt._2, ruleAt._1)

            val replacement =
              Rewrite.getExprForId(applyRuleAtId.body, ruleAt._2, NumberExpression.breadthFirst(applyRuleAtId))

            val expr = Utils.getExprForPatternInCallChain(replacement, { case e if MacroRules.transposeMapSplit.isDefinedAt(e) => }).get

            cantUndo = expr :: cantUndo

            simplify(applyRuleAtId)
          }).minBy(getNumberOfTerms)

          if (getNumberOfTerms(bestTry) < getNumberOfTerms(fused))
            return bestTry

        }

        fused
      }

    } else {
      val ruleAt = allRulesAt.head

      val applied = Rewrite.applyRuleAtId(fused, ruleAt._2, ruleAt._1)

      simplify(applied)
    }
  }

  def getNumberOfTerms(lambda: Lambda) =
    NumberExpression.breadthFirst(lambda).values.max

  def tryToEnableMoreSimplifications(lambda: Lambda,
                                     maxRulesToApply: Int,
                                     rules: List[Rule] = List()): (Lambda, Int, List[Rule]) = {
    if (maxRulesToApply < 1) {
      (lambda, 0, rules)
    } else {
      val appliedEnablingRule = applyOneEnablingRule(lambda, rules)

      val enabledMost = getTheBestRule(lambda, appliedEnablingRule)

      if (enabledMost._2 != 0) {
        enabledMost
      } else {
        val appliedOneMore =
          appliedEnablingRule.map(l => tryToEnableMoreSimplifications(l._1, maxRulesToApply - 1, l._3))

        getTheBestRule(lambda, appliedOneMore)
      }
    }
  }

  def getTheBestRule(lambda: Lambda,
                     candidates: Seq[(Lambda, Int, List[Rule])]): (Lambda, Int, List[Rule]) =
    candidates.fold(lambda, 0, List())((a, b) => if (a._2 > b._2) a else b)

  /**
   * Applies enabling rules where possible.
   *
   * @param lambda The lambda where to apply the rules
   * @return A sequence of lambdas paired with the number of simplification
   *         rules that can be applied in it
   */
  def applyOneEnablingRule(lambda: Lambda, rules: List[Rule]): Seq[(Lambda, Int, List[Rule])] = {
    val enablingRules =
      Seq(
        MacroRules.mapTransposeTransposeMapTranspose,
        MacroRules.transposeMapMapFission,
        MacroRules.mapSplitTranspose,
        MacroRules.transposeMapSplit,
        Rules.reorderTranspose
      )

    TypeChecker.check(lambda.body)

    var allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, enablingRules)

    allRulesAt = allRulesAt.filter(p => if (p._1 == MacroRules.transposeMapSplit) {

      val replacement =
        Rewrite.getExprForId(lambda.body, p._2, NumberExpression.breadthFirst(lambda))

      !cantUndo.exists(_ eq replacement)

    } else {
      true
    })

    val rewritten = allRulesAt.map(ruleAt => fuseAll(Rewrite.applyRuleAtId(lambda, ruleAt._2, ruleAt._1)))

    val tryNow = rewritten

    val possibleSimplifications =
      tryNow.map(Rewrite.listAllPossibleRewritesForRules(_, simplificationRules).length)

    (tryNow,  possibleSimplifications, allRulesAt.map(_._1 +: rules)).zipped.toSeq
  }

  /**
   * Fuse all map-map and reduce-map sequences.
   * If no fusion rules apply, returns the lambda.
   *
   * @param lambda The lambda where to apply fusion
   * @return
   */
  def fuseAll(lambda: Lambda): Lambda = {
    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, fusionRules)

    TypeChecker.check(lambda.body)

    if (allRulesAt.isEmpty)
      lambda
    else {
      val ruleAt = allRulesAt.head
      fuseAll(Rewrite.applyRuleAtId(lambda, ruleAt._2, ruleAt._1))
    }
  }
}
