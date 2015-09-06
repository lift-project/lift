package exploration

import ir.ast._

object SimplifyAndFuse {
  def apply(lambda: Lambda) = (new SimplifyAndFuse)(lambda)
}

class SimplifyAndFuse {

  /**
   * Try to simplify a lambda by eliminating sequences of operations that have
   * no effect and fuse all maps and reduces that can be fused.
   *
   * @param lambda The lambda to simplify
   * @return A lambda where possible simplifications and fusions have been performed
   */
  def apply(lambda: Lambda) = simplify(lambda)

  private var cantUndo = List[Expr]()
  private var seen = List[Expr]()

  def simplify(lambda: Lambda, maxDepth: Int = 100): Lambda = {

    if (maxDepth == 0)
      return lambda

    val fused = fuseAll(lambda)

    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(fused, simplificationRules)

    if (allRulesAt.isEmpty) {

      val enabledMost = tryToEnableMoreSimplifications(fused, 5)

      if (enabledMost._2 != 0) {
        simplify(enabledMost._1, maxDepth-1)
      } else {

        val stored = seen

        val rewrites = Rewrite
          .listAllPossibleRewrites(fused, Rules.splitTranspose)
          .filter(ruleAt => !seen.contains(ruleAt._2))

        if (rewrites.nonEmpty) {

          val bestTry = rewrites.map(ruleAt => {
            val replacement = ruleAt._1.rewrite(ruleAt._2)
            val applyRuleAtId = FunDecl.replace(fused, ruleAt._2, replacement)

            seen = ruleAt._2 :: seen

            val expr = Utils.getExprForPatternInCallChain(replacement,
              { case e if MacroRules.transposeMapSplit.isDefinedAt(e) => }).get
            cantUndo = expr :: cantUndo

            simplify(applyRuleAtId, maxDepth - 1)
          }).minBy(getNumberOfTerms)

          if (getNumberOfTerms(bestTry) < getNumberOfTerms(fused)) {
            seen = stored
            return bestTry
          }

        }

        seen = stored
        fused
      }

    } else {
      val ruleAt = allRulesAt.head

      val applied = Rewrite.applyRuleAt(fused, ruleAt._2, ruleAt._1)

      simplify(applied, maxDepth-1)
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

  private val enablingRules =
    Seq(
      MacroRules.mapTransposeTransposeMapTranspose,
      MacroRules.transposeMapMapFission,
      MacroRules.mapSplitTranspose,
      MacroRules.transposeMapSplit,
      MacroRules.movingJoin,
      Rules.reorderTranspose
    )

  /**
   * Applies enabling rules where possible.
   *
   * @param lambda The lambda where to apply the rules
   * @return A sequence of lambdas paired with the number of simplification
   *         rules that can be applied in it
   */
  def applyOneEnablingRule(lambda: Lambda, rules: List[Rule]): Seq[(Lambda, Int, List[Rule])] = {

    var allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, enablingRules)

    allRulesAt = allRulesAt.filter(p =>
      if (p._1 == MacroRules.transposeMapSplit) {
        val applyHere = p._2
        !cantUndo.exists(_ eq applyHere)
      } else {
        true
      })

    val rewritten = allRulesAt.map(ruleAt =>
      fuseAll(Rewrite.applyRuleAt(lambda, ruleAt._2, ruleAt._1)))

    val numPossibleSimplifications =
      rewritten.map(Rewrite.listAllPossibleRewritesForRules(_, simplificationRules).length)

    (rewritten,  numPossibleSimplifications, allRulesAt.map(_._1 +: rules)).zipped.toSeq
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

    if (allRulesAt.isEmpty)
      lambda
    else {
      val ruleAt = allRulesAt.head
      fuseAll(Rewrite.applyRuleAt(lambda, ruleAt._2, ruleAt._1))
    }
  }
}
