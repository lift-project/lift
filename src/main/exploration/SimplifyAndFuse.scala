package exploration

import ir.TypeChecker
import ir.ast.Lambda

object SimplifyAndFuse {

  def apply(lambda: Lambda) = simplifyAndFuse(lambda)

  def simplifyAndFuse(lambda: Lambda): Lambda = {
    val rules = simplificationRules ++ fusionRules

    TypeChecker.check(lambda.body)

    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, rules)

    if (allRulesAt.isEmpty)
      lambda
    else {
      val ruleAt = allRulesAt.head
      simplifyAndFuse(Rewrite.applyRuleAtId(lambda, ruleAt._2, ruleAt._1))
    }
  }
}
