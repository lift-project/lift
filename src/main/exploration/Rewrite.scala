package exploration

import ir._
import ir.ast._

object Rewrite {

  def getExprForId(expr: Expr, id: Int, idMap: collection.Map[Expr, Int]): Expr =
    idMap.find(pair => pair._2 == id).get._1

  def applyRuleAtId(lambda: Lambda, id: Int, rule: Rule): Lambda = {
    val replacement = applyRuleAtId(lambda.body, id, rule)
    Lambda(lambda.params, replacement)
  }

  def applyRuleAt(lambda: Lambda, expr: Expr, rule: Rule): Lambda = {
    val replacement = applyRuleAt(lambda.body, rule, expr)
    Lambda(lambda.params, replacement)
  }

  def applyRuleAtId(expr: Expr, id: Int, rule: Rule): Expr = {
    val numbering = NumberExpression.breadthFirst(expr)
    applyRuleAtId(expr, id, rule, numbering)
  }

  def depthFirstApplyRuleAtId(expr:Expr, id: Int, rule: Rule): Expr = {
    val numbering = NumberExpression.depthFirst(expr)
    applyRuleAtId(expr, id, rule, numbering)
  }

  def applyRuleAtId(expr: Expr, id: Int, rule: Rule, numbering: collection.Map[Expr, Int]): Expr = {
    val toBeReplaced = getExprForId(expr, id, numbering)
    applyRuleAt(expr, rule, toBeReplaced)
  }

  def applyRuleAt(expr: Expr, rule: Rule, toBeReplaced: Expr): Expr = {
    TypeChecker.check(expr)
    Context.updateContext(expr)
    Expr.replace(expr, toBeReplaced, rule.rewrite(toBeReplaced))
  }

  private[exploration] def listAllPossibleRewritesForRules(lambda: Lambda,
                                                           rules: Seq[Rule]): Seq[(Rule, Expr)] = {
    Context.updateContext(lambda.body)
    TypeChecker.check(lambda.body)
    rules.map(rule => listAllPossibleRewrites(lambda, rule)).reduce(_ ++ _)
  }

  private[exploration] def listAllPossibleRewrites(lambda: Lambda,
                                                   rule: Rule): Seq[(Rule, Expr)] = {
    Expr.visitWithState(Seq[(Rule, Expr)]())( lambda.body, (e, s) => {
      if (rule.rewrite.isDefinedAt(e)) {
        s :+ (rule, e)
      } else s
    })
  }

  def rewrite(lambda: Lambda, rules: Seq[Rule], levels: Int): Seq[Lambda] = {
    TypeChecker.check(lambda.body)

    val allRulesAt = listAllPossibleRewritesForRules(lambda, rules)
    val rewritten = allRulesAt.map(ruleAt => applyRuleAt(lambda, ruleAt._2, ruleAt._1))

    if (levels == 1) {
      rewritten
    } else {
      rewritten.flatMap( l => rewriteJustGenerable(l, rules, levels-1))
    }
  }

  def rewriteJustGenerable(lambda: Lambda, rules: Seq[Rule], levels: Int): Seq[Lambda] =
    rewrite(lambda, rules, levels).filter(_.isGenerable)

  def rewriteJustGenerable(lambda: Lambda, levels: Int = 1): Seq[Lambda] =
    rewriteJustGenerable(lambda, allRules, levels)

}


