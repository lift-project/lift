package rewriting

import lift.arithmetic.ArithExpr
import rewriting.utils.{NumberExpression, Utils}
import ir._
import ir.ast._
import rewriting.rules.Rule
import rewriting.macrorules.MacroRules
//import rewriting.passes.RewritingPass.globalNumbering

// binds rule to expression where rule is applicable
class PotentialRewrite(val rule: Rule, val expr: Expr) {
  assert(rule.isDefinedAt(expr))
  override def toString = rule + "->\n" + expr
}

/**
 * A more relaxed version of PotentialRewrite. While PotentialRewrite binds a rule to an expression,
 * PotentialRewriteOnPatternInstance bind a rule to an instance of a pattern.
 * The assumption is that the bound pattern is a narrowed version of the pattern of the rule.
 * An example usage of this is following:
 * - We want to apply Rules.joinSplit on a pattern `map1(map2(map3(f)))` and we have to find all potential
 *   rewrites first (in order to use a constraint solver to decide which rules to apply) and apply the rewrites later.
 * - By traversing the expression, we collect two PotentialRewrite instances: one bound to `map1(map2(..))` and
 *   another bound to `map2(map3(..))`.
 * - However, once we merge map1 and map2, the expression becomes `split(s) o map12(map3(f)) o join`, so the
 *   PotentialRewrite which is bound to `map2(map3(f))` is no longer applicable.
 * - With PotentialRewriteOnPatternInstance, we can bind the rule to two narrower patterns:
 *   `mapA(mapB(..)) if mapB == map2` and `mapA(mapB(..)) if mapB == map3`.
 * - Now we can merge the first two maps, and the resulting `map12(map3(f))` will still be matched by the second
 *   potential rewrite pattern.
 */
case class PotentialRewriteOnNarrowedPattern(rule: Rule,
                                             private val narrowedPattern: PartialFunction[Expr, Unit]) {

  def isDefinedAt(expr: Expr): Boolean = narrowedPattern.isDefinedAt(expr) && rule.isDefinedAt(expr)
}

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
    UpdateContext(expr)

    val replacement = rule.rewrite(toBeReplaced)
    var replacedInExpr = Expr.replace(expr, toBeReplaced, replacement)

    if (rule == MacroRules.splitJoinId)
      replacedInExpr = patchUpAfterSplitJoin(toBeReplaced, replacement, replacedInExpr)

    replacedInExpr
  }

  def applyRuleUntilCannot(lambda: Lambda, rule: Rule): Lambda =
    applyRulesUntilCannot(lambda, Seq(rule))

  /**
    * Apply rules one by one until no rules apply anymore
    *
    * @param lambda The lambda where to apply rules
    * @param rules The rules to apply
    * @return
    */
  def applyRulesUntilCannot(lambda: Lambda, rules: Seq[Rule]): Lambda = {
    val newBody = applyRulesUntilCannot(lambda.body, rules)

    if (newBody eq lambda.body)
      lambda
    else
      Lambda(lambda.params, newBody)
  }

  /**
    * Apply rules one by one until no rules apply anymore
    *
    * @param expr The expression where to apply rules
    * @param rules The rules to apply
    * @return
    */
  @scala.annotation.tailrec
  def applyRulesUntilCannot(expr: Expr, rules: Seq[Rule]): Expr = {
    val allRulesAt = listAllPossibleRewritesForRules(expr, rules)

    if (allRulesAt.isEmpty) {
      expr
    } else {
      val ruleAt = allRulesAt.head
      applyRulesUntilCannot(Rewrite.applyRuleAt(expr, ruleAt.rule, ruleAt.expr), rules)
    }
  }

  /**
    * Apply rules one by one until no rules apply anymore; collect applied rules
    *
    * @param expr The expression where to apply rules
    * @param rules The rules to apply
    * @param appliedRules The list of applied rules
    * @return
    */
  @scala.annotation.tailrec
  def applyRulesUntilCannot(expr: Expr, rules: Seq[Rule], appliedRules: List[Rule]): (Expr, List[Rule]) = {
    val allRulesAt = listAllPossibleRewritesForRules(expr, rules)

    if (allRulesAt.isEmpty) {
      (expr, appliedRules)
    } else {
      val ruleAt = allRulesAt.head
      applyRulesUntilCannot(Rewrite.applyRuleAt(expr, ruleAt.rule, ruleAt.expr), rules,
        appliedRules :+ ruleAt.rule)
    }
  }

  def patchUpAfterSplitJoin(toBeReplaced: Expr, replacement: Expr, replaced: Expr): Expr = {
    try {
      TypeChecker(replaced)
      replaced
    } catch {
      case _: ZipTypeException =>

        val newExpr = Utils.getLengthOfSecondDim(replacement.t)
        val oldExpr = Utils.getLengthOfSecondDim(toBeReplaced.t)

        if (oldExpr != newExpr) {
          val st = collection.immutable.Map[ArithExpr, ArithExpr]((oldExpr, newExpr))
          val tunableNodes = Utils.findTunableNodes(replaced)
          Utils.quickAndDirtySubstitution(st, tunableNodes, replaced)
        } else {
          replaced
        }
    }
  }

  def listAllPossibleRewritesForRules(lambda: Lambda, rules: Seq[Rule]): Seq[PotentialRewrite] = {
    listAllPossibleRewritesForRules(lambda.body, rules)
  }

  def listAllPossibleRewritesForRules(expr: Expr, rules: Seq[Rule]): Seq[PotentialRewrite] = {
    UpdateContext(expr)
    TypeChecker(expr)
    rules.flatMap(rule => listAllPossibleRewrites(expr, rule))
  }

  def listAllPossibleRewrites(lambda: Lambda,rule: Rule): Seq[PotentialRewrite] =
    listAllPossibleRewrites(lambda.body, rule)


  def listAllPossibleRewrites(expr: Expr, rule: Rule): Seq[PotentialRewrite] = {
    Expr.visitWithState(Seq[PotentialRewrite]())( expr, (e, s) => {
//      if (globalNumbering.contains(e) && globalNumbering(e).equals(109))
//        println("here")
      if (rule.rewrite.isDefinedAt(e)) {
        s :+ new PotentialRewrite(rule, e)
      } else s
    })
  }

  def rewrite(lambda: Lambda, rules: Seq[Rule], levels: Int): Seq[Lambda] = {

    val allRulesAt = listAllPossibleRewritesForRules(lambda, rules)
    val rewritten = allRulesAt.map(ruleAt => applyRuleAt(lambda, ruleAt.expr, ruleAt.rule))

    if (levels == 1)
      rewritten
    else
      rewritten.flatMap( l => rewrite(l, rules, levels-1))
  }
}


