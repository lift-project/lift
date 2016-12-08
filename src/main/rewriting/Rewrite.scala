package rewriting

import lift.arithmetic.ArithExpr
import rewriting.utils.{NumberExpression, Utils}
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

    val replacement = rule.rewrite(toBeReplaced)
    var replacedInExpr = Expr.replace(expr, toBeReplaced, replacement)

    if (rule == MacroRules.splitJoinId)
      replacedInExpr = patchUpAfterSplitJoin(toBeReplaced, replacement, replacedInExpr)

    replacedInExpr
  }

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
      applyRulesUntilCannot(Rewrite.applyRuleAt(expr, ruleAt._1, ruleAt._2), rules)
    }
  }

  def patchUpAfterSplitJoin(toBeReplaced: Expr, replacement: Expr, replaced: Expr): Expr = {
    try {
      TypeChecker(replaced)
      replaced
    } catch {
      case t: ZipTypeException =>

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

  def listAllPossibleRewritesForRules(lambda: Lambda, rules: Seq[Rule]): Seq[(Rule, Expr)] = {
    listAllPossibleRewritesForRules(lambda.body, rules)
  }

  def listAllPossibleRewritesForRules(expr: Expr, rules: Seq[Rule]): Seq[(Rule, Expr)] = {
    Context.updateContext(expr)
    TypeChecker.check(expr)
    rules.flatMap(rule => listAllPossibleRewrites(expr, rule))
  }

  def listAllPossibleRewrites(lambda: Lambda,rule: Rule): Seq[(Rule, Expr)] = {
    listAllPossibleRewrites(lambda.body, rule)
  }

  def listAllPossibleRewrites(expr: Expr, rule: Rule): Seq[(Rule, Expr)] = {
    Expr.visitWithState(Seq[(Rule, Expr)]())( expr, (e, s) => {
      if (rule.rewrite.isDefinedAt(e)) {
        s :+ (rule, e)
      } else s
    })
  }

  def rewriteWithoutLowering(lambda:Lambda,rules:Seq[Rule],levels:Int):Seq[Lambda] ={
    //val lStr = rewriting.utils.Utils.dumpLambdaToString(lambda)
    //println(lStr)

    TypeChecker.check(lambda.body)
    val allRulesAt = listAllPossibleRewritesForRules(lambda,rules)
    val rewritten = allRulesAt.map(ruleAt => applyRuleAt(lambda,ruleAt._2,ruleAt._1))

    if(levels == 1){
      rewritten
    }
    else{
      rewritten.flatMap(l => rewriteWithoutLowering(l,rules,levels - 1 ))
    }
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


