package exploration

import ir._
import ir.ast._
import Rules._

object Rewrite {

  def getExprForId(expr: Expr, id: Int, idMap: collection.Map[Expr, Int]): Option[Expr] = {
    var foundExpr: Option[Expr] = None

    Expr.visit(expr, e => {
      if (idMap.isDefinedAt(e))
        if (idMap(e) == id)
          foundExpr = Some(e)
    }, _ => Unit)

    foundExpr
  }

  def applyRuleAtId(lambda: Lambda, id: Int, rule: Rule): Lambda = {
    val replacement = applyRuleAtId(lambda.body, id, rule)
    Lambda(lambda.params, replacement)
  }

  def applyRuleAtId(expr: Expr, id: Int, rule: Rule): Expr = {
    TypeChecker.check(expr)
    val toBeReplaced = getExprForId(expr, id, NumberExpression.breadthFirst(expr)).get
    Expr.replace(expr, toBeReplaced, rule.rewrite(toBeReplaced))
  }

  def depthFirstApplyRuleAtId(expr:Expr, id: Int, rule: Rule): Expr = {
    TypeChecker.check(expr)
    val toBeReplaced = getExprForId(expr, id, NumberExpression.depthFirst(expr)).get
    Expr.replace(expr, toBeReplaced, rule.rewrite(toBeReplaced))
  }

  private val rules =
    Seq(
      iterateId,
      iterate1,
      gatherToScatter,
      scatterToGather,
      partialReduceToReduce,
      partialReduceReorder,
      asScalarAsVectorId,
      asVectorAsScalarId,
      transposeTransposeId,
      joinSplitId,
      splitJoinId,
      mapSeq,
      mapGlb,
      mapWrg,
      mapLcl,
      mapWarp,
      mapLane,
      splitJoin,
      mapReduceInterchange,
      mapMapTransposeZipInside,
      mapFission,
      reduceSeq,
      partialReduce,
      partialReduceSplitJoin,
      partialReduceReorder,
      partialReduceToReduce,
      vectorize,
      reduceSeqMapSeqFusion,
      mapFusion,
      mapMapInterchange,
      reorderBothSidesWithStride,
      transposeBothSides,
      mapMapTransposeZipOutside,
      splitZip
    )

  private def listAllPossibleRewritesForAllRules(lambda: Lambda): Seq[(Rule, Expr)] = {
    rules.map(rule => listAllPossibleRewrites(lambda, rule)).reduce(_ ++ _)
  }

  private def listAllPossibleRewrites(lambda: Lambda,
                                      rule: Rule): Seq[(Rule, Expr)] = {
    Context.updateContext(lambda.body, new Context)

    Expr.visitWithState(Seq[(Rule, Expr)]())( lambda.body, (e, s) => {
      if (rule.rewrite.isDefinedAt(e) && rule.isValid(e.context)) {
        s :+ (rule, e)
      } else s
    })
  }

  private def applyRuleAt(lambda: Lambda, ruleAt: (Rule, Expr)): Lambda = {
    val rule = ruleAt._1
    val oldE = ruleAt._2
    // same as FunDecl.replace( ... )
    Lambda(lambda.params, Expr.replace(lambda.body, oldE, rule.rewrite(oldE)))
  }

  def rewrite(lambda: Lambda, levels: Int = 1): Seq[Lambda] = {
    TypeChecker.check(lambda.body)

    val allRulesAt = listAllPossibleRewritesForAllRules(lambda)
    val rewritten = allRulesAt.map(ruleAt => applyRuleAt(lambda, ruleAt))

    val (g, notG) = rewritten.partition( _.isGenerable )

    if (levels == 1) {
      g
    } else {
      g ++ notG.flatMap( l => rewrite(l, levels-1))
    }
  }
}
