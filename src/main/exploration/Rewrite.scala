package exploration

import exploration.Rules._
import ir._
import ir.ast._

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

  val mapLoweringRules =
    Seq(
      mapSeq,
      mapGlb,
      mapWrg,
      mapLcl,
      mapWarp,
      mapLane
    )

  val reduceLoweringRule = reduceSeq

  val addressSpaceRules =
    Seq(
      privateMemory,
      localMemory,
      globalMemory
    )

  val simplificationRules =
    Seq(
      iterateId,
      iterate1,
      asScalarAsVectorId,
      asVectorAsScalarId,
      transposeTransposeId,
      joinSplitId,
      splitJoinId,
      removeEmptyMap
    )

  val reduceRules =
    Seq(
      partialReduceToReduce,
      partialReduceReorder,
      partialReduce,
      partialReduceSplitJoin,
      partialReduceReorder,
      partialReduceToReduce
    )

  val fusionRules =
    Seq(
      mapFusion,
      mapFusionWithZip,
      reduceMapFusion,
      reduceSeqMapSeqFusion
    )

  val fissionRules =
    Seq(
      mapFission,
      mapFissionWithZipInside,
      mapFissionWithZipOutside
    )

  val interchangeRules =
    Seq(
      mapReduceInterchange,
      mapReduceInterchangeWithZip,
      mapReducePartialReduce,
      mapMapTransposeZipInside,
      mapMapTransposeZipOutside,
      mapMapInterchange,
      transposeBothSides
    )

  val idRules =
    Seq(
      addId,
      addIdForCurrentValueInReduce,
      addCopy,
      implementOneLevelOfId,
      implementIdAsDeepCopy,
      dropId
    )

  val transposeRules =
    Seq(
      mapSplitTranspose,
      mapTransposeSplit,
      transposeMapSplit,
      splitTranspose,
      mapTransposeTransposeMapTranspose
    )

  val tupleRules =
    Seq(
      tupleMap,
      tupleFission
    )

  val rules =
    Seq(
      gatherToScatter,
      scatterToGather,
      splitJoin,
      vectorize,
      reorderBothSidesWithStride,
      splitZip
    ) ++ tupleRules ++ idRules ++ interchangeRules ++
      fissionRules ++ fusionRules++ reduceRules ++
      simplificationRules ++ addressSpaceRules ++ mapLoweringRules :+ reduceLoweringRule

  private def listAllPossibleRewritesForRules(lambda: Lambda, rules: Seq[Rule]): Seq[(Rule, Int)] = {
    rules.map(rule => listAllPossibleRewrites(lambda, rule)).reduce(_ ++ _)
  }

  private def listAllPossibleRewrites(lambda: Lambda,
                                      rule: Rule): Seq[(Rule, Int)] = {
    Context.updateContext(lambda.body, new Context)

    val numbering = NumberExpression.breadthFirst(lambda)

    Expr.visitWithState(Seq[(Rule, Int)]())( lambda.body, (e, s) => {
      if (rule.rewrite.isDefinedAt(e) && rule.isValid(e.context)) {
        s :+ (rule, numbering(e))
      } else s
    })
  }

  def rewrite(lambda: Lambda, rules: Seq[Rule], levels: Int): Seq[Lambda] = {
    TypeChecker.check(lambda.body)

    val allRulesAt = listAllPossibleRewritesForRules(lambda, rules)
    val rewritten = allRulesAt.map(ruleAt => applyRuleAtId(lambda, ruleAt._2, ruleAt._1))

    val (g, notG) = rewritten.partition( _.isGenerable )

    if (levels == 1) {
      g
    } else {
      g ++ notG.flatMap( l => rewrite(l, rules, levels-1))
    }
  }

  def rewrite(lambda: Lambda, levels: Int = 1): Seq[Lambda] =
    rewrite(lambda, rules, levels)

}
