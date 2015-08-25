package exploration

import exploration.Rules._
import ir._
import ir.ast._

object Rewrite {

  def getExprForId(expr: Expr, id: Int, idMap: collection.Map[Expr, Int]): Expr =
    idMap.find(pair => pair._2 == id).get._1

  def applyRuleAtId(lambda: Lambda, id: Int, rule: Rule): Lambda = {
    val replacement = applyRuleAtId(lambda.body, id, rule)
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
    TypeChecker.check(expr)
    Context.updateContext(expr)
    val toBeReplaced = getExprForId(expr, id, numbering)
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

  val reduceLoweringRule = Seq(reduceSeq)

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
      gatherScatterId,
      scatterGatherId,
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

  val otherRules = Seq(
    gatherToScatter,
    scatterToGather,
    splitJoin,
    vectorize,
    reorderBothSidesWithStride,
    splitZip
  )
  val rules =
    otherRules ++ tupleRules ++ idRules ++ interchangeRules ++
      fissionRules ++ fusionRules++ reduceRules ++
      simplificationRules ++ addressSpaceRules ++ mapLoweringRules ++ reduceLoweringRule

  private def listAllPossibleRewritesForRules(lambda: Lambda, rules: Seq[Rule]): Seq[(Rule, Int)] = {
    rules.map(rule => listAllPossibleRewrites(lambda, rule)).reduce(_ ++ _)
  }

  private def listAllPossibleRewrites(lambda: Lambda,
                                      rule: Rule): Seq[(Rule, Int)] = {
    Context.updateContext(lambda.body)

    val numbering = NumberExpression.breadthFirst(lambda)

    Expr.visitWithState(Seq[(Rule, Int)]())( lambda.body, (e, s) => {
      if (rule.rewrite.isDefinedAt(e)) {
        s :+ (rule, numbering(e))
      } else s
    })
  }

  def lowerByLevels(lambda: Lambda): List[Lambda] = {

    Context.updateContext(lambda.body)

    val nextToLower = FindNextToLower()(lambda)

    if (nextToLower.nonEmpty) {

      val idMap = NumberExpression.breadthFirst(lambda)

      val applicableRules = mapLoweringRules.toList.filter(rule => {
        nextToLower.map(id => {
          val expr = getExprForId(lambda.body, id, idMap)
          rule.rewrite.isDefinedAt(expr)
        }).reduce(_&&_)
      })

      val newLambdas = applicableRules.map(applyRuleToExpressions(lambda, nextToLower, _))

      newLambdas.map(lowerByLevels).reduce(_++_)
    } else {
      List(lambda)
    }
  }

  def applyRuleToExpressions(lambda: Lambda, nextToLower: List[Int], rule: Rule): Lambda =
    nextToLower.foldLeft(lambda)((l, e) => applyRuleAtId(l, e, rule))

  def lowerNextLevelWithRule(lambda: Lambda, rule: Rule) = {
    val nextToLower = FindNextToLower()(lambda)
    applyRuleToExpressions(lambda, nextToLower, rule)
  }

  def simplifyAndFuse(lambda: Lambda): Lambda = {
    val rules = simplificationRules ++ fusionRules

    TypeChecker.check(lambda.body)

    val allRulesAt = listAllPossibleRewritesForRules(lambda, rules)

    if (allRulesAt.isEmpty)
      lambda
    else {
      val ruleAt = allRulesAt.head

      simplifyAndFuse(applyRuleAtId(lambda, ruleAt._2, ruleAt._1))
    }
  }

  def rewrite(lambda: Lambda, rules: Seq[Rule], levels: Int): Seq[Lambda] = {
    TypeChecker.check(lambda.body)

    val allRulesAt = listAllPossibleRewritesForRules(lambda, rules)
    val rewritten = allRulesAt.map(ruleAt => applyRuleAtId(lambda, ruleAt._2, ruleAt._1))

    // TODO: Not all generable kernels are valid...
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

object FindNextToLower {
  def apply() = new FindNextToLower
}

class FindNextToLower {

  var expressions = List[Int]()
  var idMap = collection.Map[Expr, Int]()

  def apply(lambda: Lambda): List[Int] =
    apply(lambda.body)

  def apply(expr: Expr): List[Int] = {
    idMap = NumberExpression.breadthFirst(expr)
    find(expr)
    expressions
  }

  private def find(expr: Expr): Unit = {
    expr match {
      case call: FunCall =>
        
        call.f match {
          case Map(f) if f.body.isConcrete => expressions = idMap(call) +: expressions
          case _ => find(call.f)
        }

        call.args.foreach(find)
      case _ =>
    }
  }

  private def find(funDecl: FunDecl): Unit = {
    funDecl match {
      case lambda: Lambda => find(lambda.body)
      case fp: FPattern => find(fp.f)
      case _ =>
    }
  }
}
