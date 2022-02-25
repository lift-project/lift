package rewriting.passes

import ir.TypeChecker
import ir.ast.{AbstractMap, Expr, FunCall, Lambda}
import rewriting.passes.RewritingPass.{RewritePassParams, RuleApplicationHelper, globalNumbering}
import rewriting.rules.Rule
import rewriting.utils.NumberExpression
import rewriting.{PotentialRewriteOnNarrowedPattern, Rewrite, RewriteParamWithOrdinalValues, RewriteParamsOfVarArity}

import scala.collection.immutable.ListMap


object RewritingPass {
  /**
   * @param integerCode The value that the constraint solver will use to encode to refer to this rule
   * @param anchorExprExtractor Given an expression on which matches a rule's pattern, returns an expression with which
   *                            to associate a potential rewrite on narrowed pattern. For joinSplit rule with
   *                            pattern `mapA(mapB())`, such extractor could be `mapA(mapB()) => mapB`
   * @param narrowedPatternGenerator Given an expression, generates a pattern that is a specific case of the
   *                                 pattern of the rule. For joinSplit rule pattern `map(map())`, a narrowed pattern
   *                                 could be `map(mapX())`, where mapX has to be equal to a specific instance of map
   *                                 expression, while the outer map can be any map.
   *                                 For more details, see PotentialRewriteOnNarrowedPattern.
   */
  case class RuleApplicationHelper(integerCode: Int,
                                   anchorExprExtractor: PartialFunction[Expr, Expr],
                                   narrowedPatternGenerator: Expr => PartialFunction[Expr, Unit])

  case class RewritePassParams(exprParams: ListMap[Expr, RewriteParamWithOrdinalValues[Rule]])
    extends RewriteParamsOfVarArity(exprParams.values.toList) {
    // Reverse of exprParams for debugging purposes
    val paramExprs: ListMap[RewriteParamWithOrdinalValues[Rule], Expr] = ListMap(
      exprParams.map(p => p._2 -> p._1).toSeq: _*)

    assert(exprParams.size == paramExprs.size)

    val collectionName: String = "rewritePassParams"
  }

  object RuleApplicationHelper {
    val matchedExprItself: PartialFunction[Expr, Expr] = { case e => e }
    val exprEquals: Expr => PartialFunction[Expr, Unit] = expr => { case matchedExpr if matchedExpr == expr => }
  }

  var globalNumbering: collection.Map[Expr, Int] = null
}

/**
 * A rewriting pass represents the space of applicable rewrite rules as a parameter space so that
 * a constraint solver can be used to decide which rules to apply.
 *
 * The pass finds all subexpressions where one or more of the specified rewrite rules apply, and
 * for each such expression generates a RewriteParam[Rule], which represents a choice of rule to apply.
 * The value of RewriteParam[Rule] is optional, and if a solver chooses None as a value, no rule will be applied.
 * Each instance of rewriting pass is specific to one instance of Lambda since rewrite params are AST-dependent.
 */
class RewritingPass(val ruleApplicationHelpers: Predef.Map[Rule, RuleApplicationHelper],
                    val paramsLabel: String,
                    val lambda: Lambda,
                    val rewriteWhereverPossible: Boolean) {
  assert(ruleApplicationHelpers.values.toList.distinct.size == ruleApplicationHelpers.values.size)
  // If rewriting is optional, the integer code 0 must remain unused so that we can use it to represent "do not apply"
  assert( rewriteWhereverPossible || !ruleApplicationHelpers.exists(_._2.integerCode == 0) )

  TypeChecker(lambda)

  val lambdaNumberingBreadthFirst: collection.Map[Expr, Int] = NumberExpression.breadthFirst(lambda.body)
  val lambdaNumberingDepthFirst: collection.Map[Expr, Int] = NumberExpression.depthFirst(lambda.body)
//  globalNumbering = lambdaNumberingDepthFirst
  val lambdaStr = lambda.toString

  /**
   * Rewritable pattern instances and a set of rules that apply
   */
  val potentialRewrites: ListMap[Expr, Seq[PotentialRewriteOnNarrowedPattern]] = {

    val potentialRewrites = collection.mutable.ListMap[Expr, Seq[PotentialRewriteOnNarrowedPattern]]()

    Rewrite.listAllPossibleRewritesForRules(lambda.body, ruleApplicationHelpers.keys.toSeq).
      foreach(potentialRewrite => {

        // Extract the expression to which the potential rewrite will be bound
        // E.g. for joinSplit, that would be the inner map. For mapGlb rule, it would be the map itself
//        if (globalNumbering.contains(potentialRewrite.expr) && globalNumbering(potentialRewrite.expr).equals(109))
//          println("here")
        val anchoredExpr: Expr = ruleApplicationHelpers(potentialRewrite.rule).anchorExprExtractor(potentialRewrite.expr)

        val newRewrite = PotentialRewriteOnNarrowedPattern(
          rule = potentialRewrite.rule,
          // Generate a pattern which is specific to the subexpr we just found
          narrowedPattern = ruleApplicationHelpers(potentialRewrite.rule).narrowedPatternGenerator(anchoredExpr))

        val previouslyCollectedRewrites = potentialRewrites.getOrElse(anchoredExpr, Seq())

        potentialRewrites(anchoredExpr) = previouslyCollectedRewrites :+ newRewrite
      })

    ListMap(potentialRewrites.toSeq: _*)
  }

  /**
   * Rewrite params for all rewritable subexpressions
   */
  val rewriteParams: RewritePassParams = {
    val paramCollection = RewritePassParams(ListMap(

      potentialRewrites.map { case (rewritableExpr, potentialRewrites) =>

        val paramName = paramsLabel + "." +
          lambdaNumberingDepthFirst(rewritableExpr).toString + "." +
          (rewritableExpr match {
            case FunCall(AbstractMap(Lambda(params, _, _)), _*) => params.head.toString
            case _ =>
          })

        rewritableExpr ->
          RewriteParamWithOrdinalValues(paramName, ListMap({

            val applicableRuleIDs: Seq[(Int, Some[Rule])] = potentialRewrites.map(potentialRewrite =>
              ruleApplicationHelpers(potentialRewrite.rule).integerCode -> Some(potentialRewrite.rule))

            val noRuleID: Seq[(Int, None.type)] = if (rewriteWhereverPossible) Seq() else Seq(0 -> None)

            noRuleID ++ applicableRuleIDs
          }: _* ))
      }.toSeq.sortBy(r => lambdaNumberingDepthFirst(r._1)): _*))

    paramCollection.paramList.foreach(p => p.setFullName(p.name))

    paramCollection
  }


  private def filterExpressionsToRewrite(rewriteParamValues: RewritePassParams # RewriteParamValuesT
                                        ): Predef.Map[Expr, Rule] =
  // Ignore param values equalling None, i.e. the expressions referred to by the corresponding params
  // will not be rewritten
    Predef.Map(rewriteParams.exprParams.zip(rewriteParamValues.vs).flatMap {
      case ((fusableMapPair, mapRewriteParam), Some(mapRewriteParamValue)) =>
        Some(fusableMapPair -> mapRewriteParamValue)
      case (_, mapRewriteParamValue @ None) => None
    }.toSeq: _*)

  /**
   * Given a combination of rewrite param values chosen by a solver, applies chosen rules to
   * subexpressions and returns the rewritten lambda
   */
  def rewriteTopDown(rewritesToApply: RewritePassParams # RewriteParamValuesT): Lambda = {

    var remainingRewritesToApply = collection.mutable.Map[Expr, Rule](
      filterExpressionsToRewrite(rewritesToApply).toSeq: _*)

    val rewrittenLambdaBody = (0 until remainingRewritesToApply.size).foldLeft(lambda.body) {
      case (exprBeingRewritten, _) =>

        val nextRewriteToApply: PotentialRewriteOnNarrowedPattern =
          Expr.visitWithState[Option[PotentialRewriteOnNarrowedPattern]](None)(exprBeingRewritten, {
            case (_, rewrite @ Some(_)) => rewrite

            case (e, None) if remainingRewritesToApply.contains(e) =>
              val ruleToApply = remainingRewritesToApply(e)

              remainingRewritesToApply -= e

              Some(PotentialRewriteOnNarrowedPattern(
                rule = ruleToApply,
                narrowedPattern = ruleApplicationHelpers(ruleToApply).narrowedPatternGenerator(e)))

            case (_, None) => None
          }) match {
            case Some(rewrite) => rewrite
            case None => throw new IllegalStateException("Could not find a subexpression to rewrite")
          }

        val rewrittenExpr = Expr.replace(exprBeingRewritten, {
          case e if nextRewriteToApply.isDefinedAt(e) =>
            nextRewriteToApply.rule.rewrite(e)

          case e => e
        })

        TypeChecker(rewrittenExpr)

        rewrittenExpr
    }

    Lambda(lambda.params, rewrittenLambdaBody)
  }
}