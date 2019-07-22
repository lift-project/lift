package rewriting

import rewriting.utils.{NumberExpression, Utils}
import ir.ast._
import rewriting.macrorules.{EnablingRules, MacroRules}
import rewriting.rules.{Rule, Rules}

object SimplifyAndFuse {
  def apply(lambda: Lambda, maxTime: Long = 900000 /* 15 min */): Lambda =
    (new SimplifyAndFuse(maxTime))(lambda)

  def withoutPreventingFurtherOptimisation(lambda: Lambda, maxTime: Long = 900000): Lambda =
    (new SimplifyAndFuse(maxTime, false))(lambda)

  def sortLambdaByTerms(lambda0: Lambda, lambda1: Lambda): Boolean = {
    val terms0 = getNumberOfTerms(lambda0)
    val terms1 = getNumberOfTerms(lambda1)

    if (terms0 < terms1)
      return true

    if (terms0 > terms1)
      return false

    val mapReduces0 = countConcreteMapAndReduce(lambda0)
    val mapReduces1 = countConcreteMapAndReduce(lambda1)

    mapReduces0 < mapReduces1
  }

  def getNumberOfTerms(lambda: Lambda): Int =
    NumberExpression.breadthFirst(lambda).values.max

  def countConcreteMapAndReduce(lambda: Lambda): Int =
    Expr.visitWithState(0)(lambda.body, {
      case (FunCall(map: AbstractMap, _), a) if map.f.body.isConcrete => a+1
      case (FunCall(map: AbstractPartRed, _, _), a) if map.f.body.isConcrete => a+1
      case (_, a) => a
    })
}

class SimplifyAndFuse(val maxTime: Long, val fuseReduceMapImmediately: Boolean = true) {

  import SimplifyAndFuse._

  private var cantUndo = List[Expr]()
  private var seen = List[Expr]()
  private val startTime = System.currentTimeMillis()

  private var fusionRules = rewriting.fusionRules

  if (fuseReduceMapImmediately)
    fusionRules = fusionRules :+ MacroRules.reduceMapFusion

  private def fuseAll(lambda: Lambda): Lambda =
    Rewrite.applyRulesUntilCannot(lambda, fusionRules)

  /**
   * Try to simplify a lambda by eliminating sequences of operations that have
   * no effect and fuse all maps and reduces that can be fused.
   *
   * @param lambda The lambda to simplify
   * @return A lambda where possible simplifications and fusions have been performed
   */
  def apply(lambda: Lambda): Lambda =
    simplify(lambda)

  private def simplify(lambda: Lambda, maxDepth: Int = 100): Lambda = {
    val currentTime = System.currentTimeMillis()

    if (maxDepth == 0 || (currentTime - startTime) > maxTime)
      return lambda

    val fused = fuseAll(lambda)

    val allRulesAt = Rewrite.listAllPossibleRewritesForRules(fused, simplificationRules)

    if (allRulesAt.isEmpty) {

      val enabledMost = tryToEnableMoreSimplifications(fused, 5)

      if (enabledMost._2 != 0) {
        simplify(enabledMost._1, maxDepth-1)
      } else {

        val stored = seen

        var applyHere = fused
        var rewrites = listAndFilterSplitTransposeRewrites(fused)

        // Deal with the case where movingSplit enables the simplification
        val possiblyMoreRewrites = Rewrite
          .rewrite(fused, Seq(EnablingRules.movingSplit), 1)
          .map(l => (l, listAndFilterSplitTransposeRewrites(l)))

        if (possiblyMoreRewrites.nonEmpty) {
          val mostRewrites = possiblyMoreRewrites.maxBy(_._2.length)
          if (mostRewrites._2.length > rewrites.length) {
            rewrites = mostRewrites._2
            applyHere = mostRewrites._1
          }
        }

        if (rewrites.nonEmpty) {

          val bestTry = rewrites.map(ruleAt => {
            val replacement = ruleAt.rule.rewrite(ruleAt.expr)
            val applyRuleAtId = FunDecl.replace(applyHere, ruleAt.expr, replacement)

            //noinspection SideEffectsInMonadicTransformation
            seen = ruleAt.expr :: seen

            val expr = Utils.getExprForPatternInCallChain(replacement,
              { case e if EnablingRules.transposeMapSplit.isDefinedAt(e) => }).get
            //noinspection SideEffectsInMonadicTransformation
            cantUndo = expr :: cantUndo

            simplify(applyRuleAtId, maxDepth - 1)
          }).sortWith(sortLambdaByTerms).head

          if (sortLambdaByTerms(bestTry, fused)) {
            seen = stored
            return bestTry
          }

        }

        seen = stored
        fused
      }

    } else {
      val ruleAt = allRulesAt.head

      val applied = Rewrite.applyRuleAt(fused, ruleAt.expr, ruleAt.rule)

      simplify(applied, maxDepth-1)
    }
  }

  private def listAndFilterSplitTransposeRewrites(l: Lambda): Seq[PotentialRewrite] = {
    Rewrite
      .listAllPossibleRewrites(l, Rules.splitTranspose)
      .filter(ruleAt => !ruleAt.expr.isInstanceOf[Param] && !seen.contains(ruleAt.expr))
  }



  private def tryToEnableMoreSimplifications(lambda: Lambda,
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

  private def getTheBestRule(lambda: Lambda,
                     candidates: Seq[(Lambda, Int, List[Rule])]): (Lambda, Int, List[Rule]) =
    candidates.fold((lambda, 0, List()))((a, b) => if (a._2 > b._2) a else b)

  private val enablingRules =
    Seq(
      EnablingRules.mapTransposeTransposeMapTranspose,
      EnablingRules.transposeMapMap,
      EnablingRules.mapSplitTranspose,
      EnablingRules.transposeMapSplit,
      EnablingRules.movingJoin,
      EnablingRules.movingSplit,
      Rules.joinFromZip,
      Rules.reorderTranspose
    )

  /**
   * Applies enabling rules where possible.
   *
   * @param lambda The lambda where to apply the rules
   * @return A sequence of lambdas paired with the number of simplification
   *         rules that can be applied in it
   */
  private def applyOneEnablingRule(lambda: Lambda, rules: List[Rule]): Seq[(Lambda, Int, List[Rule])] = {

    var allRulesAt = Rewrite.listAllPossibleRewritesForRules(lambda, enablingRules)

    allRulesAt = allRulesAt.filter(p =>
      if (p.rule == EnablingRules.transposeMapSplit) {
        val applyHere = p.expr
        !cantUndo.exists(_ eq applyHere)
      } else {
        true
      })


    val rewritten = allRulesAt.map(ruleAt =>
      if (fuseReduceMapImmediately)
        fuseAll(Rewrite.applyRuleAt(lambda, ruleAt.expr, ruleAt.rule))
      else
        Rewrite.applyRuleAt(lambda, ruleAt.expr, ruleAt.rule))


    var rulesToEnable = simplificationRules

    if (!fuseReduceMapImmediately)
      rulesToEnable = rulesToEnable ++ fusionRules

    val numPossibleSimplifications =
      rewritten.map(Rewrite.listAllPossibleRewritesForRules(_, rulesToEnable).length)

    (rewritten,  numPossibleSimplifications, allRulesAt.map(_.rule +: rules)).zipped.toSeq
  }
}
