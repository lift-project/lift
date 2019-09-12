package rewriting

import com.typesafe.scalalogging.Logger
import ir.ast._
import ir.{TupleType, TypeChecker, UpdateContext}
import opencl.ir.pattern._
import rewriting.macrorules.MacroRules
import rewriting.rules._
import rewriting.utils._

case class EnabledMappings(
  global0: Boolean,
  global01: Boolean,
  global10: Boolean,
  global012: Boolean,
  global210: Boolean,
  group0: Boolean,
  group01: Boolean,
  group10: Boolean
) {
  override def toString: String =
    s"Global 0 - $global0, Global 0,1 - $global01, Global 1,0 - $global10, " +
      s"Global 0,1,2 - $global012, Global 2,1,0 - $global210, " +
      s"Group 0 - $group0, Group 0,1 - $group01, Group 1,0 - $group10"

  val isOneEnabled: Boolean = global0 | global01 | global10 | global012 | global210 |
    group0 | group01 | group10
}

object Lower {

  private val logger = Logger(this.getClass)

  private[rewriting] def patchLambda(lambda: Lambda) = {
    val partialReducesLowered = lowerPartialReduces(lambda)

    val simplified = SimplifyAndFuse(partialReducesLowered)

    val compositionWithReduceSequential = mapComposedWithReduceAsSequential(simplified)

    val reducesLowered = lowerReduces(compositionWithReduceSequential)

    val allocatedToGlobal = lastWriteToGlobal(reducesLowered)

    val removeOtherIds = dropIds(allocatedToGlobal)

    val tupleToStruct = tupleToStructInReduce(removeOtherIds)

    tupleToStruct
  }

  def pushReduceDeeper(lambda: Lambda): Lambda = {
    val partialReducesLowered = lowerPartialReduces(lambda)

    val simplified = SimplifyAndFuse(partialReducesLowered)

    val patched = mapComposedWithReduceAsSequential(simplified)
    val numbered = NumberExpression.byDepth(patched)

    val reduces = utils.Utils.collect(patched.body, { case FunCall(_: AbstractPartRed, _, _) => })
    val mapSeq = utils.Utils.collect(patched.body, { case FunCall(_: MapSeq, _) => })
    val maps = utils.Utils.collect(patched.body, { case FunCall(_: Map, _) => })

    val reduceLevels = reduces.map(numbered)
    val mapLevels = maps.map(numbered)

    // TODO: more general??
    val shouldPushReduceDeeper = reduceLevels.count(_ == 1) > 0 && mapLevels.count(_ == 1) > 0

    if (shouldPushReduceDeeper) {
      val mapsToReplace = mapSeq.filter(numbered(_) == 1)
      val reducesToReplace = reduces.filter(numbered(_) == 1)

      val mapReplaced =
        mapsToReplace.foldLeft(patched)((lambda, toReplace) =>
          Rewrite.applyRuleAt(lambda, toReplace, Rules.splitJoinMapSeq))

      val reduceReplaced =
        reducesToReplace.foldLeft(mapReplaced)((lambda, toReplace) =>
          Rewrite.applyRuleAt(lambda, toReplace, Rules.splitJoinReduce))

      SimplifyAndFuse(reduceReplaced)
    } else {
      lambda
    }
  }

  private def tupleToStructInReduce(lambda: Lambda): Lambda = {
    TypeChecker(lambda)

    // TODO: Assuming all tuple accumulators are structs (true if it's the result of rewriting)
    // TODO: and try to force writing to it. Result would be ignored otherwise
    val applyHere =
      Expr.visitLeftToRight(Seq[Expr]())(lambda.body, {

        case (FunCall(ReduceSeq(Lambda(_, b)), acc, _), exprSet)
          if acc.t.isInstanceOf[TupleType] =>

          val maybeApplies =
            Expr.visitLeftToRight(None: Option[Expr])(b, {
              case (expr, None) if CopyRules.tupleToStruct.isDefinedAt(expr) => Some(expr)
              case (_, maybeExpr) => maybeExpr
            })

          exprSet ++ maybeApplies

        case (_, exprSet) => exprSet

      })

    applyHere.foldLeft(lambda)((currentExpr, toApplyAt) => {
      Rewrite.applyRuleAt(currentExpr, toApplyAt, CopyRules.tupleToStruct)
    })
  }

  def sequential(lambda: Lambda): Lambda = {
    val temp = patchLambda(lambda)
    Rewrite.applyRuleUntilCannot(temp, OpenCLRules.mapSeq)
  }

  private[rewriting] def mapComposedWithReduceAsSequential(lambda: Lambda) =
    Rewrite.applyRulesUntilCannot(lambda, Seq(MacroRules.mapComposedWithReduceAsSequential))

  def mapCombinations(lambda: Lambda,
    enabledMappings: EnabledMappings = EnabledMappings(
      global0 = true, global01 = true, global10 = false,
      global012 = false, global210 = false,
      group0 = true, group01 = false, group10 = true
    )
  ): List[Lambda] = {

    val removeOtherIds = SimplifyAndFuse(patchLambda(lambda))

    findAllMapsLowering(removeOtherIds,enabledMappings)
  }

  def simpleMapStrategy(lambda: Lambda): Lambda = {
    val mappings = EnabledMappings(
      global0 = false, global01 = false, global10 = false,
      global012 = false, global210 = false, group0 = false,
      group01 = false, group10 = true
    )

    mapCombinations(lambda, mappings).head
  }

  private def dropIds(lambda: Lambda) =
    Rewrite.applyRulesUntilCannot(lambda, Seq(SimplificationRules.dropId, SimplificationRules.removeEmptyMap))

  def findAll(lambda: Lambda, at: (Expr) => Boolean): List[Expr] =
    Expr.visitWithState(List[Expr]())(lambda.body, findAllFunction(at))

  def findAllDepthFirst(lambda: Lambda, at: (Expr) => Boolean): List[Expr] =
    Expr.visitWithStateDepthFirst(List[Expr]())(lambda.body, findAllFunction(at))

  def findAllFunction(at: (Expr) => Boolean): (Expr, List[Expr]) => List[Expr] = (current, collected) => {
    if (at(current))
      current +: collected
    else
      collected
  }

  private def hasOneMapOnSecondLevel(lambda: Lambda): Boolean = {
    val body = lambda.body
    val levelTwoBody = Utils.getMapBody(Utils.getMapAtDepth(body, 0))

    val mapsOnLevelTwo = Utils.countMapsAtCurrentLevel(levelTwoBody)

    mapsOnLevelTwo == 1
  }

  private def hasOneMapOnThirdLevel(lambda: Lambda): Boolean = {
    val body = lambda.body
    val levelTwoBody = Utils.getMapBody(Utils.getMapAtDepth(body, 0))
    val levelThreeBody = Utils.getMapBody(Utils.getMapAtDepth(levelTwoBody, 0))

    val mapsOnLevelThree = Utils.countMapsAtCurrentLevel(levelThreeBody)

    mapsOnLevelThree == 1
  }

  def findAllMapsLowering(lambda:Lambda,enabledMappings:EnabledMappings): List[Lambda] = {
    val depthMap = NumberExpression.byDepth(lambda)
    val depthsOfUnLowered = depthMap.collect({ case (FunCall(Map(_), _*), depth) => depth })

    if (depthsOfUnLowered.isEmpty)
      return List(lambda)

    val maxDepth = depthsOfUnLowered.max + 1

    var lambdas = List[Lambda]()
    val oneMapOnLevelTwo =
      if (maxDepth > 1) hasOneMapOnSecondLevel(lambda) else false
    val oneMapOnLevelThree=
      if (oneMapOnLevelTwo && maxDepth > 2) hasOneMapOnThirdLevel(lambda) else false

    /* Global only */
    if (enabledMappings.global0) {
      val lambda1 = Lower.lowerNextLevelWithRule(lambda, OpenCLRules.mapGlb(0))
      var lambdaN = lambda1

      while (lambdaN.body.contains({ case e if OpenCLRules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, OpenCLRules.mapSeq)

      lambdas = lambdaN :: lambdas
    }

    if (enabledMappings.global10 && maxDepth > 1 && oneMapOnLevelTwo) {
      val lambda1 = Lower.lowerNextLevelWithRule(lambda, OpenCLRules.mapGlb(1))
      val lambda2 = Lower.lowerNextLevelWithRule(lambda1, OpenCLRules.mapGlb(0))
      var lambdaN = lambda2

      while (lambdaN.body.contains({ case e if OpenCLRules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, OpenCLRules.mapSeq)

      lambdas = lambdaN :: lambdas
    }


    if (enabledMappings.global01 && maxDepth > 1 && oneMapOnLevelTwo) {
      val lambda1 = Lower.lowerNextLevelWithRule(lambda, OpenCLRules.mapGlb(0))
      val lambda2 = Lower.lowerNextLevelWithRule(lambda1, OpenCLRules.mapGlb(1))
      var lambdaN = lambda2

      while (lambdaN.body.contains({ case e if OpenCLRules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, OpenCLRules.mapSeq)

      lambdas = lambdaN :: lambdas
    }

    if (enabledMappings.global012 && maxDepth > 2 && oneMapOnLevelThree) {
      val lambda1 = Lower.lowerNextLevelWithRule(lambda, OpenCLRules.mapGlb(0))
      val lambda2 = Lower.lowerNextLevelWithRule(lambda1, OpenCLRules.mapGlb(1))
      val lambda3 = Lower.lowerNextLevelWithRule(lambda2, OpenCLRules.mapGlb(2))
      var lambdaN = lambda3

      while (lambdaN.body.contains({ case e if OpenCLRules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, OpenCLRules.mapSeq)

      lambdas = lambdaN :: lambdas
    }

    if (enabledMappings.global210 && maxDepth > 2 && oneMapOnLevelThree) {
      val lambda1 = Lower.lowerNextLevelWithRule(lambda, OpenCLRules.mapGlb(2))
      val lambda2 = Lower.lowerNextLevelWithRule(lambda1, OpenCLRules.mapGlb(1))
      val lambda3 = Lower.lowerNextLevelWithRule(lambda2, OpenCLRules.mapGlb(0))
      var lambdaN = lambda3

      while (lambdaN.body.contains({ case e if OpenCLRules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, OpenCLRules.mapSeq)

      lambdas = lambdaN :: lambdas
    }

    /* Workgroup */
    if (enabledMappings.group0 && maxDepth > 1) {

      val lambda1 = Lower.lowerNextLevelWithRule(lambda, OpenCLRules.mapWrg(0))
      val lambda2 = Lower.lowerNextLevelWithRule(lambda1, OpenCLRules.mapLcl(0))
      var lambdaN = lambda2

      while (lambdaN.body.contains({ case e if OpenCLRules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, OpenCLRules.mapSeq)

      lambdas = lambdaN :: lambdas
    }

    if (enabledMappings.group01 && maxDepth > 3 && oneMapOnLevelTwo) {
      val lambda1 = Lower.lowerNextLevelWithRule(lambda, OpenCLRules.mapWrg(0))
      val lambda2 = Lower.lowerNextLevelWithRule(lambda1, OpenCLRules.mapWrg(1))
      val lambda3 = Lower.lowerNextLevelWithRule(lambda2, OpenCLRules.mapLcl(0))
      val lambda4 = Lower.lowerNextLevelWithRule(lambda3, OpenCLRules.mapLcl(1))

      var lambdaN = lambda4

      while (lambdaN.body.contains({ case e if OpenCLRules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, OpenCLRules.mapSeq)

      lambdas = lambdaN :: lambdas

    }

    if (enabledMappings.group10 && maxDepth > 3 && oneMapOnLevelTwo) {
      val lambda1 = Lower.lowerNextLevelWithRule(lambda, OpenCLRules.mapWrg(1))
      val lambda2 = Lower.lowerNextLevelWithRule(lambda1, OpenCLRules.mapWrg(0))
      val lambda3 = Lower.lowerNextLevelWithRule(lambda2, OpenCLRules.mapLcl(1))
      val lambda4 = Lower.lowerNextLevelWithRule(lambda3, OpenCLRules.mapLcl(0))

      var lambdaN = lambda4

      while (lambdaN.body.contains({ case e if OpenCLRules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, OpenCLRules.mapSeq)

      lambdas = lambdaN :: lambdas
    }

    lambdas
  }

  def simpleMapLoweringStrategy(lambda: Lambda): Lambda = {
    val mappings = EnabledMappings(
      global0 = false, global01 = false, global10 = false,
      global012 = false, global210 = false, group0 = false,
      group01 = false, group10 = true
    )

    findAllMapsLowering(lambda, mappings).head
  }

  def lastWriteToGlobal(lambda: Lambda): Lambda =
    if (isTheLastWriteNestedInReduce(lambda))
      lastReduceToGlobal(lambda)
    else
      lastMapToGlobal(lambda)

  private def lastMapToGlobal(lambda: Lambda): Lambda = {
    val lastWrite = getLastWrite(lambda).get

    val lastMap = Utils.findExpressionForPattern(lambda,
      { case FunCall(ir.ast.AbstractMap(Lambda(_, body)), _) if {
        body.contains({ case x if x eq lastWrite => }) &&
          !body.contains({ case FunCall(ir.ast.AbstractMap(_), _) => })
      } => }: PartialFunction[Expr, Unit] )

    lastMap match {
      case None => logger.warn("No last map found. Possibly using at-notation? Assume last write uses toGlobal"); lambda
      case _ => Rewrite.applyRuleAt(lambda, lastWrite, OpenCLRules.globalMemory)
    }
  }

  private def lastReduceToGlobal(lambda: Lambda): Lambda = {
    val pattern1: PartialFunction[Expr, Unit] = {
      case FunCall(Id(), _) =>
    }

    UpdateContext(lambda)

    val ids = findAllDepthFirst(lambda, pattern1.isDefinedAt)
    val pattern2: PartialFunction[Expr, Unit] = {
      case FunCall(MapSeq(f), _) if ids.contains(f.body) =>
    }
    val mapSeqs = findAllDepthFirst(lambda, pattern2.isDefinedAt)

    implementIdInMapSeq(lambda, ids.last, mapSeqs.last)
  }

  private def implementIdInMapSeq(lambda: Lambda, idToImplement: Expr, mapSeqForId: Expr): Lambda = {
    TypeChecker.check(lambda.body)

    val implementedId = CopyRules.implementIdAsDeepCopy.rewrite(idToImplement)
    val idReplaced = Expr.replace(mapSeqForId, idToImplement, implementedId)

    val idToGlobal = OpenCLRules.globalMemory.rewrite(idReplaced)
    FunDecl.replace(lambda, mapSeqForId, idToGlobal)
  }

  private def isTheLastWriteNestedInReduce(lambda: Lambda): Boolean =
    isNestedInReduce(getLastWrite(lambda).get, lambda.body)

  private def isNestedInReduce(expr: Expr, nestedIn: Expr): Boolean = {
    Utils.findExpressionForPattern(nestedIn, {
      case FunCall(ReduceSeq(f), _, _) if f.body.contains({ case e if e eq expr =>}) =>
    }: PartialFunction[Expr, Unit]).isDefined
  }

  private def getLastWrite(lambda: Lambda) =
    Utils.findExpressionForPattern(lambda, { case FunCall(_:UserFun | _:VectorizeUserFun, _*) => } : PartialFunction[Expr, Unit])

  def lowerPartialReduces(lambda: Lambda): Lambda =
    Rewrite.applyRuleUntilCannot(lambda, ReduceRules.partialReduceToReduce)

  def lowerReduces(lambda: Lambda): Lambda = {
    val stepOne = Rewrite.applyRuleUntilCannot(lambda, OpenCLRules.reduceSeq)

    val reduceSeqs = Rewrite.listAllPossibleRewrites(stepOne, CopyRules.addIdAfterReduce)

    val idsAfterAdded = reduceSeqs.foldLeft(stepOne)((lambda, pair) =>
      Rewrite.applyRuleAt(lambda, pair.expr, pair.rule))

    val values = Rewrite.listAllPossibleRewrites(idsAfterAdded, CopyRules.addIdValue)

    val valueIdsAdded = values.foldLeft(idsAfterAdded)((lambda, pair) =>
      Rewrite.applyRuleAt(lambda, pair.expr, pair.rule))

    valueIdsAdded
  }

  private def applyRuleToExpressions(lambda: Lambda, nextToLower: List[Int], rule: Rule): Lambda =
    nextToLower.foldLeft(lambda)((l, e) => Rewrite.applyRuleAtId(l, e, rule))

  def lowerNextLevelWithRule(lambda: Lambda, rule: Rule): Lambda = {
    val nextToLower = FindNextMapsToLower()(lambda)
    applyRuleToExpressions(lambda, nextToLower, rule)
  }
}

object FindNextMapsToLower {
  def apply() = new FindNextMapsToLower
}

class FindNextMapsToLower {

  private var expressions = List[Int]()
  private var idMap = collection.Map[Expr, Int]()

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
          case ir.ast.Map(f) if f.body.isConcrete => expressions = idMap(call) +: expressions
          case _ => find(call.f)
        }

        call.args.foreach(find)
      case _ =>
    }
  }

  @scala.annotation.tailrec
  private def find(funDecl: FunDecl): Unit = {
    funDecl match {
      case lambda: Lambda => find(lambda.body)
      case fp: FPattern => find(fp.f)
      case _ =>
    }
  }
}
