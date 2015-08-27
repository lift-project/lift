package exploration

import ir.{Context, TypeChecker}
import ir.ast._
import opencl.ir.pattern.{MapLcl, MapSeq, ReduceSeq}

object Lower {
  def apply(lambda: Lambda) = {

    val partialReducesLowered = lowerPartialReduces(lambda)

    val simplified = simplifyAndFuse(partialReducesLowered)

    val reducesLowered = lowerReduces(simplified)

    val reduceToGlobal = lastWriteToGlobal(reducesLowered)

    val removeOtherIds = dropIds(reduceToGlobal)

    val copiesAdded = addAndImplementIds(removeOtherIds)

    println(copiesAdded)

    val mapsLowered = simpleMapLoweringStrategy(copiesAdded)

    val assignedAddressSpaces = simpleAddressSpaceStrategy(mapsLowered)

    assignedAddressSpaces
  }

  def dropIds(lambda: Lambda) =
    lowerPatternWithRule(lambda, {case FunCall(Id(), _) =>}, Rules.dropId)

  def addAndImplementIds(lambda: Lambda) = {
    val at = Rules.addIdForCurrentValueInReduce.isDefinedAt _

    val reduceSeqs = findAll(lambda, at)

    val idsAdded = reduceSeqs.foldRight(lambda)((expr, currentLambda) =>
      FunDecl.replace(currentLambda, expr, Rules.addIdForCurrentValueInReduce.rewrite(expr)))

    val idsImplemented = lowerPatternWithRule(idsAdded,
      {case FunCall(Id(), _) =>}, Rules.implementIdAsDeepCopy)

    TypeChecker.check(idsImplemented.body)

    lowerPatternWithRule(idsImplemented, { case e if Rules.tupleMap.isDefinedAt(e) => },
       Rules.tupleMap)
  }

  def findAll(lambda: Lambda, at: (Expr) => Boolean): List[Expr] = {
    Expr.visitWithState(List[Expr]())(lambda.body, (current, collected) => {
      if (at(current))
        current +: collected
      else
        collected
    })
  }

  def lowerMaps(lambda: Lambda): Lambda = {

    // 1 is seq
    val depthMap = NumberExpression.byDepth(lambda)
    val maxDepth = depthMap.values.max

    if (maxDepth == 3) {
      val l1 = lowerNextLevelWithRule(lambda, Rules.mapGlb(1))
      val l2 = lowerNextLevelWithRule(l1, Rules.mapGlb(0))
      return l2
    }

    if (maxDepth == 4) {
      // One dim has been split
    }

    if (maxDepth == 5) {
      // 2 dims have been split
    }

    lambda
    // Reduce level gets seq
    // depth 2: Glb(1), Glb(0)
    // Glb(0), Seq
    // 3, the one that was split gets Wrg and Lcl, other Glb other dim?
    // 4 Wrg, Lcl,

  }

  def simpleMapLoweringStrategy(lambda: Lambda) = {
    val lambda1 = Lower.lowerNextLevelWithRule(lambda, Rules.mapWrg(1))
    val lambda2 = Lower.lowerNextLevelWithRule(lambda1, Rules.mapWrg(0))
    val lambda3 = Lower.lowerNextLevelWithRule(lambda2, Rules.mapLcl(1))
    val lambda4 = Lower.lowerNextLevelWithRule(lambda3, Rules.mapLcl(0))

    var lambdaN = lambda4

    while (lambdaN.body.contains({ case e if Rules.mapSeq.isDefinedAt(e) => }))
      lambdaN = lowerNextLevelWithRule(lambdaN, Rules.mapSeq)

    lambdaN
  }

  def simpleAddressSpaceStrategy(lambda: Lambda) = {
    val tupleMaps = findAll(lambda, {
      case FunCall(Tuple(_), args@_*) if args.forall({
        case FunCall(Map(_), _) => true
        case _ => false
        }) => true
      case _ => false
    })

    val mapTuples = findAll(lambda, {
      case FunCall(m: AbstractMap, _) if (m.f.body match {
        case FunCall(Tuple(_), _*) => true
        case _ => false
      }) => true
      case _ => false
    })

    val localMemories = mapTuples.foldRight(lambda)((expr, currentLambda) => {
      expr match {
        case FunCall(MapLcl(_, _), _) =>
          Context.updateContext(currentLambda.body)
          FunDecl.replace(currentLambda, expr, Rules.localMemory.rewrite(expr))
        case _ => currentLambda
      }
    })

    tupleMaps.foldRight(localMemories)((expr, currentLambda) => {
      expr match {
        case FunCall(MapSeq(_), _) =>
          Context.updateContext(currentLambda.body)
          FunDecl.replace(currentLambda, expr, Rules.privateMemory.rewrite(expr))
        case _ => currentLambda
      }
    })
  }

  def lastWriteToGlobal(lambda: Lambda): Lambda = {
    if (isTheLastWriteNestedInReduce(lambda)) {
      val idToImplement = findExpressionForPattern(lambda,
        { case FunCall(Id(), _) => } : PartialFunction[Expr, Unit]).get

      val mapSeqForId = findExpressionForPattern(lambda, {
        case FunCall(MapSeq(f), _) if f.body eq idToImplement  =>
      } : PartialFunction[Expr, Unit]).get


      TypeChecker.check(lambda.body)

      val implementedId = Rules.implementIdAsDeepCopy.rewrite(idToImplement)
      val idReplaced = Expr.replace(mapSeqForId, idToImplement, implementedId)

      val idToGlobal = Rules.globalMemory.rewrite(idReplaced)
      FunDecl.replace(lambda, mapSeqForId, idToGlobal)
    } else {

      lambda
    }
  }

  def isTheLastWriteNestedInReduce(lambda: Lambda): Boolean =
    isNestedInReduce(getLastWrite(lambda).get, lambda.body)

  def isNestedInReduce(expr: Expr, nestedIn: Expr): Boolean = {
    findExpressionForPattern(nestedIn, {
      case FunCall(ReduceSeq(f), _, _) if f.body.contains({ case e if e eq expr =>}) =>
    }: PartialFunction[Expr, Unit]).isDefined
  }

  def getLastWrite(lambda: Lambda) =
    findExpressionForPattern(lambda, { case FunCall(_:UserFun, _*) => } : PartialFunction[Expr, Unit])

  def lowerPartialReduces(lambda: Lambda) =
    lowerPatternWithRule(lambda, { case FunCall(PartRed(_), _, _) => }, Rules.partialReduceToReduce)

  def lowerReduces(lambda: Lambda) =
    lowerPatternWithRule(lambda, { case FunCall(Reduce(_), _, _) => }, Rules.reduceSeq)

  def lowerPatternWithRule(lambda: Lambda, pattern: PartialFunction[Expr, Unit], rule: Rule): Lambda = {
    TypeChecker.check(lambda.body)

    if (lambda.body.contains(pattern)) {

      val next = findExpressionForPattern(lambda, pattern).get
      val replaced = FunDecl.replace(lambda, next, rule.rewrite(next))

      lowerPatternWithRule(replaced, pattern, rule)
    } else {
      lambda
    }
  }

  def findExpressionForPattern(lambda: Lambda, pattern: PartialFunction[Expr, Unit]): Option[Expr] =
    findExpressionForPattern(lambda.body, pattern)

  def findExpressionForPattern(expr: Expr, pattern: PartialFunction[Expr, Unit]): Option[Expr] = {
    Expr.visitWithStateDepthFirst(None: Option[Expr])(expr, (e, a) =>
      a match {
        case None if pattern.isDefinedAt(e) => Some(e)
        case _ => a
      })
  }

  def lowerByLevels(lambda: Lambda): List[Lambda] = {

    Context.updateContext(lambda.body)

    val nextToLower = FindNextMapsToLower()(lambda)

    if (nextToLower.nonEmpty) {

      val idMap = NumberExpression.breadthFirst(lambda)

      val applicableRules = mapLoweringRules.toList.filter(rule => {
        nextToLower.map(id => {
          val expr = Rewrite.getExprForId(lambda.body, id, idMap)
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
    nextToLower.foldLeft(lambda)((l, e) => Rewrite.applyRuleAtId(l, e, rule))

  def lowerNextLevelWithRule(lambda: Lambda, rule: Rule) = {
    val nextToLower = FindNextMapsToLower()(lambda)
    applyRuleToExpressions(lambda, nextToLower, rule)
  }

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

object FindNextMapsToLower {
  def apply() = new FindNextMapsToLower
}

class FindNextMapsToLower {

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
