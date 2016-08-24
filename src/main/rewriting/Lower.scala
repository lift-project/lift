package rewriting


import rewriting.utils._
import ir.ast._
import ir.{Context, TypeChecker}
import opencl.ir.pattern._
import rewriting.MapTree._

import scala.collection.mutable.Seq


case class EnabledMappings(
  global0: Boolean,
  global01: Boolean,
  global10: Boolean,
  group0: Boolean,
  group01: Boolean,
  group10: Boolean
) {
  override def toString =
    s"Global 0 - $global0, Global 0,1 - $global01, Global 1,0 - $global10, " +
    s"Group 0 - $group0, Group 0,1 - $group01, Group 1,0 - $group10"
}

object Lower {
  private def patchLambda(lambda: Lambda) = {
    val partialReducesLowered = lowerPartialReduces(lambda)

    val simplified = SimplifyAndFuse(partialReducesLowered)

    val reducesLowered = lowerReduces(simplified)

    val allocatedToGlobal = lastWriteToGlobal(reducesLowered)

    val removeOtherIds = dropIds(allocatedToGlobal)

    removeOtherIds
  }

  def lowerFinal(lambda: Lambda) = {
    val copiesAdded = addAndImplementIds(patchLambda(lambda))

    val mapsLowered = simpleMapLoweringStrategy(copiesAdded)

    val assignedAddressSpaces = simpleAddressSpaceStrategy(mapsLowered)

    assignedAddressSpaces
  }

  def mapCombinations(lambda: Lambda,
    enabledMappings: EnabledMappings =
    //                 glb0, glb01, glb10, grp0, grp01, grp10
    new EnabledMappings(true, true, false, true, false, true)
  ) = {

    val removeOtherIds = SimplifyAndFuse(patchLambda(lambda))

    lowerMapsByLevels(removeOtherIds, enabledMappings)
  }

  def simpleMapStrategy(lambda: Lambda) = {
    val removeOtherIds = SimplifyAndFuse(patchLambda(lambda))

    val mapsLowered = simpleMapLoweringStrategy(removeOtherIds)

    mapsLowered
  }

  def dropIds(lambda: Lambda) = {
   lowerPatternWithRule(lambda, { case FunCall(Id(), _) => }, Rules.dropId)
  }

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
    Expr.visitWithState(List[Expr]())(lambda.body, findAllFunction(at))
  }

  def findAllDepthFirst(lambda: Lambda, at: (Expr) => Boolean): List[Expr] =
    Expr.visitWithStateDepthFirst(List[Expr]())(lambda.body, findAllFunction(at))

  def findAllFunction(at: (Expr) => Boolean): (Expr, List[Expr]) => List[Expr] = (current, collected) => {
    if (at(current))
      current +: collected
    else
      collected
  }

  private def testHasOneMapOnSecondLevel(depthMap: collection.Map[Expr,Int],maxDepth:Int): Boolean ={
    val UnLoweredMap = depthMap.filterKeys({
      case  FunCall(m: ir.ast.Map, _) if m.f.body.isConcrete => true
      case _=> false
    })
    for(i <- 0 until maxDepth){
      val mapOnCurrDepth = UnLoweredMap.filter({
        case (FunCall(m:ir.ast.Map,_),`i`) => true
        case _ => false
      })
      if(mapOnCurrDepth.nonEmpty){
        val levelTwoBody = MacroRules.getMapBody(mapOnCurrDepth.head._1)
        Utils.countMapsAtCurrentLevel(levelTwoBody) == 1
      }
    }
    false
  }

//  private def hasOneMapOnThirdLevel(lambda: Lambda): Boolean = {
//    val body = lambda.body
//    val levelTwoBody = MacroRules.getMapBody(MacroRules.getMapAtDepth(body, 0))
//    val levelThreeBody = MacroRules.getMapBody(MacroRules.getMapAtDepth(levelTwoBody, 0))
//
//    val mapsOnLevelThree = Utils.countMapsAtCurrentLevel(levelThreeBody)
//
//    mapsOnLevelThree == 1
//  }


  def lowerMapsByLevels(lambda:Lambda,enabledMappings: EnabledMappings):List[Lambda] = {

  val restrictGlobalNum = false

  val depthMap: collection.Map[Expr, Int] = NumberExpression.byDepth(lambda)
  val depthsOfUnLowered = depthMap.collect({ case (FunCall(ir.ast.Map(_), _*), depth) => depth })
  /*val UnLoweredMap = depthMap.filterKeys({
    case FunCall(Map(_),_*) => true
    case _=> false
  })*/
  val mapTree = (new MapTree)
  mapTree.parse(lambda)

  //Nothing need to be lowered
  if(mapTree.MaxDepth == 0){
    return List(lambda)
  }



  var lambdas = List[Lambda]()


  /* Global only */
  if (enabledMappings.global0 && mapTree.MaxAvaDepth >= 1) {
    val mapsOnLevel1 = mapTree.getMapsOnLevel(1)
    if (mapsOnLevel1.length > 1 && restrictGlobalNum) {
      //only allows one mapGlb!
    }
    else {
      val lambda1 = Lower.applyRuleToExpressions(lambda, mapsOnLevel1.toList, Rules.mapGlb(0))
      //TODO:Here we should ensure the input of MapGlb is in GlobalMemory!
      var lambdaN = lambda1 //mapGlb0ArgsToGlobal(lambda1)
      if (mapTree.MaxDepth > 1) {
        for (i <- 2 to mapTree.MaxDepth) {
          val mapsOnCurrLevel = mapTree.getMapsOnLevel(i)
          lambdaN = Lower.applyRuleToExpressions(lambdaN, mapsOnCurrLevel.toList, Rules.mapSeq)
        }
      }
      lambdas = lambdaN :: lambdas
    }
  }

  if (enabledMappings.global10 && mapTree.MaxAvaDepth >= 2) {
    val mapsOnLevel1 = mapTree.getMapsOnLevel(1)
    val mapsOnLevel2 = mapTree.getMapsOnLevel(2)
    if (restrictGlobalNum && (mapsOnLevel1.length > 1 || mapsOnLevel2.length > 1)) {
      //only allows one mapGlb!
    }
    else {
      val lambda1 = Lower.applyRuleToExpressions(lambda, mapsOnLevel1.toList, Rules.mapGlb(1))
      val lambda2 = Lower.applyRuleToExpressions(lambda1, mapsOnLevel2.toList, Rules.mapGlb(0))
      var lambdaN = lambda2
      if (mapTree.MaxDepth > 2) {
        for (i <- 3 to mapTree.MaxDepth) {
          val mapsOnCurrLevel = mapTree.getMapsOnLevel(i)
          lambdaN = Lower.applyRuleToExpressions(lambdaN, mapsOnCurrLevel.toList, Rules.mapSeq)
        }
      }
      lambdas = lambdaN :: lambdas
    }
  }

  if (enabledMappings.global01 && mapTree.MaxAvaDepth >= 2) {
    val mapsOnLevel1 = mapTree.getMapsOnLevel(1)
    val mapsOnLevel2 = mapTree.getMapsOnLevel(2)
    if (mapsOnLevel1.length > 1 || mapsOnLevel2.length > 1) {
      //only allows one mapGlb!
    }
    else {
      val lambda1 = Lower.applyRuleToExpressions(lambda, mapsOnLevel1.toList, Rules.mapGlb(0))
      val lambda2 = Lower.applyRuleToExpressions(lambda1, mapsOnLevel2.toList, Rules.mapGlb(1))
      var lambdaN = lambda2
      if (mapTree.MaxDepth > 2) {
        for (i <- 3 to mapTree.MaxDepth) {
          val mapsOnCurrLevel = mapTree.getMapsOnLevel(i)
          lambdaN = Lower.applyRuleToExpressions(lambdaN, mapsOnCurrLevel.toList, Rules.mapSeq)
        }
      }
      lambdas = lambdaN :: lambdas
    }
  }

  /** Workgroup */
  if (enabledMappings.group0 && mapTree.MaxAvaDepth >= 2) {
    val mapsOnLevel1 = mapTree.getMapsOnLevel(1)
    val mapsOnLevel2 = mapTree.getMapsOnLevel(2)

    val lambda1 = Lower.applyRuleToExpressions(lambda, mapsOnLevel1.toList, Rules.mapWrg(0))
    val lambda3 = Lower.applyRuleToExpressions(lambda1, mapsOnLevel2.toList, Rules.mapLcl(0))

    var lambdaN = lambda3

    if (mapTree.MaxDepth > 2) {
      for (i <- 3 to mapTree.MaxDepth) {
        val mapsOnCurrLevel = mapTree.getMapsOnLevel(i)
        lambdaN = Lower.applyRuleToExpressions(lambdaN, mapsOnCurrLevel.toList, Rules.mapSeq)
      }
    }
    lambdas = lambdaN :: lambdas
  }
  if(enabledMappings.group01 && mapTree.MaxAvaDepth >= 4){
    val mapsOnLevel1 = mapTree.getMapsOnLevel(1)
    val mapsOnLevel2 = mapTree.getMapsOnLevel(2)
    val mapsOnLevel3 = mapTree.getMapsOnLevel(3)
    val mapsOnLevel4 = mapTree.getMapsOnLevel(4)
    val lambda1 = Lower.applyRuleToExpressions(lambda,mapsOnLevel1.toList,Rules.mapWrg(0))
    val lambda2 = Lower.applyRuleToExpressions(lambda1,mapsOnLevel2.toList,Rules.mapWrg(1))
    val lambda3 = Lower.applyRuleToExpressions(lambda2,mapsOnLevel3.toList,Rules.mapLcl(0))
    val lambda4 = Lower.applyRuleToExpressions(lambda3,mapsOnLevel4.toList,Rules.mapLcl(1))
    var lambdaN = lambda4
    if (mapTree.MaxDepth > 4) {
      for (i <- 5 to mapTree.MaxDepth) {
        val mapsOnCurrLevel = mapTree.getMapsOnLevel(i)
        lambdaN = Lower.applyRuleToExpressions(lambdaN, mapsOnCurrLevel.toList, Rules.mapSeq)
      }
    }
    lambdas = lambdaN :: lambdas
  }
  if(enabledMappings.group10 && mapTree.MaxAvaDepth >= 4){
    val mapsOnLevel1 = mapTree.getMapsOnLevel(1)
    val mapsOnLevel2 = mapTree.getMapsOnLevel(2)
    val mapsOnLevel3 = mapTree.getMapsOnLevel(3)
    val mapsOnLevel4 = mapTree.getMapsOnLevel(4)
    val lambda1 = Lower.applyRuleToExpressions(lambda,mapsOnLevel1.toList,Rules.mapWrg(1))
    val lambda2 = Lower.applyRuleToExpressions(lambda1,mapsOnLevel2.toList,Rules.mapWrg(0))
    val lambda3 = Lower.applyRuleToExpressions(lambda2,mapsOnLevel3.toList,Rules.mapLcl(1))
    val lambda4 = Lower.applyRuleToExpressions(lambda3,mapsOnLevel4.toList,Rules.mapLcl(0))
    var lambdaN = lambda4
    if (mapTree.MaxDepth > 4) {
      for (i <- 5 to mapTree.MaxDepth) {
        val mapsOnCurrLevel = mapTree.getMapsOnLevel(i)
        lambdaN = Lower.applyRuleToExpressions(lambdaN, mapsOnCurrLevel.toList, Rules.mapSeq)
      }
    }
    lambdas = lambdaN :: lambdas
  }
  lambdas
}

  def lowerMaps(lambda: Lambda, enabledMappings: EnabledMappings) : List[Lambda] = {

    val depthMap: collection.Map[Expr, Int] = NumberExpression.byDepth(lambda)
    val depthsOfUnLowered = depthMap.collect({ case (FunCall(ir.ast.Map(_), _*), depth) => depth })
    /*val UnLoweredMap = depthMap.filterKeys({
      case FunCall(Map(_),_*) => true
      case _=> false
    })*/

    if (depthsOfUnLowered.isEmpty)
      return List(lambda)

    val maxDepth = depthsOfUnLowered.max + 1
    val oneMapOnLevelTwo = if(maxDepth> 1) testHasOneMapOnSecondLevel(depthMap,maxDepth) else false
    var lambdas = List[Lambda]()
    //val oneMapOnLevelTwo = if (maxDepth > 1) hasOneMapOnSecondLevel(lambda) else false
//    val oneMapOnLevelThree = if (maxDepth > 2) hasOneMapOnThirdLevel(lambda) else false

    /* Global only */
    if (enabledMappings.global0) {
      //only allows one mapGlb!
      val lambda1 = Lower.lowerFirstNWithRule(lambda,Rules.mapGlb(0),1)
      //val lambda1 = Lower.lowerNextLevelWithRule(lambda, Rules.mapGlb(0))
      var lambdaN = mapGlbArgsToGlobal(lambda1)
      //var lambdaN = reduceResultToGlobal(lambda1)
      while (lambdaN.body.contains({ case e if Rules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, Rules.mapSeq)

      lambdas = lambdaN :: lambdas
    }

    if (enabledMappings.global10 && maxDepth > 1 && oneMapOnLevelTwo) {
      val lambda1 = Lower.lowerNextLevelWithRule(lambda, Rules.mapGlb(1))
      val lambda2 = Lower.lowerNextLevelWithRule(lambda1, Rules.mapGlb(0))
      var lambdaN = lambda2
      while (lambdaN.body.contains({ case e if Rules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, Rules.mapSeq)

      lambdas = lambdaN :: lambdas
    }

    if (enabledMappings.global01 && maxDepth > 1 && oneMapOnLevelTwo) {
      val lambda1 = Lower.lowerNextLevelWithRule(lambda, Rules.mapGlb(0))
      val lambda2 = Lower.lowerNextLevelWithRule(lambda1, Rules.mapGlb(1))
      var lambdaN = lambda2
      while (lambdaN.body.contains({ case e if Rules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, Rules.mapSeq)

      lambdas = lambdaN :: lambdas
    }

/*    def addGlobalMapping(first: Int, second: Int, third: Int): Unit = {
      if (maxDepth > 2 && oneMapOnLevelTwo && oneMapOnLevelThree) {
        val lambda1 = Lower.lowerNextLevelWithRule(lambda, Rules.mapGlb(first))
        val lambda2 = Lower.lowerNextLevelWithRule(lambda1, Rules.mapGlb(second))
        val lambda3 = Lower.lowerNextLevelWithRule(lambda2, Rules.mapGlb(third))
        var lambdaN = lambda3
        while (lambdaN.body.contains({ case e if Rules.mapSeq.isDefinedAt(e) => }))
          lambdaN = lowerNextLevelWithRule(lambdaN, Rules.mapSeq)

        lambdas = lambdaN :: lambdas
      }
    }
    addGlobalMapping(0,1,2)
    addGlobalMapping(1,0,2)
    addGlobalMapping(0,2,1)
    addGlobalMapping(2,1,0)
    addGlobalMapping(2,0,1)
    addGlobalMapping(1,2,0)*/

    /** Workgroup */
    if(enabledMappings.group0 && maxDepth > 1) {
      val lambda1 = Lower.lowerNextLevelWithRule(lambda, Rules.mapWrg(0))
      val lambda3 = Lower.lowerNextLevelWithRule(lambda1, Rules.mapLcl(0))

      var lambdaN = lambda3

      while (lambdaN.body.contains({ case e if Rules.mapSeq.isDefinedAt(e) => }))
        lambdaN = lowerNextLevelWithRule(lambdaN, Rules.mapSeq)

      lambdas = lambdaN :: lambdas
    }

    def addWrgLocalMapping(first: Int, second: Int): Unit = {
      if (maxDepth > 3 && oneMapOnLevelTwo) {
        val lambda1 = Lower.lowerNextLevelWithRule(lambda, Rules.mapWrg(first))
        val lambda2 = Lower.lowerNextLevelWithRule(lambda1, Rules.mapWrg(second))
        val lambda3 = Lower.lowerNextLevelWithRule(lambda2, Rules.mapLcl(first))
        val lambda4 = Lower.lowerNextLevelWithRule(lambda3, Rules.mapLcl(second))

        var lambdaN = lambda4

        while (lambdaN.body.contains({ case e if Rules.mapSeq.isDefinedAt(e) => }))
          lambdaN = lowerNextLevelWithRule(lambdaN, Rules.mapSeq)

        lambdas = lambdaN :: lambdas
      }
    }

    if (enabledMappings.group01)
      addWrgLocalMapping(0,1)

    if (enabledMappings.group10)
      addWrgLocalMapping(1,0)

/*    def addWrgLocalMapping3D(first: Int, second: Int, third: Int): Unit = {
      if (maxDepth > 5 && oneMapOnLevelTwo && oneMapOnLevelThree) {
        val lambda1 = Lower.lowerNextLevelWithRule(lambda, Rules.mapWrg(first))
        val lambda2 = Lower.lowerNextLevelWithRule(lambda1, Rules.mapWrg(second))
        val lambda3 = Lower.lowerNextLevelWithRule(lambda2, Rules.mapWrg(third))
        val lambda4 = Lower.lowerNextLevelWithRule(lambda3, Rules.mapLcl(first))
        val lambda5 = Lower.lowerNextLevelWithRule(lambda4, Rules.mapLcl(second))
        val lambda6 = Lower.lowerNextLevelWithRule(lambda5, Rules.mapLcl(third))

        var lambdaN = lambda6

        while (lambdaN.body.contains({ case e if Rules.mapSeq.isDefinedAt(e) => }))
          lambdaN = lowerNextLevelWithRule(lambdaN, Rules.mapSeq)

        lambdas = lambdaN :: lambdas
      }
    }

    addWrgLocalMapping3D(0,1,2)
    addWrgLocalMapping3D(1,0,2)
    addWrgLocalMapping3D(0,2,1)
    addWrgLocalMapping3D(2,1,0)
    addWrgLocalMapping3D(2,0,1)
    addWrgLocalMapping3D(1,2,0)*/

    lambdas
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
        case FunCall(MapSeq(_), _) => true
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
        case FunCall(Tuple(_), args@_*) =>
         args.foldRight(currentLambda)((arg, l) => {
           arg match{
             case FunCall(MapSeq(_), _) =>
               Context.updateContext(l.body)
               FunDecl.replace(l, arg, Rules.privateMemory.rewrite(arg))
           }
         })
        case _ => currentLambda
      }
    })
  }

  def lastWriteToGlobal(lambda: Lambda): Lambda =
    if (isTheLastWriteNestedInReduce(lambda))
      lastReduceToGlobal(lambda)
    else
      lastMapToGlobal(lambda)


  private def mapGlbArgsToGlobal(lambda:Lambda):Lambda = {
    val mapGlbs = getExprForPattern(lambda,{
      case FunCall(MapGlb(_,_),_) =>
      }
    )
    val toGlobalAdded = mapGlbs.foldLeft(lambda)({
      case (currentLambda,expr) =>
        val currArg = expr.asInstanceOf[FunCall].args(0)
        val updated = currArg match{
          case p:Param =>
            expr
          case fc:FunCall =>
            val currentToGlobal = FunCall(expr.asInstanceOf[FunCall].f,toGlobal(fun((idIn) => Id()(idIn))) $ currArg)
            val Id2 = currentToGlobal.asInstanceOf[FunCall].args(0).asInstanceOf[FunCall].f.asInstanceOf[toGlobal].f.body
            val currIdsImplemented = Rewrite.applyRuleAt(currentToGlobal,Rules.implementIdAsDeepCopy,Id2)
            currIdsImplemented
        }
        Lambda(currentLambda.params,Expr.replace(currentLambda.body,expr,updated))
    }
    )

    toGlobalAdded
  }
  def getExprForPattern(lambda: Lambda, pattern: PartialFunction[Expr, Unit]): Seq[Expr] = {
    Expr.visitWithState(Seq[Expr]())(lambda.body, {
      case (expr, matches) if pattern.isDefinedAt(expr) => matches :+ expr
      case (_, matches) => matches
    })
  }

  private def lastMapToGlobal(lambda: Lambda): Lambda = {
    val lastWrite = getLastWrite(lambda).get
    val lastMap = findExpressionForPattern(lambda,
      { case FunCall(ir.ast.Map(Lambda(_, body)), _) if body eq lastWrite => }: PartialFunction[Expr, Unit] ).get

    Rewrite.applyRuleAt(lambda, lastMap, Rules.globalMemory)
  }

  private def lastReduceToGlobal(lambda: Lambda): Lambda = {
    val pattern1: PartialFunction[Expr, Unit] = {
      case FunCall(Id(), _) =>
    }

    Context.updateContext(lambda.body)

    val ids = findAllDepthFirst(lambda, pattern1.isDefinedAt)
    val pattern2: PartialFunction[Expr, Unit] = {
      case FunCall(MapSeq(f), _) if ids.contains(f.body) =>
    }
    val mapSeqs = findAllDepthFirst(lambda, pattern2.isDefinedAt)

    implementIdInMapSeq(lambda, ids.last, mapSeqs.last)
  }

  def implementIdInMapSeq(lambda: Lambda, idToImplement: Expr, mapSeqForId: Expr): Lambda = {
    TypeChecker.check(lambda.body)

    val implementedId = Rules.implementIdAsDeepCopy.rewrite(idToImplement)
    val idReplaced = Expr.replace(mapSeqForId, idToImplement, implementedId)

    val idToGlobal = Rules.globalMemory.rewrite(idReplaced)
    FunDecl.replace(lambda, mapSeqForId, idToGlobal)
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

  def lowerPartialReduces(lambda: Lambda): Lambda =
    lowerPatternWithRule(lambda, { case FunCall(PartRed(_), _, _) => }, Rules.partialReduceToReduce)

  def lowerReduces(lambda: Lambda): Lambda =
    lowerPatternWithRule(lambda, { case FunCall(Reduce(_), _, _) => }, Rules.reduceSeq)

  @scala.annotation.tailrec
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
  def lowerFirstNWithRule(lambda:Lambda,rule:Rule,firstN:Int) ={
    val nextToLower = FindNextMapsToLower()(lambda,firstN)
    applyRuleToExpressions(lambda,nextToLower,rule)
  }

  def lower(lambda: Lambda): List[Lambda] = {
    lowerByLevels(lastWriteToGlobal(lowerReduces(lambda)))
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
  def apply(lambda:Lambda,firstN:Int):List[Int] =
    apply(lambda.body,firstN)
  def apply(expr:Expr,firstN:Int):List[Int] = {
    idMap = NumberExpression.breadthFirst(expr)
    find(expr)
    if(firstN>expressions.length){
      expressions
    }
    else {
      expressions.takeRight(firstN)
    }
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
