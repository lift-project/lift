package rewriting.macrorules

import ir.{Type, TypeChecker}
import ir.ast._
import lift.arithmetic._
import rewriting.Rewrite
import rewriting.rules._
import rewriting.utils.{NumberExpression, Utils}

object ReuseRules {

  import Utils.{mapPattern, reducePattern, concretePattern, getMapAtDepth, getMapBody}

  /**
    * Tile a computation in the form Map(Map(f))
    */
  val tileMapMap: Rule = tileMapMap(?, ?)

  def tileMapMap(x: ArithExpr, y: ArithExpr): Rule =
    Rule("Tile Map(Map(f))", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, chain,_)), _)
        if getCallForBlocking(chain, lambdaParam).isDefined
      =>
        val tiled = tileMapMap(x, y, funCall)

        val innermostMap = getMapAtDepth(tiled, 3)
        val callChain = getMapBody(innermostMap)

        val innerFission = findConcretePatterns(callChain)

        var fissioned = tiled

        // Fission between reduces and maps. Blocking and tiling have to be applied separately,
        // if tiling the input as well.
        if (innerFission.length > 1) {

          fissioned = fissionBetweenConcretes(fissioned, innermostMap)

          val fissionInHere = getMapAtDepth(fissioned, 2)

          fissioned = fissionBetweenConcretes(fissioned, fissionInHere)
        }

        fissioned
    })

  private def fissionBetweenConcretes(expr: Expr, fissionInHere: Expr) = {
    val body = getMapBody(fissionInHere)
    val concretes = findConcretePatterns(body)
    val concreteIds = getIdsForExpressions(body, concretes)
    val res2 = concreteIds.tail.foldLeft(fissionInHere)((e, id) =>
      Rewrite.applyRuleAt(e, MacroRules.mapFissionAtPosition(id), e))

    Expr.replace(expr, fissionInHere, res2)
  }

  private def getIdsForExpressions(callChain: Expr, outerFissions: List[Expr]): List[Int] = {
    outerFissions.map(x =>
      Utils.getIndexForPatternInCallChain(callChain, { case e if e == x => }))
  }

  private def findConcretePatterns(callChain: Expr): List[Expr] = {
    Utils.visitFunCallChainWithState(List[Expr]())(callChain, (e, s) => {
      if (mapPattern.isDefinedAt(e) || reducePattern.isDefinedAt(e))
        e :: s
      else
        s
    })
  }

  val apply1DRegisterBlocking: Rule = apply1DRegisterBlocking(?)

  /**
    * Apply register blocking in 1 dimension.
    * @param factor The amount of work to assign to the new dimension
    * @return
    */
  def apply1DRegisterBlocking(factor: ArithExpr): Rule = Rule("1D register blocking", {
    case call@FunCall(Map(Lambda(lambdaArg, innerCall,_)), _)
      if getCallForBlocking(innerCall, lambdaArg).isDefined
    =>
      // Split-join on outermost map
      val split = Rules.splitJoin(factor).rewrite.apply(call)

      // Interchange on the newly created dimension
      val interchanged = Rewrite.depthFirstApplyRuleAtId(split, 2, MacroRules.mapMapInterchange)

      // Interchange again on every map/reduce in the innermost dimension
      val map = getMapAtDepth(interchanged, 2)
      applyInterchangeOnAllComponents(interchanged, map)
  })

  val apply2DRegisterBlocking: Rule = apply2DRegisterBlocking(?, ?)

  val apply2DRegisterBlockingNoReorder: Rule =
    apply2DRegisterBlocking(?, ?, doReorder = false)

  def apply2DRegisterBlocking(factorX: ArithExpr, factorY: ArithExpr,
    doReorder: Boolean = true): Rule =

    Rule("2D register blocking" + (if (doReorder) "" else " no reorder"), {
      case call@FunCall(Map(Lambda(lambdaArg, innerCall,_)), _)
        if getCallForBlocking(innerCall, lambdaArg).isDefined
        // TODO: is the guard good enough?
      =>
        val innerMap = getMapAtDepth(call, 1)

        // Reorder both sides of the inner map
        val realY = Utils.splitVariable(factorY, innerMap.t)
        val stride = Type.getLength(innerMap.t) / realY

        val reorderReplaced =
          if (doReorder)
            Rewrite.applyRuleAt(call, Rules.reorderBothSidesWithStride(stride), innerMap)
          else
            call

        val tiled = tileMapMap(factorX, realY, reorderReplaced)

        // Bring innermost components out by 2 levels
        val map0 = getMapAtDepth(tiled, 3)
        val firstInterchange = applyInterchangeOnAllComponents(tiled, map0)

        val map1 = getMapAtDepth(firstInterchange, 2)
        val secondInterchange = applyInterchangeOnAllComponents(firstInterchange, map1)

        secondInterchange
    })

    private def getCallForBlocking(innerCall: Expr, lambdaArg: Array[Param]): Option[Expr] = {
    val firstConcrete = Utils.getExprForPatternInCallChain(innerCall, concretePattern)
    firstConcrete match {
      case Some(FunCall(Map(_), _)) => firstConcrete
      case _ => None
    }
  }

    private def applyInterchangeOnAllComponents(replaceIn: Expr, interchangeHere: Expr): Expr = {
    var nextToInterchange = interchangeHere
    var innerInterchanged = replaceIn

    val reduceRule = MacroRules.moveReduceOutOneLevel.rewrite
    val mapRule = MacroRules.mapMapInterchange.rewrite

    val funCallPattern: PartialFunction[Expr, Unit] = { case FunCall(_, _) => }

    while (funCallPattern.isDefinedAt(nextToInterchange)) {

      TypeChecker(innerInterchanged)

      if (reduceRule.isDefinedAt(nextToInterchange)) {

        val replacement = reduceRule(nextToInterchange)
        innerInterchanged = Expr.replace(innerInterchanged, nextToInterchange, replacement)

        val reduceCall = Utils.getExprForPatternInCallChain(replacement, reducePattern).get

        reduceCall match {
          case FunCall(_, _, next) =>
            nextToInterchange = next
          case _ =>
        }

      } else if (mapRule.isDefinedAt(nextToInterchange)) {

        val replacement = mapRule(nextToInterchange)

        innerInterchanged = Expr.replace(innerInterchanged, nextToInterchange, replacement)

        val mapCall = Utils.getExprForPatternInCallChain(replacement, mapPattern).get

        mapCall match {
          case FunCall(_, next) =>
            nextToInterchange = next
          case _ =>
        }

      } else {
        nextToInterchange match {
          case FunCall(_, next) =>
            nextToInterchange = next
          case _ =>
        }
      }

    }

    innerInterchanged
  }

    private def tileMapMap(factorX: ArithExpr, factorY: ArithExpr, expr: Expr): Expr = {

      // Fission if necessary
      val mapIndex = Utils.getIndexForPatternInCallChain(getMapBody(expr), mapPattern)
      val fissioned =
        if (mapIndex > 0) MacroRules.mapFissionAtPosition(mapIndex - 1).rewrite(expr)
        else expr

    // split-join on the outer map
    val outerMap = getMapAtDepth(fissioned, 0)
    val outerMapSplitJoined = Rewrite.applyRuleAt(fissioned, Rules.splitJoin(factorX), outerMap)

    // interchange on the new dim
    val newDimension = getMapAtDepth(outerMapSplitJoined, 1)
    val interchanged = Rewrite.applyRuleAt(outerMapSplitJoined, MacroRules.mapMapInterchange, newDimension)

    // split-join on the inner map
    val innerMap = getMapAtDepth(interchanged, 1)
    val innerMapSplitJoined = Rewrite.applyRuleAt(interchanged, Rules.splitJoin(factorY), innerMap)

    // interchange on the new dim
    val secondNewDimension = getMapAtDepth(innerMapSplitJoined, 2)
    val e2 = Rewrite.applyRuleAt(innerMapSplitJoined, MacroRules.mapMapInterchange, secondNewDimension)

    e2
  }

  /**
    * Tile Map(Map(f)) o Transpose()
    */
  val tileTranspose: Rule = tileTranspose(?, ?)

  def tileTranspose(x: ArithExpr, y:ArithExpr): Rule =
    Rule("Map(Map(f)) o Transpose() => tiled", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, FunCall(Map(_), arg),_)), FunCall(Transpose(), _))
        if lambdaParam.head eq arg
      =>
        val e1 = Rewrite.applyRuleAtId(funCall, 0, tileMapMap(x, y))
        val e2 = Rewrite.applyRuleAtId(e1, 1, moveTransposeInsideTiling)
        e2
    })

  val finishRectangularTiles =
    Rule("", {
      case funCall@FunCall(Map(Lambda(_,
      FunCall(TransposeW(), FunCall(Join(),
      FunCall(Map(_), FunCall(Split(_), FunCall(Transpose(), _)))))
      ,_)), FunCall(Split(_), _:Param))
        if FissionRules.mapFissionWithZipInside.rewrite.isDefinedAt(funCall)
          && FissionRules.mapFissionWithZipInside.rewrite.isDefinedAt(
          Rewrite.getExprForId(funCall, 5, NumberExpression.breadthFirst(funCall)))
      =>
        val e1 = Rewrite.applyRuleAtId(funCall, 5, moveTransposeInsideTiling)
        val e5 = Rewrite.applyRuleAtId(e1, 0, FissionRules.mapFissionWithZipInside)
        val e6 = Rewrite.applyRuleAtId(e5, 1, FissionRules.mapFission)
        val e7 = Rewrite.applyRuleAtId(e6, 2, Rules.mapTransposeSplit)
        val e8 = Rewrite.applyRuleAtId(e7, 1, Rules.mapSplitTranspose)

        e8
    })

  /**
    * Move transposition inside tiling, that is, transpose the 2D structure of tiles
    * and then transpose each tile.
    */
  val moveTransposeInsideTiling: Rule =
    Rule("Tile(M, N) o Transpose() => " +
      "Map(Map(Transpose())) o Transpose() o Tile(M, N)", {

      // Directly applies
      case funCall @
        FunCall(Map(Lambda(_, FunCall(Split(_), FunCall(Transpose(), _:Param)),_)),
        FunCall(Split(_), FunCall(Transpose(), _)))
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 4, Rules.splitTranspose)
        val e1 = Rewrite.depthFirstApplyRuleAtId(e0, 0, FusionRules.mapFusion)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 2, SimplificationRules.transposeTransposeId)
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 0, Rules.mapSplitTranspose)
        e3

      // Applies after some manipulation
      case FunCall(Map(Lambda(param, body,_)), arg@FunCall(Split(_), FunCall(Transpose(), _)))
        if body.contains({ case FunCall(Split(_), FunCall(Transpose(), a)) if a eq param.head => })
          && Expr.visitWithState(0)(body, (e, count) => if (e eq param.head) count+1 else count) == 1
      =>

        var exprToReplace: Option[Expr] = None

        Expr.visit(body, {
          case e@FunCall(Split(_), FunCall(Transpose(), a)) if a eq param.head =>
            exprToReplace = Some(e)
          case _ =>
        }, _ => Unit)

        val newLambdaParam = Param()
        val newBody = Expr.replace(body, exprToReplace.get, newLambdaParam)

        val newParam = Param()
        val newExpr = Expr.replace(exprToReplace.get, param.head, newParam)

        val newFunCall = FunCall(Map(Lambda(Array(newLambdaParam), newBody)),
          FunCall(Map(Lambda(Array(newParam), newExpr)), arg))

        val applyHere = Utils.getExprForPatternInCallChain(newFunCall,
          { case e if moveTransposeInsideTiling.isDefinedAt(e) => }).get

        Rewrite.applyRuleAt(newFunCall, moveTransposeInsideTiling, applyHere)
    })


  val finishTiling: Rule = finishTiling(?)

  def finishTiling(x: ArithExpr): Rule =
    Rule("finishTiling", {
      case funCall @
        FunCall(Map(Lambda(_, c,_)), _)
        if Utils.getIndexForPatternInCallChain(c, mapPattern) != -1
          && (Utils.getExprForPatternInCallChain(c, mapPattern).get.contains(mapPattern)
          || Utils.getExprForPatternInCallChain(c, mapPattern).get.contains(reducePattern))
      =>

        val mapInC = getMapAtDepth(c, 0)

        val body = mapInC.asInstanceOf[FunCall].f.asInstanceOf[Map].f.body

        val expr =
          if (Utils.getIndexForPatternInCallChain(body, mapPattern) != -1)
            Utils.getExprForPatternInCallChain(body, mapPattern).get
          else
            Utils.getExprForPatternInCallChain(body, reducePattern).get

        Utils.getFinalArg(expr) match {
          case FunCall(Zip(_), _*) =>


            val middleMap = getMapAtDepth(funCall, 1)
            val ex = applyInterchangeOnAllComponents(funCall, middleMap)

            val outerMap = getMapAtDepth(ex, 0)
            val ey = applyInterchangeOnAllComponents(ex, outerMap)

            val newOuterMap = getMapAtDepth(ey, 0)
            val e0 = Rewrite.applyRuleAt(ey, Rules.splitJoin(x), newOuterMap)


            val splitZip = getMapAtDepth(e0, 0)
            val e1 = Rewrite.applyRuleAt(e0, Rules.splitIntoZip, splitZip)

            val outer = getMapAtDepth(e1, 1)
            val e2 = applyInterchangeOnAllComponents(e1, outer)

            val inner = getMapAtDepth(e2, 2)
            val e3 = applyInterchangeOnAllComponents(e2, inner)

            e3
          case _ =>

            val split =
              if (Rules.splitJoin.isDefinedAt(expr))
                Rewrite.applyRuleAt(funCall, Rules.splitJoin(x), expr)
              else {
                val appliedPartialReduce = ReduceRules.partialReduce.rewrite(expr)

                val rule = ReduceRules.partialReduceSplitJoin(x)
                val partialReduce = Utils.getExprForPatternInCallChain(
                  appliedPartialReduce, { case e if rule.isDefinedAt(e) => }).get

                val splitJoined = Rewrite.applyRuleAt(appliedPartialReduce, rule, partialReduce)

                Expr.replace(funCall, expr, splitJoined)
              }

            val innerMap = getMapAtDepth(split, 1)
            val innerExchanged = Rewrite.applyRuleAt(split, MacroRules.interchange, innerMap)

            val outerMap = getMapAtDepth(innerExchanged, 0)
            val outerExchanged = Rewrite.applyRuleAt(innerExchanged, MacroRules.interchange, outerMap)

            outerExchanged
        }
    })

  val introduceReuseFromMap: Rule = introduceReuseFromMap(?)

  def introduceReuseFromMap(arithExpr: ArithExpr): Rule = {
    Rule("introduceReuseFromMap", {
      case call@FunCall(Map(Lambda(_, body,_)), _)
        if Utils.getIndexForPatternInCallChain(body, mapPattern) != -1 ||
          Utils.getIndexForPatternInCallChain(body, { case FunCall(Reduce(_), _, _) => }) != -1
      =>

        val mapId = Utils.getIndexForPatternInCallChain(body, mapPattern)
        val reduceId = Utils.getIndexForPatternInCallChain(body, { case FunCall(Reduce(_), _, _) => })

        var splitJoined: Expr = call

        if (mapId < reduceId && mapId != -1 && reduceId != -1 || reduceId == -1) {
          val insideMap = Utils.getExprForPatternInCallChain(body, mapPattern)
          splitJoined = Rewrite.applyRuleAt(call, Rules.splitJoin(arithExpr), insideMap.get)
        } else {
          val insideReduce = Utils.getExprForPatternInCallChain(body, { case FunCall(Reduce(_), _, _) => })
          val partialReduce = Rewrite.applyRuleAt(call, ReduceRules.partialReduce, insideReduce.get)

          val newBody = getMapBody(partialReduce)

          val newReduce = Utils.getExprForPatternInCallChain(newBody, { case FunCall(PartRed(_), _, _) => })
          splitJoined = Rewrite.applyRuleAt(partialReduce, ReduceRules.partialReduceSplitJoin(arithExpr), newReduce.get)
        }

        // TODO: Only in the inside call chain
        val moveSplit = Rewrite.applyRulesUntilCannot(splitJoined, Seq(Rules.splitIntoZip))

        Rewrite.applyRuleAt(moveSplit, MacroRules.interchange, moveSplit)
    })
  }
}
