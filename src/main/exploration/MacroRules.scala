package exploration

import apart.arithmetic.{?, ArithExpr}
import exploration.utils.{NumberExpression, Utils}
import ir._
import ir.ast._

object MacroRules {

  private val mapPattern: PartialFunction[Expr, Unit] =
  { case FunCall(map: Map, _) if map.f.body.isConcrete => }

  private val mapMapPattern: PartialFunction[Expr, Unit] =
  { case FunCall(Map(Lambda(_, FunCall(map:Map, _))), _) if map.f.body.isConcrete => }

  private val reducePattern: PartialFunction[Expr, Unit] =
  { case FunCall(_: AbstractPartRed, _, _) => }

  private val splitPattern: PartialFunction[Expr, Unit] =
  { case FunCall(Split(_), _) => }

  private val concretePattern: PartialFunction[Expr, Unit] =
  { case call: FunCall if call.isConcrete(false) => }

  def getMapAtDepth(expr:Expr, depth: Int): Expr = {
    val outermostMap = Utils.getExprForPatternInCallChain(expr, mapPattern).get

    if (depth == 0)
      outermostMap
    else
      getMapAtDepth(getMapBody(outermostMap), depth - 1)
  }

  def getMapBody(expr: Expr) = {
    expr match {
      case FunCall(Map(Lambda(_, body)), _) => body
    }
  }

  /**
   * Apply map fission. Helper rule for other macro rules.
   */
  val mapFissionAtPosition: Int => Rule = position =>
    Rule("Map(... o ... o ...) => Map(... o ...) o Map(...)", {
      case funCall @ FunCall(Map(Lambda(_, body:FunCall)), _)
        if Utils.getIndexForPatternInCallChain(body,
        { case e if e eq Utils.getFinalArg(body) => }) - 1 > position
      =>
        mapFissionAtPosition(position, funCall)
    })

  private def mapFissionAtPosition(position: Int, expr: Expr): Expr = {
    var nextFission = expr
    var fissioned = expr
    var currentPos = position

    while (currentPos >= 0) {

      val replacement = Rules.mapFission.rewrite(nextFission)
      fissioned = Expr.replace(fissioned, nextFission, replacement)

      nextFission = replacement match {
        case FunCall(_: AbstractPartRed, _, arg) => arg
        case FunCall(_, arg) => arg
      }

      currentPos -= 1
    }

    currentPos = position
    var fused = fissioned

    while (currentPos > 0) {

      fused = Rules.mapFusion.rewrite(fused)

      currentPos -= 1
    }

    fused
  }

  // transpose both sides + id
  private val transposeMapMapNoFission =
    Rule("Transpose() o Map(Map(_)) => " +
      "Map(Map(_)) o Transpose()", {
      case FunCall(t,
            FunCall(Map(Lambda(p1,
              FunCall(Map(Lambda(p2,
                FunCall(f, a2))
              ), a1))
            ), arg))
        if (p1.head eq a1)
          && (p2.head eq a2)
          && Rules.isTranspose(t)
      =>
        Map(Map(f)) o Transpose() $ arg
    })

  /**
   * Move Transpose over Map(Map()), fission appropriately.
   * Used as an enabling rule when simplifying.
   */
  val transposeMapMap =
    Rule("transposeMapMap", {
      case funCall@FunCall(t,
              FunCall(Map(Lambda(p1,
                innerMapCall@FunCall(Map(Lambda(p2,
                  FunCall(f, a2)
                )), a1)
              )), _))
        if Utils.getIndexForPatternInCallChain(a1, { case e if e eq p1.head => } ) != -1
        && Utils.getIndexForPatternInCallChain(a2, { case e if e eq p2.head => } ) != -1
        && Rules.isTranspose(t)
      =>
        var fissioned: Expr = funCall

        if (!(a2 eq p2.head))
          fissioned = Expr.replace(fissioned, innerMapCall, Rules.mapFission.rewrite(innerMapCall))

        val map = Utils.getExprForPatternInCallChain(fissioned, { case FunCall(Map(_), _) => }).get

        if (!(p1.head eq a1) || !(a2 eq p2.head))
          fissioned = Expr.replace(fissioned, map, Rules.mapFission.rewrite(map))

        transposeMapMapNoFission.rewrite(fissioned)
    })

  /**
   * Apply the Map(Transpose) o Transpose o Map(Transpose) rule, fission appropriately.
   * Used as an enabling rule when simplifying.
   */
  val mapTransposeTransposeMapTranspose =
    Rule("Map(... o Transpose()) o Transpose() o Map(Transpose() o ...)) => " +
         "Map(...) o Transpose() o Map(Transpose()) o Transpose() o Map(...)", {
      case funCall@FunCall(Map(Lambda(param1, firstBody)),
      FunCall(t2,
      secondMap@FunCall(Map(Lambda(param2, FunCall(t3, a2))), _)))
        if Utils.getIndexForPatternInCallChain(a2, { case e if e eq param2.head => }) != -1
          && Utils.getIndexForPatternInCallChain(firstBody,
              { case FunCall(t, a) if Rules.isTranspose(t) && (a eq param1.head) => }) != -1
          && Rules.isTranspose(t2)
          && Rules.isTranspose(t3)
      =>

        var fissioned: Expr = funCall

        val index = Utils.getIndexForPatternInCallChain(firstBody,
          { case FunCall(Transpose(), arg) if arg eq param1.head => })

        if (index > 0)
          fissioned = mapFissionAtPosition(index - 1).rewrite(funCall)

        if (!(a2 eq param2.head))
          fissioned = Expr.replace(fissioned, secondMap, Rules.mapFission.rewrite(secondMap))

        val applyHere = Utils.getExprForPatternInCallChain(fissioned,
          { case e if Rules.mapTransposeTransposeMapTranspose.isDefinedAt(e) => }).get

        Expr.replace(fissioned, applyHere, Rules.mapTransposeTransposeMapTranspose.rewrite(applyHere))
    })

  /**
   * Apply the Transpose o Map(Split) rule, fission appropriately.
   * Used as an enabling rule when simplifying.
   */
  val transposeMapSplit =
    Rule("transposeMapSplit", {
      case funCall@FunCall(t, FunCall(Map(Lambda(p, FunCall(Split(_), a))), _))
        if Rules.isTranspose(t)
      =>

        var fissioned: Expr = funCall

        if (!(p.head eq a))
          fissioned = Rewrite.applyRuleAtId(fissioned, 1, Rules.mapFission)

        Rules.transposeMapSplit.rewrite(fissioned)
    })

  /**
   * Apply the Map(Split) o Transpose rule, fission appropriately.
   * Used as an enabling rule when simplifying.
   */
  val mapSplitTranspose =
    Rule("mapSplitTranspose", {
      case funCall@FunCall(Map(Lambda(param, body)), FunCall(t, _))
        if Utils.getIndexForPatternInCallChain(body,
            { case FunCall(Split(_), arg) if arg eq param.head => }) != -1
          && Rules.isTranspose(t)
      =>
        val index = Utils.getIndexForPatternInCallChain(body,
                { case FunCall(Split(_), arg) if arg eq param.head => })

        var fissioned: Expr = funCall

        if (index > 0)
          fissioned = mapFissionAtPosition(index-1, funCall)

        val applyHere = Utils.getExprForPatternInCallChain(fissioned,
            { case e if Rules.mapSplitTranspose.isDefinedAt(e) => }).get

        Expr.replace(fissioned, applyHere, Rules.mapSplitTranspose.rewrite(applyHere))
    })

  /**
   * A rule to move join over a map.
   * Used as an enabling rule when simplifying.
   */
  val movingJoin =
    Rule("Map(_) o Join() => Join() o Map(Map(_))", {
      case call@FunCall(Map(_), joinCall@FunCall(Join(), arg)) =>

        val splitFactor = arg.t match {
          case ArrayType(ArrayType(_, m), _) => m
        }

        val splitJoined = Rewrite.applyRuleAt(call, Rules.splitJoin(splitFactor), call)

        val newSplit = Utils.getExprForPatternInCallChain(splitJoined, splitPattern).get

        val eliminated = Rewrite.applyRuleAt(splitJoined, Rules.splitJoinId, newSplit)

        eliminated
    })

  /**
   * A rule to move split over a map.
   * Used as an enabling rule when simplifying.
   */
  val movingSplit =
    Rule("Split(n) o Map(_) => Map(Map(_)) o Split(n)", {
      case call@FunCall(Split(n), mapCall@FunCall(Map(_), _)) =>

        val splitJoined = Rewrite.applyRuleAt(call, Rules.splitJoin(n), mapCall)
        val eliminated = Rewrite.applyRuleAt(splitJoined, Rules.splitJoinId, splitJoined)

        eliminated
    })

  /**
   * Eliminate a matching split-join pair. If there are maps between them, apply
   * the split-join and splitJoinId rules, to move the pair closer to each other.
   */
  val splitJoinId: Rule = Rule("Split(n) ... Join() => Map(...)", {
    case call@FunCall(Split(n), arg)
      if Utils.visitFunCallChainWithState((false, true))(arg, (expr, state) => {
        // State = (applicable, stillPossible)
        expr match {
          case FunCall(Map(_), _) => state
          case FunCall(Join(), a)
            if Utils.innerLengthOfTypeMatches(a.t, n)
          =>
            if (state._2)
              (true, state._1)
            else
              state
          case _ => (state._1, false)
        }
      })._1
    =>

      if (Rules.splitJoinId.isDefinedAt(call)) {
        Rules.splitJoinId.rewrite(call)
      } else {
        val newCall = Rewrite.applyRuleAt(call, Rules.splitJoin(n), arg)

        val splitJoinEliminated = Rewrite.applyRuleAt(newCall, Rules.splitJoinId, newCall)

        val newSplit = Utils.getExprForPatternInCallChain(splitJoinEliminated, splitPattern).get

        Rewrite.applyRuleAt(splitJoinEliminated, splitJoinId, newSplit)
      }
  })

  /**
   * Fuse Reduce o Map, automatically applying applying the ReduceSeq and MapSeq rules.
   */
  val reduceMapFusion = Rule("Reduce o Map => ReduceSeq(fused)", {
    case funCall @ FunCall(Reduce(_), _, mapCall@FunCall(Map(_), _))
      if Rules.mapSeq.isDefinedAt(mapCall)
    =>
      val e0 = Rewrite.applyRuleAtId(funCall, 1, Rules.mapSeq)
      val e1 = Rewrite.applyRuleAtId(e0, 0, Rules.reduceSeq)
      val e2 = Rewrite.applyRuleAtId(e1, 1, Rules.reduceSeqMapSeqFusion)
      e2
  })

  /**
   * Tile a computation in the form Map(Map(f))
   */
  val tileMapMap: Rule = tileMapMap(?, ?)

  def tileMapMap(x: ArithExpr, y: ArithExpr): Rule =
    Rule("Tile a computation in the form Map(fun(y => Map(f) $ y )) $ x", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, chain)), _)
        if getCallForBlocking(chain, lambdaParam).isDefined
      =>
        val tiled = tileMapMap(x, y, funCall)

        val innerMostMap = getMapAtDepth(tiled, 3)
        val callChain = getMapBody(innerMostMap)

        val mapId = Utils.getIndexForPatternInCallChain(callChain, mapPattern)
        val reduceId = Utils.getIndexForPatternInCallChain(callChain, reducePattern)

        val hasMap = mapId != -1
        val hasReduce = reduceId != -1

        var fissioned = tiled

        // TODO: assuming maps clumped together and reduces clumped together

        // Fission between reduces and maps. Blocking and tiling have to be applied separately,
        // if tiling the input as well.
        if (hasReduce && hasMap) {
          val larger = if (mapId > reduceId) mapId else reduceId

          val firstFission = Rewrite.applyRuleAt(tiled, mapFissionAtPosition(larger - 1), innerMostMap)

          val fissionInHere = getMapAtDepth(firstFission, 2)

          fissioned = Rewrite.applyRuleAt(firstFission, Rules.mapFission, fissionInHere)
        }

        fissioned
    })

  val finishTiling: Rule = finishTiling(?)

  def finishTiling(x: ArithExpr): Rule =
    Rule("finishTiling", {
      case funCall @
        FunCall(Map(Lambda(p, c)), _)
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
          case FunCall(Zip(_), args@_*) =>


            val middleMap = getMapAtDepth(funCall, 1)
            val ex = applyInterchangeOnAllComponents(funCall, middleMap)

            val outerMap = getMapAtDepth(ex, 0)
            val ey = applyInterchangeOnAllComponents(ex, outerMap)

            val newOuterMap = getMapAtDepth(ey, 0)
            val e0 = Rewrite.applyRuleAt(ey, Rules.splitJoin(x), newOuterMap)


            val splitZip = getMapAtDepth(e0, 0)
            val e1 = Rewrite.applyRuleAt(e0, Rules.splitZip, splitZip)

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
                val appliedPartialReduce = Rules.partialReduce.rewrite(expr)

                val rule = Rules.partialReduceSplitJoin(x)
                val partialReduce = Utils.getExprForPatternInCallChain(
                appliedPartialReduce, { case e if rule.isDefinedAt(e) => }).get

                val splitJoined = Rewrite.applyRuleAt(appliedPartialReduce, rule, partialReduce)

                Expr.replace(funCall, expr, splitJoined)
              }

            val innerMap = getMapAtDepth(split, 1)
            val innerExchanged = Rewrite.applyRuleAt(split, interchange, innerMap)

            val outerMap = getMapAtDepth(innerExchanged, 0)
            val outerExchanged = Rewrite.applyRuleAt(innerExchanged, interchange, outerMap)

            outerExchanged
        }
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
        FunCall(Map(Lambda(_, FunCall(Split(_), FunCall(Transpose(), _:Param)))),
        FunCall(Split(_), FunCall(Transpose(), _)))
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 4, Rules.splitTranspose)
        val e1 = Rewrite.depthFirstApplyRuleAtId(e0, 0, Rules.mapFusion)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 2, Rules.transposeTransposeId)
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 0, Rules.mapSplitTranspose)
        e3

      // Applies after some manipulation
      case funCall@FunCall(Map(Lambda(param, body)), arg@FunCall(Split(_), FunCall(Transpose(), _)))
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

  val finishRectangularTiles =
    Rule("", {
      case funCall@FunCall(Map(Lambda(_,
      FunCall(TransposeW(), FunCall(Join(),
      FunCall(Map(_), FunCall(Split(_), FunCall(Transpose(), _)))))
      )), FunCall(Split(_), _:Param))
        if Rules.mapFissionWithZipInside.rewrite.isDefinedAt(funCall)
          && Rules.mapFissionWithZipInside.rewrite.isDefinedAt(
            Rewrite.getExprForId(funCall, 5, NumberExpression.breadthFirst(funCall)))
      =>
        val e1 = Rewrite.applyRuleAtId(funCall, 5, moveTransposeInsideTiling)
        val e5 = Rewrite.applyRuleAtId(e1, 0, Rules.mapFissionWithZipInside)
        val e6 = Rewrite.applyRuleAtId(e5, 1, Rules.mapFission)
        val e7 = Rewrite.applyRuleAtId(e6, 2, Rules.mapTransposeSplit)
        val e8 = Rewrite.applyRuleAtId(e7, 1, Rules.mapSplitTranspose)

        e8
    })

  /**
   * Tile Map(Map(f)) o Transpose()
   */
  val tileTranspose: Rule = tileTranspose(?, ?)

  def tileTranspose(x: ArithExpr, y:ArithExpr): Rule =
    Rule("Map(Map(f)) o Transpose() => tiled", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, FunCall(Map(_), arg))), FunCall(Transpose(), _))
        if lambdaParam.head eq arg
      =>
        val e1 = Rewrite.applyRuleAtId(funCall, 0, tileMapMap(x, y))
        val e2 = Rewrite.applyRuleAtId(e1, 1, moveTransposeInsideTiling)
        e2
    })

  /**
   *  Map(Reduce) interchange, fissions as appropriate, automatically chooses the rule to apply
   */
  val moveReduceOutOneLevel = Rule("Map( ... Reduce(f) ...) => Map(...) o Reduce( Map(f)) o Map(...)", {
    case call@FunCall(Map(Lambda(_, innerCall: FunCall)), arg)
      if Utils.getIndexForPatternInCallChain(innerCall, reducePattern) != -1
    =>

      var rule = Rules.mapReduceInterchange

      val reduceArg = Utils.getExprForPatternInCallChain(innerCall, reducePattern).get.asInstanceOf[FunCall].args(1)
      val reduceId = Utils.getIndexForPatternInCallChain(innerCall, reducePattern)

      var offset = 1

      val pattern: PartialFunction[Expr, Unit] =
        { case FunCall(Reduce(_), _,
               FunCall(Join(),
               FunCall(Map(Lambda(_, FunCall(PartRed(_), _, _))), _))) => }

      val patternId = Utils.getIndexForPatternInCallChain(innerCall, pattern)

      val zipPattern: PartialFunction[Expr, Unit] = { case FunCall(Zip(_), _*) => }

      val finalArg = Utils.getFinalArg(innerCall)
      val finalArgId = Utils.getIndexForPatternInCallChain(innerCall,
        { case e if e eq finalArg => })

      var fissioned: Expr = call

      if (patternId != -1) {
        rule = Rules.mapReducePartialReduce
        offset = 3
      }

      if (zipPattern.isDefinedAt(arg) && reduceArg.isAbstract) {
        rule = Rules.mapReduceInterchangeWithZipOutside

        var numFissions = finalArgId - reduceId - 2

        while (numFissions > 0) {
          fissioned = Rules.mapFissionWithZipOutside.rewrite(fissioned)
          numFissions -= 1
        }

      } else if (finalArgId > reduceId + offset) {
          fissioned = mapFissionAtPosition(reduceId + offset - 1).rewrite(fissioned)
      }

      if (reduceId > 0)
        fissioned = mapFissionAtPosition(reduceId - 1).rewrite(fissioned)

      val mapReduce = Utils.getExprForPatternInCallChain(fissioned,
      { case e if rule.isDefinedAt(e) => }).get

      TypeChecker(fissioned)
      Expr.replace(fissioned, mapReduce, rule.rewrite(mapReduce))
  })



  /** 
   * Interchange the outer map with the first inner concrete map, fissions appropriately,
   * and automatically chooses the rule to apply
   */
  val mapMapInterchange = Rule("Map( ... Map(f) ...) => Map(...) o Map(Map(f)) o Map(...)", {
    case call@FunCall(Map(Lambda(_, innerCall: FunCall)), _)
      if Utils.getIndexForPatternInCallChain(innerCall, mapPattern) != -1
    =>
      val mapId = Utils.getIndexForPatternInCallChain(innerCall, mapPattern)

      var fissioned: Expr = call

      if (mapId > 0)
        fissioned = mapFissionAtPosition(mapId - 1).rewrite(fissioned)

      val mapCall = Utils.getExprForPatternInCallChain(fissioned, mapMapPattern).get

      val zipInside = Rules.mapMapTransposeZipInside.rewrite
      val zipOutside = Rules.mapMapTransposeZipOutside.rewrite
      val interchange = Rules.mapMapInterchange.rewrite
      val transpose = Rules.transposeBothSides.rewrite

      if (zipInside.isDefinedAt(mapCall))
        Expr.replace(fissioned, mapCall, zipInside(mapCall))
      else if (zipOutside.isDefinedAt(mapCall))
        Expr.replace(fissioned, mapCall, zipOutside(mapCall))
      else if (interchange.isDefinedAt(mapCall))
        Expr.replace(fissioned, mapCall, interchange(mapCall))
      else {

        val finalArg = Utils.getFinalArg(innerCall)
        val finalArgId = Utils.getIndexForPatternInCallChain(innerCall,
          { case e if e eq finalArg => })

        if (finalArgId > mapId + 1) {
          val id = if (mapId > 0) 1 else 0
          fissioned = Rewrite.applyRuleAtId(fissioned, id, mapFissionAtPosition(0))
        }

        val newMapCall = Utils.getExprForPatternInCallChain(fissioned, mapPattern).get

        TypeChecker.check(fissioned)
        Expr.replace(fissioned, newMapCall, transpose(newMapCall))
      }
  })

  /**
   * Apply interchange on the first concrete Map or Reduce inside a Map
   */
  val interchange =
    Rule("interchange first concrete Map or Reduce", {
      case call@FunCall(Map(Lambda(_, body)), _)
        if mapMapInterchange.isDefinedAt(call) || moveReduceOutOneLevel.isDefinedAt(call)
      =>
        val firstConcrete = Utils.getExprForPatternInCallChain(body, concretePattern).get

        if (reducePattern.isDefinedAt(firstConcrete))
          moveReduceOutOneLevel.rewrite(call)
        else
          mapMapInterchange.rewrite(call)
    })

  val apply1DRegisterBlocking: Rule = apply1DRegisterBlocking(?)

  /**
   * Apply register blocking in 1 dimension.
   * @param factor The amount of work to assign to the new dimension
   * @return
   */
  def apply1DRegisterBlocking(factor: ArithExpr): Rule = Rule("1D register blocking", {
    case call@FunCall(Map(Lambda(lambdaArg, innerCall)), _)
      if getCallForBlocking(innerCall, lambdaArg).isDefined
    =>
      // Split-join on outermost map
      val split = Rules.splitJoin(factor).rewrite.apply(call)

      // Interchange on the newly created dimension
      val interchanged = Rewrite.depthFirstApplyRuleAtId(split, 2, mapMapInterchange)

      // Interchange again on every map/reduce in the innermost dimension
      val map = getMapAtDepth(interchanged, 2)
      applyInterchangeOnAllComponents(interchanged, map)
  })

  private def applyInterchangeOnAllComponents(replaceIn: Expr, interchangeHere: Expr): Expr = {
    var nextToInterchange = interchangeHere
    var innerInterchanged = replaceIn

    val reduceRule = moveReduceOutOneLevel.rewrite
    val mapRule = mapMapInterchange.rewrite

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

  val apply2DRegisterBlocking: Rule = apply2DRegisterBlocking(?, ?)

  def apply2DRegisterBlocking(factorX: ArithExpr, factorY: ArithExpr): Rule =
    Rule("2D register blocking", {
      case call@FunCall(Map(Lambda(lambdaArg, innerCall)), arg)
        if getCallForBlocking(innerCall, lambdaArg).isDefined
        // TODO: is the guard good enough?
      =>
        val innerMap = getMapAtDepth(call, 1)

        // Reorder both sides of the inner map
        val realY = if (factorY != ?) factorY else Utils.validSplitVariable(innerMap.t)
        val stride = Type.getLength(innerMap.t) / realY
        val reorderReplaced = Rewrite.applyRuleAt(call, Rules.reorderBothSidesWithStride(stride), innerMap)

        val tiled = tileMapMap(factorX, realY, reorderReplaced)

        // Bring innermost components out by 2 levels
        val map0 = getMapAtDepth(tiled, 3)
        val firstInterchange = applyInterchangeOnAllComponents(tiled, map0)

        val map1 = getMapAtDepth(firstInterchange, 2)
        val secondInterchange = applyInterchangeOnAllComponents(firstInterchange, map1)

        secondInterchange
    })

  private def tileMapMap(factorX: ArithExpr, factorY: ArithExpr, expr: Expr): Expr = {

    // Fission if necessary
    val mapIndex = Utils.getIndexForPatternInCallChain(getMapBody(expr), mapPattern)
    val fissioned = if (mapIndex > 0) mapFissionAtPosition(mapIndex - 1, expr) else expr

    // split-join on the outer map
    val outerMap = getMapAtDepth(fissioned, 0)
    val outerMapSplitJoined = Rewrite.applyRuleAt(fissioned, Rules.splitJoin(factorX), outerMap)

    // interchange on the new dim
    val newDimension = getMapAtDepth(outerMapSplitJoined, 1)
    val interchanged = Rewrite.applyRuleAt(outerMapSplitJoined, mapMapInterchange, newDimension)

    // split-join on the inner map
    val innerMap = getMapAtDepth(interchanged, 1)
    val innerMapSplitJoined = Rewrite.applyRuleAt(interchanged, Rules.splitJoin(factorY), innerMap)

    // interchange on the new dim
    val secondNewDimension = getMapAtDepth(innerMapSplitJoined, 2)
    val e2 = Rewrite.applyRuleAt(innerMapSplitJoined, mapMapInterchange, secondNewDimension)

    e2
  }

  private def getCallForBlocking(innerCall: Expr, lambdaArg: Array[Param]): Option[Expr] = {
    val firstConcrete = Utils.getExprForPatternInCallChain(innerCall, concretePattern)
    firstConcrete match {
      case Some(FunCall(Map(_), _)) => firstConcrete
      case _ => None
    }
  }
}

