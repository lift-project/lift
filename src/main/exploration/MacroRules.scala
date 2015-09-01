package exploration

import apart.arithmetic.{?, ArithExpr}
import ir._
import ir.ast._

object MacroRules {
  // transpose both sides + id
  val transposeMapMapTranspose =
    Rule("Transpose() o Map(Map(Transpose())) => " +
      "Map(Map(Transpose())) o Transpose()", {
      case FunCall(Transpose(),
            FunCall(Map(Lambda(p1,
              FunCall(Map(Lambda(p2,
                FunCall(Transpose(), a2))
              ), a1))
            ), arg))
        if (p1.head eq a1) && (p2.head eq a2)
      =>
        Map(Map(Transpose())) o Transpose() $ arg
    })

  val transposeMapMapTransposeFission =
    Rule("", {
      case funCall@FunCall(Transpose(),
              FunCall(Map(Lambda(p1,
                innerMapCall@FunCall(Map(Lambda(p2,
                  FunCall(Transpose(), a2)
                )), a1)
              )), _))
      =>
        var fissioned: Expr = funCall

        if (!(a2 eq p2.head))
          fissioned = Expr.replace(fissioned, innerMapCall, Rules.mapFission.rewrite(innerMapCall))

        val map = Utils.getExprForPatternInCallChain(fissioned, { case FunCall(Map(_), _) => }).get

        if (!(p1.head eq a1) || !(a2 eq p2.head))
          fissioned = Expr.replace(fissioned, map, Rules.mapFission.rewrite(map))

        transposeMapMapTranspose.rewrite(fissioned)
    })

  val transposeTransposeId =
    Rule("", {
      case funCall@FunCall(Map(Lambda(p, body)), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _))
        if Utils.getIndexForPatternInCallChain(body,
          { case FunCall(Transpose(), arg) if arg eq p.head => }) != -1
      =>
        val fused = Rules.mapFusion.rewrite(funCall)
        val fusedBody = getMapBody(fused)

        val applyHere = Utils.getExprForPatternInCallChain(fusedBody,
          { case e if Rules.transposeTransposeId.isDefinedAt(e) =>}).get

        Expr.replace(fused, applyHere, Rules.transposeTransposeId.rewrite(applyHere))
    })

  val transposeMapSplit =
    Rule("", {
      case funCall@FunCall(Transpose(), FunCall(Map(Lambda(p, FunCall(Split(_), a))), _))
        =>

        var fissioned: Expr = funCall

        if (!(p.head eq a))
          fissioned = Rewrite.applyRuleAtId(fissioned, 1, Rules.mapFission)

        Rules.transposeMapSplit.rewrite(fissioned)
    })

  val mapSplitTranspose =
    Rule("", {
      case funCall@FunCall(Map(Lambda(param, body)), FunCall(Transpose(), _))
        if Utils.getIndexForPatternInCallChain(body,
          { case FunCall(Split(_), arg) if arg eq param.head => }) != -1
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

  // TODO: guard
  val mapFissionAtPosition: Int => Rule = position => Rule("", {
    case funCall @ FunCall(Map(Lambda(_, FunCall(_, _*))), _) => mapFissionAtPosition(position, funCall)
  })

  def mapFissionAtPosition(position: Int, expr: Expr): Expr = {
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

  val tileMapMap: (Int, Int) => Rule = (x, y) =>
    Rule("Tile a computation in the form Map(fun(y => Map(f) $ y )) $ x", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, FunCall(Map(_), arg))), _)
        if lambdaParam.head eq arg
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 0, Rules.splitJoin(x))
        val e1 = Rewrite.depthFirstApplyRuleAtId(e0, 2, Rules.transposeBothSides)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 3, Rules.splitJoin(y))
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 5, Rules.transposeBothSides)
        e3
    })

  val tileOutput: Int => Rule = x =>
    Rule("Tile the output of a computation in the form " +
      "Map(fun(y => Map(f) $ z )) $ x", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, FunCall(Map(_), arg))), _)
        if !(lambdaParam.head eq arg)
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 0, Rules.splitJoin(x))
        val e1 = Rewrite.depthFirstApplyRuleAtId(e0, 2, Rules.mapMapInterchange)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 3, Rules.splitJoin(x))
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 5, Rules.mapMapInterchange)
        e3
    })

  val finishTilingInput: Int => Rule = x =>
    Rule("Map(x => Map(y => Map() $ Get(x, ...) Get$(x, ...) $ Zip(...)", {
      case funCall @
        FunCall(Map(Lambda(p,
        FunCall(Map(Lambda(_,
        FunCall(Map(_), FunCall(Get(_), a2)))),
        FunCall(Get(_), a1)))),
        FunCall(Zip(_), _*))
        if (p.head eq a1) && (p.head eq a2)
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 0, Rules.splitJoin(x))
        val e1 = Rewrite.applyRuleAtId(e0, 1, Rules.splitZip)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 2, Rules.mapMapTransposeZipOutside)
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 4, Rules.mapMapTransposeZipOutside)
        e3
    })

  val tileInputAndOutput: (Int, Int) => Rule = (x, y) =>
    Rule("Tile the input and output of a computation in the form " +
      "Map(fun(x => Map(fun(y => Reduce(g) o Map(f) $ Zip(x, y) )) $ ... )) $ ...", {
      case funCall @ FunCall(Map(Lambda(lambdaParam1,
      FunCall(Map(Lambda(lambdaParam2,
      FunCall(Reduce(_), _, FunCall(Map(Lambda(_, FunCall(_: UserFun, _*))),
      FunCall(Zip(_), zipArgs@_*)))
      )), arg)
      )), _)
        if !(lambdaParam1.head eq arg)
          && zipArgs.contains(lambdaParam1.head)
          && zipArgs.contains(lambdaParam2.head)
      =>
        val e1 = Rewrite.applyRuleAtId(funCall, 0, tileOutput(x))

        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 7, Rules.mapFission)
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 14, Rules.mapMapTransposeZipInside)
        val e4 = Rewrite.depthFirstApplyRuleAtId(e3, 6, mapFissionAtPosition(1))
        val e5 = Rewrite.depthFirstApplyRuleAtId(e4, 16, Rules.mapMapTransposeZipInside)

        val e6 = Rewrite.depthFirstApplyRuleAtId(e5, 17, finishTilingInput(y))

        e6
    })

  val moveTransposeInsideTiling =
    Rule("Map(Split(n) o Transpose()) o Split(m) o Transpose() => " +
      "Transpose() o Map(Transpose()) o Split(n) o Map(Split(m))", {
      case funCall @
        FunCall(Map(Lambda(_, FunCall(Split(_), FunCall(Transpose(), _)))),
        FunCall(Split(_), FunCall(Transpose(), _)))
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 4, Rules.splitTranspose)
        val e1 = Rewrite.depthFirstApplyRuleAtId(e0, 0, Rules.mapFusion)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 2, Rules.transposeTransposeId)
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 0, Rules.mapSplitTranspose)
        e3
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
        val e1 = Rewrite.applyRuleAtId(funCall, 5, Rules.mapFissionWithZipInside)
        val e4 = Rewrite.applyRuleAtId(e1, 6, moveTransposeInsideTiling)
        val e5 = Rewrite.applyRuleAtId(e4, 0, Rules.mapFissionWithZipInside)
        val e6 = Rewrite.applyRuleAtId(e5, 1, Rules.mapFission)
        val e7 = Rewrite.applyRuleAtId(e6, 2, Rules.mapTransposeSplit)
        val e8 = Rewrite.applyRuleAtId(e7, 1, Rules.mapSplitTranspose)

        e8
    })

  val tileTranspose: (Int, Int) => Rule = (x, y) =>
    Rule("Map(Map(f)) o Transpose() => tiled", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, FunCall(Map(_), arg))), FunCall(Transpose(), _))
        if lambdaParam.head eq arg
      =>
        val e1 = Rewrite.applyRuleAtId(funCall, 0, tileMapMap(x, y))
        val e2 = Rewrite.applyRuleAtId(e1, 1, mapFissionAtPosition(2))
        val e3 = Rewrite.applyRuleAtId(e2, 2, moveTransposeInsideTiling)
        e3
    })

  val transposeBothSidesWithSplit =
    Rule("Transpose() o Map( ... o ... o Split(n) ) o Transpose() => " +
      "Map( ... ) o Map(Transpose) o Split(n)", {
      case funCall @
        FunCall(Transpose(),
        FunCall(Map(Lambda(p, FunCall(_, FunCall(_, FunCall(Split(_), a))))),
        FunCall(Transpose(), _)))
        if p.head eq a
      =>
        val e0 = Rewrite.applyRuleAtId(funCall, 1, mapFissionAtPosition(1))
        val e1 = Rewrite.applyRuleAtId(e0, 2, Rules.mapSplitTranspose)
        val e2 = Rewrite.applyRuleAtId(e1, 1, Rules.transposeBothSides)
        val e3 = Rewrite.applyRuleAtId(e2, 0, Rules.transposeTransposeId)
        val e4 = Rewrite.applyRuleAtId(e3, 1, Rules.transposeTransposeId)
        e4
    })

  val reduceMapFusion = Rule("Reduce o Map => ReduceSeq(fused)", {
    case funCall @ FunCall(Reduce(_), _, mapCall@FunCall(Map(_), _))
      if Rules.mapSeq.isDefinedAt(mapCall)
    =>
      val e0 = Rewrite.applyRuleAtId(funCall, 1, Rules.mapSeq)
      val e1 = Rewrite.applyRuleAtId(e0, 0, Rules.reduceSeq)
      val e2 = Rewrite.applyRuleAtId(e1, 1, Rules.reduceSeqMapSeqFusion)
      e2
  })

  // Eliminate a matching split-join pair. If there are maps between them, apply
  // the split-join and splitJoinId rules, to move the pair closer to each other
  val splitJoinId: Rule = Rule("Split(n) ... Join() => Map(...)", {
    case call@FunCall(Split(n), arg)
      if Utils.visitFunCallChainWithState((false, true))(arg, (expr, state) => {
        // State = (applicable, stillPossible)
        expr match {
          case FunCall(Map(_), _) => state
          case FunCall(Join(), a)
            if (a.t match {
              case ArrayType(ArrayType(_, m), _) => n == m
              case _ => false
              })
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
        val splitJoined = Rules.splitJoin(n).rewrite.apply(arg)
        val newCall = Expr.replace(call, arg, splitJoined)

        TypeChecker.check(newCall)

        val splitJoinEliminated = Rules.splitJoinId.rewrite.apply(newCall)

        val newSplit = Utils.getExprForPatternInCallChain(splitJoinEliminated,
          {case FunCall(Split(_), _) => }).get

        Expr.replace(splitJoinEliminated, newSplit, splitJoinId.rewrite.apply(newSplit))
      }
  })

  private val reducePattern: PartialFunction[Expr, Unit] =
    { case FunCall(_: AbstractPartRed, _, _) => }

  // Map(Reduce) interchange, fissions as appropriate, automatically chooses the rule to apply
  val moveReduceOutOneLevel = Rule("Map( ... Reduce(f) ...) => Map(...) o Reduce( Map(f)) o Map(...)", {
    case call@FunCall(Map(Lambda(_, innerCall: FunCall)), arg)
      if innerCall.contains(reducePattern)
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

  private val mapPattern: PartialFunction[Expr, Unit] =
    { case FunCall(map: Map, _) if map.f.body.isConcrete => }

  // Interchange the outer map with the first inner concrete map, fissions appropriately,
  // automatically chooses the rule to apply
  val mapMapInterchange = Rule("Map( ... Map(f) ...) => Map(...) o Map(Map(f)) o Map(...)", {
    case call@FunCall(Map(Lambda(_, innerCall: FunCall)), _)
      if innerCall.contains(mapPattern)
    =>
      val mapId = Utils.getIndexForPatternInCallChain(innerCall, mapPattern)

      var fissioned: Expr = call

      if (mapId > 0)
        fissioned = mapFissionAtPosition(mapId - 1).rewrite(fissioned)

      val mapCall = Utils.getExprForPatternInCallChain(fissioned, mapPattern).get

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

  val apply1DRegisterBlocking: Rule = apply1DRegisterBlocking(?)

  def apply1DRegisterBlocking(factor: ArithExpr): Rule = Rule("1D register blocking", {
    case call@FunCall(Map(Lambda(lambdaArg, innerCall)), _)
      if getCallForBlocking(innerCall, lambdaArg).isDefined
    =>

      // Split-join on outermost map
      val split = Rules.splitJoin(factor).rewrite.apply(call)

      // Interchange on the newly created dimension
      val interchanged = Rewrite.depthFirstApplyRuleAtId(split, 2, mapMapInterchange)

      // Interchange again on every map/reduce in the innermost dimension

      val map1 = Utils.getExprForPatternInCallChain(interchanged, mapPattern).get
      val map2 = Utils.getExprForPatternInCallChain(getMapBody(map1), mapPattern).get
      val map3 = Utils.getExprForPatternInCallChain(getMapBody(map2), mapPattern).get

      applyInterchangeOnAllComponents(interchanged, map3)
  })

  def applyInterchangeOnAllComponents(replaceIn: Expr, interchangeHere: Expr): Expr = {
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

  def apply2DRegisterBlocking(factorX: ArithExpr, factorY:ArithExpr): Rule = Rule("", {
    case call@FunCall(Map(Lambda(lambdaArg, innerCall)), arg)
      if getCallForBlocking(innerCall, lambdaArg).isDefined
        // TODO: is the guard good enough?
    =>
      // ReorderBothSides of inner map

      val innerMap = Utils.getExprForPatternInCallChain(getMapBody(call), mapPattern).get

      var stride: ArithExpr = ?

      //TODO: fix factorY to be the actual splitting variable of that dimension
      if (factorY != ?)
        stride = Type.getLength(innerMap.t) / factorY

      val innerMapReordered = Rules.reorderBothSidesWithStride(stride).rewrite(innerMap)

      val reorderReplaced = Expr.replace(call, innerMap, innerMapReordered)

      // TODO: common sequences in here, tileOutput and tileMapMap

      // fission out the reorder

      val innerMapReorderedReplaced = getMapBody(reorderReplaced)

      val fissionPosition = Utils.getIndexForPatternInCallChain(innerMapReorderedReplaced,
        { case FunCall(Scatter(_), _) => })

      val fissioned = mapFissionAtPosition(fissionPosition, reorderReplaced)

      // split-join on the outer map

      TypeChecker(fissioned)

      val outerMap = Utils.getExprForPatternInCallChain(fissioned, mapPattern).get

      val outerSplitJoined = Rules.splitJoin(factorX).rewrite(outerMap)

      val replaced = Expr.replace(fissioned, outerMap, outerSplitJoined)

      TypeChecker(replaced)

      // interchange on the new dim

      val splitJoinedOuterMap = Utils.getExprForPatternInCallChain(replaced, mapPattern).get
      val newDimensionMap = Utils.getExprForPatternInCallChain(getMapBody(splitJoinedOuterMap), mapPattern).get

      val interchanged = mapMapInterchange.rewrite(newDimensionMap)

      val interchangeReplaced = Expr.replace(replaced, newDimensionMap, interchanged)

      // split-join on the inner map

      TypeChecker(interchangeReplaced)

      val innerMapAfterInterchange = Utils.getExprForPatternInCallChain(interchanged, mapPattern).get

      val innerMapSplitJoined = Rules.splitJoin(factorY).rewrite(innerMapAfterInterchange)

      TypeChecker(innerMapSplitJoined)

      // interchange on the new dim

      val splitJoinedInnerMap = Utils.getExprForPatternInCallChain(innerMapSplitJoined, mapPattern).get
      val secondNewDimMap = Utils.getExprForPatternInCallChain(getMapBody(splitJoinedInnerMap), mapPattern).get

      val secondInterchanged = mapMapInterchange.rewrite(secondNewDimMap)

      val e1 = Expr.replace(innerMapSplitJoined, secondNewDimMap, secondInterchanged)
      val e2 = Expr.replace(interchangeReplaced, innerMapAfterInterchange, e1)

      TypeChecker(e2)

      // interchange inside stuff twice.

      val map1 = Utils.getExprForPatternInCallChain(secondInterchanged, mapPattern).get
      val map2 = Utils.getExprForPatternInCallChain(getMapBody(map1), mapPattern).get

      val oneInterchange = applyInterchangeOnAllComponents(e2, map2)

      TypeChecker(oneInterchange)

      val map3 = Utils.getExprForPatternInCallChain(oneInterchange, mapPattern).get
      val map4 = Utils.getExprForPatternInCallChain(getMapBody(map3), mapPattern).get
      val map5 = Utils.getExprForPatternInCallChain(getMapBody(map4), mapPattern).get

      val secondInterchange = applyInterchangeOnAllComponents(oneInterchange, map5)

      secondInterchange
  })

  private def getMapBody(expr: Expr) = {
    expr match {
      case FunCall(Map(Lambda(_, body)), _) => body
    }
  }

  private def getCallForBlocking(innerCall: Expr, lambdaArg: Array[Param]): Option[Expr] = {
    Utils.getExprForPatternInCallChain(innerCall, {
      case FunCall(Map(f), arg)
        if f.body.isConcrete
       =>
    })
  }
}
