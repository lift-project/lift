package exploration

import ir.TypeChecker
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

  val moveReduceOutOneLevel = Rule("Map( ... Reduce(f) ...) => Map(...) o Reduce( Map(f)) o Map(...)", {
    case call@FunCall(Map(Lambda(lambdaParam, innerCall: FunCall)), arg)
      if (Utils.getFinalArg(innerCall) eq lambdaParam.head)
        && innerCall.contains({ case FunCall(_: AbstractPartRed, _, _) => })
    =>

      val reduceId = Utils.getIndexForPatternInCallChain(innerCall,
        { case FunCall(_: AbstractPartRed, _, _) => })
      val finalArg = Utils.getFinalArg(innerCall)
      val finalArgId = Utils.getIndexForPatternInCallChain(innerCall,
        { case e if e eq finalArg => })

      var fissioned: Expr = call

      if (finalArgId > reduceId + 1) {
        fissioned = mapFissionAtPosition(reduceId).rewrite(fissioned)
      }

      if (reduceId > 0) {
        fissioned = mapFissionAtPosition(reduceId - 1).rewrite(fissioned)
      }

      val mapReduce = Utils.getExprForPatternInCallChain(fissioned,
      { case e if Rules.mapReduceInterchange.isDefinedAt(e) => }).get

      TypeChecker(fissioned)
      // TODO: try to apply mapReducePartialReduce first
      Expr.replace(fissioned, mapReduce, Rules.mapReduceInterchange.rewrite(mapReduce))
  })

}
