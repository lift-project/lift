package rewriting.macrorules

import ir._
import ir.ast._
import rewriting.Rewrite
import rewriting.rules.{FissionRules, Rule, Rules, SimplificationRules}
import rewriting.utils.Utils

object EnablingRules {

  // transpose both sides + id
  private val transposeMapMapNoFission =
    Rule("Transpose() o Map(Map(_)) => " +
      "Map(Map(_)) o Transpose()", {
      case FunCall(t,
            FunCall(Map(Lambda(p1,
              FunCall(Map(Lambda(p2,
                FunCall(f, a2),_)
              ), a1),_)
            ), arg))
        if (p1.head eq a1)
          && (p2.head eq a2)
          && Utils.isTranspose(t)
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
                  FunCall(_, a2)
                ,_)), a1)
              ,_)), _))
        if Utils.getIndexForPatternInCallChain(a1, { case e if e eq p1.head => } ) != -1
        && Utils.getIndexForPatternInCallChain(a2, { case e if e eq p2.head => } ) != -1
        && Utils.isTranspose(t)
      =>
        var fissioned: Expr = funCall

        if (!(a2 eq p2.head))
          fissioned = Expr.replace(fissioned, innerMapCall, FissionRules.mapFission.rewrite(innerMapCall))

        val map = Utils.getExprForPatternInCallChain(fissioned, { case FunCall(Map(_), _) => }).get

        if (!(p1.head eq a1) || !(a2 eq p2.head))
          fissioned = Expr.replace(fissioned, map, FissionRules.mapFission.rewrite(map))

        transposeMapMapNoFission.rewrite(fissioned)
    })

  /**
   * Apply the Map(Transpose) o Transpose o Map(Transpose) rule, fission appropriately.
   * Used as an enabling rule when simplifying.
   */
  val mapTransposeTransposeMapTranspose =
    Rule("Map(... o Transpose()) o Transpose() o Map(Transpose() o ...)) => " +
         "Map(...) o Transpose() o Map(Transpose()) o Transpose() o Map(...)", {
      case funCall@FunCall(Map(Lambda(param1, firstBody,_)),
      FunCall(t2,
      secondMap@FunCall(Map(Lambda(param2, FunCall(t3, a2),_)), _)))
        if Utils.getIndexForPatternInCallChain(a2, { case e if e eq param2.head => }) != -1
          && Utils.getIndexForPatternInCallChain(firstBody,
              { case FunCall(t, a) if Utils.isTranspose(t) && (a eq param1.head) => }) != -1
          && Utils.isTranspose(t2)
          && Utils.isTranspose(t3)
      =>

        var fissioned: Expr = funCall

        val index = Utils.getIndexForPatternInCallChain(firstBody,
          { case FunCall(t, arg) if Utils.isTranspose(t) && (arg eq param1.head) => })

        if (index > 0)
          fissioned = MacroRules.mapFissionAtPosition(index - 1).rewrite(funCall)

        if (!(a2 eq param2.head))
          fissioned = Expr.replace(fissioned, secondMap, FissionRules.mapFission.rewrite(secondMap))

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
      case funCall@FunCall(t, FunCall(Map(Lambda(p, FunCall(Split(_), a),_)), _))
        if Utils.isTranspose(t)
      =>

        var fissioned: Expr = funCall

        if (!(p.head eq a))
          fissioned = Rewrite.applyRuleAtId(fissioned, 1, FissionRules.mapFission)

        Rules.transposeMapSplit.rewrite(fissioned)
    })

  /**
   * Apply the Map(Split) o Transpose rule, fission appropriately.
   * Used as an enabling rule when simplifying.
   */
  val mapSplitTranspose =
    Rule("mapSplitTranspose", {
      case funCall@FunCall(Map(Lambda(param, body,_)), FunCall(t, _))
        if Utils.getIndexForPatternInCallChain(body,
            { case FunCall(Split(_), arg) if arg eq param.head => }) != -1
          && Utils.isTranspose(t)
      =>
        val index = Utils.getIndexForPatternInCallChain(body,
                { case FunCall(Split(_), arg) if arg eq param.head => })

        var fissioned: Expr = funCall

        if (index > 0)
          fissioned = MacroRules.mapFissionAtPosition(index-1).rewrite(funCall)

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
      case call@FunCall(Map(_), FunCall(Join(), arg)) =>

        val splitFactor = arg.t match {
          case ArrayType(ArrayTypeWSWC(_,m,_)) => m
          case ArrayType(_) | NoType | TupleType(_) | UndefType =>
            throw new TypeException(arg.t, "a valid array type", arg)
        }

        val splitJoined = Rewrite.applyRuleAt(call, Rules.splitJoin(splitFactor), call)

        val newSplit = Utils.getExprForPatternInCallChain(splitJoined, Utils.splitPattern).get

        val eliminated = Rewrite.applyRuleAt(splitJoined, SimplificationRules.splitJoinId, newSplit)

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
        val eliminated = Rewrite.applyRuleAt(splitJoined, SimplificationRules.splitJoinId, splitJoined)

        eliminated
    })
}
