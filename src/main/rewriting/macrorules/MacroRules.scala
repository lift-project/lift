package rewriting.macrorules

import ir._
import ir.ast._
import lift.arithmetic.{?, ArithExpr}
import opencl.ir.pattern._
import rewriting.Rewrite
import rewriting.rules._
import rewriting.utils.Utils

object MacroRules {

  import rewriting.utils.Utils.{mapPattern, reducePattern, splitPattern, concretePattern}

  private val mapMapPattern: PartialFunction[Expr, Unit] =
  { case FunCall(Map(Lambda(_, FunCall(map:Map, _), _)), _) if map.f.body.isConcrete => }

  private def isUserFunCall(expr: Expr): Boolean = expr match {
     case FunCall(_: UserFun, _*)  | FunCall(_: VectorizeUserFun, _*) => true
     case _ => false
  }

  private def isUserFunCallOrWrappedWithMemorySpace(expr: Expr): Boolean = expr match {
    case FunCall(toGlobal(Lambda(_, body, _)), _*) => isUserFunCallOrWrappedWithMemorySpace(body)
    case FunCall(toLocal(Lambda(_, body, _)), _*) => isUserFunCallOrWrappedWithMemorySpace(body)
    case FunCall(toPrivate(Lambda(_, body, _)), _*) => isUserFunCallOrWrappedWithMemorySpace(body)
    case _ => isUserFunCall(expr)
  }

  val userFunCompositionToPrivate = Rule("userFunCompositionToPrivate", {
    case call@FunCall(uf, args@_*)
      if isUserFunCallOrWrappedWithMemorySpace(call) &&
        args.count(expr => isUserFunCall(expr) &&
          OpenCLRules.privateMemory.isDefinedAt(expr)) > 0
    =>

      val newArgs =
        args.map(expr =>
          if (isUserFunCall(expr) && OpenCLRules.privateMemory.isDefinedAt(expr))
            OpenCLRules.privateMemory.rewrite(expr)
          else
            expr
        )

      uf(newArgs:_*)
  })

  val mapComposedWithReduceAsSequential = Rule("mapComposedWithReduceAsSequential", {
    case call@FunCall(_: AbstractMap, FunCall(_: AbstractPartRed, _*))
      if OpenCLRules.mapSeq.isDefinedAt(call)
    =>
      OpenCLRules.mapSeq.rewrite(call)
  })

  /**
   * Apply map fission. Helper rule for other macro rules.
   */
  val mapFissionAtPosition: Int => Rule = position =>
    Rule("Map(... o ... o ...) => Map(... o ...) o Map(...)", {
      case funCall @ FunCall(Map(Lambda(_, body:FunCall, _)), _)
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

      val fissionRule =
        if (FissionRules.mapFission.isDefinedAt(nextFission)) FissionRules.mapFission
        else if (FissionRules.mapFissionCreateZip.isDefinedAt(nextFission)) FissionRules.mapFissionCreateZip
        else throw new NotImplementedError()

      val replacement = fissionRule.rewrite(nextFission)
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

      fused = FusionRules.mapFusion.rewrite(fused)

      currentPos -= 1
    }

    fused
  }

  val reshapeMapMap =
    Rule("Map(Map(f)) => Split() o Join() o Map(Map(f)) o Split() o Join()", {
      case call: FunCall if Rules.joinSplit.isDefinedAt(call) =>

        val joined = Rewrite.applyRuleAt(call, Rules.joinSplit, call)
        val map = Utils.getExprForPatternInCallChain(joined, mapPattern).get
        val split = Rewrite.applyRuleAt(joined, Rules.splitJoin, map)

        split
    })

  val vectorizeReduce: Rule = vectorizeReduce(?)

  def vectorizeReduce(vectorWidth: ArithExpr): Rule =
    Rule("vectorizeReduce", {
      case fc@FunCall(Reduce(_), _, _) =>

        val part = Rewrite.applyRuleAt(fc, ReduceRules.partialReduce, fc)
        val partRed = Utils.getExprForPatternInCallChain(part,
          { case FunCall(PartRed(_), _*) => }).get
        val res =
          Rewrite.applyRuleAt(part, OpenCLRules.partialReduceVectorize(vectorWidth), partRed)

        res
    })

  val partialReduceWithReorder: Rule = partialReduceWithReorder(?)

  def partialReduceWithReorder(strideGiven: ArithExpr): Rule =
    Rule("partialReduceWithReorder", {
      case funCall@FunCall(Reduce(_), _, arg) =>

        val stride = Utils.splitVariable(strideGiven, arg.t)
        val len = Type.getLength(arg.t)
        val splitFactor = len /^ stride

        // Rewrite Reduce to Reduce o Join() o Map(Reduce) o Split o ReorderStride
        val part = Rewrite.applyRuleAt(funCall, ReduceRules.partialReduce, funCall)
        val partRed = Utils.getExprForPatternInCallChain(part, { case FunCall(PartRed(_), _*) => }).get

        val reorder = Rewrite.applyRuleAt(part, ReduceRules.partialReduceReorder(stride), partRed)
        val partRed2 = Utils.getExprForPatternInCallChain(reorder, { case FunCall(PartRed(_), _*) => }).get

        val splitJoin = Rewrite.applyRuleAt(reorder, ReduceRules.partialReduceSplitJoin(splitFactor), partRed2)

        // Find maps before the reduce
        val maps = Utils.visitFunCallChainWithState((true, List[Expr]()))(arg, (e, s) => {
          if (s._1)
            e match {
              case call@FunCall(Map(_), _) => (true, s._2 :+ call)
              case _ => (false, s._2)
            }
          else
            s
        })._2

        // Rewrite maps before the reduce to Scatter() o Join() o Map(Map) o Split() o Gather()
        val rewrittenMaps = maps
          .map(x => Rewrite.applyRuleAt(x, Rules.reorderBothSidesWithStride(stride), x))
          .map({ case x@FunCall(_, call@FunCall(Map(_), _)) =>
            Rewrite.applyRuleAt(x, Rules.splitJoin(splitFactor), call) })

        // Replace the maps in splitJoin with the rewritten ones
        (maps, rewrittenMaps).zipped.foldLeft(splitJoin)((e, s) => Expr.replace(e, s._1, s._2))
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

      if (SimplificationRules.splitJoinId.isDefinedAt(call)) {
        SimplificationRules.splitJoinId.rewrite(call)
      } else {
        val newCall = Rewrite.applyRuleAt(call, Rules.splitJoin(n), arg)

        val splitJoinEliminated = Rewrite.applyRuleAt(newCall, SimplificationRules.splitJoinId, newCall)

        val newSplit = Utils.getExprForPatternInCallChain(splitJoinEliminated, splitPattern).get

        Rewrite.applyRuleAt(splitJoinEliminated, splitJoinId, newSplit)
      }
  })

  /**
   * Fuse Reduce o Map, automatically applying applying the
   * ReduceSeq and MapSeq rules as necessary.
   */
  val reduceMapFusion = Rule("Reduce o Map => ReduceSeq(fused)", {
    case funCall @ FunCall(Reduce(_), _, mapCall@FunCall(Map(_), _))
      if OpenCLRules.mapSeq.isDefinedAt(mapCall)
    =>
      val e0 = Rewrite.applyRuleAtId(funCall, 1, OpenCLRules.mapSeq)
      val e1 = Rewrite.applyRuleAtId(e0, 0, OpenCLRules.reduceSeq)
      val e2 = Rewrite.applyRuleAtId(e1, 0, FusionRules.reduceSeqMapSeqFusion)
      e2

    case funCall @ FunCall(ReduceSeq(_), _, mapCall@FunCall(Map(_), _))
      if OpenCLRules.mapSeq.isDefinedAt(mapCall)
    =>
      val e0 = Rewrite.applyRuleAtId(funCall, 1, OpenCLRules.mapSeq)
      val e1 = Rewrite.applyRuleAtId(e0, 0, FusionRules.reduceSeqMapSeqFusion)
      e1

    case funCall @ FunCall(Reduce(_), _, FunCall(MapSeq(_), _))
    =>
      val e0 = Rewrite.applyRuleAtId(funCall, 0, OpenCLRules.reduceSeq)
      val e1 = Rewrite.applyRuleAtId(e0, 0, FusionRules.reduceSeqMapSeqFusion)
      e1

    case funCall @ FunCall(ReduceSeq(_), _, FunCall(MapSeq(_), _))
    =>
      Rewrite.applyRuleAtId(funCall, 0, FusionRules.reduceSeqMapSeqFusion)
  })

  /**
   *  Map(Reduce) interchange, fissions as appropriate, automatically chooses the rule to apply
   */
  val moveReduceOutOneLevel = Rule("Map( ... Reduce(f) ...) => Map(...) o Reduce( Map(f)) o Map(...)", {
    case call@FunCall(Map(Lambda(_, innerCall: FunCall, _)), arg)
      if Utils.getIndexForPatternInCallChain(innerCall, reducePattern) != -1
    =>

      var rule = InterchangeRules.mapReduceInterchange

      val reduceArg = Utils.getExprForPatternInCallChain(innerCall, reducePattern).get.asInstanceOf[FunCall].args(1)
      val reduceId = Utils.getIndexForPatternInCallChain(innerCall, reducePattern)

      var offset = 1

      val pattern: PartialFunction[Expr, Unit] =
        { case FunCall(Reduce(_), _,
               FunCall(Join(),
               FunCall(Map(Lambda(_, FunCall(PartRed(_), _, _), _)), _))) => }

      val patternId = Utils.getIndexForPatternInCallChain(innerCall, pattern)

      val zipPattern: PartialFunction[Expr, Unit] = { case FunCall(Zip(_), _*) => }

      val finalArg = Utils.getFinalArg(innerCall)
      val finalArgId = Utils.getIndexForPatternInCallChain(innerCall,
        { case e if e eq finalArg => })

      var fissioned: Expr = call

      if (patternId != -1) {

        val reduceOption = Utils.getExprForPatternInCallChain(innerCall,
          { case FunCall(Reduce(_), _, _) => })

        if (reduceOption.isDefined)
          fissioned = Rewrite.applyRuleAt(fissioned, OpenCLRules.reduceSeq, reduceOption.get)

        rule = InterchangeRules.mapReducePartialReduce
        offset = 3
      }

      if (zipPattern.isDefinedAt(arg) && reduceArg.isAbstract) {
        rule = InterchangeRules.mapReduceInterchangeWithZipOutside

        var numFissions = finalArgId - reduceId - 2

        while (numFissions > 0) {
          fissioned = FissionRules.mapFissionWithZipOutside.rewrite(fissioned)
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
    case call@FunCall(Map(Lambda(_, innerCall: FunCall, _)), _)
      if Utils.getIndexForPatternInCallChain(innerCall, mapPattern) != -1
    =>

      val mapId = Utils.getIndexForPatternInCallChain(innerCall, mapPattern)

      var fissioned: Expr = call

      if (mapId > 0)
        fissioned = mapFissionAtPosition(mapId - 1).rewrite(fissioned)

      // TODO: Makes mapMapTransposeZipOutside applicable for gemv & nbody
      // Restricted to only apply in that case, breaks stuff if more general.
      // Doesn't seem like a great solution.
      if (FissionRules.mapFissionCreateZip.isDefinedAt(fissioned))
        fissioned = Rewrite.applyRuleAt(fissioned, FissionRules.mapFissionCreateZip, fissioned)

      val mapCall = Utils.getExprForPatternInCallChain(fissioned, mapMapPattern).get

      val zipInside = InterchangeRules.mapMapTransposeZipInside.rewrite
      val zipOutside = InterchangeRules.mapMapTransposeZipOutside.rewrite
      val interchange = InterchangeRules.mapMapInterchange.rewrite
      val transpose = InterchangeRules.transposeBothSides.rewrite

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
        if (transpose.isDefinedAt(newMapCall))
          Expr.replace(fissioned, newMapCall, transpose(newMapCall))
        else
          throw new NotImplementedError()
      }
  })

  /**
   * Apply interchange on the first concrete Map or Reduce inside a Map
   */
  val interchange =
    Rule("interchange first concrete Map or Reduce", {
      case call@FunCall(Map(Lambda(_, body, _)), _)
        if mapMapInterchange.isDefinedAt(call) || moveReduceOutOneLevel.isDefinedAt(call)
      =>
        val firstConcrete = Utils.getExprForPatternInCallChain(body, concretePattern).get

        if (reducePattern.isDefinedAt(firstConcrete))
          moveReduceOutOneLevel.rewrite(call)
        else
          mapMapInterchange.rewrite(call)
   })

}

