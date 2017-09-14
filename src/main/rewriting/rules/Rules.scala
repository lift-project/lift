package rewriting.rules

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir.pattern._
import rewriting.utils.Utils

object Rules {

  /* Rules required for 2D stencil rewrite *///TODO find appropriate names

  /* Slide-promotion *///TODO not really because of map(join)... find better name
  val slidePromotion = Rule("Slide(u,v) o Map(Join()) => Map(Map(Join())) o Slide(u,v)",{
    case FunCall(Slide(u,v), FunCall(Map(Lambda(_, FunCall(Join(), _))), arg)) =>
      Map(Map(Join())) o Slide(u,v) $ arg
  })

  val slideSwap = Rule("Slide(u,v) o Map(Map(Slide(n,s))) => Map(Map(Map(Slide(n,s)))) o Slide(u,v)",{
    case FunCall(Slide(u,v), FunCall(Map(Lambda(
      _, FunCall(Map(Lambda(
        _, FunCall(Slide(n,s), _))), _))), arg)) =>
     Map(Map(Map(Slide(n,s)))) o Slide(u,v) $ arg
  })

  val joinSwap = Rule("Join() o Map(Map(f)) => Map(f) o Join()", {
    case FunCall(Join(), FunCall(Map(Lambda(_, FunCall(map@Map(_), _))), arg)) =>
      map o Join() $ arg
  })

  // todo reduce on layer of maps and use map fission before applying this rule
  val transposeSwap = Rule("Map(Map(Map(Transpose()))) o Map(Transpose()) => Map(Transpose()) o Map(Map(Map(Transpose())))", {
    case FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(),
      _))), _))), _))), FunCall(Map(Lambda(
    _, FunCall(
    Transpose(), _))), arg)) =>
      Map(Transpose()) o Map(Map(Map(Transpose()))) $ arg
  })

  // todo reduce on layer of maps and use map fission before applying this rule
  val slideTransposeSwap = Rule("Map(Map(Map(Slide(u,v)))) o Map(Transpose()) => Map(Transpose) o Map(Map(Map(Slide(u,v))))", {
    case FunCall(Map(Lambda(
      _, FunCall(Map(Lambda(
        _, FunCall(Map(Lambda(
          _, FunCall(Slide(u,v), _))), _))), _))), FunCall(Map(Lambda(
    _, FunCall(
    Transpose(), _))), arg)) =>
      Map(Transpose()) o Map(Map(Map(Slide(u,v)))) $ arg
  })

  val slideTransposeReordering = Rule("Map(Slide(u,v)) o Map(Transpose()) => " +
    "Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Slide(u,v)))", {
    case FunCall(Map(Lambda(
      _, FunCall(Slide(u,v), _))), FunCall(Map(Lambda(
        _, FunCall(Transpose(), _))), arg)) =>
      Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Slide(u,v))) $ arg
  })

  val transposeMapJoinReordering = Rule("Transpose() o Map(Join()) => Join() o Map(Transpose()) o Transpose()", {
    case FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), _))), arg)) =>
      Join() o Map(Transpose()) o Transpose() $ arg
  })

  val idTransposeTranspose = Rule("id => Transpose() o Transpose()", {
    case FunCall(Id(), arg) =>
      Transpose() o Transpose() $ arg
  })

  /* Reorder commutativity rules */

  val gatherToScatter = Rule("Map() o Gather(f) => Scatter(f) o Map()", {
    case FunCall(Map(f), FunCall(Gather(g), arg)) =>
      Scatter(g) o Map(f) $ arg
  })

  val scatterToGather = Rule("Scatter(f) o Map() => Map() o Gather(f)", {
    case FunCall(Scatter(g), FunCall(Map(f), arg)) =>
      Map(f) o Gather(g) $ arg
  })

  /* Slide rules */
  val slideTiling: Rule = slideTiling(?)

  def slideTiling(tileStep: ArithExpr) = Rule("Slide(n, s) => Join() o Map(Slide(n, s)) o Slide(u, v)", {
    case FunCall(Slide(n,s), arg) if
      tileStep == ? ||  // either we set it to valid value
      ArithExpr.isSmaller(s,tileStep).getOrElse(false) || // tile is bigger (valid)
      tileStep.equals(s) => // tile is as big as previous slide (creates one sliding window, valid)

      val step = if (tileStep == ?) Utils.validSplitVariable(arg.t) else tileStep
      val overlap = n-s
      Join() o Map(Slide(n, s)) o Slide(step + overlap, step) $ arg
  })

  /* Split-join rule */
  val splitJoin: Rule = splitJoin(?)

  def splitJoin(split: ArithExpr) = Rule("Map(f) => Join() o Map(Map(f)) o Split(I)", {
    case FunCall(Map(f), arg) =>
      val chunkSize = if (split == ?) Utils.validSplitVariable(arg.t) else split
      Join() o Map(Map(f)) o Split(chunkSize) $ arg
  })

  val joinSplit = Rule("Map(Map(f)) => Split(I) o Map(f) o Join()", {
    case call @ FunCall(Map(Lambda(Array(p), FunCall(Map(f), mapArg))), arg)
      if p == mapArg =>
      val length = arg.t match {
        case at: ArrayType => Type.getLength(at.elemT)
        case ty => throw new TypeException(ty, "Array(_)", call)
      }
      Split(length) o Map(f) o Join() $ arg
  })

  val splitJoinReduce = Rule("Reduce(f) => Join() o Map(Reduce(f)) o Split(N)", {
    case FunCall(red: Reduce, init, arg) =>

      val length = arg.t match { case ArrayTypeWS(_, n) => n }

      Join() o Map(fun(x => red(init, x))) o Split(length) $ arg
  })

  /* Cancellation rules */

  val asScalarAsVectorId = Rule("joinVec o splitVec => id", {
    case FunCall(asScalar(), FunCall(asVector(_), arg)) => arg
  })

  val asVectorAsScalarId = Rule("splitVec(_) o joinVec() => id", {
    case FunCall(asVector(n), FunCall(asScalar(), arg))
      if (arg.t match {
        case ArrayType(VectorType(_, m)) => n == m
        case _ => false
      }) => arg
  })

  val transposeTransposeId = Rule("Transpose() o Transpose() => id", {
    case FunCall(t1, FunCall(t2, arg))
      if isTranspose(t1) && isTranspose(t2)
    =>
      arg
  })

  val transposeTransposeId2 = Rule("Transpose() o Transpose() => Id()", {
    case FunCall(t1, FunCall(t2, arg))
      if isTranspose(t1) && isTranspose(t2)
    => Id() $ arg
  })

  val joinSplitId = Rule("Join() o Split(_) => id", {
    case FunCall(Join(), FunCall(Split(_), arg)) => arg
  })

  val splitJoinId = Rule("Split(_) o Join() => id", {
    case FunCall(Split(n), FunCall(Join(), arg))
      if Utils.innerLengthOfTypeMatches(arg.t, n) => arg
  })

  val scatterGatherId = Rule("Scatter(f) o Gather(f) => id", {
    case FunCall(Scatter(ReorderWithStride(s1)), FunCall(Gather(ReorderWithStride(s2)), arg))
      if Utils.expressionsMatch(s1, s2)
    => arg
  })

  val gatherScatterId = Rule("Gather(f) o Scatter(f) => id", {
    case FunCall(Gather(ReorderWithStride(s1)), FunCall(Scatter(ReorderWithStride(s2)), arg))
      if Utils.expressionsMatch(s1, s2)
    => arg
  })

  /* Stride accesses or normal accesses */

  // TODO

  /* Other */

  val reorderBothSidesWithStride: Rule = reorderBothSidesWithStride(?)

  def reorderBothSidesWithStride(stride: ArithExpr): Rule = {
    Rule("Map(f) => Reorder(g^{-1}) o Map(f) o Reorder(g)", {
      case FunCall(map@Map(_), arg) =>
        val s = if (stride == ?) Utils.validSplitVariable(arg.t) else stride
        Scatter(ReorderWithStride(s)) o map o Gather(ReorderWithStride(s)) $ arg
    })
  }

  def isTranspose(funDecl: FunDecl): Boolean =
    funDecl.isInstanceOf[Transpose] || funDecl.isInstanceOf[TransposeW]

  val mapSplitTranspose = Rule("Map(Split(n)) o Transpose()" +
                               "Transpose() o Map(Transpose()) o Split(n)", {
    case FunCall(Map(Lambda(param, FunCall(Split(n), a))), FunCall(t, arg))
      if (param.head eq a) && isTranspose(t)
    =>
      Transpose() o Map(Transpose()) o Split(n) $ arg
  })

  val mapTransposeSplit = Rule("Map(Transpose()) o Split(n)" +
    "Transpose() o Map(Split(n)) o Transpose()", {
    case FunCall(Map(Lambda(param, FunCall(t, a))), FunCall(Split(n), arg))
      if (param.head eq a) && isTranspose(t)
    =>
      Transpose() o Map(Split(n)) o Transpose() $ arg
  })

  val transposeMapSplit = Rule("Transpose() o Map(Split(n))" +
    "Map(Transpose()) o Split(n) o Transpose()", {
    case FunCall(t, FunCall(Map(Lambda(param, FunCall(Split(n), a))), arg))
      if (param.head eq a) && isTranspose(t)
    =>
      Map(Transpose()) o Split(n) o Transpose() $ arg
  })

  val splitTranspose = Rule("Split(n) o Transpose() => " +
        "Map(Transpose()) o Transpose() o Map(Split(n))", {
        case FunCall(Split(n), FunCall(t, arg))
          if isTranspose(t)
        =>
          Map(Transpose()) o Transpose() o Map(Split(n)) $ arg
  })

  val mapTransposeTransposeMapTranspose =
    Rule("Map(Transpose()) o Transpose() o Map(Transpose())) => " +
         "Transpose() o Map(Transpose()) o Transpose()", {
      case FunCall(Map(Lambda(param1, FunCall(t1, a1))),
            FunCall(t2,
            FunCall(Map(Lambda(param2, FunCall(t3, a2))), arg)))
        if (param1.head eq a1)
          && (param2.head eq a2)
          && isTranspose(t1)
          && isTranspose(t2)
          && isTranspose(t3)
      =>
        Transpose() o Map(Transpose()) o Transpose() $ arg
    })

  // TODO: Does it matter there is a Map after the Split? Saves a fusion.
  val splitIntoZip = Rule("Map(fun(x => Map()  ) o Split() $ Zip(...) => " +
                      "Map(x => Map() $ Zip(Get(n, x) ... ) $ Zip(Split $ ...)", {
    case FunCall(Map(Lambda(lambdaParam, FunCall(Map(mapLambda), mapArg))), FunCall(Split(n), FunCall(Zip(_), zipArgs@_*)))
      if lambdaParam.head eq mapArg
    =>
      val newZipArgs = zipArgs.map(arg => Split(n) $ arg)

      val newLambdaParam = Param()

      val innerZipArgs = zipArgs.indices.map(Get(_)(newLambdaParam))

      Map(Lambda(Array(newLambdaParam), Map(mapLambda) $ Zip(innerZipArgs:_*))) $ Zip(newZipArgs:_*)
  })

  private val joinCall: PartialFunction[Expr, Unit] = { case FunCall(Join(), _) => }
  private val getOutermostLengths: PartialFunction[Expr, (ArithExpr, ArithExpr)] =
  { case FunCall(_, arg) => arg.t match {
    case ArrayTypeWS(ArrayTypeWS(_, inner), outer) => (inner, outer)
  }}

  val joinFromZip = Rule(" Zip(Join() $ ..., Join() $ ..., ...) => " +
                      "Join() o Zip(...)", {
    case FunCall(Zip(_), zipArgs@_*)
      if zipArgs.forall(joinCall.isDefinedAt) && zipArgs.map(getOutermostLengths).distinct.size == 1
    =>

      val newArgs = zipArgs.map({ case FunCall(_, arg) => arg })
      val p = Param()
      val innerArgs = newArgs.indices.map(Get(p, _))

      Join() o Map(Lambda(Array(p), Zip(innerArgs:_*))) $ Zip(newArgs:_*)
  })

  val removeDuplicateZipArg = Rule("removeDuplicateZipArgument", {
    case FunCall(Map(Lambda(Array(p), body)), FunCall(Zip(_), args@_*))
      if args.distinct.size < args.size && !(p eq body) &&
        // p only used in get
        !body.contains({
          case FunCall(f, args@_*) if args.exists(_ eq p) && !f.isInstanceOf[Get] =>
        })
    =>

      val oldGets =
        Utils.collect(body, { case FunCall(Get(_), arg) if arg eq p => })

      val newArgs = args.distinct

      val newParam = Param()

      if (newArgs.size == 1) {

        val newBody = oldGets.foldLeft(body)((currentBody, getCall) =>
          Expr.replace(currentBody, getCall, newParam))

        Map(Lambda(Array(newParam), newBody)) $ newArgs.head

      } else {
        val newGetIndex = args.map(newArgs.indexOf)

        val newBody = oldGets.foldLeft(body)({
          case (currentBody, getCall@FunCall(Get(n), _)) =>

            val newId = newGetIndex(n)
            val newGet = Get(newId)(newParam)
            Expr.replace(currentBody, getCall, newGet)
        })

        val newZip = Zip(newArgs: _*)
        Map(Lambda(Array(newParam), newBody)) $ newZip
      }
  })

  val flattenZips = Rule("flattenZips", {
    case FunCall(Map(Lambda(Array(p), body)), FunCall(Zip(_), zipArgs@_*))
      if zipArgs.exists({
        case FunCall(Zip(_), _*) => true
        case _ => false
      })  && !(p eq body) &&
        {
          val (zips, _) = zipArgs.zipWithIndex.partition({
            case (FunCall(Zip(_), _*), _) => true
            case _ => false
          })

          // If the zip result is used, it must be in a get
          val nonZipsUsedCorrectly = !body.contains({
            case FunCall(f, args@_*) if args.exists(_ eq p) && !f.isInstanceOf[Get] =>
          })

          val zipsUsedCorrectly = zips.map(_._2).forall(id => {

            // If this component is used, it must have a second get around it
            val thisZipUsages = Utils.collect(body, {
              case FunCall(Get(actual_id), arg) if actual_id == id && (p eq arg) => })

            thisZipUsages.forall(zipUse =>
              body.contains({ case FunCall(Get(_), arg) if zipUse eq arg => }))
          })

          nonZipsUsedCorrectly && zipsUsedCorrectly
        }
    =>

      val newArgs = zipArgs.flatMap({
        case FunCall(Zip(_), args@_*) => args
        case other => Seq(other)
      })

      val sizes = zipArgs.map({
        case FunCall(Zip(n), _*) => n
        case _ => 1
      })

      val starts = sizes.scan(0)(_+_)

      val newParam = Param()

      val oldGets = Utils.collect(body, { case FunCall(Get(_), arg) if arg eq p => })
      val oldGetsToNestedZip = Utils.collect(body, { case FunCall(Get(_), FunCall(Get(_), arg)) if arg eq p => })

      val nestedGetIds = oldGetsToNestedZip.map({ case FunCall(Get(id2), FunCall(Get(id1), _)) => (id1, id2) })
      val nonNestedGetIds = oldGets.map({ case FunCall(Get(id1), _) => id1 })

      val newNestedIds = nestedGetIds.map(pair => starts(pair._1) + pair._2)
      val newNonNestedIds = nonNestedGetIds.map(starts(_))

      // Replace nested first
      val newBody = (oldGetsToNestedZip ++ oldGets, newNestedIds ++ newNonNestedIds).zipped.foldLeft(body)(
        (currentBody, pair) => {
          val oldGet = pair._1
          val newId = pair._2
          Expr.replace(currentBody, oldGet, Get(newParam, newId))
        })


     Map(Lambda(Array(newParam), newBody)) $ Zip(newArgs:_*)
  })

  val transposeMapTransposeReorder =
    Rule("Map(Gather(f) o Transpose()) o Transpose() => " +
         " Map(Transpose()) o Transpose() o Map(Map(Gather(f)))", {
      case FunCall(Map(Lambda(p,
              FunCall(f:Gather, FunCall(t1, a))
           )), FunCall(t2, arg))
        if (p.head eq a)
          && isTranspose(t1)
          && isTranspose(t2)
      =>
        Map(Transpose()) o Transpose() o Map(Map(f)) $ arg
    })

  val reorderTranspose =
    Rule("Transpose() o Scatter(f) => Map(Scatter(f)) o Transpose()", {
      case FunCall(t, FunCall(f:Scatter, arg))
        if isTranspose(t)
      =>
        Map(f) o Transpose() $ arg
    })


  val dropId = Rule("Id() => Epsilon()", {
    case FunCall(Id(), arg) => arg
    case FunCall(Map(Lambda(Array(_), FunCall(Id(), _))), arg) => arg
    case FunCall(Map(Lambda(Array(_), FunCall(
    Map(Lambda(Array(_), FunCall(Id(), _))), _))), arg) => arg
  })


  val lambdaInline = Rule("lambdaInline", {
    case FunCall(Lambda(Array(x), b), p)
      if !p.isConcrete(true) &&
        Utils.getFinalArg(p).isInstanceOf[Param] =>

      val finalArg = Utils.getFinalArg(p)
      val inlined = Expr.replace(b, x, p)

      // Some trickery to make sure all FunCalls are rebuilt and
      // the same object doesn't appear twice
      val tempParam = Param()
      val tempExpr = Expr.replace(inlined, finalArg, tempParam)
      Expr.replace(tempExpr, tempParam, finalArg)
  })

  val tupleInline = Rule("tupleInline", {
    case FunCall(Lambda(params, body), args@_*)
      if {
        val id = args.indexWhere({
          case FunCall(Tuple(_), _*) => true
          case _ => false
        })

        if (id != -1) {
          val param = params(id)

          val containsNonGetUsage =
            body.contains({
              case FunCall(f, args@_*)
                if args.contains(param) && !f.isInstanceOf[Get] =>
            })

          !containsNonGetUsage
        } else {
          false
        }
      }
    =>

      val id = args.indexWhere({
        case FunCall(Tuple(_), _*) => true
        case _ => false
      })

      val tupleCall = args(id)
      val tupleArgs = tupleCall.asInstanceOf[FunCall].args

      val param = params(id)

      val gets = Utils.findGets(body, param)

      val bodyReplaced = gets.foldLeft(body)({
        case (current, get@FunCall(Get(n), _)) =>
          Expr.replace(current, get, tupleArgs(n))
      })

     if (args.length > 1) {
       val newLambda = Lambda(params.diff(Array(param)), bodyReplaced)
       newLambda(args.diff(Seq(tupleCall)): _*)
     } else {
       bodyReplaced
     }
  })

  val removeEmptyMap = Rule("Map(fun(x => x)) $ a => a", {
    case FunCall(map: AbstractMap, arg)
      if map.f.params.head eq map.f.body
    =>
      arg
  })

  val bSearch = Rule("Search(f) => BSearch(f)", {
    case FunCall(Search(f), arg) => BSearch(f) $ arg
  })

  val lSearch = Rule("Search(f) => BSearch(f)", {
    case FunCall(Search(f), arg) => LSearch(f) $ arg
  })

  val absSearchB = Rule("BSearch(f) => Search(f)", {
    case FunCall(BSearch(f), arg) => Search(f) $ arg
  })

  val absSearchL = Rule("LSearch(f) => Search(f)", {
    case FunCall(LSearch(f), arg) => Search(f) $ arg
  })

}
