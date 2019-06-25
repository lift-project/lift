package rewriting.rules

import ir._
import ir.ast._
import rewriting.utils.Utils

object SimplificationRules {

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
      if Utils.isTranspose(t1) && Utils.isTranspose(t2)
    =>
      arg
  })

  val transposeTransposeId2 = Rule("Transpose() o Transpose() => Id()", {
    case FunCall(t1, FunCall(t2, arg))
      if Utils.isTranspose(t1) && Utils.isTranspose(t2)
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

  // Zip(a, b, a) => Zip(a, b)
  val removeDuplicateZipArg = Rule("removeDuplicateZipArgument", {
    case FunCall(Map(Lambda(Array(p), body, _)), FunCall(Zip(_), args@_*))
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

  // Zip(Zip(a, b), Zip(c, d), e) => Zip(a, b, c, d, e)
  val flattenZips = Rule("flattenZips", {
    case FunCall(Map(Lambda(Array(p), body, _)), FunCall(Zip(_), zipArgs@_*))
      if zipArgs.exists({
        case FunCall(Zip(_), _*) => true
        case _ => false
      })  && !(p eq body) &&
        {
          // To be able to replace all old usages of p they must all appear
          // in body with the appropriate number of Gets. Otherwise, some result of Zip
          // can leave body and we can't replace it from inside this rule.

          // Check that all usages of `p` are of the form `FunCall(Get(_), p)`
          val nonZipsUsedCorrectly = !body.contains({
            case FunCall(f, args@_*) if args.exists(_ eq p) && !f.isInstanceOf[Get] =>
          })

          // Find the indices of zips being inlined into the existing one
          val zipIndices = zipArgs.zipWithIndex.collect({
            case (FunCall(Zip(_), _*), index) => index
          })

          // Check That all usages of `p` that refer to e.g. `Zip(a,b)` are of the form
          // `FunCall(Get(_), FunCall(Get(_), p))`
          val zipsUsedCorrectly = zipIndices.forall(id => {

            val thisZipUsages = Utils.collect(body, {
              case FunCall(Get(actual_id), arg) if actual_id == id && (p eq arg) => })

            // If this component is used, it must have a second get around it
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

  val removeEmptyMap = Rule("Map(fun(x => x)) $ a => a", {
    case FunCall(map: AbstractMap, arg)
      if map.f.params.head eq map.f.body
    =>
      arg
  })

  val lambdaInline = Rule("lambdaInline", {
    case FunCall(Lambda(Array(x), b, _), p)
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

  val lambdaInlineParam = Rule("lambdaInlineParam", {
    case FunCall(Lambda(Array(x), b, _), p: Param) =>

      val finalArg = Utils.getFinalArg(p)
      val inlined = Expr.replace(b, x, p)

      // Some trickery to make sure all FunCalls are rebuilt and
      // the same object doesn't appear twice
      val tempParam = Param()
      val tempExpr = Expr.replace(inlined, finalArg, tempParam)
      Expr.replace(tempExpr, tempParam, finalArg)
  })

  val tupleInline = Rule("tupleInline", {
    case FunCall(Lambda(params, body,_), args@_*)
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

  val dropId = Rule("Id() => Epsilon()", {
    case FunCall(Id(), arg) => arg
    case FunCall(Map(Lambda(Array(_), FunCall(Id(), _), _)), arg) => arg
    case FunCall(Map(Lambda(Array(_), FunCall(
    Map(Lambda(Array(_), FunCall(Id(), _), _)), _), _)), arg) => arg
  })

}
