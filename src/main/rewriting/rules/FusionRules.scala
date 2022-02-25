package rewriting.rules

import ir.{ArrayType, Size, TupleType}
import ir.ast._
import opencl.ir.pattern.{MapSeq, ReduceSeq}
import rewriting.utils.Utils

object FusionRules {

  val reduceSeqMapSeqFusion = Rule("ReduceSeq o MapSeq => ReduceSeq(fused)", {
    case FunCall(ReduceSeq(f: Lambda), init, FunCall(MapSeq(g : Lambda), arg))
    =>
      val paramUsedMultipleTimes =
        Expr.visitWithState(0)(f.body, (e, c) => {
          e match {
            case p2: Param => if (f.params(1).eq(p2)) c + 1 else c
            case _ => c
          }}) > 1

      if (!paramUsedMultipleTimes) {
        val acc = f.params.head
        val a = Param()
        val newG = Expr.replace(g.body, g.params.head, a)
        val newF = Expr.replace(f.body, f.params(1), newG)
        ReduceSeq(Lambda(Array(acc, a), newF), init) $ arg
      } else {
        ReduceSeq(fun((acc, next) => f(acc, g(next))), init) $ arg
      }
  })

  val mapFusion = Rule("Map(f) o Map(g) => Map(f o g)", {

    case FunCall(Map(l1@Lambda(p1, f, _)), FunCall(Map(l2@Lambda(p2, g, _)), arg)) =>
      val paramUsedMultipleTimes =
        Expr.visitWithState(0)(f, (e, c) => {
          e match {
            case p2: Param => if (p1.head.eq(p2)) c + 1 else c
            case _ => c
          }}) > 1

      // TODO: Avoid simplifier getting stuck. Figure out what's different
      if (paramUsedMultipleTimes) {
        Map(l1 o l2) $ arg
      } else {
        val newLambda = Lambda(p2, Expr.replace(f, p1.head, g))
        Map(newLambda) $ arg
      }

    case FunCall(MapSeq(l1@Lambda1(p1, f)), FunCall(MapSeq(l2@Lambda1(p2, g)), arg)) =>
      val paramUsedMultipleTimes =
        Expr.visitWithState(0)(f, (e, c) => {
          e match {
            case p2: Param => if (p1.head.eq(p2)) c + 1 else c
            case _ => c
          }}) > 1

      // TODO: Avoid simplifier getting stuck. Figure out what's different
      if (paramUsedMultipleTimes) {
        Map(l1 o l2) $ arg
      } else {
        val newLambda = Lambda(p2, Expr.replace(f, p1.head, g))
        MapSeq(newLambda) $ arg
      }

    case FunCall(Map(Lambda(p1, f1, _)),
    call@FunCall(Lambda(_, FunCall(Map(Lambda(_, f2, _)), _), _), _)) =>

      val newBody = Expr.replace(f1, p1.head, f2)

      Expr.replace(call, f2, newBody)
  })

  val mapFusionWithZip =
    Rule("Map(fun(x => f $ arg )) $ Zip( ..., Map(g) , ...)", {
      case FunCall(Map(Lambda(p, call@FunCall(_, args@ _* ), _)), FunCall(Zip(_), zipArgs@_*))
        if args.last.contains({
          case FunCall(Get(n), a)
            if (a eq p.head) && n == zipArgs.indexWhere({
              case FunCall(Map(_), _) => true
              case _ => false
            })
          =>
        }) && zipArgs.count({
          case FunCall(Map(_), _) => true
          case _ => false
        }) == 1 // TODO: What if several arguments in zip have maps?
      =>

        val mapInZipIndex = zipArgs.indexWhere({
          case FunCall(Map(_), _) => true
          case _ => false
        })

        val (g, newArg) = zipArgs(mapInZipIndex) match {
          case FunCall(Map(lambda), mapArg) => (lambda, mapArg)
        }

        val newZipArgs = zipArgs.updated(mapInZipIndex, newArg)

        val newLambdaParam = Param()
        val gets = Utils.findGets(call, p.head)
        val newGets = gets.map({
          case FunCall(Get(i), _) if i == mapInZipIndex =>

            val lambdaCall = g $ Get(newLambdaParam, i)
            if (SimplificationRules.lambdaInline.isDefinedAt(lambdaCall))
              SimplificationRules.lambdaInline.rewrite(lambdaCall)
            else
              lambdaCall

          case FunCall(Get(i), _) => Get(newLambdaParam, i)
        })

        val newCall = (gets, newGets).zipped.foldLeft(call: Expr)((current, pair) =>
          Expr.replace(current, pair._1, pair._2))

        Map(Lambda(Array(newLambdaParam), newCall)) $ Zip(newZipArgs:_*)
    })

  val fuseZipTuple = Rule("fuseZipTuple", {
    case FunCall(Lambda(Array(p), body, _), FunCall(Tuple(n), tupleArgs@_*))
      if getZipForZipTupleFusion(body, n, p).isDefined
    =>

      val zip = getZipForZipTupleFusion(body, n, p)

      val zipArgs = zip.get.asInstanceOf[FunCall].args
      val zipArgIds = zipArgs.collect({ case FunCall(Get(i), _) => i })

      zipArgIds.zipWithIndex.foldLeft(body)({
        case (currentExpr,(i, j)) =>
          Expr.replace(currentExpr, zipArgs(j), tupleArgs(i))
      })

  })

  def getZipForZipTupleFusion(body: Expr, n: Int, p: Param): Option[Expr] = {
    Utils.getExprForPatternInCallChain(body, {
      case FunCall(Zip(n2), args@_*)
        if n == n2 && args.collect({
          case FunCall(Get(n3), p2) if p eq p2 => n3
        }).distinct.length == n
      =>
    })
  }

  val reduceFusionInZip = Rule("reduceFusionInZip", {
    case FunCall(Zip(_), args@_*)
      if args.forall({
        case FunCall(Reduce(_), _: Value, _) => true
        case _ => false
      })
    =>

      val initVals = args.map({
        case FunCall(_, v: Value, _) => v
      })

      val newInitType = TupleType(initVals.map(_.t):_*)

      val newInitString = "{ " + initVals.mkString(", ") + " }"

      val newInitValue = Value(newInitString, newInitType)
      val newAcc = Param()
      val newParam = Param()

      val reduces = args.zipWithIndex.map({
        case (FunCall(Reduce(f), _, _), n) =>
          f(Get(newAcc, n), Get(newParam, n))
      })

      val newZipArgs = args.map({
        case FunCall(_, _, a) => a
      })

      Reduce(Lambda(Array(newAcc, newParam), Tuple(reduces:_*)), newInitValue) $
      Zip(newZipArgs:_*)
  })


  val tupleMap = Rule("Tuple(Map(f) $ .. , Map(g) $ .., ...) => " +
    "Unzip() o Map(x => Tuple(f $ Get(x, 0), g $ Get(x, 1), ...) $ Zip(...) ", {
    case FunCall(Tuple(_), args@_*)
      if args.forall({
        case FunCall(Map(_), _) => true
        case _ => false
      }) && args.map(_.t.asInstanceOf[ArrayType with Size].size).distinct.length == 1
    =>
      val zipArgs = args.map({
        case FunCall(_, mapArgs) => mapArgs
      })

      val lambdaParam = Param()

      val maps = args.zipWithIndex.map({
        case (FunCall(Map(f), _), n) => f $ Get(lambdaParam, n)
      })

      Unzip() o Map(Lambda(Array(lambdaParam), Tuple(maps:_*))) $ Zip(zipArgs:_*)
  })

  val mapFusionInZip = Rule("mapFusionInZip", {
    case FunCall(Zip(_), args@_*)
      if args.forall({
        case FunCall(Map(_), _) => true
        case _ => false
      }) && args.map(_.t.asInstanceOf[ArrayType with Size].size).distinct.length == 1
    =>

      val zipArgs = args.map({
        case FunCall(_, mapArgs) => mapArgs
      })

      val lambdaParam = Param()

      val maps = args.zipWithIndex.map({
        case (FunCall(Map(f), _), n) =>
          val body = f.body
          val p = f.params.head
          Expr.replace(body, p, Get(lambdaParam, n))
      })

      Map(Lambda(Array(lambdaParam), Tuple(maps:_*))) $ Zip(zipArgs:_*)
  })
}
