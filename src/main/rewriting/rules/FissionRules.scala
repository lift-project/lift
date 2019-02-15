package rewriting.rules

import ir.ast._
import opencl.ir.pattern.{MapSeq, ReduceSeq}
import rewriting.utils.Utils

object FissionRules {


  // More restricted than necessary. Could pull out any arg.
  val extractFromMap = Rule("Map(x => f(x, g(...))) $ in => " +
  "(y => Map(x => f(x, y)) $ in) o g(...)", {
    case call@FunCall(Map(Lambda(Array(p), FunCall(_, arg), _)) , _)
      if !arg.contains({ case y if y eq p => })
    =>
      val newParam = Param()

      val newCall = Expr.replace(call, arg, newParam)

      val lambda = Lambda(Array(newParam), newCall)
      val newExpr = FunCall(lambda, arg)

      newExpr

    case call@FunCall(Map(Lambda(Array(p), FunCall(_, _, arg), _)), _)
      if !arg.contains({ case y if y eq p => })
    =>
      val newParam = Param()

      val newCall = Expr.replace(call, arg, newParam)

      val lambda = Lambda(Array(newParam), newCall)
      val newExpr = FunCall(lambda, arg)

      newExpr
  })

  val mapFission = Rule("Map(f o g) => Map(f) o Map(g)", {
    case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(fun2, p2@_*)), _)), arg)
      if !fun1.isInstanceOf[FPattern] ||
        !fun1.asInstanceOf[FPattern].f.body.contains({ case a if a eq p1.head => })
    =>
      Map(fun1) o Map(Lambda(p1, fun2(p2:_*))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(r: AbstractPartRed, init, FunCall(fun2, p2)), _)), arg)
      if !r.f.body.contains({ case a if a eq p1.head => }) &&
        !init.contains({ case a if a eq p1.head => })
    =>

      Map(fun((x) => r(init, x))) o Map(Lambda(p1, fun2(p2))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(r: AbstractPartRed, init, p2)), _)), arg)
      if !fun1.isInstanceOf[FPattern] ||
        !fun1.asInstanceOf[FPattern].f.body.contains({ case a if a eq p1.head => })
    =>
      Map(fun1) o Map(Lambda(p1, r(init, p2))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(r1: AbstractPartRed, init1,
    FunCall(r2: AbstractPartRed, init2, p2)), _)), arg)
      if !r1.f.body.contains({ case a if a eq p1.head => }) &&
        !init1.contains({ case a if a eq p1.head => })
    =>
      Map(fun((x) => r1(init1, x))) o Map(Lambda(p1, r2(init2, p2))) $ arg
  })

  val mapFissionCreateZip = Rule("Map(x => f(x, g(...)) => Map(f) $ Zip(..., Map(g)  )", {
    case FunCall(Map(Lambda(p1, FunCall(fun1, call@FunCall(fun2, p2)), _)), arg)
      if fun1.isInstanceOf[FPattern] &&
        fun1.asInstanceOf[FPattern].f.body.contains({ case a if a eq p1.head => }) &&
      call.contains({ case a if a eq p1.head => }) && !arg.contains({ case FunCall(Zip(_), _*) => })
    =>

      val fp = fun1.asInstanceOf[FPattern]
      val origLambda = fp.f
      val newParam = Param()
      val get1 = Get(newParam, 1)

      // Use a partial function to make sure every replacement gets a different object
      val replaceP1: PartialFunction[Expr, Expr] =
      { case p: Param if p eq p1.head => Get(newParam, 0) }

      val newBody = Expr.replace(fp.f.body, p1.head, replaceP1)

      val newLambda = Lambda(origLambda.params, newBody)
      val newFp = fp.copy(newLambda)

      Map(Lambda(Array(newParam), newFp $ get1)) $
        Zip(arg, Map(Lambda(p1, fun2(p2))) $ arg)

    // TODO: Which behaviour should this rule have? Tests assume arg is used several times.
    // TODO: Original rule assumes x is used in f. Now assumes x is used in f & g.
    case FunCall(Map(Lambda(p1, FunCall(r: AbstractPartRed, init, FunCall(fun2, p2)), _)), arg)
      if r.f.body.contains({ case a if a eq p1.head => }) ||
        init.contains({ case a if a eq p1.head => })
    =>

      val origLambda = r.f
      val newParam = Param()
      val get1 = Get(newParam, 1)

      // Use a partial function to make sure every replacement gets a different object
      val replaceP1: PartialFunction[Expr, Expr] =
      { case p: Param if p eq p1.head => Get(newParam, 0) }

      val newBody = Expr.replace(origLambda.body, p1.head, replaceP1)

      val newLambda = Lambda(origLambda.params, newBody)
      val newInit = Expr.replace(init, p1.head, replaceP1)

      Map(Lambda(Array(newParam), r.copy(newLambda)(newInit, get1))) $
        Zip(arg, Map(Lambda(p1, fun2(p2))) $ arg)

    case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(r: AbstractPartRed, init, p2)), _)), arg)
      if fun1.isInstanceOf[FPattern] &&
        fun1.asInstanceOf[FPattern].f.body.contains({ case a if a eq p1.head => })
    =>

      val fp = fun1.asInstanceOf[FPattern]
      val origLambda = fp.f
      val newParam= Param()
      val get1 = Get(newParam, 1)

      // Use a partial function to make sure every replacement gets a different object
      val replaceP1: PartialFunction[Expr, Expr] =
      { case p: Param if p eq p1.head => Get(newParam, 0) }

      val newBody = Expr.replace(fp.f.body, p1.head, replaceP1)

      val newLambda = Lambda(origLambda.params, newBody)
      val newFp = fp.copy(newLambda)

      Map(Lambda(Array(newParam), newFp $ get1)) $
        Zip(arg, Map(Lambda(p1, r.copy(r.f)(init, p2))) $ arg)

    case FunCall(Map(Lambda(p1, FunCall(r1: AbstractPartRed, init1,
    FunCall(r2: AbstractPartRed, init2, p2)), _)), arg)
      if r1.f.body.contains({ case a if a eq p1.head => }) ||
        init1.contains({ case a if a eq p1.head => })
    =>

      val origLambda = r1.f
      val newParam = Param()
      val get1 = Get(newParam, 1)

      // Use a partial function to make sure every replacement gets a different object
      val replaceP1: PartialFunction[Expr, Expr] =
      { case p: Param if p eq p1.head => Get(newParam, 0) }

      val newBody = Expr.replace(origLambda.body, p1.head, replaceP1)

      val newLambda = Lambda(origLambda.params, newBody)
      val newInit = Expr.replace(init1, p1.head, replaceP1)

      Map(Lambda(Array(newParam), r1.copy(newLambda)(newInit, get1))) $
        Zip(arg,  Map(Lambda(p1, r2.copy(r2.f)(init2, p2))) $ arg)
  })

  val mapFissionWithZipInside = Rule("Map(fun(x => ... Zip(..., f $ x, ...))) " +
                               "Map(fun(y => ... Zip(..., y, ...))) o Map(f)", {
    case FunCall(Map(Lambda(lambdaParam, body, _)), arg)
      if Expr.visitWithState(0)(body, (e, count) => {
        e match {
          case FunCall(Zip(_), args@_*)
            if args.count(containsParam(_, lambdaParam.head)) == 1 => count + 1
          case _ => count
        }
      }) == 1
    =>
      var exprToReplaceInZip: Option[Expr] = None

      Expr.visit(body, {
        case FunCall(Zip(_), args@_*) =>
          args.find(containsParam(_, lambdaParam.head)) match {
            case e: Some[Expr] => exprToReplaceInZip = e
            case _ =>
          }
        case _ =>
      }, _ => Unit)

      val newLambdaParam = Param()
      val newBody = Expr.replace(body, exprToReplaceInZip.get, newLambdaParam)

      val newMapLambdaParam = Param()
      val newExpr = Expr.replace(exprToReplaceInZip.get, lambdaParam.head, newMapLambdaParam)

      FunCall(Map(Lambda(Array(newLambdaParam), newBody)),
        FunCall(Map(Lambda(Array(newMapLambdaParam), newExpr)), arg))
  })


  private def containsParam(expr: Expr, param: Param): Boolean =
    expr.contains({
      case FunCall(_, p: Param) if p eq param =>
    })

  val mapFissionWithZipOutside = Rule("Map(fun(x => ...o f $ Get(x, i))) $ Zip(..., y, ...) " +
    "Map(fun(z => ... $ Get(z, i)) $ Zip(..., Map(f) $ y, ...)", {
    case FunCall(Map(Lambda(lambdaParam,
    c@FunCall(_, arg), _
    )), FunCall(Zip(_), zipArgs@_*))
      if isMapFissionWithZipOutsideValid(lambdaParam, arg)
    =>
      applyMapFissionWithZipOutside(c, arg, zipArgs)

    case FunCall(Map(Lambda(lambdaParam,
    c@FunCall(_: AbstractPartRed, _, arg@FunCall(_, FunCall(Get(_), _))), _
    )), FunCall(Zip(_), zipArgs@_*))
      if isMapFissionWithZipOutsideValid(lambdaParam, arg)
    =>
      applyMapFissionWithZipOutside(c, arg, zipArgs)

  })

  private def applyMapFissionWithZipOutside(c: FunCall, arg: Expr, zipArgs: Seq[Expr]): Expr = {
    val toBeReplaced = Utils.getExprForPatternInCallChain(arg, getCallPattern).get

    val (f, i, x) = toBeReplaced match {
      case FunCall(h, FunCall(Get(j), y)) => (h, j, y)
    }

    val newZipArgs = zipArgs.updated(i, Map(f) $ zipArgs(i))
    val newLambdaParam = Param()
    val interimDots = Expr.replace(c, toBeReplaced, Get(i)(newLambdaParam))
    val newDots = Expr.replace(interimDots, x, newLambdaParam)

    Map(Lambda(Array(newLambdaParam), newDots)) $ Zip(newZipArgs: _*)
  }

  private def isMapFissionWithZipOutsideValid(lambdaParam: Array[Param], arg: Expr): Boolean = {
    Utils.getExprForPatternInCallChain(arg, getCallPattern) match {
      case Some(FunCall(_, FunCall(Get(_), x))) if lambdaParam.head eq x => true
      case _ => false
    }
  }

  private val getCallPattern: PartialFunction[Expr, Unit] = { case FunCall(_, FunCall(Get(_), _)) => }

  val tupleFission =
    Rule("Tuple(f $... , g $ ..., ...) => " +
         "Tuple(f $ Get( , 0), g $ Get( , 1), ...) o Tuple(...)", {
    case FunCall(Tuple(_), args@_*)
      if args.forall({
        case FunCall(_, FunCall(_, _)) => true
        case _ => false
      })
    =>

      val param = Param()

      val firsts = args.zipWithIndex.map({
        case (FunCall(f, _), i) => f $ Get(param, i)
      })

      val seconds = args.map({
        case FunCall(_, f) => f
      })

      FunCall(Lambda(Array(param), Tuple(firsts:_*)), Tuple(seconds:_*))
  })
}
