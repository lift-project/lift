package rewriting.rules

import ir.ast.{Expr, FPattern, FPattern2, FunCall, Lambda, Let, Param, Tuple}
import rewriting.utils.NumberExpression

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object FactorizationRules {

  private var commonSubExprAsSeenInArgs: Option[Seq[Expr]] = None
  val commonSubExprInTuples = Rule("Tuple(f(g(a)), h(g(a))) => Let(p => Tuple(f(p), h(p))) $ g(a)", {
    case c@FunCall(Tuple(_), args@_*) if args.length > 1 && {
      val argsSubExprs = args.map(arg => NumberExpression.breadthFirst(arg)).
        map(argSubExpr => argSubExpr.toSeq.sortWith(_._2 < _._2))

      // The problem with subexpressions is that even the semantically equivalent ones have differing
      // parameters (different instances of the Param class) in the inner lambdas, and normal equality checks
      // will not evaluate them as equal.
      // The code below finds inner lambdas and replaces their parameters with new ones in such a way that
      // equal lambdas will have the same parameters

      // In the subexpressions, replace parameters in lambdas with new ones from a list.
      val newParams: ArrayBuffer[Param] = new ArrayBuffer()
      val argsSubExprsWithNormalisedParams = argsSubExprs.map(argSubExpr => argSubExpr.map(exprWithId => {
        val expr = exprWithId._1
        val exprId = exprWithId._2
        val substitutionMap = Expr.visitWithState(collection.Map[Param, Param]())(expr, {
          case (e: Expr, substMap: collection.Map[Param, Param]) =>

            def collectParams(params: Seq[Param],
                              collectedParams: collection.Map[Param, Param]): collection.Map[Param, Param] = {
              val uniqueParamsN = collectedParams.size + params.length
              // Grow the list of new params if necessary
              while (newParams.length <= uniqueParamsN) newParams += Param()
              collectedParams ++
                params.zipWithIndex.map(oldParam => oldParam._1 -> newParams(collectedParams.size + oldParam._2))
            }

            e match {
              // If necessary, add ReduceWhileSeq here or extend ReduceWhileSeq with FPattern2
              case FunCall(fp: FPattern2, _@_*)            => val substMapUpd = collectParams(fp.f1.params, substMap)
                                                              collectParams(fp.f2.params, substMapUpd)
              case FunCall(fp: FPattern, _@_*)             => collectParams(fp.f.params, substMap)
              case FunCall(Lambda(oldParams, _, _), _@_*)  => collectParams(oldParams, substMap)
              case _ => substMap
            }})

        (Expr.replace(expr, {
          case p: Param => if (substitutionMap.contains(p)) substitutionMap(p) else p
          case e => e
        }), exprId)}))

      // Intersection
      val commonSubExprs = argsSubExprsWithNormalisedParams.head.filter(arg0SubExpr =>
        argsSubExprsWithNormalisedParams.tail.map(_.map(_._1).exists(Expr.equals(_, arg0SubExpr._1))).reduce(_ && _))

      if (commonSubExprs.nonEmpty) {
        // Do not use param-normalised subexpressions, use the original ones.
        // The smaller the ID, the (non-strictly) bigger the expression (because of the breadth-first search).
        // Hence ".head" below
        val commonSubExprIdsInArgs = argsSubExprsWithNormalisedParams.map(argSubExprsWithNormalisedParams =>
          argSubExprsWithNormalisedParams.find(e => Expr.equals(e._1, commonSubExprs.head._1)).get._2)
        commonSubExprAsSeenInArgs = Some(argsSubExprs.zip(commonSubExprIdsInArgs).map {
          case (subExprs: Seq[(Expr, Int)], subExprId: Int) => subExprs.find(e =>  e._2 == subExprId).get._1
        })
        true
      } else false
    } =>
      println(commonSubExprAsSeenInArgs.get)
      val newParam = Param()
      val argsWithoutCommonSubExpr = args.zip(commonSubExprAsSeenInArgs.get).map(a => {
        val (arg, commonSubExpr) = a
        Expr.replace(arg, commonSubExpr, newParam)})
      new Let(Array(newParam), Tuple(argsWithoutCommonSubExpr: _*)) $ commonSubExprAsSeenInArgs.get.head
  })
}
