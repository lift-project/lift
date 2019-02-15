package rewriting.rules

import ir.{ArrayType, Type}
import ir.ast._
import opencl.ir.pattern.ReduceSeq
import rewriting.utils.Utils

object InterchangeRules {

  val mapMapTransposeZipOutside = Rule("Map(fun(x => Map(fun(a => )) $ Get(n, ) $ Zip(..., A, ...)  => " +
    "Transpose() o Map(fun(a => Map() $ Zip(..., a, ...)) o Transpose() $ A", {
    case FunCall(Map(Lambda(outerLambdaParam,
    FunCall(Map(Lambda(innerLambdaParam,
    expr, _
    )), FunCall(Get(n), getParam)), _
    )), FunCall(Zip(_), zipArgs@_*))

      if getParam eq outerLambdaParam.head
    =>
      // Find all Get patterns that refer to the an element from the zipped array
      // and have to be replaced in expr
      val gets = Utils.findGets(expr, outerLambdaParam.head)

      // Find 'A'
      val newArg = zipArgs(n)

      // Construct new Get patterns and replace them for the old ones
      val newParam = Param()
      val newGets = gets zip gets.map(get => Get(newParam, get.f.asInstanceOf[Get].n))
      val newExpr = newGets.foldRight(expr)((get, e) => Expr.replace(e, get._1, get._2))

      // Create a get pattern for an element 'a'
      val finalNewExpr = Expr.replace(newExpr, innerLambdaParam.head, Get(n)(newParam))

      // Replace 'a' with a new parameter to create the new arguments for zip
      val secondNewParam = Param()
      val newZipArgs = zipArgs.updated(n, secondNewParam)

      TransposeW() o Map(Lambda(Array(secondNewParam),
        Map(Lambda(Array(newParam), finalNewExpr)) $ Zip(newZipArgs: _*)
      )) o Transpose() $ newArg
  })

  val transposeBothSides = Rule("Map(fun(a => Map(f) $ a)) $ A => " +
    "Transpose() o Map(fun(a =>Map(f) $ a)) o Transpose() $ A  ", {
    case FunCall(Map(f@Lambda(param, FunCall(Map(_), a), _)), arg)
      if param.head eq a
    =>
      TransposeW() o Map(f) o Transpose() $ arg
  })

  val mapMapTransposeZipInside = Rule("Map(fun(a => Map() $ Zip(..., ... $ a, ...)) $ A => " +
    "Transpose() o Map(Map(fun(a => ))) $ Zip(..., Transpose() o Map(...) $ A, ...) ", {
    case FunCall(Map(Lambda(Array(outerLambdaParam),
    FunCall(Map(Lambda(Array(innerLambdaParam),
    expr, _
    )), FunCall(Zip(_), zipArgs@_*))
    , _)), arg)
      if zipArgs.count(Utils.getFinalArg(_) eq outerLambdaParam) >= 1
    =>
      // Find all Get patterns that refer to the an element from the zipped array
      // and have to be replaced in expr
      val gets = Utils.findGets(expr, innerLambdaParam)

      // Find which Get pattern corresponds to the component containing an element of 'a'
      val zipToReplace = zipArgs.zipWithIndex.filter(e =>
        Utils.getFinalArg(e._1) eq outerLambdaParam
      )

      // Create the new Get patterns with a new parameter
      val newParam = Param()
      val getPairs = gets zip gets.map(get => Get(newParam, get.f.asInstanceOf[Get].n))

      // Separate the Get pattern containing an element of 'a', as it will now refer
      // to the component containing an element of 'A'
      val (one, two) = getPairs.partition(x =>
        zipToReplace.map(_._2).contains(x._1.f.asInstanceOf[Get].n)
      )

      // Replace most of the old Get patterns with new ones
      val newExpr = two.foldRight(expr)((get, e) => Expr.replace(e, get._1, get._2))

      // Create a new parameter for an element of 'a' and replace for the Get referring
      // an element of 'a'
      val secondNewParam = Param()

      val finalNewExpr = one.zipWithIndex.foldLeft(newExpr)((expr, p) => {
        val id = p._2 //zipToReplace.find(_._1 eq p._1).get._2
        val newThing = if (one.size == 1) secondNewParam else Get(secondNewParam, id)
        Expr.replace(expr, p._1._1, newThing)
      })

      val elems = zipToReplace.map(pair =>
        if (!(pair._1 eq outerLambdaParam)) {
          // Isolate any splits/joins/transposes/computation inside the zip
          val thirdNewParam = Param()

          val replace = Expr.replace(pair._1, outerLambdaParam, thirdNewParam)

          // And include them before the transpose
          val map = Map(Lambda(Array(thirdNewParam), replace))
          Transpose() o map $ arg
        } else {
          Transpose() $ arg
        })

      // Create the arguments for the zip, replacing '... o a' with 'Transpose() o Map(...) $ A'
      val newZipArgs = (zipToReplace, elems).zipped.foldLeft(zipArgs)((arguments, y) => {
        val elem = y._2
        val id = y._1._2

        arguments.updated(id, elem)
      })

      val newArgs = if (one.size == 1) one.map(_._2).head else Zip(one.map(_._2):_*)

      // Construct the final expression
      val lambda = Lambda(Array(secondNewParam), finalNewExpr)
      TransposeW() o Map(Lambda(Array(newParam), Map(lambda) $ newArgs)) $ Zip(newZipArgs: _*)
  })

    val mapMapInterchange = Rule("Map(fun(a => Map(fun( b => ... ) $ B) $ A => " +
    "Transpose() o Map(fun(b => Map(fun( a => ... ) $ A) $ B", {
    case FunCall(Map(Lambda(a, FunCall(Map(Lambda(b, expr, _)), bArg), _)), aArg)
      if !bArg.contains({ case e if e eq a.head => })
    =>
      TransposeW() o Map(Lambda(b, FunCall(Map(Lambda(a, expr)), aArg))) $ bArg
  })

  val mapReduceInterchange = Rule("Map(Reduce(f)) => Transpose() o Reduce(Map(f)) o Transpose()", {
    case FunCall(Map(Lambda(lambdaParams,
          FunCall(r@AbstractPartRed(Lambda(innerParams, expr, _)), init: Value, arg), _
         )), mapArg)
      if lambdaParams.head eq arg
    =>
      val newInit = Value(init.value, ArrayType(init.t, Type.getLength(mapArg.t)))

      val newMapParam = Param()
      val newExpr = innerParams.zipWithIndex.foldLeft(expr)((e, pair) =>
        Expr.replace(e, pair._1, Get(pair._2)(newMapParam)))

      val lambda = fun((acc, c) => Map(Lambda(Array(newMapParam), newExpr)) $ Zip(acc, c))
      TransposeW()( r.copy(lambda)(newInit, Transpose() $ mapArg))
  })

  val mapReduceInterchangeWithZipOutside =
    Rule("Map(fun(x => Reduce(f, Get(x, 0)) $ Get(x, 1) ) $ Zip(a, b) => " +
         "Transpose() o Reduce(fun((acc, y) => Map(f) $ Zip(acc, y) ), a ) o Transpose() $ b", {
      case FunCall(Map(Lambda(lambdaParams,
      FunCall(r@AbstractPartRed(Lambda(innerParams, expr, _)), FunCall(Get(i), a1), FunCall(Get(j), a2)), _
      )), FunCall(Zip(2), zipArgs@_*))
        if (lambdaParams.head eq a1) && (lambdaParams.head eq a2)
      =>
        val newInit = zipArgs(i)
        val newArg = zipArgs(j)

        val acc = Param()
        val next = Param()
        val mapParam = Param()

        val interimExpr = Expr.replace(expr, innerParams(i), Get(i)(mapParam))
        val finalExpr = Expr.replace(interimExpr, innerParams(j), Get(j)(mapParam))

        Transpose() ( r.copy(Lambda(Array(acc, next),
          Map(Lambda(Array(mapParam), finalExpr)) $ Zip(acc, next)
        ))(newInit, Transpose() $ newArg))
    })

  // TODO: Should use Reduce instead of PartRed, as PartRed can return something that is
  // TODO: not length one, and the output type can break. Will need to check some
  // TODO: other way that both fs are the same.
  val mapReducePartialReduce =
    Rule("Map(Reduce(f, init) o Join() o Map(PartRed(f, init2)) ) => " +
      "Transpose() o Reduce((acc, a) => Join() o Map(x => PartRed(f, Get(x, 0)) $ Get(x, 1)) $ Zip(acc, a) , Array(init)) o Transpose()", {
      case FunCall(Map(Lambda(p1,
      FunCall(ReduceSeq(f1), init1: Value, FunCall(Join(), FunCall(Map(Lambda(p2,
        FunCall(PartRed(_), init2: Value, a2), _)), a1))), _
      )), arg)
        if (p1.head eq a1) && (p2.head eq a2) && init1 == init2
      =>
        val newInit = Value(init2.value, ArrayType(init2.t, Type.getLength(arg.t)))

        TransposeW() o ReduceSeq(fun((acc, a) =>
          Join() o Map(fun(x =>
            PartRed(f1, Get(x, 0)) $ Get(x,1)
          )) $ Zip(acc, a)
        ), newInit) o Transpose() $ arg
    })
}
