package exploration

import apart.arithmetic._
import ir._
import ir.ast._
import opencl.ir.pattern._

case class Rule(desc: String,
                rewrite: PartialFunction[Expr, Expr])

object Rules {

  /* Iterate decomposition rule */

  // TODO

  /* Reorder commutativity rules */

  val gatherToScatter = Rule("Map() o Gather(f) => Scatter(f) o Map()", {
    case FunCall(Map(f), FunCall(Gather(g), arg)) =>
      Scatter(g) o Map(f) $ arg
  })

  val scatterToGather = Rule("Scatter(f) o Map() => Map() o Gather(f)", {
    case FunCall(Scatter(g), FunCall(Map(f), arg)) =>
      Map(f) o Gather(g) $ arg
  })

  /* Split-join rule */

  val splitJoin: Rule = splitJoin(?)

  def splitJoin(split: ArithExpr) = Rule("Map(f) => Join() o Map(Map(f)) o Split(I)", {
    case FunCall(Map(f), arg) =>
      val chunkSize = if (split == ?) validSplitVariable(arg.t) else split
      Join() o Map(Map(f)) o Split(chunkSize) $ arg
  })

  /* Reduce rules */

  val partialReduce = Rule("Reduce(f) => Reduce(f) o PartRed(f)", {
    case FunCall(Reduce(f), init, arg) =>
      Reduce(f, init) o PartRed(f, init) $ arg
  })

  val partialReduceToReduce = Rule("PartRed(f) => Reduce(f)", {
    case FunCall(PartRed(f), init, arg) =>
      Reduce(f, init) $ arg
  })

  val partialReduceReorder: Rule = partialReduceReorder(?)

  def partialReduceReorder(s: ArithExpr): Rule =
    Rule("PartRed(f) => PartRed(f) o Reorder", {
      case FunCall(PartRed(f), init, arg) =>
        val stride = if (s == ?) validSplitVariable(arg.t) else s
        PartRed(f, init) o Gather(ReorderWithStride(stride)) $ arg
    })

  // TODO: iterate

  val partialReduceSplitJoin: Rule = partialReduceSplitJoin(?)

  def partialReduceSplitJoin(split: ArithExpr): Rule =
    Rule("PartRed(f) => Join() o Map(PartRed(f)) o Split()", {
      case FunCall(PartRed(f), init, arg) =>
        val chunkSize = if (split == ?) validSplitVariable(arg.t) else split
        Join() o Map(PartRed(f, init)) o Split(chunkSize) $ arg
    })

  /* Cancellation rules */

  val asScalarAsVectorId = Rule("joinVec o splitVec => id", {
    case FunCall(asScalar(), FunCall(asVector(_), arg)) => arg
  })

  val asVectorAsScalarId = Rule("splitVec(_) o joinVec() => id", {
    case FunCall(asVector(n), FunCall(asScalar(), arg))
      if (arg.t match {
        case ArrayType(VectorType(_, m), _) => n == m
        case _ => false
      }) => arg
  })

  val transposeTransposeId = Rule("Transpose() o Transpose() => id", {
    case FunCall(Transpose(), FunCall(Transpose(), arg)) => arg
    case FunCall(TransposeW(), FunCall(Transpose(), arg)) => arg
    case FunCall(Transpose(), FunCall(TransposeW(), arg)) => arg
    case FunCall(TransposeW(), FunCall(TransposeW(), arg)) => arg
  })

  val joinSplitId = Rule("Join() o Split(_) => id", {
    case FunCall(Join(), FunCall(Split(_), arg)) => arg
  })

  val splitJoinId = Rule("Split(_) o Join() => id", {
    case FunCall(Split(n), FunCall(Join(), arg))
      if (arg.t match {
        case ArrayType(ArrayType(_, m), _) => n == m
        case _ => false
      }) => arg
  })

  val scatterGatherId = Rule("Scatter(f) o Gather(f) => id", {
    case FunCall(Scatter(f1), FunCall(Gather(f2), arg))
      if f1 == f2
    => arg
  })

  val gatherScatterId = Rule("Gather(f) o Scatter(f) => id", {
    case FunCall(Gather(f1), FunCall(Scatter(f2), arg))
      if f1 == f2
    => arg
  })

  /* Fusion Rules */

  val reduceSeqMapSeqFusion = Rule("ReduceSeq o MapSeq => ReduceSeq(fused)", {
    case FunCall(ReduceSeq(f: Lambda2), init, FunCall(MapSeq(g : Lambda1), arg))
    =>
      val acc = f.params.head
      val a = Param()
      val newG = Expr.replace(g.body, g.params.head, a)
      val newF = Expr.replace(f.body, f.params(1), newG)
      ReduceSeq(new Lambda2(Array(acc, a), newF), init) $ arg
  })

  val mapFusion = Rule("Map(f) o Map(g) => Map(f o g)", {
    case FunCall(Map(Lambda(p1, f)), FunCall(Map(Lambda(p2, g)), arg)) =>
      val newLambda = Lambda(p2, Expr.replace(f, p1.head, g))
      Map(newLambda) $ arg
  })

  /* Map rules */

  val mapSeq = Rule("Map(f) => MapSeq(f)", {
    case FunCall(Map(f), arg)
      if f.body.isConcrete
    => MapSeq(f)(arg)
  })

  val mapGlb: Rule = mapGlb(0)

  def mapGlb(dim: Int): Rule =  Rule("Map(f) => MapGlb(f)", {
    case call@FunCall(Map(f), arg)
      // check that none of these are nested inside
      if !f.body.contains({
          case FunCall(MapGlb(dimNested, _), _) if dim == dimNested =>
          case FunCall(_: MapWrg, _) =>
          case FunCall(_: MapLcl, _) =>
          case FunCall(_: MapWarp, _) =>
          case FunCall(_: MapLane, _) =>
        })
        && f.body.isConcrete
        && !(call.context.inMapGlb(dim) ||
             call.context.inMapWrg.reduce(_ || _) ||
             call.context.inMapWarp)
    =>
      MapGlb(f)(arg)
  })

  def mapWrg(dim: Int): Rule = Rule("Map(f) => MapWrg(f)", {
    case call@FunCall(Map(f), arg)
      // check that there is a nested map inside ...
      if f.body.contains({
        case FunCall(_: Map, _) =>
        })
        // and that none of these are nested inside
        && !f.body.contains({
          case FunCall(_: MapGlb, _) =>
          case FunCall(MapWrg(dimNested, _), _) if dim == dimNested =>
        })
        && f.body.isConcrete
        && !(call.context.inMapGlb.reduce(_ || _) ||
             call.context.inMapWrg(dim) ||
             call.context.inMapWarp)
    =>
      MapWrg(dim)(f)(arg)
  })

  val mapWrg: Rule = mapWrg(0)

  def mapLcl(dim: Int): Rule = Rule("Map(f) => MapLcl(f)", {
    case call@FunCall(Map(f), arg)
      // check that none of these are nested inside
      if !f.body.contains({
          case FunCall(MapLcl(dimNested, _), _) if dim == dimNested =>
          case FunCall(_: MapWarp, _) =>
          case FunCall(_: MapLane, _) =>
        })
        && f.body.isConcrete
        && call.context.inMapWrg(dim)
        && !call.context.inMapLcl(dim)
    =>
      MapLcl(dim)(f)(arg)
  })

  val mapLcl: Rule = mapLcl(0)

  val mapWarp = Rule("Map(f) => MapWarp(f)", {
    case call@FunCall(Map(f), arg)
      // check if there is a nested map inside
      if f.body.contains({
          case FunCall(_: Map, _) =>
        })
        // and that none of these are nested inside
        && !f.body.contains({
          case FunCall(_: MapGlb, _) =>
          case FunCall(_: MapWrg, _) =>
          case FunCall(_: MapLcl, _) =>
          case FunCall(_: MapWarp, _) =>
          case FunCall(_: MapLane, _) =>
        })
        && f.body.isConcrete
        && !(call.context.inMapGlb.reduce(_ || _) ||
             call.context.inMapWrg.reduce(_ || _) ||
             call.context.inMapWarp)
    => MapWarp(f)(arg)
  })

  val mapLane = Rule("Map(f) => MapLane(f)", {
    case call@FunCall(Map(f), arg)
      // check that none of these are nested inside
      if !f.body.contains({
        case FunCall(_: MapLane) =>
        })
        && f.body.isConcrete
        && call.context.inMapWarp
        && !call.context.inMapLane
    =>
      MapLane(f)(arg)
  })

  /* Reduce Rule */

  val reduceSeq = Rule("Reduce(f) => ReduceSeq(f)", {
    case FunCall(Reduce(f), init, arg) =>

      // Construct id functions using the type of init
      val idFunction: FunDecl = generateId(init.t)

      val newInit = if (init.isInstanceOf[Value]) idFunction $ init else init

      // TODO: address space if last?
      Id() o ReduceSeq(f, newInit) $ arg
  })

  /* Stride accesses or normal accesses */

  // TODO

  /* Address space rules */

  val privateMemory = Rule("Map(f) => toPrivate(Map(f))", {
    case FunCall(f: AbstractMap, arg)
      if f.isInstanceOf[MapLcl] || f.isInstanceOf[MapSeq]
    =>
      toPrivate(f) $ arg
  })

  val localMemory = Rule("Map(f) => toLocal(Map(f))", {
    case FunCall(f: AbstractMap, arg)
      if f.isInstanceOf[MapLcl] || f.isInstanceOf[MapSeq]
    =>
      toLocal(f) $ arg
  })

  val globalMemory = Rule("Map(f) => toGlobal(Map(f))", {
    case FunCall(f: AbstractMap, arg)
      if f.isInstanceOf[MapLcl] || f.isInstanceOf[MapSeq]
    =>
      toGlobal(f) $ arg
  })

  /* Vectorization rule */

  val vectorize = Rule("Map(uf) => asScalar() o MapGlb(Vectorize(4)(uf)) o asVector(4)", {
    case FunCall(Map(Lambda(_, FunCall(uf: UserFun, _))), arg) =>
      asScalar() o Map(Vectorize(4)(uf)) o asVector(4) $ arg
  })

  /* Other */

  val mapFission = Rule("Map(f o g) => Map(f) o Map(g)", {
    case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(fun2, p2)))), arg)
      if p2.contains({ case a => a eq p1.head })
    =>
      Map(fun1) o Map(Lambda(p1, fun2(p2))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(Reduce(fun1), init, FunCall(fun2, p2)))), arg)
      if p2.contains({ case a => a eq p1.head })
    =>
      Map(Reduce(fun1, init)) o Map(Lambda(p1, fun2(p2))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(Reduce(fun2), init, p2)))), arg)
      if p2.contains({ case a => a eq p1.head })
    =>
      Map(fun1) o Map(Lambda(p1, Reduce(fun2, init)(p2))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(Reduce(fun1), init1, FunCall(Reduce(fun2), init2, p2)))), arg)
      if p2.contains({ case a => a eq p1.head })
    =>
      Map(Reduce(fun1, init1)) o Map(Lambda(p1, Reduce(fun2, init2)(p2))) $ arg
  })

  val mapMapInterchange = Rule("Map(fun(a => Map(fun( b => ... ) $ B) $ A => " +
    "Transpose() o Map(fun(b => Map(fun( a => ... ) $ A) $ B", {
    case FunCall(Map(Lambda(a, FunCall(Map(Lambda(b, expr)), bArg))), aArg)
      if !bArg.contains({
        case e if e eq a.head =>
      })
    =>
      TransposeW() o Map(Lambda(b, FunCall(Map(Lambda(a, expr)), aArg))) $ bArg
  })

  val mapReduceInterchange = Rule("Map(Reduce(f)) => Transpose() o Reduce(Map(f)) o Transpose()", {
    case FunCall(Map(Lambda(lambdaParams,
          FunCall(Reduce(Lambda(innerParams, expr)), init: Value, arg)
         )), mapArg)
      if lambdaParams.head eq arg
    =>
      val newInit = Value(init.value, ArrayType(init.t, Type.getLength(mapArg.t)))

      val newMapParam = Param()
      val newExpr = innerParams.zipWithIndex.foldLeft(expr)((e, pair) =>
        Expr.replace(e, pair._1, Get(pair._2)(newMapParam)))

      TransposeW() o Reduce(fun((acc, c) => Map(Lambda(Array(newMapParam), newExpr)) $ Zip(acc, c)),
        newInit) o Transpose() $ mapArg
  })

  val mapReduceInterchangeWithZip =
    Rule("Map(fun(x => Reduce(f, Get(x, 0)) $ Get(x, 1) ) $ Zip(a, b) => " +
         "Transpose() o Reduce(fun((acc, y) => Map(f) $ Zip(acc, y) ), a ) o Transpose() $ b", {
      case FunCall(Map(Lambda(lambdaParams,
      FunCall(Reduce(Lambda(innerParams, expr)), FunCall(Get(i), a1), FunCall(Get(j), a2))
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

        Transpose() o Reduce(Lambda(Array(acc, next),
          Map(Lambda(Array(mapParam), finalExpr)) $ Zip(acc, next)
        ), newInit) o Transpose() $ newArg
    })

  val mapReducePartialReduce =
    Rule("Map(Reduce(f, init) o Join() o Map(PartRed(f, init2)) ) => " +
      "Transpose() o Reduce((acc, a) => Join() o Map(x => PartRed(f, Get(x, 0)) $ Get(x, 1)) $ Zip(acc, a) , Array(init)) o Transpose()", {
      case c@ FunCall(Map(Lambda(p1,
      FunCall(Reduce(f1), init: Value, FunCall(Join(), FunCall(Map(Lambda(p2,
        FunCall(PartRed(f2), _, a2))), a1)))
      )), arg)
        if (p1.head eq a1) && (p2.head eq a2)
      =>
        val newInit = Value(init.value, ArrayType(init.t, Type.getLength(arg.t)))

        TransposeW() o Reduce(fun((acc, a) =>
          Join() o Map(fun(x =>
            PartRed(f1, Get(x, 0)) $ Get(x,1)
          )) $ Zip(acc, a)
        ), newInit) o Transpose() $ arg
    })

  val reorderBothSidesWithStride: Rule = reorderBothSidesWithStride(?)

  def reorderBothSidesWithStride(stride: ArithExpr): Rule = {
    Rule("Map(f) => Reorder(g^{-1}) o Map(f) o Reorder(g)", {
      case FunCall(map@Map(_), arg) =>
        val s = if (stride == ?) validSplitVariable(arg.t) else stride
        Scatter(ReorderWithStride(stride)) o map o Gather(ReorderWithStride(s)) $ arg
    })
  }

  def getFinalArg(expr: Expr): Expr = {
    expr match {
      case FunCall(_, arg) => getFinalArg(arg)
      case FunCall(r : AbstractPartRed, _, arg) => arg
      case _ => expr
    }
  }

  // TODO: restrict to Map(Map(f)) and create a special case for Map(Reduce)?
  val transposeBothSides = Rule("Map(fun(a => ... $ a)) $ A => " +
    "Transpose() o Map(fun(a =>... $ a)) o Transpose() $ A  ", {
    case outerCall@FunCall(Map(f@Lambda(lambdaArg, innerCall@FunCall(_,_))), arg)
      if (lambdaArg.head eq getFinalArg(innerCall))
        && (outerCall.t match {
        case ArrayType(ArrayType(_, _), _) => true
        case _ => false
      })
        && (arg.t match {
        case ArrayType(ArrayType(_, _), _) => true
        case _ => false
      })
        && !innerCall.contains({
        case splitCall@FunCall(Split(_), _)
          if NumberExpression.byDepth(outerCall)(splitCall) < 2
        =>
        case c @ FunCall(Join(), _)
          if (c.args.head.t match {
          case ArrayType(ArrayType(_, lenInner), lenOuter) =>
            lenInner != Cst(1) && lenOuter != Cst(1)
          case _ => true
        }) =>
      })
    =>
      TransposeW() o Map(f) o Transpose() $ arg
  })

  val mapMapTransposeZipInside = Rule("Map(fun(a => Map() $ Zip(..., a, ...)) $ A => " +
    "Transpose() o Map(Map(fun(a => ))) $ Zip(..., Transpose() $ A, ...) ", {
    case FunCall(Map(
    Lambda(outerLambdaParam, FunCall(Map(
    Lambda(innerLambdaParam, expr
    )), FunCall(Zip(_), zipArgs@_*))
    )), arg)
      if zipArgs.contains(outerLambdaParam.head)
    =>
      // Find all Get patterns that refer to the an element from the zipped array
      // and have to be replaced in expr
      val gets = findGets(expr, innerLambdaParam.head)

      // Find which Get pattern corresponds to the component containing an element of 'a'
      val zipToReplace = zipArgs.zipWithIndex.find(e => e._1 eq outerLambdaParam.head).get

      // Create the new Get patterns with a new parameter
      val newParam = Param()
      val getPairs = gets zip gets.map(get => Get(newParam, get.f.asInstanceOf[Get].n))

      // Separate the Get pattern containing an element of 'a', as it will now refer
      // to the component containing an element of 'A'
      val (one, two) = getPairs.partition(_._1.f.asInstanceOf[Get].n == zipToReplace._2)

      // Replace most of the old Get patterns with new ones
      val newExpr = two.foldRight(expr)((get, e) => Expr.replace(e, get._1, get._2))

      // Create a new parameter for an element of 'a' and replace for the Get referring
      // an element of 'a'
      val secondNewParam = Param()
      val finalNewExpr = Expr.replace(newExpr, one.head._1, secondNewParam)

      // Create the arguments for the zip, replacing 'a' with 'Transpose() $ A'
      val newZipArgs = zipArgs.updated(zipToReplace._2, Transpose() $ arg)

      // Construct the final expression
      val lambda = Lambda(Array(secondNewParam), finalNewExpr)
      Transpose() o Map(Lambda(Array(newParam), Map(lambda) $ one.head._2)) $ Zip(newZipArgs: _*)
  })

  val mapMapTransposeZipOutside = Rule("Map(fun(x => Map(fun(a => )) $ Get(n, ) $ Zip(..., A, ...)  => " +
    "Transpose() o Map(fun(a => Map() $ Zip(..., a, ...)) o Transpose() $ A", {
    case c@FunCall(Map(Lambda(outerLambdaParam,
    FunCall(Map(Lambda(innerLambdaParam,
    expr
    )), getCall@FunCall(Get(n), getParam))
    )), FunCall(Zip(_), zipArgs@_*))

      if getParam eq outerLambdaParam.head
    =>
      // Find all Get patterns that refer to the an element from the zipped array
      // and have to be replaced in expr
      val gets = findGets(expr, outerLambdaParam.head)

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

      Transpose() o Map(Lambda(Array(secondNewParam),
        Map(Lambda(Array(newParam), finalNewExpr)) $ Zip(newZipArgs: _*)
      )) o Transpose() $ newArg
  })

  val mapFusionWithZip =
    Rule("Map(fun(x => f $ arg )) $ Zip( ..., Map(g) , ...)", {
      case FunCall(Map(f@Lambda(p, call@FunCall(_, arg))), FunCall(Zip(_), zipArgs@_*))
        if arg.contains({
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
        val gets = findGets(call, p.head)
        val newGets = gets.map({
          case FunCall(Get(i), _) if i == mapInZipIndex => g $ Get(newLambdaParam, i)
          case FunCall(Get(i), _) => Get(newLambdaParam, i)
        })

        val newCall = (gets, newGets).zipped.foldLeft(call: Expr)((current, pair) =>
          Expr.replace(current, pair._1, pair._2))

        Map(Lambda(Array(newLambdaParam), newCall)) $ Zip(newZipArgs:_*)
    })

  val mapSplitTranspose = Rule("Map(Split(n)) o Transpose()" +
                               "Transpose() o Map(Transpose()) o Split(n)", {
    case FunCall(Map(Lambda(param, FunCall(Split(n), a))), FunCall(Transpose(), arg))
      if param.head eq a
    =>
      Transpose() o Map(Transpose()) o Split(n) $ arg
  })

  val mapTransposeSplit = Rule("Map(Transpose()) o Split(n)" +
    "Transpose() o Map(Split(n)) o Transpose()", {
    case FunCall(Map(Lambda(param, FunCall(Transpose(), a))), FunCall(Split(n), arg))
      if param.head eq a
    =>
      Transpose() o Map(Split(n)) o Transpose() $ arg
  })

  val transposeMapSplit = Rule("Transpose() o Map(Split(n))" +
    "Map(Transpose()) o Split(n) o Transpose()", {
    case FunCall(Transpose(), FunCall(Map(Lambda(param, FunCall(Split(n), a))), arg))
      if param.head eq a
    =>
      Map(Transpose()) o Split(n) o Transpose() $ arg
  })

  val splitTranspose = Rule("Split(n) o Transpose()" +
        "Map(Transpose()) o Transpose() o Map(Split(n))", {
        case FunCall(Split(n), FunCall(Transpose(), arg)) =>
          Map(Transpose()) o Transpose() o Map(Split(n)) $ arg
  })

  val mapTransposeTransposeMapTranspose =
    Rule("Map(Transpose()) o Transpose() o Map(Transpose())) => " +
         "Transpose() o Map(Transpose()) o Transpose()", {
      case FunCall(Map(Lambda(param1, FunCall(Transpose(), a1))),
            FunCall(Transpose(),
            FunCall(Map(Lambda(param2, FunCall(Transpose(), a2))), arg)))
        if (param1.head eq a1) && (param2.head eq a2)
      =>
        Transpose() o Map(Transpose()) o Transpose() $ arg

      case FunCall(Map(Lambda(param1, FunCall(Transpose(), a1))),
          FunCall(TransposeW(),
          FunCall(Map(Lambda(param2, FunCall(Transpose(), a2))), arg)))
        if (param1.head eq a1) && (param2.head eq a2)
      =>
        Transpose() o Map(Transpose()) o Transpose() $ arg
    })

  val splitZip = Rule("Map(fun(x => Map()  ) o Split() $ Zip(...) => " +
                      "Map(x => Map() $ Zip(Get(n, x) ... ) $ Zip(Split $ ...)", {
    case FunCall(Map(Lambda(lambdaParam, FunCall(Map(mapLambda), mapArg))), FunCall(Split(n), FunCall(Zip(_), zipArgs@_*)))
      if lambdaParam.head eq mapArg
    =>
      val newZipArgs = zipArgs.map(arg => Split(n) $ arg)

      val newLambdaParam = Param()

      val innerZipArgs = zipArgs.indices.map(Get(_)(newLambdaParam))

      Map(Lambda(Array(newLambdaParam), Map(mapLambda) $ Zip(innerZipArgs:_*))) $ Zip(newZipArgs:_*)
  })

  val mapFissionWithZipInside = Rule("Map(fun(x => ... Zip(..., f $ x, ...))) " +
                               "Map(fun(y => ... Zip(..., y, ...))) o Map(f)", {
    case FunCall(Map(Lambda(lambdaParam, body)), arg)
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

      FunCall(Map(Lambda(Array(newLambdaParam), newBody)), FunCall(Map(Lambda(Array(newMapLambdaParam), newExpr)), arg))
  })

  val mapFissionWithZipOutside = Rule("Map(fun(x => ...o f $ Get(x, i))) $ Zip(..., y, ...) " +
    "Map(fun(z => ... $ Get(z, i)) $ Zip(..., Map(f) $ y, ...)", {
    case FunCall(Map(Lambda(lambdaParam,
           c@FunCall(dots, FunCall(f, FunCall(Get(i), x)))
        )), FunCall(Zip(n), zipArgs@_*))
      if lambdaParam.head eq x
    =>
      val newZipArgs = zipArgs.updated(i, Map(f) $ zipArgs(i))
      val newLambdaParam = Param()
      val newDots = Expr.replace(c, x, newLambdaParam).asInstanceOf[FunCall].f

      Map(Lambda(Array(newLambdaParam), newDots(Get(newLambdaParam, i)))) $ Zip(newZipArgs:_*)

    case FunCall(Map(Lambda(lambdaParam,
    c@FunCall(dots: AbstractPartRed, init, FunCall(f, FunCall(Get(i), x)))
    )), FunCall(Zip(n), zipArgs@_*))
      if lambdaParam.head eq x
    =>
      val newZipArgs = zipArgs.updated(i, Map(f) $ zipArgs(i))
      val newLambdaParam = Param()
      val newDots = Expr.replace(c, x, newLambdaParam).asInstanceOf[FunCall].f
      val newInit = Expr.replace(init, x, newLambdaParam)

      Map(Lambda(Array(newLambdaParam), newDots(newInit, Get(newLambdaParam, i)))) $ Zip(newZipArgs:_*)
  })

  def containsParam(expr: Expr, param: Param): Boolean =
    expr.contains({
      case FunCall(_, p: Param) if p eq param =>
    })

  private def generateCopy(t: Type): FunDecl = {
    t match {
      case TupleType(tt@_*) =>
        val newParam = Param()
        val argSequence = tt.zipWithIndex.map(p => generateCopy(p._1) $ Get(newParam, p._2))
        Lambda(Array(newParam), Tuple(argSequence:_*))
      case ArrayType(elemT, _) =>
        Map(generateCopy(elemT))
      case _ => generateId(t)
    }
  }

  private def generateId(t: Type): FunDecl = {
    t match {
      case TupleType(tt@_*) =>
        val newParam = Param()
        val argSequence = tt.zipWithIndex.map(p => Id() $ Get(newParam, p._2))
        Lambda(Array(newParam), Tuple(argSequence:_*))
      case ArrayType(elemT, _) =>
        Map(Id())
      case ScalarType(_, _) | VectorType(_, _) =>
        UserFun("id" + t, "x", "{ return x; }", t, t)
      case _ => throw TypeException(s"Can't generate id function for $t")
    }
  }

  val dropId = Rule("Id() => Epsilon()", {
    case FunCall(Id(), arg) =>
      arg
  })

  val implementOneLevelOfId = Rule("Id() => ", {
    case FunCall(Id(), arg) =>
      generateId(arg.t) $ arg
  })

  val implementIdAsDeepCopy = Rule("Id() => ", {
    case FunCall(Id(), arg) =>
      generateCopy(arg.t) $ arg
  })

  val addId = Rule("f => f o Id()", {
    case FunCall(f, arg) =>
      f o Id() $ arg
  })

  val addIdForCurrentValueInReduce = Rule("", {
    case call@FunCall(ReduceSeq(l), _, _) =>
      val params = l.params
      val body = l.body
      val newParam = Param()

      val paramForCurrent = params(1)

      val tempBody = Expr.replace(body, paramForCurrent, newParam)

      val newBody = Lambda(Array(newParam), tempBody) o Id() $ paramForCurrent

      Expr.replace(call, body, newBody)
  })

  val addCopy = Rule("f => f o Id()", {
    case FunCall(f, arg) =>
      f o generateCopy(arg.t) $ arg
  })

  val tupleMap = Rule("Tuple(Map(f) $ .. , Map(g) $ .., ...) => " +
    "Unzip() o Map(x => Tuple(f $ Get(x, 0), g $ Get(x, 1), ...) $ Zip(...) ", {
    case FunCall(Tuple(_), args@_*)
      if args.forall({
        case arg@FunCall(Map(_), _) => arg.t.isInstanceOf[ArrayType]
        case _ => false
      }) && args.map(_.t.asInstanceOf[ArrayType].len).distinct.length == 1
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

  val tupleFission =
    Rule("Tuple(f $... , g $ ..., ...) => " +
         "Tuple(f $ Get( , 0), g $ Get( , 1), ...) o Tuple(...)", {
    case call@FunCall(Tuple(_), args@_*)
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

  val iterate1 = Rule("Iterate(1, x) => x", {
    case FunCall(Iterate(n, f), arg) if n.eval == 1 => f(arg)
  })

  val iterateId = Rule("Iterate(0, _) => Epsilon", {
    case FunCall(Iterate(n, _), arg) if n.eval == 0 => arg
  })

  val removeEmptyMap = Rule("Map(fun(x => x)) $ a => a", {
    case FunCall(Map(Lambda(params, body)), arg)
      if params.head eq body
    =>
      arg
  })

  /* Macro rules */

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

      val replacement = mapFission.rewrite(nextFission)
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

      fused = mapFusion.rewrite(fused)

      currentPos -= 1
    }

    fused
  }

  val tileMapMap: (Int, Int) => Rule = (x, y) =>
    Rule("Tile a computation in the form Map(fun(y => Map(f) $ y )) $ x", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, FunCall(Map(_), arg))), _)
        if lambdaParam.head eq arg
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 0, splitJoin(x))
        val e1 = Rewrite.depthFirstApplyRuleAtId(e0, 2, transposeBothSides)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 3, splitJoin(y))
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 5, transposeBothSides)
        e3
    })

  val tileOutput: Int => Rule = x =>
    Rule("Tile the output of a computation in the form " +
         "Map(fun(y => Map(f) $ z )) $ x", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, FunCall(Map(_), arg))), _)
        if !(lambdaParam.head eq arg)
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 0, splitJoin(x))
        val e1 = Rewrite.depthFirstApplyRuleAtId(e0, 2, mapMapInterchange)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 3, splitJoin(x))
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 5, mapMapInterchange)
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
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 0, splitJoin(x))
        val e1 = Rewrite.applyRuleAtId(e0, 1, splitZip)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 2, mapMapTransposeZipOutside)
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 4, mapMapTransposeZipOutside)
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
        val e1 = Rewrite.applyRuleAtId(funCall, 0, Rules.tileOutput(x))

        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 7, Rules.mapFission)
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 14, Rules.mapMapTransposeZipInside)
        val e4 = Rewrite.depthFirstApplyRuleAtId(e3, 6, Rules.mapFissionAtPosition(1))
        val e5 = Rewrite.depthFirstApplyRuleAtId(e4, 16, Rules.mapMapTransposeZipInside)

        val e6 = Rewrite.depthFirstApplyRuleAtId(e5, 17, Rules.finishTilingInput(y))

        e6
    })

  val moveTransposeInsideTiling =
    Rule("Map(Split(n) o Transpose()) o Split(m) o Transpose() => " +
         "Transpose() o Map(Transpose()) o Split(n) o Map(Split(m))", {
      case funCall @
        FunCall(Map(Lambda(_, FunCall(Split(_), FunCall(Transpose(), _)))),
        FunCall(Split(_), FunCall(Transpose(), _)))
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 4, splitTranspose)
        val e1 = Rewrite.depthFirstApplyRuleAtId(e0, 0, mapFusion)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 2, transposeTransposeId)
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 0, mapSplitTranspose)
        e3
    })

  val tileTranspose: (Int, Int) => Rule = (x, y) =>
    Rule("Map(Map(f)) o Transpose() => tiled", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, FunCall(Map(_), arg))), FunCall(Transpose(), _))
        if lambdaParam.head eq arg
      =>
        val e1 = Rewrite.applyRuleAtId(funCall, 0, Rules.tileMapMap(x, y))
        val e2 = Rewrite.applyRuleAtId(e1, 1, Rules.mapFissionAtPosition(2))
        val e3 = Rewrite.applyRuleAtId(e2, 2, Rules.moveTransposeInsideTiling)
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
        val e0 = Rewrite.applyRuleAtId(funCall, 1, Rules.mapFissionAtPosition(1))
        val e1 = Rewrite.applyRuleAtId(e0, 2, mapSplitTranspose)
        val e2 = Rewrite.applyRuleAtId(e1, 1, transposeBothSides)
        val e3 = Rewrite.applyRuleAtId(e2, 0, transposeTransposeId)
        val e4 = Rewrite.applyRuleAtId(e3, 1, transposeTransposeId)
        e4
    })

  val reduceMapFusion = Rule("Reduce o Map => ReduceSeq(fused)", {
    case funCall @ FunCall(Reduce(_), _, FunCall(Map(_), _))
    =>
      val e0 = Rewrite.applyRuleAtId(funCall, 1, mapSeq)
      val e1 = Rewrite.applyRuleAtId(e0, 0, reduceSeq)
      val e2 = Rewrite.applyRuleAtId(e1, 1, reduceSeqMapSeqFusion)
      e2
  })

  private def findGets(expr: Expr, tupleParam: Expr): List[FunCall] = {
    Expr.visitWithState(List[FunCall]())(expr, (e, s) => {
      e match {
        case get@FunCall(Get(_), getParam) if getParam eq tupleParam => get :: s
        case _ => s
      }
    })
  }

  private def validSplitVariable(t: Type): ArithExpr = {
    t match {
      case ArrayType(_, len) => Var(RangeMul(Cst(1), len, Cst(2)))
      case _ => throw new TypeException(t, "ArrayType")
    }
  }
}
