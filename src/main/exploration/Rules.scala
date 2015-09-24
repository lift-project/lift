package exploration

import apart.arithmetic._
import exploration.utils.Utils
import ir._
import ir.ast._
import opencl.ir.pattern._

case class Rule(desc: String,
                rewrite: PartialFunction[Expr, Expr]) {

  def isDefinedAt(expr: Expr) = rewrite.isDefinedAt(expr)

  override def toString: String = desc
}

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
      val chunkSize = if (split == ?) Utils.validSplitVariable(arg.t) else split
      Join() o Map(Map(f)) o Split(chunkSize) $ arg
  })

  /* Reduce rules */

  val partialReduce = Rule("Reduce(f) => Reduce(f) o PartRed(f)", {
    case FunCall(Reduce(f), init, arg) =>

      // Need to replace the parameters for the partial reduce
      // Otherwise the 2 will end up sharing and give the wrong result
      val newAcc = Param()
      val newElem = Param()

      val expr = Expr.replace(f.body, f.params.head, newAcc)
      val finalExpr = Expr.replace(expr, f.params(1), newElem)

      Reduce(f, init) o PartRed(Lambda(Array(newAcc, newElem), finalExpr), init) $ arg
  })

  val partialReduceToReduce = Rule("PartRed(f) => Reduce(f)", {
    case FunCall(PartRed(f), init, arg) =>
      Reduce(f, init) $ arg
  })

  val partialReduceReorder: Rule = partialReduceReorder(?)

  def partialReduceReorder(s: ArithExpr): Rule =
    Rule("PartRed(f) => PartRed(f) o Reorder", {
      case FunCall(PartRed(f), init, arg) =>
        val stride = if (s == ?) Utils.validSplitVariable(arg.t) else s
        PartRed(f, init) o Gather(ReorderWithStride(stride)) $ arg
    })

  // TODO: iterate

  val partialReduceSplitJoin: Rule = partialReduceSplitJoin(?)

  def partialReduceSplitJoin(split: ArithExpr): Rule =
    Rule("PartRed(f) => Join() o Map(PartRed(f)) o Split()", {
      case FunCall(PartRed(f), init, arg) =>
        val chunkSize = if (split == ?) Utils.validSplitVariable(arg.t) else split
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
    case FunCall(t1, FunCall(t2, arg))
      if isTranspose(t1) && isTranspose(t2)
    =>
      arg
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
      MapGlb(dim)(f)(arg)
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
      val idFunction: FunDecl = generateCopy(init.t)

      val newInit = if (init.isInstanceOf[Value]) idFunction $ init else init

      MapSeq(Id()) o ReduceSeq(f, newInit) $ arg
  })

  /* Stride accesses or normal accesses */

  // TODO

  /* Address space rules */

  val privateMemoryId = Rule("Id => toPrivate(Id)", {
    case call@FunCall(Id(), arg)
      if call.context.inMapLcl.reduce(_ || _)
    =>
      toPrivate(Id()) $ arg
  })

  val privateMemory = Rule("Map(f) => toPrivate(Map(f))", {
    case FunCall(f: AbstractMap, arg)
      if f.isInstanceOf[MapLcl] || f.isInstanceOf[MapSeq]
    =>
      toPrivate(f) $ arg
  })


  val localMemory = Rule("Map(f) => toLocal(Map(f))", {
    case call@FunCall(f: AbstractMap, arg)
      if call.context.inMapWrg.reduce(_ || _)
    =>
      toLocal(f) $ arg
  })

  val localMemoryId = Rule("Id => toLocal(Id())", {
    case call@FunCall(Id(), arg)
      if call.context.inMapWrg.reduce(_ || _)
    =>
      toLocal(Id()) $ arg
  })

  val globalMemory = Rule("Map(f) => toGlobal(Map(f))", {
    case FunCall(f: AbstractMap, arg) =>
      toGlobal(f) $ arg
  })

  /* Vectorization rule */

  val vectorize: Rule = vectorize(?)

  def vectorize(vectorWidth: ArithExpr): Rule =
    Rule("Map(uf) => asScalar() o Map(Vectorize(n)(uf)) o asVector(4)", {
      case FunCall(Map(Lambda(p, FunCall(uf: UserFun, ufArg))), arg)
        if (p.head eq ufArg) && !ufArg.t.isInstanceOf[VectorType]
      =>
        // TODO: force the width to be less than the array length
        val n = if (vectorWidth == ?) Var(RangeMul(2, 16, 2)) else vectorWidth
        asScalar() o Map(VectorizeUserFun(n, uf)) o asVector(n) $ arg
    })

  /* Other */

  val mapFission = Rule("Map(f o g) => Map(f) o Map(g)", {
    case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(fun2, p2)))), arg)
      if p2.contains({ case a => a eq p1.head })
    =>
      Map(fun1) o Map(Lambda(p1, fun2(p2))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(r: AbstractPartRed, init, FunCall(fun2, p2)))), arg)
      if p2.contains({ case a => a eq p1.head })
    =>
      Map(Reduce(r.f, init)) o Map(Lambda(p1, fun2(p2))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(r: AbstractPartRed, init, p2)))), arg)
      if p2.contains({ case a => a eq p1.head })
    =>
      Map(fun1) o Map(Lambda(p1, Reduce(r.f, init)(p2))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(r1: AbstractPartRed, init1,
                FunCall(r2: AbstractPartRed, init2, p2)))), arg)
      if p2.contains({ case a => a eq p1.head })
    =>
      Map(Reduce(r1.f, init1)) o Map(Lambda(p1, Reduce(r2.f, init2)(p2))) $ arg
  })

  val mapMapInterchange = Rule("Map(fun(a => Map(fun( b => ... ) $ B) $ A => " +
    "Transpose() o Map(fun(b => Map(fun( a => ... ) $ A) $ B", {
    case FunCall(Map(Lambda(a, FunCall(Map(Lambda(b, expr)), bArg))), aArg)
      if !bArg.contains({ case e if e eq a.head => })
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

  val mapReduceInterchangeWithZipOutside =
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
        val s = if (stride == ?) Utils.validSplitVariable(arg.t) else stride
        Scatter(ReorderWithStride(s)) o map o Gather(ReorderWithStride(s)) $ arg
    })
  }

  val transposeBothSides = Rule("Map(fun(a => Map(f) $ a)) $ A => " +
    "Transpose() o Map(fun(a =>Map(f) $ a)) o Transpose() $ A  ", {
    case FunCall(Map(f@Lambda(param, FunCall(Map(_), a))), arg)
      if param.head eq a
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
      val gets = Utils.findGets(expr, innerLambdaParam.head)

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

      Transpose() o Map(Lambda(Array(secondNewParam),
        Map(Lambda(Array(newParam), finalNewExpr)) $ Zip(newZipArgs: _*)
      )) o Transpose() $ newArg
  })

  val mapFusionWithZip =
    Rule("Map(fun(x => f $ arg )) $ Zip( ..., Map(g) , ...)", {
      case FunCall(Map(f@Lambda(p, call@FunCall(_, args@ _* ))), FunCall(Zip(_), zipArgs@_*))
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
          case FunCall(Get(i), _) if i == mapInZipIndex => g $ Get(newLambdaParam, i)
          case FunCall(Get(i), _) => Get(newLambdaParam, i)
        })

        val newCall = (gets, newGets).zipped.foldLeft(call: Expr)((current, pair) =>
          Expr.replace(current, pair._1, pair._2))

        Map(Lambda(Array(newLambdaParam), newCall)) $ Zip(newZipArgs:_*)
    })

  def isTranspose(funDecl: FunDecl) =
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

      FunCall(Map(Lambda(Array(newLambdaParam), newBody)),
        FunCall(Map(Lambda(Array(newMapLambdaParam), newExpr)), arg))
  })

  private val getCallPattern: PartialFunction[Expr, Unit] = { case FunCall(_, FunCall(Get(_), _)) => }

  val mapFissionWithZipOutside = Rule("Map(fun(x => ...o f $ Get(x, i))) $ Zip(..., y, ...) " +
    "Map(fun(z => ... $ Get(z, i)) $ Zip(..., Map(f) $ y, ...)", {
    case FunCall(Map(Lambda(lambdaParam,
           c@FunCall(dots, arg)
        )), FunCall(Zip(n), zipArgs@_*))
      if isMapFissionWithZipOutsideValid(lambdaParam, arg)
    =>
      applyMapFissionWithZipOutside(c, arg, zipArgs)

    case FunCall(Map(Lambda(lambdaParam,
            c@FunCall(dots: AbstractPartRed, init, arg@FunCall(f, FunCall(Get(i), x)))
         )), FunCall(Zip(n), zipArgs@_*))
      if isMapFissionWithZipOutsideValid(lambdaParam, arg)
    =>
      applyMapFissionWithZipOutside(c, arg, zipArgs)

  })

  private def applyMapFissionWithZipOutside(c: FunCall, arg: Expr, zipArgs: Seq[Expr]): Expr = {
    val toBeReplaced: Expr = Utils.getExprForPatternInCallChain(arg, getCallPattern).get

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

  def isId(expr: Expr): Boolean =
    expr match {
      case FunCall(Id(), _) => true
      case _ => false
    }

  val addIdMapLcl = Rule("MapLcl(f) => MapLcl(f) o Id()", {
    case call@FunCall(map:MapLcl, arg)
      if !isId(arg) && !call.context.inMapLcl.reduce(_ || _)
    =>
      map o Id() $ arg
  })

  val addIdForCurrentValueInReduce = Rule("reduce", {
    case call@FunCall(ReduceSeq(l), _, _)
      if !Utils.visitFunCallChainWithState(false)(l.body, (e, b) => isId(e) || b)
      && !l.body.contains( {case FunCall(Id(), a) if a eq l.params(1)=>  })
    =>
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
}

