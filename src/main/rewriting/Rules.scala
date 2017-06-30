package rewriting

import lift.arithmetic._
import rewriting.utils.Utils
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.ast._
import opencl.ir.pattern._

case class Rule(desc: String,
                rewrite: PartialFunction[Expr, Expr]) {

  def isDefinedAt(expr: Expr) = rewrite.isDefinedAt(expr)

  override def toString: String = desc
}

object Rules {

  /* Rules required for 2D stencil rewrite *///TODO find appropriate names

  /* Slide-promotion *///TODO not really because of map(join)... find better name
  val slidePromotion = Rule("Slide(u,v) o Map(Join()) => Map(Map(Join())) o Slide(u,v)",{
    case FunCall(Slide(u,v), FunCall(Map(Lambda(Array(_), FunCall(Join(), joinArg))), arg)) =>
      Map(Map(Join())) o Slide(u,v) $ arg
  })

  val slideSwap = Rule("Slide(u,v) o Map(Map(Slide(n,s))) => Map(Map(Map(Slide(n,s)))) o Slide(u,v)",{
    case FunCall(Slide(u,v), FunCall(Map(Lambda(
      Array(_), FunCall(Map(Lambda(
        Array(_), FunCall(Slide(n,s), slideArg))), mapArg))), arg)) =>
     Map(Map(Map(Slide(n,s)))) o Slide(u,v) $ arg
  })

  val joinSwap = Rule("Join() o Map(Map(f)) => Map(f) o Join()", {
    case FunCall(Join(), FunCall(Map(Lambda(Array(_), FunCall(map@Map(_), _))), arg)) =>
      map o Join() $ arg
  })

  // todo reduce on layer of maps and use map fission before applying this rule
  val transposeSwap = Rule("Map(Map(Map(Transpose()))) o Map(Transpose()) => Map(Transpose()) o Map(Map(Map(Transpose())))", {
    case FunCall(Map(Lambda(Array(_), FunCall(Map(Lambda(Array(_), FunCall(Map(Lambda(Array(_), FunCall(Transpose(),
      transposeArg))), mapArg1))), mapArg2))), FunCall(Map(Lambda(
    Array(_), FunCall(
    Transpose(), transposeArg2))), arg)) =>
      Map(Transpose()) o Map(Map(Map(Transpose()))) $ arg
  })

  // todo reduce on layer of maps and use map fission before applying this rule
  val slideTransposeSwap = Rule("Map(Map(Map(Slide(u,v)))) o Map(Transpose()) => Map(Transpose) o Map(Map(Map(Slide(u,v))))", {
    case FunCall(Map(Lambda(
      Array(_), FunCall(Map(Lambda(
        Array(_), FunCall(Map(Lambda(
          Array(_), FunCall(Slide(u,v), slideArg))), mapArg1))), mapArg2))), FunCall(Map(Lambda(
    Array(_), FunCall(
    Transpose(), transposeArg))), arg)) =>
      Map(Transpose()) o Map(Map(Map(Slide(u,v)))) $ arg
  })

  val slideTransposeReordering = Rule("Map(Slide(u,v)) o Map(Transpose()) => " +
    "Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Slide(u,v)))", {
    case FunCall(Map(Lambda(
      Array(_), FunCall(Slide(u,v), slideArg))), FunCall(Map(Lambda(
        Array(_), FunCall(Transpose(), transposeArg))), arg)) =>
      Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Slide(u,v))) $ arg
  })

  val transposeMapJoinReordering = Rule("Transpose() o Map(Join()) => Join() o Map(Transpose()) o Transpose()", {
    case FunCall(Transpose(), FunCall(Map(Lambda(Array(_), FunCall(Join(), joinArg))), arg)) =>
      Join() o Map(Transpose()) o Transpose() $ arg
  })

  val idTransposeTranspose = Rule("id => Transpose() o Transpose()", {
    case FunCall(Id(), arg) =>
      Transpose() o Transpose() $ arg
  })

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

  /* Reduce rules */

  val partialReduce = Rule("Reduce(f) => Reduce(f) o PartRed(f)", {
    case FunCall(Reduce(f), init: Value, arg) =>

      // Need to replace the parameters for the partial reduce
      // Otherwise the 2 will end up sharing and give the wrong result
      val newAcc = Param()
      val newElem = Param()

      val expr = Expr.replace(f.body, f.params.head, newAcc)
      val finalExpr = Expr.replace(expr, f.params(1), newElem)

      // Make sure both get different objects, otherwise
      // there can be problems with sharing
      val initCopy = init.copy

      Reduce(f, init) o
        PartRed(Lambda(Array(newAcc, newElem), finalExpr), initCopy) $ arg
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
//  val partialReduceIterate: Rule = partialReduceIterate(?)
//
//  def partialReduceIterate(s: ArithExpr): Rule =
//    Rule("PartRed(f) => iterate(s)(PartRed(f))", {
//      case FunCall(PartRed(f), init, arg) =>
//        val iter = if (s == ?) Cst(1) else s
//        Iterate(iter)(PartRed(f, init)) $ arg
//    })


  val partialReduceSplitJoin: Rule = partialReduceSplitJoin(?)

  def partialReduceSplitJoin(split: ArithExpr): Rule =
    Rule("PartRed(f) => Join() o Map(PartRed(f)) o Split()", {
      case FunCall(PartRed(f), init, arg) =>
        val chunkSize = if (split == ?) Utils.validSplitVariable(arg.t) else split
        Join() o Map(PartRed(f, init)) o Split(chunkSize) $ arg
    })
  
  val partialReduceVectorize: Rule = partialReduceVectorize(?)
  
  def partialReduceVectorize(vectorWidth: ArithExpr): Rule =
    Rule("PartRed(f) => Join() o Map(PartRed(f)) o Split()", {
      case FunCall(PartRed(Lambda(_, FunCall(uf:UserFun, _*))), init:Value, arg)
        if !init.t.isInstanceOf[TupleType] && !init.t.isInstanceOf[VectorType] =>
        // TODO: force the width to be less than the array length
        val n = if (vectorWidth == ?) Var(RangeMul(2, 16, 2)) else vectorWidth
        asScalar() o PartRed(VectorizeUserFun(n, uf), init.vectorize(n)) o asVector(n) $ arg
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

  /* Fusion Rules */

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

    case FunCall(Map(l1@Lambda(p1, f)), FunCall(Map(l2@Lambda(p2, g)), arg)) =>
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

    case FunCall(Map(Lambda(p1, f1)),
    call@FunCall(Lambda(_, FunCall(Map(Lambda(_, f2)), _)), _)) =>

      val newBody = Expr.replace(f1, p1.head, f2)

      Expr.replace(call, f2, newBody)
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
             call.context.inMapWrg(dim) ||
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
        && !(call.context.inMapGlb(dim) ||
             call.context.inMapWrg(dim) ||
             call.context.inMapWarp)
    =>
      MapWrg(dim)(f)(arg)
  })

  val mapAtomWrg = Rule("MapWrg(f) => MapAtomWrg(f)", {
    case FunCall(MapWrg(dim, f), arg) => MapAtomWrg(dim)(f)(arg)
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

  val mapAtomLcl = Rule("MapLcl(f) => MapAtomLcl(f)", {
    case FunCall(MapLcl(dim, f), arg) => MapAtomLcl(dim)(f)(arg)
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
      ReduceSeq(f, init) $ arg
  })

  val reduceSeqUnroll = Rule("ReduceSeq(f) => ReduceSeqUnroll(f)", {
    case FunCall(ReduceSeq(f), init, arg) =>
      ReduceSeqUnroll(f, init) $ arg
  })
  /* Stride accesses or normal accesses */

  // TODO

  /* Address space rules */

  val privateMemory = Rule("Map(f) => toPrivate(Map(f))", {
    case call@FunCall(Id(), arg)
      if call.context.inMapLcl.reduce(_ || _)
    =>
      toPrivate(Id()) $ arg

    case call@FunCall(uf: UserFun, arg@_*)
      if call.context.inMapWrg.reduce(_ || _)
    =>
      toPrivate(uf)(arg:_*)

    case FunCall(f: AbstractMap, arg)
      if f.isInstanceOf[MapLcl] || f.isInstanceOf[MapSeq]
    =>
      toPrivate(f) $ arg
  })

  val localMemory = Rule("Map(f) => toLocal(Map(f))", {
    case call@FunCall(Id(), arg)
      if call.context.inMapWrg.reduce(_ || _)
    =>
      toLocal(Id()) $ arg

    case call@FunCall(uf: UserFun, arg@_*)
      if call.context.inMapWrg.reduce(_ || _)
    =>
      toLocal(uf)(arg:_*)

    case call@FunCall(f: AbstractMap, arg)
      if call.context.inMapWrg.reduce(_ || _)
    =>
      toLocal(f) $ arg
  })

  val globalMemory = Rule("Map(f) => toGlobal(Map(f))", {
    case FunCall(f: AbstractMap, arg) =>
      toGlobal(f) $ arg
  })

  /* Vectorization rule */

  val vectorize: Rule = vectorize(?)

  def vectorize(vectorWidth: ArithExpr): Rule =
    Rule("Map(uf) => asScalar() o Map(Vectorize(n)(uf)) o asVector(n)", {
      case FunCall(Map(Lambda(p, FunCall(uf: UserFun, ufArg))), arg)
        if (p.head eq ufArg) && !ufArg.t.isInstanceOf[VectorType] && !ufArg.t.isInstanceOf[TupleType]
      =>
        // TODO: force the width to be less than the array length
        val n = if (vectorWidth == ?) Var(RangeMul(2, 16, 2)) else vectorWidth
        asScalar() o Map(VectorizeUserFun(n, uf)) o asVector(n) $ arg
    })

  def vectorizeMapZip(vectorWidth: ArithExpr): Rule =
    Rule("Map(uf) $ Zip(a, b) => asScalar() o Map(Vectorize(n)(uf)) o asVector(n)", {
      case FunCall(Map(Lambda(p, FunCall(uf: UserFun, ufArgs@_*))), FunCall(Zip(_), zipArgs@_*))
        if zipArgs.forall(arg => !arg.t.isInstanceOf[VectorType] && !arg.t.isInstanceOf[TupleType]) &&
          ufArgs.forall({
            case FunCall(Get(_), x) if x == p.head => true
            case _ => false
          })
      =>
        // TODO: force the width to be less than the array length
        val n = if (vectorWidth == ?) Var(RangeMul(2, 16, 2)) else vectorWidth
        val newZipArgs = zipArgs.map(arg => asVector(n) $ arg)
        val newParam = Param()
        val newUfArgs = ufArgs.map({
          case FunCall(Get(i), _) => FunCall(Get(i), newParam)
        })

        asScalar() o Map(Lambda(Array(newParam), VectorizeUserFun(n, uf)(newUfArgs:_*))) $ Zip(newZipArgs:_*)
    })

  /* OpenCL builtins */

  val dotBuiltin = Rule("", {
    case FunCall(Reduce(Lambda(rp, FunCall(uf:UserFun, a1, a2))), init,
    FunCall(asScalar(),
    FunCall(Map(Lambda(mp,FunCall(VectorizeUserFun(Cst(4), multUf),
          FunCall(Get(n), multA1), FunCall(Get(m), multA2)) )), arg)))
      if uf == add &&
        init.t == opencl.ir.Float &&
        rp.contains(a1) &&
        rp.contains(a2) &&
        multUf == mult &&
        multA1.eq(mp.head) &&
        multA2.eq(mp.head)
    =>

      Reduce(add, init) o Map(fun(x => dot(Get(x, n), Get(x, m)))) $ arg
  })

  val dotBuiltinSeq = Rule("", {
    case FunCall(ReduceSeq(Lambda(rp, FunCall(uf:UserFun, a1, a2))), init,
    FunCall(asScalar(),
    FunCall(m: AbstractMap, arg)))
      if uf == add &&
        init.t == opencl.ir.Float &&
        rp.contains(a1) &&
        rp.contains(a2) &&
        (m.isInstanceOf[Map] || m.isInstanceOf[MapSeq]) &&
        (m.f.body match {
          case FunCall(VectorizeUserFun(Cst(4), multUf),
          FunCall(Get(_), multA1), FunCall(Get(_), multA2))
            if multUf == mult && multA1.eq(m.f.params.head) &&
              multA2.eq(m.f.params.head)
          => true
          case _ => false
        })
    =>

      ReduceSeq(add, init) o MapSeq(dot) $ arg
  })

  /* Other */

  // More restricted than necessary. Could pull out any arg.
  val extractFromMap = Rule("Map(x => f(x, g(...))) $ in => " +
  "(y => Map(x => f(x, y)) $ in) o g(...)", {
    case call@FunCall(Map(Lambda(Array(p), FunCall(_, arg))) , _)
      if !arg.contains({ case y if y eq p => })
    =>
      val newParam = Param()

      val newCall = Expr.replace(call, arg, newParam)

      val lambda = Lambda(Array(newParam), newCall)
      val newExpr = FunCall(lambda, arg)

      newExpr

    case call@FunCall(Map(Lambda(Array(p), FunCall(_, _, arg))), _)
      if !arg.contains({ case y if y eq p => })
    =>
      val newParam = Param()

      val newCall = Expr.replace(call, arg, newParam)

      val lambda = Lambda(Array(newParam), newCall)
      val newExpr = FunCall(lambda, arg)

      newExpr
  })

  val mapFission = Rule("Map(f o g) => Map(f) o Map(g)", {
    case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(fun2, p2)))), arg)
      if !fun1.isInstanceOf[FPattern] ||
        !fun1.asInstanceOf[FPattern].f.body.contains({ case a if a eq p1.head => })
    =>
      Map(fun1) o Map(Lambda(p1, fun2(p2))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(r: AbstractPartRed, init, FunCall(fun2, p2)))), arg)
      if !r.f.body.contains({ case a if a eq p1.head => }) &&
        !init.contains({ case a if a eq p1.head => })
    =>

      Map(fun((x) => r(init, x))) o Map(Lambda(p1, fun2(p2))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(r: AbstractPartRed, init, p2)))), arg)
      if !fun1.isInstanceOf[FPattern] ||
        !fun1.asInstanceOf[FPattern].f.body.contains({ case a if a eq p1.head => })
    =>
      Map(fun1) o Map(Lambda(p1, r(init, p2))) $ arg

    case FunCall(Map(Lambda(p1, FunCall(r1: AbstractPartRed, init1,
    FunCall(r2: AbstractPartRed, init2, p2)))), arg)
      if !r1.f.body.contains({ case a if a eq p1.head => }) &&
        !init1.contains({ case a if a eq p1.head => })
    =>
      Map(fun((x) => r1(init1, x))) o Map(Lambda(p1, r2(init2, p2))) $ arg
  })

  val mapFission2 = Rule("Map(x => f(x, g(...)) => Map(f) $ Zip(..., Map(g)  )", {
    case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(fun2, p2)))), arg)
      if fun1.isInstanceOf[FPattern] &&
        fun1.asInstanceOf[FPattern].f.body.contains({ case a if a eq p1.head => })
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

    case FunCall(Map(Lambda(p1, FunCall(r: AbstractPartRed, init, FunCall(fun2, p2)))), arg)
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

    case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(r: AbstractPartRed, init, p2)))), arg)
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
    FunCall(r2: AbstractPartRed, init2, p2)))), arg)
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

  val mapMapInterchange = Rule("Map(fun(a => Map(fun( b => ... ) $ B) $ A => " +
    "Transpose() o Map(fun(b => Map(fun( a => ... ) $ A) $ B", {
    case FunCall(Map(Lambda(a, FunCall(Map(Lambda(b, expr)), bArg))), aArg)
      if !bArg.contains({ case e if e eq a.head => })
    =>
      TransposeW() o Map(Lambda(b, FunCall(Map(Lambda(a, expr)), aArg))) $ bArg
  })

  val mapReduceInterchange = Rule("Map(Reduce(f)) => Transpose() o Reduce(Map(f)) o Transpose()", {
    case FunCall(Map(Lambda(lambdaParams,
          FunCall(r@AbstractPartRed(Lambda(innerParams, expr)), init: Value, arg)
         )), mapArg)
      if lambdaParams.head eq arg
    =>
      val newInit = Value(init.value, ArrayTypeWSWC(init.t, Type.getLength(mapArg.t)))

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
      FunCall(r@AbstractPartRed(Lambda(innerParams, expr)), FunCall(Get(i), a1), FunCall(Get(j), a2))
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
        FunCall(PartRed(_), init2: Value, a2))), a1)))
      )), arg)
        if (p1.head eq a1) && (p2.head eq a2) && init1 == init2
      =>
        val newInit = Value(init2.value, ArrayTypeWSWC(init2.t, Type.getLength(arg.t)))

        TransposeW() o ReduceSeq(fun((acc, a) =>
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

  val mapMapTransposeZipInside = Rule("Map(fun(a => Map() $ Zip(..., ... $ a, ...)) $ A => " +
    "Transpose() o Map(Map(fun(a => ))) $ Zip(..., Transpose() o Map(...) $ A, ...) ", {
    case FunCall(Map(Lambda(outerLambdaParam,
      FunCall(Map(Lambda(innerLambdaParam,
        expr
      )), FunCall(Zip(_), zipArgs@_*))
    )), arg)
      if zipArgs.count(Utils.getFinalArg(_) eq outerLambdaParam.head) == 1
    =>
      // Find all Get patterns that refer to the an element from the zipped array
      // and have to be replaced in expr
      val gets = Utils.findGets(expr, innerLambdaParam.head)

      // Find which Get pattern corresponds to the component containing an element of 'a'
      val zipToReplace = zipArgs.zipWithIndex.find(e =>
        e._1 eq zipArgs.find(Utils.getFinalArg(_) eq outerLambdaParam.head).get
      ).get

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

      var elem = Transpose() $ arg
      if (!(zipToReplace._1 eq outerLambdaParam.head)) {
        // Isolate any splits/joins/transposes/computation inside the zip
        val thirdNewParam = Param()

        val replace = Expr.replace(zipToReplace._1, outerLambdaParam.head, thirdNewParam)

        // And include them before the transpose
        val map = Map(Lambda(Array(thirdNewParam), replace))
        elem = Transpose() o map $ arg
      }

      // Create the arguments for the zip, replacing '... o a' with 'Transpose() o Map(...) $ A'
      val newZipArgs = zipArgs.updated(zipToReplace._2, elem)

      // Construct the final expression
      val lambda = Lambda(Array(secondNewParam), finalNewExpr)
      Transpose() o Map(Lambda(Array(newParam), Map(lambda) $ one.head._2)) $ Zip(newZipArgs: _*)
  })

  val mapMapTransposeZipOutside = Rule("Map(fun(x => Map(fun(a => )) $ Get(n, ) $ Zip(..., A, ...)  => " +
    "Transpose() o Map(fun(a => Map() $ Zip(..., a, ...)) o Transpose() $ A", {
    case FunCall(Map(Lambda(outerLambdaParam,
    FunCall(Map(Lambda(innerLambdaParam,
    expr
    )), FunCall(Get(n), getParam))
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
      case FunCall(Map(Lambda(p, call@FunCall(_, args@ _* ))), FunCall(Zip(_), zipArgs@_*))
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

  // TODO: Does it matter there is a Map after the Split?
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
           c@FunCall(_, arg)
        )), FunCall(Zip(_), zipArgs@_*))
      if isMapFissionWithZipOutsideValid(lambdaParam, arg)
    =>
      applyMapFissionWithZipOutside(c, arg, zipArgs)

    case FunCall(Map(Lambda(lambdaParam,
            c@FunCall(_: AbstractPartRed, _, arg@FunCall(_, FunCall(Get(_), _)))
         )), FunCall(Zip(_), zipArgs@_*))
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
      // TODO: How to properly distinguish between structs and several variables? This
      // TODO: could turn variables into a struct. Sometimes this also could be useful
      // Struct
      case TupleType(tt@_*) if tt.forall(t =>
          t.isInstanceOf[ScalarType] || t.isInstanceOf[VectorType]) =>
        UserFun("id" + Type.name(t), "x", "{ return x; }", t, t)

      // Several variables
      case TupleType(tt@_*) =>
        val newParam = Param()
        val argSequence = tt.zipWithIndex.map(p => generateCopy(p._1) $ Get(newParam, p._2))
        Lambda(Array(newParam), Tuple(argSequence:_*))

      case ArrayType(elemT) =>
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
      case ArrayType(_) =>
        Map(Id())
      case ScalarType(_, _) | VectorType(_, _) =>
        UserFun("id" + t, "x", "{ return x; }", t, t)
      case _ => throw TypeException(s"Can't generate id function for $t")
    }
  }

  val dropId = Rule("Id() => Epsilon()", {
    case FunCall(Id(), arg) => arg
    case FunCall(Map(Lambda(Array(_), FunCall(Id(), innerArg))), arg) => arg
    case FunCall(Map(Lambda(Array(_), FunCall(
    Map(Lambda(Array(_), FunCall(Id(), innerArg))), innerArg2))), arg) => arg
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

  val addIdRed = Rule("f => f o Id()", {
    case FunCall(f: AbstractPartRed, init, arg) =>
      f(init, Id() $ arg)
  })

  val addIdAfterReduce = Rule("f => Id() o f", {
    case call@FunCall(_: ReduceSeq, _*) =>
      FunCall(MapSeq(Id()), call)
  })

  val addIdValue = Rule("Value(...) => Id() $ Value(...)", {
    case v: Value =>
      FunCall(generateCopy(v.t), v)
  })

  def isId(expr: Expr): Boolean =
    expr match {
      case FunCall(Id(), _) => true
      case _ => false
    }

  val addIdMapWrg = Rule("MapWrg(f) => MapWrg(f o Id())", {
    case call@FunCall(MapWrg(dim, f:Lambda1) , arg)
      if !f.body.contains( {case FunCall(MapWrg(_,_), a) =>})
      && !f.body.contains( {case FunCall(Id(), b) =>  })
      => MapWrg(dim, f o Id()) $ arg
  })

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

      // Construct the FunCall directly to avoid inlining
      val newBody =
        FunCall(Lambda(Array(newParam), tempBody),  FunCall(Id(), paramForCurrent))

      Expr.replace(call, body, newBody)
  })

  val addCopy = Rule("f => f o Id()", {
    case FunCall(f, arg) =>
      f o generateCopy(arg.t) $ arg
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

  val tupleToStruct = Rule("tupleToStruct", {
    case call@FunCall(Tuple(_), _*)
      if {
        var containsArray = false

        Type.visit(call.t, _ => Unit,
          t => if (t.isInstanceOf[ArrayType]) containsArray = true)

        !containsArray
      }
    =>

      val copyFun = UserFun(s"id_${Type.name(call.t)}", "x",
        "{ return x; }", call.t, call.t)

      copyFun $ call
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
        case (FunCall(Map(f), _), n) => f $ Get(lambdaParam, n)
      })

      Map(Lambda(Array(lambdaParam), Tuple(maps:_*))) $ Zip(zipArgs:_*)
  })

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

  val fuseZipTuple = Rule("fuseZipTuple", {
    case FunCall(Lambda(Array(p), body), FunCall(Tuple(n), tupleArgs@_*))
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

  val iterate1 = Rule("Iterate(1, x) => x", {
    case FunCall(Iterate(n, f), arg) if n.eval == 1 => f(arg)
  })

  val iterateId = Rule("Iterate(0, _) => Epsilon", {
    case FunCall(Iterate(n, _), arg) if n.eval == 0 => arg
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
