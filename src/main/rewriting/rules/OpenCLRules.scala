package rewriting.rules

import ir.{TupleType, VectorType}
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.ast._
import opencl.ir.pattern._

object OpenCLRules {

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

  /* Address space rules */

  val privateMemory = Rule("Map(f) => toPrivate(Map(f))", {
    case call@FunCall(Id(), arg)
      if call.context.inMapLcl.reduce(_ || _)
    =>
      toPrivate(Id()) $ arg

    case call@FunCall(uf: UserFun, arg@_*)
      if call.context.inMapWrg.reduce(_ || _) || call.context.inMapGlb.reduce(_ || _)
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
    case FunCall(uf: UserFun, args@_*) =>
      toGlobal(uf)(args:_*)
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

  val partialReduceVectorize: Rule = partialReduceVectorize(?)

  def partialReduceVectorize(vectorWidth: ArithExpr): Rule =
    Rule("PartRed(f) => Join() o Map(PartRed(f)) o Split()", {
      case FunCall(PartRed(Lambda(_, FunCall(uf:UserFun, _*))), init:Value, arg)
        if !init.t.isInstanceOf[TupleType] && !init.t.isInstanceOf[VectorType] =>
        // TODO: force the width to be less than the array length
        val n = if (vectorWidth == ?) Var(RangeMul(2, 16, 2)) else vectorWidth
        asScalar() o PartRed(VectorizeUserFun(n, uf), init.vectorize(n)) o asVector(n) $ arg
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
}
