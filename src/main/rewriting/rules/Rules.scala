package rewriting.rules

import backends.c.common.common_ir.{Marker2, Marker3}
import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir.pattern._
import rewriting.utils.Utils

object Rules {

  import rewriting.utils.Utils.isTranspose

  /* Rules required for 2D stencil rewrite *///TODO find appropriate names

  /* Slide-promotion *///TODO not really because of map(join)... find better name
  val slidePromotion = Rule("Slide(u,v) o Map(Join()) => Map(Map(Join())) o Slide(u,v)",{
    case FunCall(Slide(u,v), FunCall(Map(Lambda(_, FunCall(Join(), _),_)), arg)) =>
      Map(Map(Join())) o Slide(u,v) $ arg
  })

  val slideSwap = Rule("Slide(u,v) o Map(Map(Slide(n,s))) => Map(Map(Map(Slide(n,s)))) o Slide(u,v)",{
    case FunCall(Slide(u,v), FunCall(Map(Lambda(
      _, FunCall(Map(Lambda(
        _, FunCall(Slide(n,s), _),_)), _),_)), arg)) =>
     Map(Map(Map(Slide(n,s)))) o Slide(u,v) $ arg
  })

  val joinSwap = Rule("Join() o Map(Map(f)) => Map(f) o Join()", {
    case FunCall(Join(), FunCall(Map(Lambda(_, FunCall(map@Map(_), _),_)), arg)) =>
      map o Join() $ arg
  })

  // todo reduce on layer of maps and use map fission before applying this rule
  val transposeSwap = Rule("Map(Map(Map(Transpose()))) o Map(Transpose()) => Map(Transpose()) o Map(Map(Map(Transpose())))", {
    case FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(),
      _),_)), _),_)), _),_)), FunCall(Map(Lambda(
    _, FunCall(
    Transpose(), _),_)), arg)) =>
      Map(Transpose()) o Map(Map(Map(Transpose()))) $ arg
  })

  // todo reduce on layer of maps and use map fission before applying this rule
  val slideTransposeSwap = Rule("Map(Map(Map(Slide(u,v)))) o Map(Transpose()) => Map(Transpose) o Map(Map(Map(Slide(u,v))))", {
    case FunCall(Map(Lambda(
      _, FunCall(Map(Lambda(
        _, FunCall(Map(Lambda(
          _, FunCall(Slide(u,v), _),_)), _),_)), _),_)), FunCall(Map(Lambda(
    _, FunCall(
    Transpose(), _),_)), arg)) =>
      Map(Transpose()) o Map(Map(Map(Slide(u,v)))) $ arg
  })

  val slideTransposeReordering = Rule("Map(Slide(u,v)) o Map(Transpose()) => " +
    "Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Slide(u,v)))", {
    case FunCall(Map(Lambda(
      _, FunCall(Slide(u,v), _),_)), FunCall(Map(Lambda(
        _, FunCall(Transpose(), _),_)), arg)) =>
      Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Slide(u,v))) $ arg
  })

  val transposeMapJoinReordering = Rule("Transpose() o Map(Join()) => Join() o Map(Transpose()) o Transpose()", {
    case FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), _),_)), arg)) =>
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

      val step = Utils.splitVariable(tileStep, arg.t)
      val overlap = n-s
      Join() o Map(Slide(n, s)) o Slide(step + overlap, step) $ arg
  })

  val mapSeqSlide = Rule("Map(fun(m => {})) o Slide(n,s) => MapSeqSlide(fun(m => {} )),n,s)",
    {
      case FunCall(Map(lambda), FunCall(Slide(n,s), arg)) =>
        MapSeqSlide(lambda,n,s) $ arg
    })

  val mapSeqSlideSeq = Rule("MapSeq(fun(m => {})) o Slide(n,s) => MapSeqSlide(fun(m => {} )),n,s)",
    {
      case FunCall(MapSeq(lambda), FunCall(Slide(n,s), arg)) =>
        MapSeqSlide(lambda,n,s) $ arg
    })

  /* Split-join rule */
  val splitJoin: Rule = splitJoin(?)

  def splitJoin(split: ArithExpr) = Rule("Map(f) => Join() o Map(Map(f)) o Split(I)", {
    case FunCall(Map(f), arg) =>
      val chunkSize = Utils.splitVariable(split, arg.t)
      Join() o Map(Map(f)) o Split(chunkSize) $ arg
  })

  val splitJoinMapSeq: Rule = splitJoinMapSeq(?)

  def splitJoinMapSeq(split: ArithExpr) = Rule("Map(f) => Join() o Map(Map(f)) o Split(I)", {
    case FunCall(MapSeq(f), arg) =>
      val chunkSize = Utils.splitVariable(split, arg.t)
      Join() o Map(MapSeq(f)) o Split(chunkSize) $ arg
  })


  val splitJoinMapSeqHost: Rule = splitJoinMapSeqHost(?)

  def splitJoinMapSeqHost(split: ArithExpr) = Rule("Map(f) => Join() o Map(Map(f)) o Split(I)", {
    case FunCall(MapSeq(f), arg) =>
      val chunkSize = Utils.splitVariable(split, arg.t)
      Join() o MapSeq(MapSeq(f)) o Split(chunkSize) $ arg
  })


  val splitJoinMapSeqHostMarker: Rule = splitJoinMapSeqHostMarker(?)

  def splitJoinMapSeqHostMarker(split: ArithExpr) = Rule("Marker() o Map(f) => Join() o Map(Map(f)) o Split(I)", {
    case FunCall(Marker2(true), FunCall(MapSeq(f), arg) ) =>
      val chunkSize = Utils.splitVariable(split, arg.t)
      Join() o MapSeq(MapSeq(f)) o Split(chunkSize) $ arg
    case FunCall(Marker2(false), FunCall(MapSeq(f), arg) ) =>
       FunCall(MapSeq(f), arg)
  })

  val splitJoinMapSeqHostMarker3: Rule = splitJoinMapSeqHostMarker3(?)

  def splitJoinMapSeqHostMarker3(split: ArithExpr) = Rule("Marker() o Map(f) => Join() o Map(Map(f)) o Split(I)", {
    case FunCall(Marker3(_, _, tunable_params, cancelCombo), FunCall(MapSeq(f), arg) ) if tunable_params != cancelCombo =>
      //val chunkSize = Utils.splitVariable(split, arg.t)
      assert(tunable_params.size == 3)
      Join() o MapSeq(MapSeq(f)) o Split(tunable_params(0)) $ arg
    case FunCall(Marker3(_, _, _, _), FunCall(MapSeq(f), arg) ) =>
      FunCall(MapSeq(f), arg)
  })

  val joinSplit = Rule("Map(Map(f)) => Split(I) o Map(f) o Join()", {
    case call @ FunCall(Map(Lambda(Array(p), FunCall(Map(f), mapArg),_)), arg)
      if p == mapArg =>
      val length = arg.t match {
        case at: ArrayType => Type.getLength(at.elemT)
        case ty => throw new TypeException(ty, "Array(_)", call)
      }
      Split(length) o Map(f) o Join() $ arg
  })

  // Required for avoiding data races
  val splitJoinReduce = Rule("Reduce(f) $ data => Join() o Map(Reduce(f)) o Split(data.length)", {
    case FunCall(red: Reduce, init, arg) =>

      val length = arg.t match { case ArrayTypeWS(_, n) => n }

      Join() o Map(fun(x => red(init, x))) o Split(length) $ arg

    case FunCall(red: ReduceSeq, init, arg) =>

      val length = arg.t match { case ArrayTypeWS(_, n) => n }

      Join() o Map(fun(x => red(init, x))) o Split(length) $ arg
  })


  /* Stride accesses or normal accesses */

  // TODO

  /* Other */

  val reorderBothSidesWithStride: Rule = reorderBothSidesWithStride(?)

  def reorderBothSidesWithStride(stride: ArithExpr): Rule = {
    Rule("Map(f) => Reorder(g^{-1}) o Map(f) o Reorder(g)", {
      case FunCall(map@Map(_), arg) =>
        val s = Utils.splitVariable(stride, arg.t)
        Scatter(ReorderWithStride(s)) o map o Gather(ReorderWithStride(s)) $ arg
    })
  }


  val mapSplitTranspose = Rule("Map(Split(n)) o Transpose()" +
                               "Transpose() o Map(Transpose()) o Split(n)", {
    case FunCall(Map(Lambda(param, FunCall(Split(n), a),_)), FunCall(t, arg))
      if (param.head eq a) && isTranspose(t)
    =>
      Transpose() o Map(Transpose()) o Split(n) $ arg
  })

  val mapTransposeSplit = Rule("Map(Transpose()) o Split(n)" +
    "Transpose() o Map(Split(n)) o Transpose()", {
    case FunCall(Map(Lambda(param, FunCall(t, a),_)), FunCall(Split(n), arg))
      if (param.head eq a) && isTranspose(t)
    =>
      Transpose() o Map(Split(n)) o Transpose() $ arg
  })

  val transposeMapSplit = Rule("Transpose() o Map(Split(n))" +
    "Map(Transpose()) o Split(n) o Transpose()", {
    case FunCall(t, FunCall(Map(Lambda(param, FunCall(Split(n), a),_)), arg))
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
      case FunCall(Map(Lambda(param1, FunCall(t1, a1),_)),
            FunCall(t2,
            FunCall(Map(Lambda(param2, FunCall(t3, a2),_)), arg)))
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
    case FunCall(Map(Lambda(lambdaParam, FunCall(Map(mapLambda), mapArg),_)), FunCall(Split(n), FunCall(Zip(_), zipArgs@_*)))
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
    "Join() o Map(Zip(...)) $ Zip(...)", {
    case FunCall(Zip(_), zipArgs@_*)
      if zipArgs.forall(joinCall.isDefinedAt) && zipArgs.map(getOutermostLengths).distinct.size == 1
    =>

      val newArgs = zipArgs.map({ case FunCall(_, arg) => arg })
      val p = Param()
      val innerArgs = newArgs.indices.map(Get(p, _))

      Join() o Map(Lambda(Array(p), Zip(innerArgs:_*))) $ Zip(newArgs:_*)
  })

  val transposeMapTransposeReorder =
    Rule("Map(Gather(f) o Transpose()) o Transpose() => " +
         " Map(Transpose()) o Transpose() o Map(Map(Gather(f)))", {
      case FunCall(Map(Lambda(p,
              FunCall(f:Gather, FunCall(t1, a))
      ,_)), FunCall(t2, arg))
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
