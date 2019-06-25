package exploration

import ir._
import ir.ast._
import ir.view.{NoView, View, ViewMem}
import lift.arithmetic._
import lift.arithmetic.simplifier.SimplifyPow
import opencl.generator.{NDRange, RangesAndCounts}
import opencl.ir._
import opencl.ir.pattern._
import opencl.ir.OpenCLMemory.getAllMemoryVars
import rewriting.{EnabledMappings, Lower}

package object detection {

  def getNumDimensions(lambda: Lambda): Int = {

    val dims = Expr.visitWithState(Set[Int]())(lambda.body, {
      case (FunCall(MapLcl(dim, _), _), set) => set + dim
      case (FunCall(MapGlb(dim, _), _), set) => set + dim
      case (_, set) => set
    })

    dims.size
  }

  def prepareLambda(f: Lambda): Unit = {
    if (f.body.context == null || f.body.view == NoView ||
      f.body.mem == UnallocatedMemory || f.body.t == UndefType) {

      TypeChecker(f)
      InferOpenCLAddressSpace(f)
      RangesAndCounts(f, NDRange(?, ?, ?), NDRange(?, ?, ?), collection.Map())
      OpenCLMemoryAllocator(f)
      View(f)
      UpdateContext(f)
    }
  }

  def getUserFunName(decl: FunDecl): String = {
    decl match {
      case uf: UserFun => uf.name
      case vuf: VectorizeUserFun => vuf.userFun.name
      case _ => throw new IllegalArgumentException(decl.toString)
    }
  }

  def isTuple(expr: Expr): Boolean =
    expr match {
      case FunCall(Tuple(_), _*) => true
      case _ => false
    }

  def getArgumentsAtSuitableLevel(f: Lambda, testNesting: FunCall => Boolean): Seq[Expr] = {
    prepareLambda(f)

    Expr.visitWithState(Seq[Expr]())(f.body, {
      case (call@FunCall(_: UserFun | _: VectorizeUserFun, args@_*), seq)
        if testNesting(call) && !args.exists(isTuple) // Can't emit the view for Tuple

      => seq ++ args
      case (_, seq) => seq
    }).distinct.diff(f.params).filter({
      case FunCall(_: UserFun | _: VectorizeUserFun, _*) => false
      case _: Value => false // TODO: A copy will have been made here
      case _ => true
    })
  }

  def getLocationsForCopy(lambda: Lambda, reuseExpr: Expr, testContext: FunCall => Boolean): Seq[(Expr, Var)] = {
    val sourceView = View.getSubViews(reuseExpr.view).last
    sourceView match {
      case ViewMem(sourceVar, _) =>

        // Find which "strategic" Id location(s) would copy the required variable and is suitable for local memory
        // TODO: Doesn't work for all reuse... Does it matter? Still gets what we care about
        Expr.visitWithState(Seq[(Expr, Var)]())(lambda.body, {
          case (funCall@FunCall(Id(), _*), seq)
            if getAllMemoryVars(funCall.mem).contains(sourceVar) && testContext(funCall)
          =>
            seq :+ (funCall, sourceVar)
          case (_, seq) => seq
        })

      case _ => Seq()
    }
  }

  private val M = Var("M", StartFromRange(1))
  private val N = Var("N", StartFromRange(1))
  private val K = Var("K", StartFromRange(1))

  val gemvPartialReduceWithReorderNoRace: Lambda =
    fun(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(Float, M),
      ArrayType(Float, N),
      Float, Float,
      (p_0, p_1, p_2, p_3, p_4) =>
        FunCall(MapWrg(0)(fun((p_5) =>
          FunCall(Join(),
            FunCall(MapLcl(0)(fun((p_6) =>
              FunCall(toGlobal(fun((p_7) =>
                FunCall(MapSeq(fun((p_8) =>
                  FunCall(add,
                    FunCall(mult, p_8, p_3),
                    FunCall(mult,
                      FunCall(Get(1), p_5), p_4)))), p_7))),
                FunCall(ReduceSeq(fun((p_9, p_10) =>
                  FunCall(add, p_9, p_10))),
                  FunCall(idfloat, Value("0.0f", Float)), p_6)))),
              FunCall(Split(Cst(128)),
                FunCall(Join(),
                  FunCall(MapLcl(0)(fun((p_11) =>
                    FunCall(ReduceSeq(fun((p_12, p_13) =>
                      FunCall(add, p_12,
                        FunCall(mult,
                          FunCall(Get(0), p_13),
                          FunCall(Get(1), p_13))))),
                      FunCall(idfloat, Value("0.0f", Float)), p_11))),
                    FunCall(Split( M * SimplifyPow(Cst(128), Cst(-1)) ),
                      FunCall(Gather(ReorderWithStride(128)),
                        FunCall(Zip(2), p_1,
                          FunCall(Get(0), p_5))))))))))),
          FunCall(Zip(2), p_0, p_2)))

  val gemvIntroduceReuse = fun(
    ArrayType(ArrayType(Float, M), N),
    ArrayType(Float, M),
    ArrayType(Float, N),
    Float, Float,
    (p_0, p_1, p_2, p_3, p_4) =>
      FunCall(Join(),
        FunCall(MapWrg(0)(fun((p_5) =>
          FunCall(TransposeW(),
            FunCall(MapSeq(fun((p_6) =>
              FunCall(toGlobal(fun((p_7) =>
                FunCall(MapLcl(0)(fun((p_8) =>
                  FunCall(add,
                    FunCall(mult,
                      FunCall(Get(1), p_8), p_3),
                    FunCall(mult,
                      FunCall(Get(1),
                        FunCall(Get(0), p_8)), p_4)))), p_7))),
                FunCall(Zip(2), p_5, p_6)))),
              FunCall(ReduceSeq(fun((p_9, p_10) =>
                FunCall(Join(),
                  FunCall(MapLcl(0)(fun((p_11) =>
                    FunCall(ReduceSeq(fun((p_12, p_13) =>
                      FunCall(add, p_12,
                        FunCall(mult,
                          FunCall(Get(0), p_13),
                          FunCall(Get(1), p_13))))),
                      FunCall(Get(0), p_11),
                      FunCall(Zip(2),
                        FunCall(Get(0), p_10),
                        FunCall(Get(1), p_11))))),
                    FunCall(Zip(2), p_9,
                      FunCall(Get(1), p_10)))))),
                FunCall(MapLcl(0)(fun((p_14) =>
                  FunCall(idfloat, p_14))), Value("0.0f", ArrayType(Float, 64))),
                FunCall(Zip(2),
                  FunCall(Split(Cst(64)), p_1),
                  FunCall(Transpose(),
                    FunCall(Map(fun((p_15) =>
                      FunCall(Split(Cst(64)),
                        FunCall(Get(0), p_15)))), p_5)))))))),
          FunCall(Split(Cst(64)),
            FunCall(Zip(2), p_0, p_2)))))

  val mm1DBlocked: Lambda = {
    val v__3 = Var("", RangeMul(1,1+M,2))

    fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N),
      (p_0, p_1) =>
        FunCall(Join(),
          FunCall(MapGlb(1)(fun((p_2) =>
            FunCall(TransposeW(),
              FunCall(MapGlb(0)(fun((p_3) =>
                FunCall(TransposeW(),
                  FunCall(toGlobal(fun((p_4) =>
                    FunCall(MapSeq(fun((p_5) =>
                      FunCall(MapSeq(fun((p_6) =>
                        FunCall(idfloat, p_6))), p_5))), p_4))),
                    FunCall(ReduceSeq(fun((p_7, p_8) =>
                      FunCall(MapSeq(fun((p_9) =>
                        FunCall(add,
                          FunCall(Get(0), p_9),
                          FunCall(mult,
                            FunCall(Get(1), p_9),
                            FunCall(Get(1), p_8))))),
                        FunCall(Zip(2), p_7,
                          FunCall(Get(0), p_8))))),
                      FunCall(MapSeq(fun((p_10) =>
                        FunCall(idfloat, p_10))), Value("0.0f", ArrayType(Float, v__3))),
                      FunCall(Zip(2),
                        FunCall(Transpose(), p_2), p_3)))))), p_1)))),
            FunCall(Split(v__3), p_0))))
  }

  val mm2DBlocked: Lambda = {
    val v__3 = Var("", RangeMul(1,1+M,2))
    val v__4 = Var("", RangeMul(1,1+N,2))

    fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N),
      (p_0, p_1) =>
        FunCall(Map(fun((p_2) =>
          FunCall(Scatter(ReorderWithStride(N / v__4)), p_2))),
          FunCall(Join(),
            FunCall(MapGlb(1)(fun((p_3) =>
              FunCall(TransposeW(),
                FunCall(Join(),
                  FunCall(MapGlb(0)(fun((p_4) =>
                    FunCall(TransposeW(),
                      FunCall(Map(fun((p_5) =>
                        FunCall(TransposeW(), p_5))),
                        FunCall(TransposeW(),
                          FunCall(toGlobal(fun((p_6) =>
                            FunCall(MapSeq(fun((p_7) =>
                              FunCall(MapSeq(fun((p_8) =>
                                FunCall(MapSeq(fun((p_9) =>
                                  FunCall(idfloat, p_9))), p_8))), p_7))), p_6))),
                            FunCall(ReduceSeq(fun((p_10, p_11) =>
                              FunCall(MapSeq(fun((p_12) =>
                                FunCall(MapSeq(fun((p_13) =>
                                  FunCall(add,
                                    FunCall(Get(0), p_13),
                                    FunCall(mult,
                                      FunCall(Get(1), p_12),
                                      FunCall(Get(1), p_13))))),
                                  FunCall(Zip(2),
                                    FunCall(Get(0), p_12),
                                    FunCall(Get(1), p_11))))),
                                FunCall(Zip(2), p_10,
                                  FunCall(Get(0), p_11))))),
                              FunCall(MapSeq(fun((p_14) =>
                                FunCall(MapSeq(fun((p_15) =>
                                  FunCall(idfloat, p_15))), p_14))), Value("0.0f", ArrayType(ArrayType(Float, v__4), v__3))),
                              FunCall(Zip(2),
                                FunCall(Transpose(), p_3),
                                FunCall(Transpose(), p_4))))))))),
                    FunCall(Split(v__4),
                      FunCall(Gather(ReorderWithStride(N / v__4)), p_1))))))),
              FunCall(Split(v__3), p_0)))))
  }

  val mmTiled: Lambda = {
    val v__3 = Var("", RangeMul(1,1+M,2))
    val v__4 = Var("", RangeMul(1,1+N,2))
    val v__5 = Var("", RangeMul(1,1+K,2))

    // Tile reused across threads
    fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, N), K),
      (p_0, p_1) =>
        FunCall(Join(),
          FunCall(MapWrg(1)(fun((p_2) =>
            FunCall(TransposeW(),
              FunCall(Join(),
                FunCall(MapWrg(0)(fun((p_3) =>
                  FunCall(TransposeW(),
                    FunCall(Map(fun((p_4) =>
                      FunCall(TransposeW(), p_4))),
                      FunCall(TransposeW(),
                        FunCall(toGlobal(fun((p_5) =>
                          FunCall(MapSeq(fun((p_6) =>
                            FunCall(MapLcl(1)(fun((p_7) =>
                              FunCall(MapLcl(0)(fun((p_8) =>
                                FunCall(idfloat, p_8))), p_7))), p_6))), p_5))),
                          FunCall(ReduceSeq(fun((p_9, p_10) =>
                            FunCall(MapLcl(1)(fun((p_11) =>
                              FunCall(Join(),
                                FunCall(MapLcl(0)(fun((p_12) =>
                                  FunCall(ReduceSeq(fun((p_13, p_14) =>
                                    FunCall(add, p_13,
                                      FunCall(mult,
                                        FunCall(Get(0), p_14),
                                        FunCall(Get(1), p_14))))),
                                    FunCall(Get(0), p_12),
                                    FunCall(Zip(2),
                                      FunCall(Get(1), p_11),
                                      FunCall(Get(1), p_12))))),
                                  FunCall(Zip(2),
                                    FunCall(Get(0), p_11),
                                    FunCall(Transpose(),
                                      FunCall(Get(1), p_10))))))),
                              FunCall(Zip(2), p_9,
                                FunCall(Transpose(),
                                  FunCall(Get(0), p_10)))))),
                            FunCall(MapLcl(1)(fun((p_15) =>
                              FunCall(MapLcl(0)(fun((p_16) =>
                                FunCall(idfloat, p_16))), p_15))), Value("0.0f", ArrayType(ArrayType(Float, v__4), v__3))),
                            FunCall(Zip(2),
                              FunCall(Split(v__5),
                                FunCall(Transpose(), p_2)), p_3)))))))),
                  FunCall(Transpose(),
                    FunCall(Map(fun((p_17) =>
                      FunCall(Transpose(), p_17))),
                      FunCall(Split(v__5),
                        FunCall(Map(fun((p_18) =>
                          FunCall(Split(v__4), p_18))), p_1))))))))),
            FunCall(Split(v__3), p_0))))
  }

  val mmTiled1DBlocked: Lambda = {
    val v__3 = Var("", RangeMul(1,1+M,2))
    val v__4 = Var("", RangeMul(1,1+K,2))
    val v__5 = Var("", RangeMul(1,1+N,2))
    val v__6 = Var("", RangeMul(1,1+v__3,2))

    fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, N), K),
      (p_0, p_1) =>
        FunCall(Join(),
          FunCall(MapWrg(1)(fun((p_2) =>
            FunCall(TransposeW(),
              FunCall(Join(),
                FunCall(MapWrg(0)(fun((p_3) =>
                  FunCall(TransposeW(),
                    FunCall(Join(),
                      FunCall(Map(fun((p_4) =>
                        FunCall(TransposeW(),
                          FunCall(Map(fun((p_5) =>
                            FunCall(TransposeW(), p_5))),
                            FunCall(TransposeW(), p_4))))),
                        FunCall(TransposeW(),
                          FunCall(toGlobal(fun((p_6) =>
                            FunCall(MapSeq(fun((p_7) =>
                              FunCall(MapLcl(1)(fun((p_8) =>
                                FunCall(MapLcl(0)(fun((p_9) =>
                                  FunCall(MapSeq(fun((p_10) =>
                                    FunCall(idfloat, p_10))), p_9))), p_8))), p_7))), p_6))),
                            FunCall(ReduceSeq(fun((p_11, p_12) =>
                              FunCall(MapLcl(1)(fun((p_13) =>
                                FunCall(Join(),
                                  FunCall(MapLcl(0)(fun((p_14) =>
                                    FunCall(ReduceSeq(fun((p_15, p_16) =>
                                      FunCall(MapSeq(fun((p_17) =>
                                        FunCall(add,
                                          FunCall(Get(0), p_17),
                                          FunCall(mult,
                                            FunCall(Get(1), p_17),
                                            FunCall(Get(1), p_16))))),
                                        FunCall(Zip(2), p_15,
                                          FunCall(Get(0), p_16))))),
                                      FunCall(Get(0), p_14),
                                      FunCall(Zip(2),
                                        FunCall(Transpose(),
                                          FunCall(Get(1), p_13)),
                                        FunCall(Get(1), p_14))))),
                                    FunCall(Zip(2),
                                      FunCall(Get(0), p_13),
                                      FunCall(Get(1), p_12)))))),
                                FunCall(Zip(2), p_11,
                                  FunCall(Split(v__6),
                                    FunCall(Transpose(),
                                      FunCall(Get(0), p_12))))))),
                              FunCall(MapLcl(1)(fun((p_18) =>
                                FunCall(MapLcl(0)(fun((p_19) =>
                                  FunCall(MapSeq(fun((p_20) =>
                                    FunCall(idfloat, p_20))), p_19))), p_18))), Value("0.0f", ArrayType(ArrayType(ArrayType(Float, v__6), v__5), v__3*1/^v__6))),
                              FunCall(Zip(2),
                                FunCall(Split(v__4),
                                  FunCall(Transpose(), p_2)), p_3))))))))),
                  FunCall(Transpose(),
                    FunCall(Map(fun((p_21) =>
                      FunCall(Split(v__5),
                        FunCall(Transpose(), p_21)))),
                      FunCall(Split(v__4), p_1)))))))),
            FunCall(Split(v__3), p_0))))
  }

  val mmTiled2DBlocked: Lambda = {
    val v__3 = SizeVar("")
    val v__4 = SizeVar("")
    val v__5 = SizeVar("")
    val v__6 = SizeVar("")
    val v__7 = SizeVar("")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (p_0, p_1) =>
        FunCall(Join(),
          FunCall(Map(fun((p_2) =>
            FunCall(TransposeW(),
              FunCall(Join(),
                FunCall(Map(fun((p_3) =>
                  FunCall(TransposeW(),
                    FunCall(Join(),
                      FunCall(Map(fun((p_4) =>
                        FunCall(Map(fun((p_5) =>
                          FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))),
                          FunCall(TransposeW(),
                            FunCall(Join(),
                              FunCall(Map(fun((p_6) =>
                                FunCall(TransposeW(),
                                  FunCall(Map(fun((p_7) =>
                                    FunCall(TransposeW(), p_7))),
                                    FunCall(TransposeW(), p_6))))),
                                FunCall(TransposeW(), p_4))))))),
                        FunCall(TransposeW(),
                          FunCall(ReduceSeq(fun((p_9, p_10) =>
                            FunCall(Map(fun((p_11) =>
                              FunCall(Join(),
                                FunCall(Map(fun((p_12) =>
                                  FunCall(ReduceSeq(fun((p_14, p_15) =>
                                    FunCall(Map(fun((p_16) =>
                                      FunCall(Map(fun((p_17) =>
                                        FunCall(add,
                                          FunCall(Get(0), p_17),
                                          FunCall(mult,
                                            FunCall(Get(1), p_16),
                                            FunCall(Get(1), p_17))))),
                                        FunCall(Zip(2),
                                          FunCall(Get(0), p_16),
                                          FunCall(Get(1), p_15))))),
                                      FunCall(Zip(2), p_14,
                                        FunCall(Get(0), p_15))))),
                                    FunCall(Get(0), p_12),
                                    FunCall(Zip(2),
                                      FunCall(Transpose(),
                                        FunCall(Get(1), p_11)),
                                      FunCall(Transpose(),
                                        FunCall(Get(1), p_12)))))),
                                  FunCall(Zip(2),
                                    FunCall(Get(0), p_11),
                                    FunCall(Split(v__4),
                                      FunCall(Gather(ReorderWithStride(v__3 / v__4)),
                                        FunCall(Transpose(),
                                          FunCall(Get(1), p_10))))))))),
                              FunCall(Zip(2), p_9,
                                FunCall(Split(v__5),
                                  FunCall(Transpose(),
                                    FunCall(Get(0), p_10))))))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3*1/^v__4), v__6*1/^v__5)),
                            FunCall(Zip(2), p_2, p_3)))))))),
                  FunCall(Transpose(),
                    FunCall(Map(fun((p_22) =>
                      FunCall(Transpose(), p_22))),
                      FunCall(Split(v__7),
                        FunCall(Map(fun((p_23) =>
                          FunCall(Split(v__3), p_23))), p_1))))))))),
            FunCall(Transpose(),
              FunCall(Map(fun((p_24) =>
                FunCall(Transpose(), p_24))),
                FunCall(Split(v__7),
                  FunCall(Map(fun((p_25) =>
                    FunCall(Split(v__6), p_25))), p_0)))))))

    val enabledMappings = EnabledMappings(
      global0 = false, global01 = false, global10 = false,
      global012 = false, global210 = false, group0 = false,
      group01 = false, group10 = true
    )

    Lower.mapCombinations(f, enabledMappings).head
  }

  val gesummvPartialReduceWithReorderNoRace: Lambda = {
    val v__2 = SizeVar("")

    val f = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) =>
      FunCall(Join(),
        FunCall(Map(fun((p_5) =>
          FunCall(Map(fun((p_6) =>
            FunCall(add,
              FunCall(mult,
                FunCall(Get(0), p_6), p_3),
              FunCall(mult,
                FunCall(Get(1), p_6), p_4)))),
            FunCall(Reduce(fun((p_7, p_8) =>
              FunCall(Tuple(2),
                FunCall(add,
                  FunCall(Get(0), p_7),
                  FunCall(Get(0), p_8)),
                FunCall(add,
                  FunCall(Get(1), p_7),
                  FunCall(Get(1), p_8))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)),
              FunCall(Join(),
                FunCall(Map(fun((p_9) =>
                  FunCall(PartRed(fun((p_10, p_11) =>
                    FunCall(Tuple(2),
                      FunCall(add,
                        FunCall(Get(0), p_10),
                        FunCall(Get(0), p_11)),
                      FunCall(add,
                        FunCall(Get(1), p_10),
                        FunCall(Get(1), p_11))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)), p_9))),
                  FunCall(Split( K * SimplifyPow(v__2, Cst(-1)) ),
                    FunCall(Gather(ReorderWithStride(v__2)),
                      FunCall(Scatter(ReorderWithStride(v__2)),
                        FunCall(Join(),
                          FunCall(Map(fun((p_12) =>
                            FunCall(Map(fun((p_13) =>
                              FunCall(Tuple(2),
                                FunCall(mult,
                                  FunCall(Get(0), p_13),
                                  FunCall(Get(1), p_13)),
                                FunCall(mult,
                                  FunCall(Get(2), p_13),
                                  FunCall(Get(1), p_13))))), p_12))),
                            FunCall(Split( K * SimplifyPow(v__2, Cst(-1)) ),
                              FunCall(Gather(ReorderWithStride(v__2)),
                                FunCall(Zip(3),
                                  FunCall(Get(0), p_5), p_2,
                                  FunCall(Get(1), p_5))))))))))))))),
          FunCall(Zip(2), p_0, p_1))))

    val enabledMappings = EnabledMappings(
      global0 = false, global01 = false, global10 = false,
      global012 = false, global210 = false, group0 = true,
      group01 = false, group10 = false
    )

    Lower.mapCombinations(Lower.pushReduceDeeper(f), enabledMappings).head
  }
}
