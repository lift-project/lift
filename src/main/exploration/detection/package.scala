package exploration

import ir.ast.{Expr, FunCall, Lambda, _}
import ir.view.{NoView, View}
import ir.{TypeChecker, UnallocatedMemory, UpdateContext, _}
import lift.arithmetic.{?, _}
import opencl.generator.{NDRange, RangesAndCounts}
import opencl.ir.pattern.{MapGlb, MapLcl, _}
import opencl.ir.{InferOpenCLAddressSpace, OpenCLMemoryAllocator, _}
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
    if (f.body.context == null || f.body.view == NoView || f.body.mem == UnallocatedMemory) {
      TypeChecker(f)
      InferOpenCLAddressSpace(f)
      RangesAndCounts(f, NDRange(?, ?, ?), NDRange(?, ?, ?), collection.Map())
      OpenCLMemoryAllocator(f)
      View(f)
      UpdateContext(f)
    }
  }

  private val M = Var("M", StartFromRange(1))
  private val N = Var("N", StartFromRange(1))
  private val K = Var("K", StartFromRange(1))

  val mmIntroduceReuse = fun(
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

  val mmTiled: Lambda = {
    val K = Var("K", StartFromRange(1))
    val M = Var("M", StartFromRange(1))
    val N = Var("N", StartFromRange(1))
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
}
