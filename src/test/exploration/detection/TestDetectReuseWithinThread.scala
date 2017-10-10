package exploration.detection

import exploration.MemoryMappingRewrite
import ir._
import ir.ast._
import ir.view.{View, ViewAccess, ViewMap, ViewMem}
import lift.arithmetic.{RangeMul, StartFromRange, Var}
import opencl.ir.OpenCLMemory.getAllMemoryVars
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Test

class TestDetectReuseWithinThread {
  private val v_K_0 = Var("K", StartFromRange(1))
  private val v_M_1 = Var("M", StartFromRange(1))
  private val v_N_2 = Var("N", StartFromRange(1))

  private def getReuseCandidates(f: Lambda) = {
    val numDimensions = getNumDimensions(f)

    prepareLambda(f)

    val args = Expr.visitWithState(Seq[Expr]())(f.body, {
      case (call@FunCall(_: UserFun | _: VectorizeUserFun, args@_*), seq)
        if call.context.inMapLcl.count(b => b) + call.context.inMapGlb.count(b => b) == numDimensions
      => seq ++ args
      case (_, seq) => seq
    }).distinct.diff(f.params).filter({
      case FunCall(_: UserFun | _: VectorizeUserFun, _*) => false
      case _: Value => false
      case _ => true
    })

    args.filterNot(arg => getNumberOfPrivateAccesses(arg) >= getNumberOfSequentialDimensions(f, arg))
  }

  private def getNumberOfSequentialDimensions(f: Lambda, expr: Expr) = {

    val views = View.getSubViews(expr.view)

    Expr.visitWithState(0)(f.body, {
      case (FunCall(fp: ReduceSeq, acc, _), count)
        if containsExprButNotParallel(fp, expr) && !views.contains(acc.view) => count + 1
      case (FunCall(fp: MapSeq, _), count) if containsExprButNotParallel(fp, expr) => count + 1
      case (_, count) => count
    })
  }

  private def containsExprButNotParallel(fp: FunDecl with FPattern, expr: Expr) = {
    fp.f.body.contains({ case e if e eq expr => }) && // e contains expr
      !fp.f.body.contains({ case FunCall(MapLcl(_, _), _) => }) && // e doesn't contain parallel
      !fp.f.body.contains({ case FunCall(MapWrg(_, _), _) => }) &&
      !fp.f.body.contains({ case FunCall(MapGlb(_, _), _) => })
  }

  private def getNumberOfPrivateAccesses(expr: Expr) = {
    val views = View.getSubViews(expr.view)

    val sequentialMapViews = views.takeWhile({
      case ViewAccess(v, _, _) => v.toString.startsWith("v_i_")
      case ViewMap(_, v, _) => v.toString.startsWith("v_i_")
      case _ => true
    })

    val viewMaps = sequentialMapViews.count({
      case ViewMap(_, v, _) => v.toString.startsWith("v_i_")
      case _ => false
    })

    val viewAccesses = sequentialMapViews.count({
      case ViewAccess(v, _, _) => v.toString.startsWith("v_i_")
      case _ => false
    })

    viewAccesses - viewMaps
  }

  private def printStrategicLocations(lambda: Lambda): Unit = {
    val strategicLocationsMarked = MemoryMappingRewrite.addIdsForPrivate(lambda)
    val reuseCandidates = getReuseCandidates(strategicLocationsMarked)
    val tryHere = reuseCandidates.flatMap(getRuleLocationCandidates(strategicLocationsMarked, _))

    println
    println(strategicLocationsMarked)
    println(tryHere.mkString(", "))
    println
  }

  private def getRuleLocationCandidates(strategicLocationsMarked: Lambda, reuseExpr: Expr) = {
    val sourceView = View.getSubViews(reuseExpr.view).last

    sourceView match {
      case ViewMem(sourceVar, _) =>

        // Find which "strategic" Id location(s) would copy the required variable and is suitable for local memory
        // TODO: Doesn't work for all reuse... Does it matter? Still gets what we care about
        Expr.visitWithState(Seq[(Expr, Var)]())(strategicLocationsMarked.body, {
          case (funCall@FunCall(Id(), _*), seq)
            if getAllMemoryVars(funCall.mem).contains(sourceVar) &&
              (funCall.context.inMapLcl.reduce(_ || _) || funCall.context.inMapGlb.reduce(_ || _)) // TODO: insideAll
          =>
            seq :+ (funCall, sourceVar)
          case (_, seq) => seq
        })

      case _ => Seq()
    }
  }

  @Test
  def oneDRegBlock(): Unit = {
    val v__3 = Var("", RangeMul(1,1+v_M_1,2))

    val f = fun(
      ArrayType(ArrayType(Float, v_K_0), v_M_1),
      ArrayType(ArrayType(Float, v_K_0), v_N_2),
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

    printStrategicLocations(f)
  }

  @Test
  def twoDRegBlock(): Unit = {
    val v__3 = Var("", RangeMul(1,1+v_M_1,2))
    val v__4 = Var("", RangeMul(1,1+v_N_2,2))

    val f = fun(
      ArrayType(ArrayType(Float, v_K_0), v_M_1),
      ArrayType(ArrayType(Float, v_K_0), v_N_2),
      (p_0, p_1) =>
        FunCall(Map(fun((p_2) =>
          FunCall(Scatter(ReorderWithStride(v_N_2 / v__4)), p_2))),
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
                      FunCall(Gather(ReorderWithStride(v_N_2 / v__4)), p_1))))))),
              FunCall(Split(v__3), p_0)))))

    printStrategicLocations(f)
  }

  @Test
  def tilingAndOneDRegBlock(): Unit = {
    val v__3 = Var("", RangeMul(1,1+v_M_1,2))
    val v__4 = Var("", RangeMul(1,1+v_K_0,2))
    val v__5 = Var("", RangeMul(1,1+v_N_2,2))
    val v__6 = Var("", RangeMul(1,1+v__3,2))

    val f = fun(
      ArrayType(ArrayType(Float, v_K_0), v_M_1),
      ArrayType(ArrayType(Float, v_N_2), v_K_0),
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

    printStrategicLocations(f)
  }

  @Test
  def tilingAndTwoDRegBlock(): Unit = {
    val v__3 = Var("", RangeMul(1,1+v_M_1,2))
    val v__4 = Var("", RangeMul(1,1+v_N_2,2))
    val v__5 = Var("", RangeMul(1,1+v_K_0,2))
    val v__6 = Var("", RangeMul(1,1+v__3,2))
    val v__7 = Var("", RangeMul(1,1+v__4,2))

    val f = fun(
      ArrayType(ArrayType(Float, v_K_0), v_M_1),
      ArrayType(ArrayType(Float, v_N_2), v_K_0),
      (p_0, p_1) =>
        FunCall(Join(),
          FunCall(MapWrg(1)(fun((p_2) =>
            FunCall(TransposeW(),
              FunCall(Join(),
                FunCall(MapWrg(0)(fun((p_3) =>
                  FunCall(TransposeW(),
                    FunCall(Join(),
                      FunCall(Map(fun((p_4) =>
                        FunCall(Map(fun((p_5) =>
                          FunCall(Scatter(ReorderWithStride(v__4 / v__7)), p_5))),
                          FunCall(TransposeW(),
                            FunCall(Join(),
                              FunCall(Map(fun((p_6) =>
                                FunCall(TransposeW(),
                                  FunCall(Map(fun((p_7) =>
                                    FunCall(TransposeW(), p_7))),
                                    FunCall(TransposeW(), p_6))))),
                                FunCall(TransposeW(), p_4))))))),
                        FunCall(TransposeW(),
                          FunCall(toGlobal(fun((p_8) =>
                            FunCall(MapSeq(fun((p_9) =>
                              FunCall(MapLcl(1)(fun((p_10) =>
                                FunCall(MapLcl(0)(fun((p_11) =>
                                  FunCall(MapSeq(fun((p_12) =>
                                    FunCall(MapSeq(fun((p_13) =>
                                      FunCall(idfloat, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))),
                            FunCall(ReduceSeq(fun((p_14, p_15) =>
                              FunCall(MapLcl(1)(fun((p_16) =>
                                FunCall(Join(),
                                  FunCall(MapLcl(0)(fun((p_17) =>
                                    FunCall(ReduceSeq(fun((p_18, p_19) =>
                                      FunCall(MapSeq(fun((p_20) =>
                                        FunCall(MapSeq(fun((p_21) =>
                                          FunCall(add,
                                            FunCall(Get(0), p_21),
                                            FunCall(mult,
                                              FunCall(Get(1), p_20),
                                              FunCall(Get(1), p_21))))),
                                          FunCall(Zip(2),
                                            FunCall(Get(0), p_20),
                                            FunCall(Get(1), p_19))))),
                                        FunCall(Zip(2), p_18,
                                          FunCall(Get(0), p_19))))),
                                      FunCall(Get(0), p_17),
                                      FunCall(Zip(2),
                                        FunCall(Transpose(),
                                          FunCall(Get(1), p_16)),
                                        FunCall(Transpose(),
                                          FunCall(Get(1), p_17)))))),
                                    FunCall(Zip(2),
                                      FunCall(Get(0), p_16),
                                      FunCall(Split(v__7),
                                        FunCall(Gather(ReorderWithStride(v__4 / v__7)),
                                          FunCall(Transpose(),
                                            FunCall(Get(1), p_15))))))))),
                                FunCall(Zip(2), p_14,
                                  FunCall(Split(v__6),
                                    FunCall(Transpose(),
                                      FunCall(Get(0), p_15))))))),
                              FunCall(MapLcl(1)(fun((p_22) =>
                                FunCall(MapLcl(0)(fun((p_23) =>
                                  FunCall(MapSeq(fun((p_24) =>
                                    FunCall(MapSeq(fun((p_25) =>
                                      FunCall(idfloat, p_25))), p_24))), p_23))), p_22))), Value("0.0f", ArrayType(ArrayType(ArrayType(ArrayType(Float, v__7), v__6), v__4*1/^v__7), v__3*1/^v__6))),
                              FunCall(Zip(2),
                                FunCall(Split(v__5),
                                  FunCall(Transpose(), p_2)),
                                FunCall(Split(v__5),
                                  FunCall(Transpose(), p_3))))))))))),
                  FunCall(Split(v__4),
                    FunCall(Transpose(), p_1))))))),
            FunCall(Split(v__3), p_0))))

    printStrategicLocations(f)
  }
}
