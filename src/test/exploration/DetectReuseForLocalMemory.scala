package exploration

import ir._
import ir.ast._
import ir.view.{View, ViewAccess, ViewMap, ViewMem}
import lift.arithmetic._
import opencl.generator.{NDRange, RangesAndCounts}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Test

class DetectReuseForLocalMemory {

  private val v_M_0 = Var("M", StartFromRange(1))
  private val v_N_1 = Var("N", StartFromRange(1))

  def getNumDimensions(lambda: Lambda): Int = {

    val dims = Expr.visitWithState(Set[Int]())(lambda.body, {
      case (FunCall(MapLcl(dim, _), _), set) => set + dim
      case (_, set) => set
    })

    dims.size
  }

  @Test
  def introduceReuse(): Unit = {

    // Reuse across threads
    val f = fun(
      ArrayType(ArrayType(Float, v_M_0), v_N_1),
      ArrayType(Float, v_M_0),
      ArrayType(Float, v_N_1),
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

    val bla = MemoryMappingRewrite.addIdsForLocal(f)
    val reused = getReuseCandidates(bla)
    println(reused.mkString(", "))

    val readingVar = View.getSubViews(reused.last.view).last.asInstanceOf[ViewMem].v

    val numDimension = getNumDimensions(bla)

    println(bla)

    // Find which "strategic" Id location would copy the required variable and is suitable for local memory
    val tryHere = Expr.visitWithState(Seq[Expr]())(bla.body, {
      case (e@FunCall(Id(), _*) , seq) if getAllMemoryVars(e.mem).contains(readingVar) && !e.context.inMapLcl.reduce(_ || _) && e.context.inMapWrg.count(b => b) == numDimension =>
        seq :+ e
      case (_, seq) => seq
    })

    println(tryHere.mkString(", "))
  }

  private def getAllMemoryVars(memory: Memory): Seq[Var] = {
    memory match {
      case OpenCLMemoryCollection(subMemories, _) => subMemories.flatMap(getAllMemoryVars)
      case _ => Seq(memory.variable)
    }
  }

  private def getReuseCandidates(f: Lambda) = {
    val numDimensions = getNumDimensions(f)

    TypeChecker(f)
    InferOpenCLAddressSpace(f)
    RangesAndCounts(f, NDRange(?, ?, ?), NDRange(?, ?, ?), collection.Map())
    OpenCLMemoryAllocator(f)
    View(f)
    UpdateContext(f)

    val args = Expr.visitWithState(Seq[Expr]())(f.body, {
      case (call@FunCall(_: UserFun | _: VectorizeUserFun, args@_*), seq)
        if call.context.inMapWrg.count(b => b) == numDimensions
      => seq ++ args
      case (_, seq) => seq
    }).distinct.diff(f.params).filter({
      case FunCall(_: UserFun | _: VectorizeUserFun, _*) => false
      case _: Value => false
      case _ => true
    })

    args.filterNot(getNumberOfLocalAccesses(_) == numDimensions)
  }

  private def getNumberOfLocalAccesses(a: Expr) = {
    val views = View.getSubViews(a.view)

    val viewMaps = views.count({
      case ViewMap(_, v, _) => v.toString.contains("l_id")
      case _ => false
    })

    val viewAccesses = views.count({
      case ViewAccess(v, _, _) => v.toString.contains("l_id")
      case _ => false
    })

    viewAccesses - viewMaps
  }

  @Test
  def mmPlainTiling(): Unit = {
    val v_K_0 = Var("K", StartFromRange(1))
    val v_M_1 = Var("M", StartFromRange(1))
    val v_N_2 = Var("N", StartFromRange(1))
    val v__3 = Var("", RangeMul(1,1+v_M_1,2))
    val v__4 = Var("", RangeMul(1,1+v_N_2,2))
    val v__5 = Var("", RangeMul(1,1+v_K_0,2))

    // Tile reused across threads
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

    println(f)
    val reuseCandidates = getReuseCandidates(f)
    println(reuseCandidates.mkString(", "))
  }

  @Test
  def partialReduceWithReorderNoRace(): Unit = {

    // Communication between threads
    val f = fun(
      ArrayType(ArrayType(Float, v_M_0), v_N_1),
      ArrayType(Float, v_M_0),
      ArrayType(Float, v_N_1),
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
                    FunCall(Split( v_M_0 * Pow(Cst(128), Cst(-1)) ),
                      FunCall(Gather(ReorderWithStride(128)),
                        FunCall(Zip(2), p_1,
                          FunCall(Get(0), p_5))))))))))),
          FunCall(Zip(2), p_0, p_2)))

    println(f)
    val reuseCandidates = getReuseCandidates(f)
    println(reuseCandidates.mkString(", "))
  }

}
