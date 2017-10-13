package exploration.detection

import exploration.detection.DetectReuseWithinThread._
import ir._
import ir.ast._
import lift.arithmetic.{RangeMul, StartFromRange, Var}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Test

class TestDetectReuseWithinThread {

  private val v_K_0 = Var("K", StartFromRange(1))
  private val v_M_1 = Var("M", StartFromRange(1))
  private val v_N_2 = Var("N", StartFromRange(1))

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
    printStrategicLocations(mmTiled2DBlocked)
  }
}
