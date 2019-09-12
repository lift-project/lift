package rewriting

import ir._
import ir.ast._
import lift.arithmetic._
import lift.arithmetic.simplifier.SimplifyPow
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

class TestInferNDRange {

  @Test
  def mmKeplerBest(): Unit = {
    val factory = (variables: Seq[ArithExpr]) => {
      val v_M_0 = variables(0)
      val v_K_1 = variables(1)
      val v_N_2 = variables(2)
      val v__3 = variables(3)
      val v__4 = variables(4)
      val v__5 = variables(5)
      val v__6 = variables(6)
      val v__7 = variables(7)

      fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1), ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1), (p_0, p_1) => FunCall(Join(), FunCall(MapWrg(1)(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(MapWrg(0)(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_4))))))), FunCall(TransposeW(), FunCall(toGlobal(fun((p_8) => FunCall(MapSeq(fun((p_9) => FunCall(MapLcl(1)(fun((p_10) => FunCall(MapLcl(0)(fun((p_11) => FunCall(MapSeq(fun((p_12) => FunCall(MapSeq(fun((p_13) => FunCall(id, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))), FunCall(ReduceSeq(fun((p_14, p_15) => FunCall(fun((p_16) => FunCall(MapLcl(1)(fun((p_17) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_18) => FunCall(MapSeq(fun((p_19) => p_19)), FunCall(ReduceSeq(fun((p_20, p_21) => FunCall(fun((p_22) => FunCall(MapSeq(fun((p_23) => FunCall(MapSeq(fun((p_24) => FunCall(add, FunCall(Get(0), p_24), FunCall(mult, FunCall(Get(1), p_23), FunCall(Get(1), p_24))))), FunCall(Zip(2), FunCall(Get(0), p_23), FunCall(Get(1), p_22))))), FunCall(Zip(2), p_20, FunCall(Get(0), p_22)))), FunCall(toPrivate(fun((p_25) => FunCall(fun((p_26) => FunCall(Tuple(2), FunCall(MapSeq(fun((p_27) => FunCall(id, p_27))), FunCall(Get(0), p_26)), FunCall(MapSeq(fun((p_28) => FunCall(id, p_28))), FunCall(Get(1), p_26)))), p_25))), p_21)))), FunCall(Get(0), p_18), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p_17)), FunCall(Transpose(), FunCall(Get(1), p_18))))))), FunCall(Zip(2), FunCall(Get(0), p_17), FunCall(Split(v__4), FunCall(Gather(ReorderWithStride(v__3 / v__4)), FunCall(Transpose(), FunCall(Get(1), p_16))))))))), FunCall(Zip(2), p_14, FunCall(Split(v__5), FunCall(Transpose(), FunCall(Get(0), p_16)))))), FunCall(toLocal(fun((p_29) => FunCall(fun((p_30) => FunCall(Unzip(), FunCall(MapLcl(1)(fun((p_31) => FunCall(Tuple(2), FunCall(MapLcl(0)(fun((p_32) => FunCall(id, p_32))), FunCall(Get(0), p_31)), FunCall(MapLcl(0)(fun((p_33) => FunCall(id, p_33))), FunCall(Get(1), p_31))))), FunCall(Zip(2), FunCall(Get(0), p_30), FunCall(Get(1), p_30))))), p_29))), p_15)))), FunCall(MapLcl(1)(fun((p_34) => FunCall(MapLcl(0)(fun((p_35) => FunCall(MapSeq(fun((p_36) => FunCall(MapSeq(fun((p_37) => FunCall(id, p_37))), p_36))), p_35))), p_34))), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3 * 1 /^ v__4), v__6 * 1 /^ v__5))), FunCall(Zip(2), p_2, p_3))))))))), FunCall(Transpose(), FunCall(Map(fun((p_38) => FunCall(Transpose(), p_38))), FunCall(Split(v__7), FunCall(Map(fun((p_39) => FunCall(Split(v__3), p_39))), p_1))))))))), FunCall(Transpose(), FunCall(Map(fun((p_40) => FunCall(Transpose(), p_40))), FunCall(Split(v__7), FunCall(Map(fun((p_41) => FunCall(Split(v__6), p_41))), p_0)))))))
    }

    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")

    val f = factory(Seq[ArithExpr](v_M_0, v_K_1, v_N_2, 128, 4, 8, 64 ,8))

    val (local, global) = InferNDRange(f)

    assertEquals(Cst(32), local(0))
    assertEquals(Cst(8), local(1))
    assertEquals(Cst(1), local(2))

    assertEquals(v_N_2 /^ 4, global(0))
    assertEquals(v_M_0 /^ 8, global(1))
    assertEquals(Cst(1), global(2))
  }

  @Test
  def random0(): Unit = {
    val factory = (variables: Seq[ArithExpr]) => {
      val v_M_0 = variables(0)
      val v_K_1 = variables(1)
      val v_N_2 = variables(2)
      val v__3 = variables(3)
      val v__4 = variables(4)
      val v__5 = variables(5)
      val v__6 = variables(6)
      val v__7 = variables(7)

      val idfloat = UserFun("idfloat", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
      val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
      val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
      fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1), ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1),(p_0, p_1) => FunCall(Join(), FunCall(MapWrg(1)(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(MapWrg(0)(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(Scatter(ReorderWithStride((v__3) / (v__4))), p_5))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_4))))))), FunCall(TransposeW(), FunCall(toGlobal(fun((p_8) => FunCall(MapSeq(fun((p_9) => FunCall(MapLcl(1)(fun((p_10) => FunCall(MapLcl(0)(fun((p_11) => FunCall(MapSeq(fun((p_12) => FunCall(MapSeq(fun((p_13) => FunCall(idfloat, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))), FunCall(ReduceSeq(fun((p_14, p_15) => FunCall(fun((p_16) => FunCall(MapLcl(1)(fun((p_17) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_18) => FunCall(MapSeq(fun((p_19) => p_19)), FunCall(ReduceSeq(fun((p_20, p_21) => FunCall(fun((p_22) => FunCall(MapSeq(fun((p_23) => FunCall(MapSeq(fun((p_24) => FunCall(add, FunCall(Get(0), p_24), FunCall(mult, FunCall(Get(1), p_23), FunCall(Get(1), p_24))))), FunCall(Zip(2), FunCall(Get(0), p_23), FunCall(Get(1), p_22))))), FunCall(Zip(2), p_20, FunCall(Get(0), p_22)))), FunCall(toPrivate(fun((p_25) => FunCall(fun((p_26) => FunCall(Tuple(2), FunCall(Get(0), p_26), FunCall(MapSeq(fun((p_27) => FunCall(idfloat, p_27))), FunCall(Get(1), p_26)))), p_25))), p_21)))), FunCall(Get(0), p_18), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p_17)), FunCall(Transpose(), FunCall(Get(1), p_18))))))), FunCall(Zip(2), FunCall(Get(0), p_17), FunCall(Split(v__4), FunCall(Gather(ReorderWithStride((v__3) / (v__4))), FunCall(Transpose(), FunCall(Get(1), p_16))))))))), FunCall(Zip(2), p_14, FunCall(Split(v__5), FunCall(Transpose(), FunCall(Get(0), p_16)))))), FunCall(toLocal(fun((p_28) => FunCall(fun((p_29) => FunCall(Unzip(), FunCall(MapLcl(1)(fun((p_30) => FunCall(Tuple(2), FunCall(MapLcl(0)(fun((p_31) => FunCall(idfloat, p_31))), FunCall(Get(0), p_30)), FunCall(MapLcl(0)(fun((p_32) => FunCall(idfloat, p_32))), FunCall(Get(1), p_30))))), FunCall(Zip(2), FunCall(Get(0), p_29), FunCall(Get(1), p_29))))), p_28))), p_15)))), FunCall(MapLcl(1)(fun((p_33) => FunCall(MapLcl(0)(fun((p_34) => FunCall(MapSeq(fun((p_35) => FunCall(MapSeq(fun((p_36) => FunCall(idfloat, p_36))), p_35))), p_34))), p_33))), FunCall(toLocal(fun((p_37) => FunCall(MapLcl(1)(fun((p_38) => FunCall(MapLcl(0)(fun((p_39) => FunCall(MapSeq(fun((p_40) => FunCall(MapSeq(fun((p_41) => FunCall(idfloat, p_41))), p_40))), p_39))), p_38))), p_37))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), (v__3*1/^(v__4))), (v__6*1/^(v__5)))))), FunCall(Zip(2), p_2, p_3))))))))), FunCall(Transpose(), FunCall(Map(fun((p_42) => FunCall(Transpose(), p_42))), FunCall(Split(v__7), FunCall(Map(fun((p_43) => FunCall(Split(v__3), p_43))), p_1))))))))), FunCall(Transpose(), FunCall(Map(fun((p_44) => FunCall(Transpose(), p_44))), FunCall(Split(v__7), FunCall(Map(fun((p_45) => FunCall(Split(v__6), p_45))), p_0)))))))
    }

    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")

    val f = factory(Seq[ArithExpr](v_M_0, v_K_1, v_N_2, 8, 2, 4, 128, 64))

    val (local, global) = InferNDRange(f)

    assertEquals(Cst(4), local(0))
    assertEquals(Cst(32), local(1))
    assertEquals(Cst(1), local(2))

    assertEquals(v_N_2 /^ 2, global(0))
    assertEquals(v_M_0 /^ 4, global(1))
    assertEquals(Cst(1), global(2))
  }

  @Test
  def fixDependedingOnToGlobalLocation(): Unit = {
    val v_M_0 = SizeVar("M")
    val v_N_1 = SizeVar("N")

    val idfloat = UserFun("idfloat", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
    val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
    val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)

    val f0 =
      fun(
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
                      FunCall(toPrivate(fun((p_9, p_10) =>
                        FunCall(mult, p_9, p_10))), p_8, p_3),
                      FunCall(toPrivate(fun((p_11, p_12) =>
                        FunCall(mult, p_11, p_12))),
                        FunCall(Get(1), p_5), p_4)))), p_7))),
                  FunCall(ReduceSeq(fun((p_13, p_14) =>
                    FunCall(add, p_13, p_14))),
                    FunCall(idfloat, Value("0.0f", Float)), p_6)))),
                FunCall(Split(Cst(128)),
                  FunCall(Join(),
                    FunCall(MapLcl(0)(fun((p_15) =>
                      FunCall(MapSeq(fun((p_16) =>
                        FunCall(toLocal(fun((p_17) =>
                          FunCall(idfloat, p_17))), p_16))),
                        FunCall(ReduceSeq(fun((p_18, p_19) =>
                          FunCall(add, p_18,
                            FunCall(mult,
                              FunCall(Get(0), p_19),
                              FunCall(Get(1), p_19))))),
                          FunCall(idfloat, Value("0.0f", Float)), p_15)))),
                      FunCall(Split( v_M_0 * SimplifyPow(Cst(128), Cst(-1)) ),
                        FunCall(Gather(ReorderWithStride(128)),
                          FunCall(Zip(2), p_1,
                            FunCall(Get(0), p_5))))))))))),
            FunCall(Zip(2), p_0, p_2)))

    val f1 =
      fun(
        ArrayType(ArrayType(Float, v_M_0), v_N_1),
        ArrayType(Float, v_M_0),
        ArrayType(Float, v_N_1),
        Float, Float,
        (p_0, p_1, p_2, p_3, p_4) =>
          FunCall(MapWrg(0)(fun((p_5) =>
            FunCall(Join(),
              FunCall(toGlobal(fun((p_7) =>
                FunCall(MapLcl(0)(fun((p_6) =>
                  FunCall(MapSeq(fun((p_8) =>
                    FunCall(add,
                      FunCall(toPrivate(fun((p_9, p_10) =>
                        FunCall(mult, p_9, p_10))), p_8, p_3),
                      FunCall(toPrivate(fun((p_11, p_12) =>
                        FunCall(mult, p_11, p_12))),
                        FunCall(Get(1), p_5), p_4)))),
                    FunCall(ReduceSeq(fun((p_13, p_14) =>
                      FunCall(add, p_13, p_14))),
                      FunCall(idfloat, Value("0.0f", Float)), p_6)))), p_7))),
                FunCall(Split(Cst(128)),
                  FunCall(Join(),
                    FunCall(MapLcl(0)(fun((p_15) =>
                      FunCall(MapSeq(fun((p_16) =>
                        FunCall(toLocal(fun((p_17) =>
                          FunCall(idfloat, p_17))), p_16))),
                        FunCall(ReduceSeq(fun((p_18, p_19) =>
                          FunCall(add, p_18,
                            FunCall(mult,
                              FunCall(Get(0), p_19),
                              FunCall(Get(1), p_19))))),
                          FunCall(idfloat, Value("0.0f", Float)), p_15)))),
                      FunCall(Split( v_M_0 * SimplifyPow(Cst(128), Cst(-1)) ),
                        FunCall(Gather(ReorderWithStride(128)),
                          FunCall(Zip(2), p_1,
                            FunCall(Get(0), p_5))))))))))),
            FunCall(Zip(2), p_0, p_2)))

    val (local0, global0) = InferNDRange(f0)
    val (local1, global1) = InferNDRange(f1)

    assertEquals(local0, local1)
    assertEquals(global0, global1)
  }

}
