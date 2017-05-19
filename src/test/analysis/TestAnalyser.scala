package analysis

import ir._
import ir.ast._
import lift.arithmetic.ArithExpr.substitute
import lift.arithmetic._
import opencl.ir._
import opencl.ir.ast._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.InferNDRange

class TestAnalyser {

  @Test
  def keplerBestParametrisedVsConst(): Unit = {
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
    val p0 = SizeVar("0")
    val p1 = SizeVar("1")
    val p2 = SizeVar("2")
    val p3 = SizeVar("3")
    val p4 = SizeVar("4")

    val lambda = factory(Seq[ArithExpr](v_M_0, v_K_1, v_N_2, p0, p1, p2, p3, p4))
    val lambda2 = factory(Seq[ArithExpr](v_M_0, v_K_1, v_N_2, 128 ,4 ,8 ,64, 8))

    val valueMap = collection.immutable.Map[ArithExpr, ArithExpr](
      v_M_0 -> 1024,
      v_K_1 -> 1024,
      v_N_2 -> 1024,
      p0 -> 128,
      p1 -> 4,
      p2 -> 8,
      p3 -> 64,
      p4 -> 8
    )

    val (loc, glob) = InferNDRange(lambda)
    val localSizes = InferNDRange.substituteInNDRange(loc, valueMap)
    val globalSizes = InferNDRange.substituteInNDRange(glob, valueMap)
    

    val localSizes2 = localSizes.map(substitute(_, valueMap))
    val globalSizes2 = globalSizes.map(substitute(_, valueMap))

    val exact = true

    val accessCounts = AccessCounts(lambda, localSizes, globalSizes)
    val accessCounts2 = AccessCounts(lambda2, localSizes2, globalSizes2, valueMap)

    assertEquals(accessCounts2.getStores(GlobalMemory, exact),
      substitute(accessCounts.getStores(GlobalMemory, exact), valueMap))
    assertEquals(accessCounts2.getLoads(GlobalMemory, exact),
      substitute(accessCounts.getLoads(GlobalMemory, exact), valueMap))
    assertEquals(accessCounts2.getStores(LocalMemory, exact),
       substitute(accessCounts.getStores(LocalMemory, exact), valueMap))
    assertEquals(accessCounts2.getLoads(LocalMemory, exact),
      substitute(accessCounts.getLoads(LocalMemory, exact), valueMap))
    assertEquals(accessCounts2.getStores(PrivateMemory, exact),
      substitute(accessCounts.getStores(PrivateMemory, exact), valueMap))
    assertEquals(accessCounts2.getLoads(PrivateMemory, exact),
      substitute(accessCounts.getLoads(PrivateMemory, exact), valueMap))

    val functionCounts = FunctionCounts(lambda, localSizes, globalSizes)
    val functionCounts2 = FunctionCounts(lambda2, localSizes2, globalSizes2, valueMap)

    assertEquals(functionCounts2.getFunctionCount(add, exact),
      substitute(functionCounts.getFunctionCount(add, exact), valueMap))
    assertEquals(functionCounts2.getFunctionCount(mult, exact),
      substitute(functionCounts.getFunctionCount(mult, exact), valueMap))
    assertEquals(functionCounts2.getAddMultCount(exact),
      substitute(functionCounts.getAddMultCount(exact), valueMap))
    assertEquals(functionCounts2.getVectorisedAddMultCount(exact),
      substitute(functionCounts.getVectorisedAddMultCount(exact), valueMap))
    assertEquals(functionCounts2.getFunctionCount(dot, exact),
      substitute(functionCounts.getFunctionCount(dot, exact), valueMap))

    val controlFlow = ControlFlow(lambda, localSizes, globalSizes)
    val controlFlow2 = ControlFlow(lambda2, localSizes2, globalSizes2, valueMap)

    assertEquals(controlFlow2.getIfStatements(exact),
      substitute(controlFlow.getIfStatements(exact), valueMap))
    // TODO: Can't tell for count without the param values. Has a case where
    // TODO: it's impossible to see that a loop has only one iteration and will
    // TODO: implemented as a single statement.
//    assertEquals(controlFlow2.getForStatements(exact),
//      substitute(controlFlow.getForStatements(exact), valueMap))

    val barrierCounts = BarrierCounts(lambda, localSizes, globalSizes)
    val barrierCounts2 = BarrierCounts(lambda2, localSizes2, globalSizes2, valueMap)

    assertEquals(barrierCounts2.getTotalCount(exact),
      substitute(barrierCounts.getTotalCount(exact), valueMap))

    val memoryAmounts = MemoryAmounts(lambda, localSizes, globalSizes)
    val memoryAmounts2 = MemoryAmounts(lambda2, localSizes2, globalSizes2, valueMap)

    assertEquals(memoryAmounts2.getGlobalMemoryUsed(exact),
      substitute(memoryAmounts.getGlobalMemoryUsed(exact), valueMap))
    assertEquals(memoryAmounts2.getLocalMemoryUsed(exact),
      substitute(memoryAmounts.getLocalMemoryUsed(exact), valueMap))
    assertEquals(memoryAmounts2.getPrivateMemoryUsed(exact),
      substitute(memoryAmounts.getPrivateMemoryUsed(exact), valueMap))
  }

  @Test
  def a20a6c283140b4f147c8659bc6fd6972474efaad3c9f84f4315a0ada34e4d46e_64_4_8_128_16(): Unit = {
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
      fun(ArrayType(ArrayType(Float, v_M_0), v_K_1), ArrayType(ArrayType(Float, v_N_2), v_K_1),(p_0, p_1) =>
        FunCall(Join(),
          FunCall(MapWrg(1)(fun((p_2) =>
            FunCall(TransposeW(),
              FunCall(Join(),
                FunCall(MapWrg(0)(fun((p_3) =>
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
                          FunCall(toGlobal(fun((p_8) =>
                            FunCall(MapSeq(fun((p_9) =>
                              FunCall(MapLcl(1)(fun((p_10) =>
                                FunCall(MapLcl(0)(fun((p_11) =>
                                  FunCall(MapSeq(fun((p_12) =>
                                    FunCall(MapSeq(fun((p_13) =>
                                      FunCall(idfloat, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))),
                            FunCall(ReduceSeq(fun((p_14, p_15) =>
                              FunCall(fun((p_16) =>
                                FunCall(MapLcl(1)(fun((p_17) =>
                                  FunCall(Join(),
                                    FunCall(MapLcl(0)(fun((p_18) =>
                                      FunCall(MapSeq(fun((p_19) => p_19)),
                                        FunCall(ReduceSeq(fun((p_20, p_21) =>
                                          FunCall(fun((p_22) =>
                                            FunCall(MapSeq(fun((p_23) =>
                                              FunCall(MapSeq(fun((p_24) =>
                                                FunCall(add,
                                                  FunCall(Get(0), p_24),
                                                  FunCall(mult,
                                                    FunCall(Get(1), p_23),
                                                    FunCall(Get(1), p_24))))),
                                                FunCall(Zip(2),
                                                  FunCall(Get(0), p_23),
                                                  FunCall(Get(1), p_22))))),
                                              FunCall(Zip(2), p_20,
                                                FunCall(Get(0), p_22)))),
                                            FunCall(toPrivate(fun((p_25) =>
                                              FunCall(fun((p_26) =>
                                                FunCall(Tuple(2),
                                                  FunCall(Get(0), p_26),
                                                  FunCall(MapSeq(fun((p_27) =>
                                                    FunCall(idfloat, p_27))),
                                                    FunCall(Get(1), p_26)))), p_25))), p_21)))),
                                          FunCall(Get(0), p_18),
                                          FunCall(Zip(2),
                                            FunCall(Transpose(),
                                              FunCall(Get(1), p_17)),
                                            FunCall(Transpose(),
                                              FunCall(Get(1), p_18))))))),
                                      FunCall(Zip(2),
                                        FunCall(Get(0), p_17),
                                        FunCall(Split(v__4),
                                          FunCall(Gather(ReorderWithStride(v__3 / v__4)),
                                            FunCall(Transpose(),
                                              FunCall(Get(1), p_16))))))))),
                                  FunCall(Zip(2), p_14,
                                    FunCall(Split(v__5),
                                      FunCall(Transpose(),
                                        FunCall(Get(0), p_16)))))),
                                FunCall(toLocal(fun((p_28) =>
                                  FunCall(fun((p_29) =>
                                    FunCall(Unzip(),
                                      FunCall(MapLcl(1)(fun((p_30) =>
                                        FunCall(Tuple(2),
                                          FunCall(asScalar(),
                                            FunCall(MapLcl(0)(fun((p_31) =>
                                              FunCall(VectorizeUserFun(4,idfloat), p_31))),
                                              FunCall(asVector(4),
                                                FunCall(Get(0), p_30)))),
                                          FunCall(asScalar(),
                                            FunCall(MapLcl(0)(fun((p_32) =>
                                              FunCall(VectorizeUserFun(4,idfloat), p_32))),
                                              FunCall(asVector(4),
                                                FunCall(Get(1), p_30))))))),
                                        FunCall(Zip(2),
                                          FunCall(Get(0), p_29),
                                          FunCall(Get(1), p_29))))), p_28))), p_15)))),
                              FunCall(MapLcl(1)(fun((p_33) =>
                                FunCall(MapLcl(0)(fun((p_34) =>
                                  FunCall(MapSeq(fun((p_35) =>
                                    FunCall(MapSeq(fun((p_36) =>
                                      FunCall(idfloat, p_36))), p_35))), p_34))), p_33))), Value("0.0f", ArrayType(ArrayType(ArrayType(ArrayType(Float, v__4), v__5), v__3*1/^v__4), v__6*1/^v__5))),
                              FunCall(Zip(2), p_2, p_3))))))))),
                  FunCall(Transpose(),
                    FunCall(Map(fun((p_37) =>
                      FunCall(Transpose(), p_37))),
                      FunCall(Split(v__7),
                        FunCall(Map(fun((p_38) =>
                          FunCall(Split(v__3), p_38))), p_1))))))))),
            FunCall(Transpose(),
              FunCall(Map(fun((p_39) =>
                FunCall(Transpose(), p_39))),
                FunCall(Split(v__7),
                  FunCall(Map(fun((p_40) =>
                    FunCall(Split(v__6), p_40))), p_0)))))))
    }

    val M = SizeVar("M")
    val K = SizeVar("K")
    val N = SizeVar("N")

    val lambda = factory(Seq[ArithExpr](M, K, N, 64, 4, 8, 128, 16))

    val valueMap = collection.immutable.Map[ArithExpr, ArithExpr](
      M -> 1024,
      K -> 1024,
      N -> 1024
    )

    val (localSizes, globalSizes) = InferNDRange(lambda)

    val actualLocalSizes = localSizes.map(substitute(_, valueMap))
    val actualGlobalSizes = globalSizes.map(substitute(_, valueMap))

    val exact = true

    val memoryAmounts = MemoryAmounts(lambda, actualLocalSizes, actualGlobalSizes, valueMap)
    val accessCounts = AccessCounts(lambda, actualLocalSizes, actualGlobalSizes, valueMap)
    val barrierCounts = BarrierCounts(lambda, actualLocalSizes, actualGlobalSizes, valueMap)
    val controlFlow = ControlFlow(lambda, actualLocalSizes, actualGlobalSizes, valueMap)
    val functionCounts = FunctionCounts(lambda, actualLocalSizes, actualGlobalSizes, valueMap)

    val globalMemory = memoryAmounts.getGlobalMemoryUsed(exact).evalLong
    val localMemory = memoryAmounts.getLocalMemoryUsed(exact).evalLong
    val privateMemory = memoryAmounts.getPrivateMemoryUsed(exact).evalLong

    assertEquals(12582912l, globalMemory)
    assertEquals(12288l, localMemory)
    assertEquals(272l, privateMemory)

    val scalarGlobalStores =
      accessCounts.scalarStores(GlobalMemory, UnknownPattern, exact).evalDouble
    val scalarGlobalLoads =
      accessCounts.scalarLoads(GlobalMemory, UnknownPattern, exact).evalDouble
    val scalarLocalStores =
      accessCounts.scalarStores(LocalMemory, UnknownPattern, exact).evalDouble
    val scalarLocalLoads =
      accessCounts.scalarLoads(LocalMemory, UnknownPattern, exact).evalDouble

    assertEquals(0.0, scalarGlobalStores, 0.0)
    assertEquals(0.0, scalarGlobalLoads, 0.0)
    assertEquals(0.0, scalarLocalStores, 0.0)
    assertEquals(32768.0, scalarLocalLoads, 0.0)

    val privateScalarStores =
      accessCounts.scalarStores(PrivateMemory, UnknownPattern, exact).evalDouble
    val privateScalarLoads =
      accessCounts.scalarLoads(PrivateMemory, UnknownPattern, exact).evalDouble

    assertEquals(69664.0, privateScalarStores, 0.0)
    assertEquals(98368.0, privateScalarLoads, 0.0)

    val barriers = barrierCounts.getTotalCount(exact).evalDouble

    // TODO: Could be 128
    assertEquals(256.0, barriers, 0.0)

    val scalarCoalescedGlobalStores =
      accessCounts.scalarStores(GlobalMemory, CoalescedPattern, exact).evalDouble
    val scalarCoalescedGlobalLoads =
      accessCounts.scalarLoads(GlobalMemory, CoalescedPattern, exact).evalDouble

    assertEquals(32.0, scalarCoalescedGlobalStores, 0.0)
    assertEquals(0.0, scalarCoalescedGlobalLoads, 0.0)

    val scalarCoalescedLocalStores =
      accessCounts.scalarStores(LocalMemory, CoalescedPattern, exact).evalDouble
    val scalarCoalescedLocalLoads =
      accessCounts.scalarLoads(LocalMemory, CoalescedPattern, exact).evalDouble

    assertEquals(0.0, scalarCoalescedLocalStores, 0.0)
    assertEquals(4096.0, scalarCoalescedLocalLoads, 0.0)

    val vectorCoalescedGlobalStores =
      accessCounts.vectorStores(GlobalMemory, CoalescedPattern, exact).evalDouble
    val vectorCoalescedGlobalLoads =
      accessCounts.vectorLoads(GlobalMemory, CoalescedPattern, exact).evalDouble

    assertEquals(0.0, vectorCoalescedGlobalStores, 0.0)
    assertEquals(192.0, vectorCoalescedGlobalLoads, 0.0)

    val vectorCoalescedLocalStores =
      accessCounts.vectorStores(LocalMemory, CoalescedPattern, exact).evalDouble
    val vectorCoalescedLocalLoads =
      accessCounts.vectorLoads(LocalMemory, CoalescedPattern, exact).evalDouble

    assertEquals(192.0, vectorCoalescedLocalStores, 0.0)
    assertEquals(0.0, vectorCoalescedLocalLoads, 0.0)

    val vectorGlobalStores =
      accessCounts.vectorStores(GlobalMemory, UnknownPattern, exact).evalDouble
    val vectorGlobalLoads =
      accessCounts.vectorLoads(GlobalMemory, UnknownPattern, exact).evalDouble

    assertEquals(0.0, vectorGlobalStores, 0.0)
    assertEquals(0.0, vectorGlobalLoads, 0.0)

    val vectorLocalStores =
      accessCounts.vectorStores(LocalMemory, UnknownPattern, exact).evalDouble
    val vectorLocalLoads =
      accessCounts.vectorLoads(LocalMemory, UnknownPattern, exact).evalDouble

    assertEquals(0.0, vectorLocalStores, 0.0)
    assertEquals(0.0, vectorLocalLoads, 0.0)

    val vectorPrivateStores =
      accessCounts.vectorStores(PrivateMemory, UnknownPattern, exact).evalDouble
    val vectorPrivateLoads =
      accessCounts.vectorLoads(PrivateMemory, UnknownPattern, exact).evalDouble

    assertEquals(0.0, vectorPrivateStores, 0.0)
    assertEquals(0.0, vectorPrivateLoads, 0.0)

    val ifStatements = controlFlow.getIfStatements(exact).evalDouble
    val forBranches = controlFlow.getForBranches(exact).evalDouble

    assertEquals(0.0, ifStatements, 0.0)
    assertEquals(1216.0, forBranches, 0.0)

    val dotCount = functionCounts.getFunctionCount(dot, exact).evalDouble

    assertEquals(0.0, dotCount, 0.0)
  }

}
