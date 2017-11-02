package exploration.mapping

import exploration.MemoryMappingRewrite
import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.LongTestsEnabled
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.utils.Utils.getHash

class TestMemoryMappingRewrite {

  LongTestsEnabled()

  @Test
  def unrolledReduce(): Unit = {
    val highLevel = fun(ArrayType(Float, N),(p_0) => FunCall(Join(), FunCall(Map(fun((p_1) => FunCall(Reduce(fun((p_2, p_3) => FunCall(add, p_2, p_3))), Value("0.0f", Float), p_1))), FunCall(Slide(3,1), FunCall(Pad(1,1,Pad.Boundary.Clamp), p_0)))))
    val gold = fun(ArrayType(Float, N),(p_0) =>
      FunCall(Join(),
        FunCall(MapGlb(0)(fun((p_1) =>
          FunCall(toGlobal(fun((p_2) =>
            FunCall(MapSeq(fun((p_3) =>
              FunCall(idfloat, p_3))), p_2))),
              FunCall(ReduceSeqUnroll(fun((p_5, p_6) =>
                    FunCall(add, p_5, p_6))),
                FunCall(idfloat, Value("0.0f", Float)), p_1)))),
          FunCall(Slide(3,1),
            FunCall(Pad(1,1,Pad.Boundary.Clamp), p_0)))))
    val goldHash = getHash(gold)

    val mapped = MemoryMappingRewrite.lowerLambda(highLevel, enabledMappings, unroll = true)
    println(mapped)
    assertTrue(mapped.exists(getHash(_) == goldHash))
  }

  @Test
  def kmeans(): Unit = {

    val v_P_0 = SizeVar("P")
    val v_F_1 = SizeVar("F")
    val v_C_2 = SizeVar("C")

    val currentDistance = UserFun("currentDistance", Array("x", "y"), """|{ return (x - y) * (x - y); }""".stripMargin, Seq(Float, Float), Float)
    val test = UserFun("test", Array("dist", "tuple"), """|{float min_dist = tuple._0;int i          = tuple._1;int index      = tuple._2;if (dist < min_dist) {  Tuple t = {dist, i + 1, i};  return t;} else {  Tuple t = {min_dist, i + 1, index};  return t;}}""".stripMargin, Seq(Float, TupleType(Float, Int, Int)), TupleType(Float, Int, Int))
    val select_ = UserFun("select_", Array("tuple"), """|{ return tuple._2; }""".stripMargin, Seq(TupleType(Float, Int, Int)), Int)

    val idTuple_float_int_int = UserFun("idTuple3_float_int_int", Array("x"), """|{ return x; }""".stripMargin, Seq(TupleType(Float, Int, Int)), TupleType(Float, Int, Int))

    val start = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_P_0), v_F_1), ArrayTypeWSWC(ArrayTypeWSWC(Float, v_F_1), v_C_2),(p_0, p_1) => FunCall(Map(fun((p_2) => FunCall(Map(fun((p_3) => FunCall(Map(fun((p_4) => FunCall(select_, p_4))), p_3))), FunCall(ReduceSeq(fun((p_5, p_6) => FunCall(Map(fun((p_7) => FunCall(test, FunCall(Get(0), p_7), FunCall(Get(1), p_7)))), FunCall(Zip(2), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(add, p_8, FunCall(currentDistance, FunCall(Get(0), p_9), FunCall(Get(1), p_9))))), Value("0.0f", Float), FunCall(Zip(2), p_2, p_6)), p_5)))), Value("{3.40282347e+38, 0, 0}", ArrayTypeWSWC(TupleType(Float, Int, Int), 1)), p_1)))), FunCall(Transpose(), p_0)))

    val kmeansGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_P_0), v_F_1), ArrayTypeWSWC(ArrayTypeWSWC(Float, v_F_1), v_C_2),(p_0, p_1) =>
      FunCall(MapGlb(0)(fun((p_2) =>
        FunCall(MapSeq(fun((p_3) =>
            FunCall(MapSeq(fun((p_5) =>
              FunCall(toGlobal(select_), p_5))), p_3))),
          FunCall(ReduceSeq(fun((p_6, p_7) =>
                FunCall(MapSeq(fun((p_10) =>
                  FunCall(test,
                    FunCall(Get(0), p_10),
                    FunCall(Get(1), p_10)))),
                  FunCall(Zip(2),
                      FunCall(ReduceSeq(fun((p_12, p_13) =>
                            FunCall(add, p_12,
                              FunCall(toPrivate(currentDistance),
                                FunCall(Get(0), p_13),
                                FunCall(Get(1), p_13))))),
                        FunCall(idfloat, Value("0.0f", Float)),
                        FunCall(Zip(2), p_2, p_7)), p_6)))),
            FunCall(MapSeq(fun((p_16) =>
              FunCall(idTuple_float_int_int, p_16))), Value("{3.40282347e+38, 0, 0}", ArrayTypeWSWC(TupleType(Float, Int, Int), 1))), p_1)))),
        FunCall(Transpose(), p_0)))
    val kmeansHash = getHash(kmeansGold)

    val mapped = MemoryMappingRewrite.lowerLambda(start, enabledMappings)

    assertTrue(mapped.exists(getHash(_) == kmeansHash))
  }
}
