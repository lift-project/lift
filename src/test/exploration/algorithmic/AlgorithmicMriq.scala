package exploration.algorithmic

import exploration.HighLevelRewrite
import ir._
import ir.ast._
import lift.arithmetic._
import opencl.executor.LongTestsEnabled
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import rewriting.utils.Utils.getHash

class AlgorithmicMriq {

  LongTestsEnabled()

  private val mapFun = UserFun("mapFun",
    Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag"),
    """{
      |    #define PIx2 6.2831853071795864769252867665590058f
      |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
      |    Tuple2_float_float bla = { PhiMag * cos(expArg), PhiMag * sin(expArg) };
      |    return  bla;
      |}""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float), TupleType(Float, Float))

  private val reduceFun = UserFun("reduceFun",
    Array("x", "y"),
    """{
          | x._0 += y._0;
          | x._1 += y._1;
          | return x;
        }""".stripMargin,
    Seq(TupleType(Float, Float), TupleType(Float, Float)), TupleType(Float, Float))


  private val mriqComputeQ = fun(
    ArrayType(Float, X),
    ArrayType(Float, X),
    ArrayType(Float, X),
    ArrayType(TupleType(Float, Float, Float, Float), K),
    (x, y, z, kValues) =>
      Map(\(t =>
        Reduce(reduceFun, Value("{ 0.0f, 0.0f}", TupleType(Float, Float))) o
          Map(\(k => mapFun(t._0, t._1, t._2, k._0, k._1, k._2, k._3))) $ kValues
      )) $ Zip(x, y, z)
  )

  def mriqComputeQRewrite(): Unit = {

    val introduceReuseGold = fun(ArrayType(Float, X), ArrayType(Float, X), ArrayType(Float, X), ArrayType(TupleType(Float, Float, Float, Float), K),(p_0, p_1, p_2, p_3) => FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_5, p_6) => FunCall(Join(), FunCall(Map(fun((p_7) => FunCall(PartRed(fun((p_8, p_9) => FunCall(reduceFun, p_8, p_9))), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))), FunCall(Zip(2), p_5, p_6))))), Value("{ 0.0f, 0.0f}", ArrayType(TupleType(Float, Float), v__2)), FunCall(Transpose(), FunCall(Map(fun((p_10) => FunCall(Split(v__3), p_10))), FunCall(Map(fun((p_11) => FunCall(Join(), p_11))), FunCall(TransposeW(), FunCall(Map(fun((p_12) => FunCall(Map(fun((p_13) => FunCall(Map(fun((p_14) => FunCall(mapFun, FunCall(Get(0), p_13), FunCall(Get(1), p_13), FunCall(Get(2), p_13), FunCall(Get(0), p_14), FunCall(Get(1), p_14), FunCall(Get(2), p_14), FunCall(Get(3), p_14)))), p_12))), p_4))), FunCall(Split(v__4), p_3)))))))))), FunCall(Split(v__2), FunCall(Zip(3), p_0, p_1, p_2)))))
    val introduceReuseHash = getHash(introduceReuseGold)

    val partialReduceWithReorderGold = fun(ArrayType(Float, X), ArrayType(Float, X), ArrayType(Float, X), ArrayType(TupleType(Float, Float, Float, Float), K),(p_0, p_1, p_2, p_3) => FunCall(Map(fun((p_4) => FunCall(Reduce(fun((p_5, p_6) => FunCall(reduceFun, p_5, p_6))), Value("{ 0.0f, 0.0f}", TupleType(Float, Float)), FunCall(Join(), FunCall(Map(fun((p_7) => FunCall(PartRed(fun((p_8, p_9) => FunCall(reduceFun, p_8, p_9))), Value("{ 0.0f, 0.0f}", TupleType(Float, Float)), p_7))), FunCall(Split( K * Pow(v__2, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Scatter(ReorderWithStride(v__2)), FunCall(Join(), FunCall(Map(fun((p_10) => FunCall(Map(fun((p_11) => FunCall(mapFun, FunCall(Get(0), p_4), FunCall(Get(1), p_4), FunCall(Get(2), p_4), FunCall(Get(0), p_11), FunCall(Get(1), p_11), FunCall(Get(2), p_11), FunCall(Get(3), p_11)))), p_10))), FunCall(Split( K * Pow(v__2, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__2)), p_3)))))))))))), FunCall(Zip(3), p_0, p_1, p_2)))
    val partialReduceWithReorderHash = getHash(partialReduceWithReorderGold)

    val rewriter = new HighLevelRewrite(4, 2, 4)
    val rewrittenLambdas = rewriter(mriqComputeQ)

    val possiblePartialReduce = rewrittenLambdas.filter(_._2 == partialReduceWithReorderSeq)
    val possibleIntroduceReuse = rewrittenLambdas.filter(_._2 == introduceReuseSeq)

    check(possiblePartialReduce, partialReduceWithReorderHash)
    check(possibleIntroduceReuse, introduceReuseHash)
  }
}
