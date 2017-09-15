package exploration

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.executor.LongTestsEnabled
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.Assert._
import org.junit.Test
import rewriting.macrorules.{MacroRules, ReuseRules, SlideTiling}
import rewriting.rules.Rules
import rewriting.utils.DumpToFile

class TestHighLevelRewrite {

  LongTestsEnabled()

  private val N = SizeVar("N")
  private val M = SizeVar("M")
  private val K = SizeVar("K")
  private val v__1 = SizeVar("")
  private val v__2 = SizeVar("")
  private val v__3 = SizeVar("")
  private val v__4 = SizeVar("")
  private val v__5 = SizeVar("")
  private val v__6 = SizeVar("")
  private val v__7 = SizeVar("")
  private val v__8 = SizeVar("")
  private val v__9 = SizeVar("")
  private val v__10 = SizeVar("")

  private val mmTransposedB = fun(
    ArrayType(ArrayType(Float, K), M),
    ArrayType(ArrayType(Float, K), N),
    (A, B) => {
      Map(fun(aRow =>
        Map(fun(bCol =>
          Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
        )) $ B
      )) $ A
    })

  private val mmTransposedA = fun(
    ArrayType(ArrayType(Float, M), K),
    ArrayType(ArrayType(Float, N), K),
    (A, B) => {
      Map(fun(aRow =>
        Map(fun(bCol =>
          Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
        )) o Transpose() $ B
      )) o Transpose() $ A
    })

  private val gemv = fun(
    ArrayType(ArrayType(Float, M), N),
    ArrayType(Float, M),
    ArrayType(Float, N),
    Float,
    Float,
    (matrix, vectorX, vectorY, alpha, beta) => {
      Map(fun(t =>
        Map(fun(x =>
          add(
            mult(x, alpha),
            mult(Get(t, 1), beta)
          )
        )) o
          Reduce(add, 0.0f) o
          Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(vectorX, Get(t, 0))
      )) $ Zip(matrix, vectorY)
    })

  private val stencil1D = fun(
    ArrayType(Float, N),
    input => Join() o Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
  )

  private def mvAlpha = fun(
    ArrayType(ArrayType(Float, K), N),
    ArrayType(Float, K),
    Float,
    (matrix, vector, alpha) =>
      Join() o
        Map(fun(row =>
          Map(fun(x => mult(x, alpha))) o
            Reduce(add, 0.0f) o Map(fun(y => mult(y._0, y._1))) $ Zip(row, vector)
        )) $ matrix
  )

  private def vecAdd = fun(
    ArrayType(Float, K),
    ArrayType(Float, K),
    (a,b) => Map(fun(x => add(x._0, x._1))) $ Zip(a, b)
  )

  private val gesummv = fun(
    ArrayType(ArrayType(Float, K), N),
    ArrayType(ArrayType(Float, K), N),
    ArrayType(Float, K),
    Float,
    Float,
    (A, B, x, alpha, beta) =>
      vecAdd(mvAlpha(A, x, alpha), mvAlpha(B, x, beta))
  )

  private def getHash(lambda: Lambda): String =
    DumpToFile.Sha256Hash(DumpToFile.dumpLambdaToString(lambda))

  @Test
  def gesummvRewrite(): Unit = {

    val basicFusedGold = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, FunCall(Get(0), p_6), p_3), FunCall(mult, FunCall(Get(1), p_6), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_7), FunCall(Get(0), p_8)), FunCall(add, FunCall(Get(1), p_7), FunCall(Get(1), p_8))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)), FunCall(Map(fun((p_9) => FunCall(Tuple(2), FunCall(mult, FunCall(Get(0), p_9), FunCall(Get(1), p_9)), FunCall(mult, FunCall(Get(2), p_9), FunCall(Get(1), p_9))))), FunCall(Zip(3), FunCall(Get(0), p_5), p_2, FunCall(Get(1), p_5))))))), FunCall(Zip(2), p_0, p_1))))
    val basicFusedHash = getHash(basicFusedGold)

    val partialReduceWithReorder = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, FunCall(Get(0), p_6), p_3), FunCall(mult, FunCall(Get(1), p_6), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_7), FunCall(Get(0), p_8)), FunCall(add, FunCall(Get(1), p_7), FunCall(Get(1), p_8))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)), FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(PartRed(fun((p_10, p_11) => FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_10), FunCall(Get(0), p_11)), FunCall(add, FunCall(Get(1), p_10), FunCall(Get(1), p_11))))), Value("{ 0.0f, 0.0f }", TupleType(Float, Float)), p_9))), FunCall(Split( K * Pow(v__2, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Scatter(ReorderWithStride(v__2)), FunCall(Join(), FunCall(Map(fun((p_12) => FunCall(Map(fun((p_13) => FunCall(Tuple(2), FunCall(mult, FunCall(Get(0), p_13), FunCall(Get(1), p_13)), FunCall(mult, FunCall(Get(2), p_13), FunCall(Get(1), p_13))))), p_12))), FunCall(Split(K * Pow(v__2, Cst(-1)) ), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Zip(3), FunCall(Get(0), p_5), p_2, FunCall(Get(1), p_5))))))))))))))), FunCall(Zip(2), p_0, p_1))))
    val partialReduceWithReorderHash = getHash(partialReduceWithReorder)

    val localMemoryGold = fun(ArrayType(ArrayType(Float, K), N), ArrayType(ArrayType(Float, K), N), ArrayType(Float, K), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), FunCall(Map(fun((p_6) => FunCall(Map(fun((p_7) => FunCall(add, FunCall(mult, FunCall(Get(0), p_7), p_3), FunCall(mult, FunCall(Get(1), p_7), p_4)))), p_6))), FunCall(Transpose(), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(Join(), FunCall(Map(fun((p_10) => FunCall(PartRed(fun((p_11, p_12) => FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_11), FunCall(Get(0), p_12)), FunCall(add, FunCall(Get(1), p_11), FunCall(Get(1), p_12))))), FunCall(Get(0), p_10), FunCall(Get(1), p_10)))), FunCall(Zip(2), p_8, p_9))))), Value("{ 0.0f, 0.0f }", ArrayType(TupleType(Float, Float), v__2)), FunCall(Transpose(), FunCall(Map(fun((p_13) => FunCall(Split(v__3), FunCall(Join(), p_13)))), FunCall(TransposeW(), FunCall(Map(fun((p_14) => FunCall(Map(fun((p_15) => FunCall(Map(fun((p_16) => FunCall(Tuple(2), FunCall(mult, FunCall(Get(0), p_16), FunCall(Get(1), p_16)), FunCall(mult, FunCall(Get(2), p_16), FunCall(Get(1), p_16))))), FunCall(Zip(3), FunCall(Get(0), p_15), FunCall(Get(1), p_14), FunCall(Get(1), p_15))))), FunCall(Zip(2), FunCall(Get(0), p_14), FunCall(Get(2), p_14))))), FunCall(Zip(3), FunCall(Transpose(), FunCall(Map(fun((p_17) => FunCall(Split(v__4), FunCall(Get(0), p_17)))), p_5)), FunCall(Split(v__4), p_2), FunCall(Transpose(), FunCall(Map(fun((p_18) => FunCall(Split(v__4), FunCall(Get(1), p_18)))), p_5)))))))))))))), FunCall(Split(v__2), FunCall(Zip(2), p_0, p_1))))))
    val localMemoryHash = getHash(localMemoryGold)

    val rewriter = new HighLevelRewrite(4, 2, 4)
    val rewrittenLambdas = rewriter(gesummv)

    val amdSeq = Seq(MacroRules.partialReduceWithReorder)
     val clblastSeq =
      Seq(Rules.splitJoin, MacroRules.interchange,
        ReuseRules.introduceReuseFromMap, ReuseRules.introduceReuseFromMap)

    val possibleAmd = rewrittenLambdas.filter(_._2 == amdSeq)
    val possibleFused = rewrittenLambdas.filter(_._2 == Seq())
    val possibleClblast = rewrittenLambdas.filter(_._2 == clblastSeq)

    assertTrue(possibleAmd.exists(pair => getHash(pair._1) == partialReduceWithReorderHash))
    assertTrue(possibleFused.exists(pair => getHash(pair._1) == basicFusedHash))
    assertTrue(possibleClblast.exists(pair => getHash(pair._1) == localMemoryHash))
  }

  @Test ( expected = classOf[TypeException] )//see Issue #114
  def rewriteLambdaUsingInt(): Unit = {
    val div9 = UserFun("div9", "x", "{ return x/9; }", Int, Int)
    val stencilInt = fun(
      ArrayType(ArrayType(Int, 6408), 4802),
      input => {
        Map(Map( \(neighborhood => Map(div9) o Reduce(addI, 0.0f) o Join() $ neighborhood)
          //                                                ^ this must be of type Int
			)) o Slide2D(3, 1) $ input
    })

    val rewriter = new HighLevelRewrite(4, 2, 2)
    rewriter(stencilInt)
  }

  @Test
  def stencil1DRewrite(): Unit = {

    val rewriter = new HighLevelRewrite(4, 2, 2)
    val rewrittenLambdas = rewriter(stencil1D)

    val gold = fun(ArrayType(Float, N),(p_0) => FunCall(Join(), FunCall(Join(), FunCall(Map(fun((p_1) => FunCall(Map(fun((p_2) => FunCall(Reduce(fun((p_3, p_4) => FunCall(add, p_3, p_4))), Value("0.0f", Float), p_2))), FunCall(Slide(3,1), p_1)))), FunCall(Slide(2+v__1,v__1), FunCall(Pad(1,1,Pad.Boundary.Clamp), p_0))))))
    val goldHash = getHash(gold)

    val tiledSeq = Seq(SlideTiling.tileStencils)
    val possibleTiled = rewrittenLambdas.filter(_._2 == tiledSeq)

    assertTrue(possibleTiled.exists(pair => getHash(pair._1) == goldHash))
  }

  @Test
  def mmTBRewrite(): Unit = {

    val vectorisedGold = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N),(p_0, p_1) => FunCall(Map(fun((p_2) => FunCall(Map(fun((p_3) => FunCall(Reduce(fun((p_4, p_5) => FunCall(add, p_4, p_5))), Value("0.0f", Float), FunCall(asScalar(), FunCall(PartRed(fun((p_6, p_7) => FunCall(VectorizeUserFun(4,add), p_6, p_7))), Value("0.0f", VectorType(Float, 4)), FunCall(asVector(4), FunCall(asScalar(), FunCall(Map(fun((p_8) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))), FunCall(Zip(2), FunCall(asVector(4), p_2), FunCall(asVector(4), p_3)))))))))), p_1))), p_0))
    val vectorisedHash = getHash(vectorisedGold)

    val partiallyVectorisedTiledGold = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), p_4))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_5, p_6) => FunCall(Map(fun((p_7) => FunCall(Join(), FunCall(Map(fun((p_8) => FunCall(PartRed(fun((p_9, p_10) => FunCall(add, p_9, p_10))), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))), FunCall(Zip(2), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))))), FunCall(Zip(2), p_5, p_6)))), Value("0.0f", ArrayType(ArrayType(Float, v__3), v__4)), FunCall(Transpose(), FunCall(Map(fun((p_11) => FunCall(Transpose(), FunCall(Map(fun((p_12) => FunCall(Split(v__5), p_12))), p_11)))), FunCall(Map(fun((p_13) => FunCall(TransposeW(), p_13))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_14) => FunCall(TransposeW(), FunCall(Map(fun((p_15) => FunCall(TransposeW(), FunCall(Map(fun((p_16) => FunCall(asScalar(), FunCall(Map(fun((p_17) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_17), FunCall(Get(1), p_17)))), FunCall(Zip(2), FunCall(asVector(4), p_15), FunCall(asVector(4), p_16)))))), FunCall(Transpose(), FunCall(Get(1), p_14)))))), FunCall(Transpose(), FunCall(Get(0), p_14)))))), FunCall(Zip(2), FunCall(Split(v__6), FunCall(Transpose(), p_2)), FunCall(Split(v__6), FunCall(Transpose(), p_3))))))))))))))), FunCall(Split(v__3), p_1)))))), FunCall(Split(v__4), p_0))))
    val partiallyVectorisedTiledHash = getHash(partiallyVectorisedTiledGold)

    val rewriter = new HighLevelRewrite(4, 2, 5)

    val rewrittenLambdas = rewriter(mmTransposedB)

    val vectorisedSeq =
      Seq(rewriter.vecZip, rewriter.vecRed)
    val partiallyVectorisedTiledSeq =
      Seq(ReuseRules.tileMapMap, ReuseRules.finishTiling,
        ReuseRules.finishTiling, rewriter.vecZip)

    val possibleVectorised = rewrittenLambdas.filter(_._2 == vectorisedSeq)
    val possiblePartiallyVectorisedTiled =
      rewrittenLambdas.filter(_._2 == partiallyVectorisedTiledSeq)

    assertTrue(possibleVectorised.exists(pair => getHash(pair._1) == vectorisedHash))
    assertTrue(possiblePartiallyVectorisedTiled.exists(pair =>
      getHash(pair._1) == partiallyVectorisedTiledHash))
  }

  @Test
  def mmTARewrite(): Unit = {

    val plainTilingGold = fun(ArrayType(ArrayType(Float, M), K), ArrayType(ArrayType(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), p_4))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_5, p_6) => FunCall(Map(fun((p_7) => FunCall(Join(), FunCall(Map(fun((p_8) => FunCall(PartRed(fun((p_9, p_10) => FunCall(add, p_9, p_10))), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))), FunCall(Zip(2), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))))), FunCall(Zip(2), p_5, p_6)))), Value("0.0f", ArrayType(ArrayType(Float, v__3), v__4)), FunCall(Transpose(), FunCall(Map(fun((p_11) => FunCall(Transpose(), FunCall(Map(fun((p_12) => FunCall(Split(v__5), p_12))), p_11)))), FunCall(Map(fun((p_13) => FunCall(TransposeW(), p_13))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_14) => FunCall(TransposeW(), FunCall(Map(fun((p_15) => FunCall(TransposeW(), FunCall(Map(fun((p_16) => FunCall(Map(fun((p_17) => FunCall(mult, FunCall(Get(0), p_17), FunCall(Get(1), p_17)))), FunCall(Zip(2), p_15, p_16)))), FunCall(Transpose(), FunCall(Get(1), p_14)))))), FunCall(Transpose(), FunCall(Get(0), p_14)))))), FunCall(Zip(2), FunCall(Split(v__6), FunCall(Transpose(), p_2)), FunCall(Split(v__6), FunCall(Transpose(), p_3))))))))))))))), FunCall(Split(v__3), FunCall(Transpose(), p_1))))))), FunCall(Split(v__4), FunCall(Transpose(), p_0)))))
    val plainTilingHash = getHash(plainTilingGold)

    val oneDGold = fun(ArrayType(ArrayType(Float, M), K), ArrayType(ArrayType(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), p_5))), FunCall(TransposeW(), p_4))))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_6, p_7) => FunCall(Map(fun((p_8) => FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(PartRed(fun((p_10, p_11) => FunCall(Map(fun((p_12) => FunCall(add, FunCall(Get(0), p_12), FunCall(Get(1), p_12)))), FunCall(Zip(2), p_10, p_11)))), FunCall(Get(0), p_9), FunCall(Get(1), p_9)))), FunCall(Zip(2), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))))), FunCall(Zip(2), p_6, p_7)))), Value("0.0f", ArrayType(ArrayType(ArrayType(Float, v__3), v__4), v__5*1/^v__3)), FunCall(Transpose(), FunCall(Map(fun((p_13) => FunCall(Transpose(), FunCall(Map(fun((p_14) => FunCall(Split(v__6), FunCall(Transpose(), p_14)))), FunCall(Transpose(), p_13))))), FunCall(Split(v__3), FunCall(Map(fun((p_15) => FunCall(TransposeW(), p_15))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_16) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_17) => FunCall(Map(fun((p_18) => FunCall(TransposeW(), p_18))), FunCall(TransposeW(), FunCall(Map(fun((p_19) => FunCall(TransposeW(), FunCall(Map(fun((p_20) => FunCall(Map(fun((p_21) => FunCall(mult, p_21, FunCall(Get(1), p_20)))), FunCall(Get(0), p_20)))), FunCall(Zip(2), FunCall(Transpose(), p_17), p_19))))), FunCall(Transpose(), FunCall(Get(1), p_16))))))), FunCall(Split(v__7), FunCall(Transpose(), FunCall(Get(0), p_16)))))))), FunCall(Zip(2), FunCall(Split(v__8), FunCall(Transpose(), p_2)), FunCall(Split(v__8), FunCall(Transpose(), p_3))))))))))))))))), FunCall(Split(v__4), FunCall(Transpose(), p_1))))))), FunCall(Split(v__5), FunCall(Transpose(), p_0)))))
    val oneDHash = getHash(oneDGold)

    val twoDGold = fun(ArrayType(ArrayType(Float, M), K), ArrayType(ArrayType(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_4))), FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_5)))))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(Map(fun((p_10) => FunCall(Join(), FunCall(Map(fun((p_11) => FunCall(PartRed(fun((p_12, p_13) => FunCall(Map(fun((p_14) => FunCall(Map(fun((p_15) => FunCall(add, FunCall(Get(0), p_15), FunCall(Get(1), p_15)))), FunCall(Zip(2), FunCall(Get(0), p_14), FunCall(Get(1), p_14))))), FunCall(Zip(2), p_12, p_13)))), FunCall(Get(0), p_11), FunCall(Get(1), p_11)))), FunCall(Zip(2), FunCall(Get(0), p_10), FunCall(Get(1), p_10)))))), FunCall(Zip(2), p_8, p_9)))), Value("0.0f", ArrayType(ArrayType(ArrayType(ArrayType(Float, v__4), v__5), v__3*1/^v__4), v__6*1/^v__5)), FunCall(Transpose(), FunCall(Map(fun((p_16) => FunCall(Transpose(), FunCall(Map(fun((p_17) => FunCall(Split(v__7), FunCall(Transpose(), FunCall(Map(fun((p_18) => FunCall(Transpose(), p_18))), FunCall(Transpose(), p_17)))))), FunCall(Split(v__4), FunCall(Transpose(), FunCall(Map(fun((p_19) => FunCall(Gather(ReorderWithStride(v__3 / v__4)), p_19))), p_16))))))), FunCall(Split(v__5), FunCall(Map(fun((p_20) => FunCall(TransposeW(), p_20))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_21) => FunCall(TransposeW(), FunCall(Map(fun((p_22) => FunCall(TransposeW(), FunCall(Scatter(ReorderWithStride(v__3 / v__8)), p_22)))), FunCall(Join(), FunCall(Map(fun((p_23) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_24) => FunCall(TransposeW(), FunCall(Map(fun((p_25) => FunCall(TransposeW(), p_25))), FunCall(TransposeW(), FunCall(Map(fun((p_26) => FunCall(Map(fun((p_27) => FunCall(Map(fun((p_28) => FunCall(mult, p_27, p_28))), FunCall(Get(1), p_26)))), FunCall(Get(0), p_26)))), FunCall(Zip(2), FunCall(Transpose(), p_23), FunCall(Transpose(), p_24)))))))), FunCall(Split(v__8), FunCall(Gather(ReorderWithStride(v__3 / v__8)), FunCall(Transpose(), FunCall(Get(1), p_21))))))))), FunCall(Split(v__9), FunCall(Transpose(), FunCall(Get(0), p_21))))))))), FunCall(Zip(2), FunCall(Split(v__10), FunCall(Transpose(), p_2)), FunCall(Split(v__10), FunCall(Transpose(), p_3)))))))))))))))))), FunCall(Split(v__3), FunCall(Transpose(), p_1))))))), FunCall(Split(v__6), FunCall(Transpose(), p_0)))))
    val twoDHash = getHash(twoDGold)

    val rewriter = new HighLevelRewrite(4, 2, 5)

    val rewrittenLambdas = rewriter(mmTransposedA)

    val plainTilingSeq =
      Seq(ReuseRules.tileMapMap, ReuseRules.finishTiling, ReuseRules.finishTiling)

    val tilingWith1DBlockingSeq =
      Seq(ReuseRules.tileMapMap, ReuseRules.finishTiling,
        ReuseRules.apply1DRegisterBlocking, ReuseRules.apply1DRegisterBlocking,
        ReuseRules.finishTiling)

    val tilingWith2DBlockingSeq =
      Seq(ReuseRules.tileMapMap, ReuseRules.finishTiling,
        ReuseRules.apply2DRegisterBlocking, ReuseRules.apply2DRegisterBlocking,
        ReuseRules.finishTiling)

    val possiblePlainTiling = rewrittenLambdas.filter(_._2 == plainTilingSeq)
    val possibleOneD = rewrittenLambdas.filter(_._2 == tilingWith1DBlockingSeq)
    val possibleTwoD = rewrittenLambdas.filter(_._2 == tilingWith2DBlockingSeq)

    assertTrue(possiblePlainTiling.exists(pair => getHash(pair._1) == plainTilingHash))
    assertTrue(possibleOneD.exists(pair => getHash(pair._1) == oneDHash))
    assertTrue(possibleTwoD.exists(pair => getHash(pair._1) == twoDHash))
  }

  @Test
  def gemvRewrite(): Unit = {

    val amdGold = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, p_6, p_3), FunCall(mult, FunCall(Get(1), p_5), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(add, p_7, p_8))), Value("0.0f", Float), FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(PartRed(fun((p_10, p_11) => FunCall(add, p_10, p_11))), Value("0.0f", Float), p_9))), FunCall(Split(M*1/^v__2), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Scatter(ReorderWithStride(v__2)), FunCall(Join(), FunCall(Map(fun((p_12) => FunCall(Map(fun((p_13) => FunCall(mult, FunCall(Get(0), p_13), FunCall(Get(1), p_13)))), p_12))), FunCall(Split(M*1/^v__2), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Zip(2), p_1, FunCall(Get(0), p_5))))))))))))))), FunCall(Zip(2), p_0, p_2)))
    val amdHash = getHash(amdGold)

    val vectorisedGold = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, p_6, p_3), FunCall(mult, FunCall(Get(1), p_5), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(add, p_7, p_8))), Value("0.0f", Float), FunCall(asScalar(), FunCall(PartRed(fun((p_9, p_10) => FunCall(VectorizeUserFun(4,add), p_9, p_10))), Value("0.0f", VectorType(Float, 4)), FunCall(asVector(4), FunCall(asScalar(), FunCall(Map(fun((p_11) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_11), FunCall(Get(1), p_11)))), FunCall(Zip(2), FunCall(asVector(4), p_1), FunCall(asVector(4), FunCall(Get(0), p_5)))))))))))), FunCall(Zip(2), p_0, p_2)))
    val vectorisedHash = getHash(vectorisedGold)

    val clblastGold = fun(ArrayType(ArrayType(Float, M), N), ArrayType(Float, M), ArrayType(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), FunCall(Map(fun((p_6) => FunCall(Map(fun((p_7) => FunCall(add, FunCall(mult, FunCall(Get(1), p_7), p_3), FunCall(mult, FunCall(Get(1), FunCall(Get(0), p_7)), p_4)))), FunCall(Zip(2), p_5, p_6)))), FunCall(Transpose(), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(Join(), FunCall(Map(fun((p_10) => FunCall(PartRed(fun((p_11, p_12) => FunCall(add, p_11, p_12))), FunCall(Get(0), p_10), FunCall(Get(1), p_10)))), FunCall(Zip(2), p_8, p_9))))), Value("0.0f", ArrayType(Float, v__2)), FunCall(Transpose(), FunCall(Map(fun((p_13) => FunCall(Split(v__3), FunCall(Join(), p_13)))), FunCall(TransposeW(), FunCall(Map(fun((p_14) => FunCall(Map(fun((p_15) => FunCall(Map(fun((p_16) => FunCall(mult, FunCall(Get(0), p_16), FunCall(Get(1), p_16)))), FunCall(Zip(2), FunCall(Get(0), p_14), p_15)))), FunCall(Get(1), p_14)))), FunCall(Zip(2), FunCall(Split(v__4), p_1), FunCall(Transpose(), FunCall(Map(fun((p_17) => FunCall(Split(v__4), FunCall(Get(0), p_17)))), p_5)))))))))))))), FunCall(Split(v__2), FunCall(Zip(2), p_0, p_2)))))
    val clblastHash = getHash(clblastGold)

    val rewriter = new HighLevelRewrite(4, 2, 5)

    val rewrittenLambdas = rewriter(gemv)

    val amdSeq = Seq(MacroRules.partialReduceWithReorder)
    val vectorisedSeq = Seq(rewriter.vecZip, rewriter.vecRed)
    val clblastSeq =
      Seq(Rules.splitJoin, MacroRules.interchange,
        ReuseRules.introduceReuseFromMap, ReuseRules.introduceReuseFromMap)

    val possibleAmd = rewrittenLambdas.filter(_._2 == amdSeq)
    val possibleVectorised = rewrittenLambdas.filter(_._2 == vectorisedSeq)
    val possibleClblast = rewrittenLambdas.filter(_._2 == clblastSeq)

    assertTrue(possibleAmd.exists(pair => getHash(pair._1) == amdHash))
    assertTrue(possibleVectorised.exists(pair => getHash(pair._1) == vectorisedHash))
    assertTrue(possibleClblast.exists(pair => getHash(pair._1) == clblastHash))
  }

}
