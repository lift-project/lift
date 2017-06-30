package exploration

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.LongTestsEnabled
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.Assert._
import org.junit.Test
import rewriting.MacroRules
import rewriting.utils.Utils

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

  val mmTransposedB = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    (A, B) => {
      Map(fun(aRow =>
        Map(fun(bCol =>
          Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
        )) $ B
      )) $ A
    })

  val mmTransposedA = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
    (A, B) => {
      Map(fun(aRow =>
        Map(fun(bCol =>
          Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
        )) o Transpose() $ B
      )) o Transpose() $ A
    })

  val gemv = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    ArrayTypeWSWC(Float, M),
    ArrayTypeWSWC(Float, N),
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

  val stencil1D = fun(
    ArrayTypeWSWC(Float, N),
    input => Join() o Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
  )

  def getHash(lambda: Lambda): String =
    Utils.Sha256Hash(Utils.dumpLambdaToString(lambda))

  @Test ( expected = classOf[TypeException] )//see Issue #114
  def rewriteLambdaUsingInt {
    val div9 = UserFun("div9", "x", "{ return x/9; }", Int, Int)
    val stencilInt = fun(
      ArrayType(ArrayType(Int, 6408), 4802),
      input => {
        Map(Map( \(neighborhood => Map(div9) o Reduce(addI, 0.0f) o Join() $ neighborhood)
          //                                                ^ this must be of type Int
			)) o Slide2D(3, 1) $ input
    })

    val rewriter = new HighLevelRewrite(4, 2, 2)
    val rewrittenLambdas = rewriter(stencilInt)
  }

  @Test
  def stencil1DRewrite(): Unit = {

    val rewriter = new HighLevelRewrite(4, 2, 2)
    val rewrittenLambdas = rewriter(stencil1D)

    val gold = fun(ArrayTypeWSWC(Float, N),(p_0) => FunCall(Join(), FunCall(Join(), FunCall(Map(fun((p_1) => FunCall(Map(fun((p_2) => FunCall(Reduce(fun((p_3, p_4) => FunCall(add, p_3, p_4))), Value("0.0f", Float), p_2))), FunCall(Slide(3,1), p_1)))), FunCall(Slide((2+v__1),v__1), FunCall(Pad(1,1,Pad.Boundary.Clamp), p_0))))))
    val goldHash = getHash(gold)

    val tiledSeq = Seq(MacroRules.tileStencils)
    val possibleTiled = rewrittenLambdas.filter(_._2 == tiledSeq)

    assertTrue(possibleTiled.exists(pair => getHash(pair._1) == goldHash))
  }

  @Test
  def mmTBRewrite(): Unit = {

    val vectorisedGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),(p_0, p_1) => FunCall(Map(fun((p_2) => FunCall(Map(fun((p_3) => FunCall(Reduce(fun((p_4, p_5) => FunCall(add, p_4, p_5))), Value("0.0f", Float), FunCall(asScalar(), FunCall(PartRed(fun((p_6, p_7) => FunCall(VectorizeUserFun(4,add), p_6, p_7))), Value("0.0f", VectorType(Float, 4)), FunCall(asVector(4), FunCall(asScalar(), FunCall(Map(fun((p_8) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))), FunCall(Zip(2), FunCall(asVector(4), p_2), FunCall(asVector(4), p_3)))))))))), p_1))), p_0))
    val vectorisedHash = getHash(vectorisedGold)

    val partiallyVectorisedTiledGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), p_4))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_5, p_6) => FunCall(Map(fun((p_7) => FunCall(Join(), FunCall(Map(fun((p_8) => FunCall(PartRed(fun((p_9, p_10) => FunCall(add, p_9, p_10))), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))), FunCall(Zip(2), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))))), FunCall(Zip(2), p_5, p_6)))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4)), FunCall(Transpose(), FunCall(Map(fun((p_11) => FunCall(Transpose(), FunCall(Map(fun((p_12) => FunCall(Split(v__5), p_12))), p_11)))), FunCall(Map(fun((p_13) => FunCall(Transpose(), p_13))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(fun((p_14) => FunCall(Transpose(), FunCall(Map(fun((p_15) => FunCall(Transpose(), FunCall(Map(fun((p_16) => FunCall(asScalar(), FunCall(Map(fun((p_17) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_17), FunCall(Get(1), p_17)))), FunCall(Zip(2), FunCall(asVector(4), p_15), FunCall(asVector(4), p_16)))))), FunCall(Transpose(), FunCall(Get(1), p_14)))))), FunCall(Transpose(), FunCall(Get(0), p_14)))))), FunCall(Zip(2), FunCall(Split(v__6), FunCall(Transpose(), p_2)), FunCall(Split(v__6), FunCall(Transpose(), p_3))))))))))))))), FunCall(Split(v__3), p_1)))))), FunCall(Split(v__4), p_0))))
    val partiallyVectorisedTiledHash = getHash(partiallyVectorisedTiledGold)

    val rewriter = new HighLevelRewrite(4, 2, 5)

    val rewrittenLambdas = rewriter(mmTransposedB)

    val vectorisedSeq =
      Seq(rewriter.vecZip, rewriter.vecRed)
    val partiallyVectorisedTiledSeq =
      Seq(MacroRules.tileMapMap, MacroRules.finishTiling,
        MacroRules.finishTiling, rewriter.vecZip)

    val possibleVectorised = rewrittenLambdas.filter(_._2 == vectorisedSeq)
    val possiblePartiallyVectorisedTiled =
      rewrittenLambdas.filter(_._2 == partiallyVectorisedTiledSeq)

    assertTrue(possibleVectorised.exists(pair => getHash(pair._1) == vectorisedHash))
    assertTrue(possiblePartiallyVectorisedTiled.exists(pair =>
      getHash(pair._1) == partiallyVectorisedTiledHash))
  }

  @Test
  def mmTARewrite(): Unit = {

    val plainTilingGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), p_4))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_5, p_6) => FunCall(Map(fun((p_7) => FunCall(Join(), FunCall(Map(fun((p_8) => FunCall(PartRed(fun((p_9, p_10) => FunCall(add, p_9, p_10))), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))), FunCall(Zip(2), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))))), FunCall(Zip(2), p_5, p_6)))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4)), FunCall(Transpose(), FunCall(Map(fun((p_11) => FunCall(Transpose(), FunCall(Map(fun((p_12) => FunCall(Split(v__5), p_12))), p_11)))), FunCall(Map(fun((p_13) => FunCall(Transpose(), p_13))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(fun((p_14) => FunCall(Transpose(), FunCall(Map(fun((p_15) => FunCall(Transpose(), FunCall(Map(fun((p_16) => FunCall(Map(fun((p_17) => FunCall(mult, FunCall(Get(0), p_17), FunCall(Get(1), p_17)))), FunCall(Zip(2), p_15, p_16)))), FunCall(Transpose(), FunCall(Get(1), p_14)))))), FunCall(Transpose(), FunCall(Get(0), p_14)))))), FunCall(Zip(2), FunCall(Split(v__6), FunCall(Transpose(), p_2)), FunCall(Split(v__6), FunCall(Transpose(), p_3))))))))))))))), FunCall(Split(v__3), FunCall(Transpose(), p_1))))))), FunCall(Split(v__4), FunCall(Transpose(), p_0)))))
    val plainTilingHash = getHash(plainTilingGold)

    val oneDGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), p_5))), FunCall(TransposeW(), p_4))))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_6, p_7) => FunCall(Map(fun((p_8) => FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(PartRed(fun((p_10, p_11) => FunCall(Map(fun((p_12) => FunCall(add, FunCall(Get(0), p_12), FunCall(Get(1), p_12)))), FunCall(Zip(2), p_10, p_11)))), FunCall(Get(0), p_9), FunCall(Get(1), p_9)))), FunCall(Zip(2), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))))), FunCall(Zip(2), p_6, p_7)))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4), v__5*1/^v__3)), FunCall(Transpose(), FunCall(Map(fun((p_13) => FunCall(Transpose(), FunCall(Map(fun((p_14) => FunCall(Split(v__6), FunCall(Transpose(), p_14)))), FunCall(Transpose(), p_13))))), FunCall(Split(v__3), FunCall(Map(fun((p_15) => FunCall(Transpose(), p_15))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(fun((p_16) => FunCall(Transpose(), FunCall(Join(), FunCall(Map(fun((p_17) => FunCall(Map(fun((p_18) => FunCall(Transpose(), p_18))), FunCall(TransposeW(), FunCall(Map(fun((p_19) => FunCall(Transpose(), FunCall(Map(fun((p_20) => FunCall(Map(fun((p_21) => FunCall(mult, p_21, FunCall(Get(1), p_20)))), FunCall(Get(0), p_20)))), FunCall(Zip(2), FunCall(Transpose(), p_17), p_19))))), FunCall(Transpose(), FunCall(Get(1), p_16))))))), FunCall(Split(v__7), FunCall(Transpose(), FunCall(Get(0), p_16)))))))), FunCall(Zip(2), FunCall(Split(v__8), FunCall(Transpose(), p_2)), FunCall(Split(v__8), FunCall(Transpose(), p_3))))))))))))))))), FunCall(Split(v__4), FunCall(Transpose(), p_1))))))), FunCall(Split(v__5), FunCall(Transpose(), p_0)))))
    val oneDHash = getHash(oneDGold)

    val twoDGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),(p_0, p_1) => FunCall(Join(), FunCall(Map(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_3) => FunCall(TransposeW(), FunCall(Map(fun((p_4) => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_4))), FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_5)))))), FunCall(TransposeW(), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(Map(fun((p_10) => FunCall(Join(), FunCall(Map(fun((p_11) => FunCall(PartRed(fun((p_12, p_13) => FunCall(Map(fun((p_14) => FunCall(Map(fun((p_15) => FunCall(add, FunCall(Get(0), p_15), FunCall(Get(1), p_15)))), FunCall(Zip(2), FunCall(Get(0), p_14), FunCall(Get(1), p_14))))), FunCall(Zip(2), p_12, p_13)))), FunCall(Get(0), p_11), FunCall(Get(1), p_11)))), FunCall(Zip(2), FunCall(Get(0), p_10), FunCall(Get(1), p_10)))))), FunCall(Zip(2), p_8, p_9)))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3*1/^v__4), v__6*1/^v__5)), FunCall(Transpose(), FunCall(Map(fun((p_16) => FunCall(Transpose(), FunCall(Map(fun((p_17) => FunCall(Split(v__7), FunCall(Transpose(), FunCall(Map(fun((p_18) => FunCall(Transpose(), p_18))), FunCall(Transpose(), p_17)))))), FunCall(Split(v__4), FunCall(Transpose(), FunCall(Map(fun((p_19) => FunCall(Gather(ReorderWithStride(v__3 / v__4)), p_19))), p_16))))))), FunCall(Split(v__5), FunCall(Map(fun((p_20) => FunCall(Transpose(), p_20))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(fun((p_21) => FunCall(Transpose(), FunCall(Map(fun((p_22) => FunCall(Transpose(), FunCall(Scatter(ReorderWithStride(v__3 / v__8)), p_22)))), FunCall(Join(), FunCall(Map(fun((p_23) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_24) => FunCall(TransposeW(), FunCall(Map(fun((p_25) => FunCall(Transpose(), p_25))), FunCall(Transpose(), FunCall(Map(fun((p_26) => FunCall(Map(fun((p_27) => FunCall(Map(fun((p_28) => FunCall(mult, p_27, p_28))), FunCall(Get(1), p_26)))), FunCall(Get(0), p_26)))), FunCall(Zip(2), FunCall(Transpose(), p_23), FunCall(Transpose(), p_24)))))))), FunCall(Split(v__8), FunCall(Gather(ReorderWithStride(v__3 / v__8)), FunCall(Transpose(), FunCall(Get(1), p_21))))))))), FunCall(Split(v__9), FunCall(Transpose(), FunCall(Get(0), p_21))))))))), FunCall(Zip(2), FunCall(Split(v__10), FunCall(Transpose(), p_2)), FunCall(Split(v__10), FunCall(Transpose(), p_3)))))))))))))))))), FunCall(Split(v__3), FunCall(Transpose(), p_1))))))), FunCall(Split(v__6), FunCall(Transpose(), p_0)))))
    val twoDHash = getHash(twoDGold)

    val rewriter = new HighLevelRewrite(4, 2, 5)

    val rewrittenLambdas = rewriter(mmTransposedA)

    val plainTilingSeq =
      Seq(MacroRules.tileMapMap, MacroRules.finishTiling, MacroRules.finishTiling)

    val tilingWith1DBlockingSeq =
      Seq(MacroRules.tileMapMap, MacroRules.finishTiling,
        MacroRules.apply1DRegisterBlocking, MacroRules.apply1DRegisterBlocking,
        MacroRules.finishTiling)

    val tilingWith2DBlockingSeq =
      Seq(MacroRules.tileMapMap, MacroRules.finishTiling,
        MacroRules.apply2DRegisterBlocking, MacroRules.apply2DRegisterBlocking,
        MacroRules.finishTiling)

    val possiblePlainTiling = rewrittenLambdas.filter(_._2 == plainTilingSeq)
    val possibleOneD = rewrittenLambdas.filter(_._2 == tilingWith1DBlockingSeq)
    val possibleTwoD = rewrittenLambdas.filter(_._2 == tilingWith2DBlockingSeq)

    assertTrue(possiblePlainTiling.exists(pair => getHash(pair._1) == plainTilingHash))
    assertTrue(possibleOneD.exists(pair => getHash(pair._1) == oneDHash))
    assertTrue(possibleTwoD.exists(pair => getHash(pair._1) == twoDHash))
  }

  @Test
  def gemvRewrite(): Unit = {

    val amdGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), ArrayTypeWSWC(Float, M), ArrayTypeWSWC(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, p_6, p_3), FunCall(mult, FunCall(Get(1), p_5), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(add, p_7, p_8))), Value("0.0f", Float), FunCall(Join(), FunCall(Map(fun((p_9) => FunCall(PartRed(fun((p_10, p_11) => FunCall(add, p_10, p_11))), Value("0.0f", Float), p_9))), FunCall(Split(M*1/^v__2), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Scatter(ReorderWithStride(v__2)), FunCall(Join(), FunCall(Map(fun((p_12) => FunCall(Map(fun((p_13) => FunCall(mult, FunCall(Get(0), p_13), FunCall(Get(1), p_13)))), p_12))), FunCall(Split(M*1/^v__2), FunCall(Gather(ReorderWithStride(v__2)), FunCall(Zip(2), p_1, FunCall(Get(0), p_5))))))))))))))), FunCall(Zip(2), p_0, p_2)))
    val amdHash = getHash(amdGold)

    val vectorisedGold = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), ArrayTypeWSWC(Float, M), ArrayTypeWSWC(Float, N), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(add, FunCall(mult, p_6, p_3), FunCall(mult, FunCall(Get(1), p_5), p_4)))), FunCall(Reduce(fun((p_7, p_8) => FunCall(add, p_7, p_8))), Value("0.0f", Float), FunCall(asScalar(), FunCall(PartRed(fun((p_9, p_10) => FunCall(VectorizeUserFun(4,add), p_9, p_10))), Value("0.0f", VectorType(Float, 4)), FunCall(asVector(4), FunCall(asScalar(), FunCall(Map(fun((p_11) => FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_11), FunCall(Get(1), p_11)))), FunCall(Zip(2), FunCall(asVector(4), p_1), FunCall(asVector(4), FunCall(Get(0), p_5)))))))))))), FunCall(Zip(2), p_0, p_2)))
    val vectorisedHash = getHash(vectorisedGold)

    val rewriter = new HighLevelRewrite(4, 2, 5)

    val rewrittenLambdas = rewriter(gemv)

    val amdSeq = Seq(MacroRules.partialReduceWithReorder)
    val vectorisedSeq = Seq(rewriter.vecZip, rewriter.vecRed)

    val possibleAmd = rewrittenLambdas.filter(_._2 == amdSeq)
    val possibleVectorised = rewrittenLambdas.filter(_._2 == vectorisedSeq)

    assertTrue(possibleAmd.exists(pair => getHash(pair._1) == amdHash))
    assertTrue(possibleVectorised.exists(pair => getHash(pair._1) == vectorisedHash))
  }

}
