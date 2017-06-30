package opencl.generator.matrixMultiplication


import benchmarks.GEMM
import ir._
import ir.ast._
import lift.arithmetic.ArithExpr
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.{AfterClass, BeforeClass, Test}

object CGO_2017 {
  @BeforeClass def before(): Unit =
    Executor.loadAndInit()

  @AfterClass def after(): Unit =
    Executor.shutdown()
}

class CGO_2017 {

  val N = GEMM.N
  val M = GEMM.M
  val K = GEMM.K

  val mSize = 1024
  val kSize = 1024
  val nSize = 1024

  val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
  val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
  val matrixC = Array.tabulate(kSize, mSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

  val alpha = 1.5f
  val beta = 2.5f

  val mmGold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten
  val gemmGold = Utils.matrixMatrixMultiply(matrixA, matrixB, matrixC, alpha, beta).flatten

  @Test
  def clblast_kepler_mm_TN(): Unit = {

    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val vectorWidth = 2

    val factory = (variables: Seq[ArithExpr]) => {
      val v_M_0 = variables(0)
      val v_K_1 = variables(1)
      val v_N_2 = variables(2)
      val v__3 = variables(3)
      val v__4 = variables(4)
      val v__5 = variables(5)
      val v__6 = variables(6)
      val v__7 = variables(7)
      val v__8 = variables(8)

      val idfloat = UserFun("idfloat", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
      val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
      val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
      fun(
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1),
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1),
        (p_0, p_1) =>
        FunCall(Join(),
          FunCall(MapWrg(1)(fun((p_2) =>
            FunCall(TransposeW(),
              FunCall(Join(),
                FunCall(MapWrg(0)(fun((p_3) =>
                  FunCall(TransposeW(),
                    FunCall(Join(),
                      FunCall(Map(fun((p_4) =>
                        Map(Join() o Scatter(ReorderWithStride(v__5/v__3*vectorWidth)) o Split(vectorWidth)) $
                          FunCall(TransposeW(), FunCall(Join(),
                            FunCall(Map(fun((p_5) =>
                              FunCall(TransposeW(),
                                FunCall(Map(fun((p_6) =>
                                  FunCall(TransposeW(), p_6))),
                                  FunCall(TransposeW(), p_5))))),
                              FunCall(TransposeW(), p_4)))))),
                        FunCall(TransposeW(),
                          FunCall(MapSeq(fun((p_7) =>
                            FunCall(toGlobal(fun((p_8) =>
                              FunCall(MapLcl(1)(fun((p_9) =>
                                FunCall(MapLcl(0)(fun((p_10) =>
                                  FunCall(MapSeq(fun((p_11) =>
                                    FunCall(asScalar(),
                                      FunCall(MapSeq(fun((p_12) =>
                                        FunCall(VectorizeUserFun(vectorWidth,idfloat), p_12))),
                                        FunCall(asVector(vectorWidth), p_11))))), p_10))), p_9))), p_8))), p_7))),
                            FunCall(ReduceSeq(fun((p_13, p_14) =>
                              FunCall(fun((p_15) =>
                                FunCall(MapLcl(1)(fun((p_16) =>
                                  FunCall(Join(),
                                    FunCall(MapLcl(0)(fun((p_17) =>
                                      FunCall(MapSeq(fun((p_18) => p_18)),
                                        FunCall(ReduceSeq(fun((p_19, p_20) =>
                                          FunCall(fun((p_21) =>
                                            FunCall(MapSeq(fun((p_22) =>
                                              FunCall(MapSeq(fun((p_23) =>
                                                FunCall(add,
                                                  FunCall(Get(0), p_23),
                                                  FunCall(mult,
                                                    FunCall(Get(1), p_22),
                                                    FunCall(Get(1), p_23))))),
                                                FunCall(Zip(2),
                                                  FunCall(Get(0), p_22),
                                                  FunCall(Get(1), p_21))))),
                                              FunCall(Zip(2), p_19,
                                                FunCall(Get(0), p_21)))),
                                            FunCall(toPrivate(fun((p_24) =>
                                              FunCall(fun((p_25) =>
                                                FunCall(Tuple(2),
                                                  FunCall(MapSeq(fun((p_26) =>
                                                    FunCall(idfloat, p_26))),
                                                    FunCall(Get(0), p_25)),
                                                  FunCall(MapSeq(fun((p_27) =>
                                                    FunCall(idfloat, p_27))),
                                                    FunCall(Get(1), p_25)))), p_24))), p_20)))),
                                          FunCall(Get(0), p_17),
                                          FunCall(Zip(2),
                                            FunCall(Transpose(),
                                              FunCall(Get(1), p_16)),
                                            FunCall(Transpose(),
                                              FunCall(Get(1), p_17))))))),
                                      FunCall(Zip(2),
                                        FunCall(Get(0), p_16),
                                        FunCall(Split(v__3),
                                          Join() o Gather(ReorderWithStride(v__5/v__3* vectorWidth)) o Split(vectorWidth) $
                                            FunCall(Transpose(),
                                            FunCall(Get(1), p_15)))))))),
                                  FunCall(Zip(2), p_13,
                                    FunCall(Split(v__4),
                                      FunCall(Transpose(),
                                        FunCall(Get(0), p_15)))))),
                                FunCall(toLocal(fun((p_28) =>
                                  FunCall(fun((p_29) =>
                                    FunCall(Tuple(2),
                                      FunCall(Split(v__5),
                                        FunCall(Join(),
                                          FunCall(MapLcl(1)(fun((p_30) =>
                                            FunCall(asScalar(),
                                              FunCall(MapLcl(0)(fun((p_31) =>
                                                FunCall(VectorizeUserFun(vectorWidth,idfloat), p_31))),
                                                FunCall(asVector(vectorWidth), p_30))))),
                                            FunCall(Split(v__6),
                                              FunCall(Join(),
                                                FunCall(Get(0), p_29)))))),
                                      FunCall(MapLcl(1)(fun((p_32) =>
                                        FunCall(asScalar(),
                                          FunCall(MapLcl(0)(fun((p_33) =>
                                            FunCall(VectorizeUserFun(vectorWidth,idfloat), p_33))),
                                            FunCall(asVector(vectorWidth), p_32))))),
                                        FunCall(Get(1), p_29)))), p_28))), p_14)))),
                              FunCall(MapLcl(1)(fun((p_34) =>
                                FunCall(MapLcl(0)(fun((p_35) =>
                                  FunCall(MapSeq(fun((p_36) =>
                                    FunCall(MapSeq(fun((p_37) =>
                                      FunCall(idfloat, p_37))), p_36))), p_35))), p_34))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4), v__7 * 1 /^ v__3), v__5 * 1 /^ v__4))),
                              FunCall(Zip(2), p_2, p_3))))))))),
                  FunCall(Transpose(),
                    FunCall(Map(fun((p_38) =>
                      FunCall(Transpose(), p_38))),
                      FunCall(Split(v__8),
                        FunCall(Map(fun((p_39) =>
                          FunCall(Split(v__7), p_39))), p_1))))))))),
            FunCall(Transpose(),
              FunCall(Map(fun((p_40) =>
                FunCall(Transpose(), p_40))),
                FunCall(Split(v__8),
                  FunCall(Map(fun((p_41) =>
                    FunCall(Split(v__5), p_41))), p_0)))))))
    }

    val param = 8

    val f = factory(Seq[ArithExpr](M, K, N,param,8,64, 128, 128, 16))

    val code = Compile(f, 128/param, 8, 1, N/param, M/8,1, collection.immutable.Map())

    val (output: Array[Float], _) =
      Execute(128/param, 8, 1, 1024/param, 1024/8, 1, (true, true))(code, f, matrixA.transpose, matrixB)

    assertArrayEquals(mmGold, output, 0.001f)
  }

  @Test
  def clblast_kepler_sgemm_TN(): Unit = {

    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val param = 8

    val f = GEMM.clblast_kepler

    val code = Compile(f, 128/param, 8, 1, N/param, M/8,1, collection.immutable.Map())

    val (output: Array[Float], _) =
      Execute(128/param, 8, 1, 1024/param, 1024/8, 1, (true, true))(
        code, f, matrixA.transpose, matrixB, matrixC,
        alpha, beta
      )

    assertArrayEquals(gemmGold, output, 0.001f)
  }

  @Test
  def clblast_hawaii_mm_TN(): Unit = {

    val v__3 = 4
    val v__4 = 8
    val vectorWidth = 4

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (p_0, p_1) =>
        FunCall(Map(fun((p_2) =>
          p_2)),
          FunCall(Join(),
            FunCall(MapGlb(1)(fun((p_3) =>
              FunCall(TransposeW(),
                FunCall(Join(),
                  FunCall(MapGlb(0)(fun((p_4) =>
                    FunCall(TransposeW(),
                      FunCall(Map(fun((p_5) =>
                        FunCall(TransposeW(), p_5))),
                        FunCall(TransposeW(),
                          toGlobal(MapSeq(MapSeq(asScalar() o MapSeq(VectorizeUserFun(4, id)) o asVector(4)))) $
                            FunCall(ReduceSeq(fun((p_6, p_7) =>
                              fun(x =>
                                FunCall(MapSeq(fun((p_8) =>
                                  FunCall(MapSeq(fun((p_9) =>
                                    FunCall(add,
                                      FunCall(Get(0), p_9),
                                      FunCall(mult,
                                        FunCall(Get(1), p_8),
                                        FunCall(Get(1), p_9))))),
                                    FunCall(Zip(2),
                                      FunCall(Get(0), p_8),
                                      FunCall(Get(1), x))))),
                                  FunCall(Zip(2), p_6,
                                    FunCall(Get(0), x)))
                              ) $ Tuple(
                                toPrivate(MapSeq(id)) o Get(0) $ p_7,
                                asScalar() o toPrivate(MapSeq(VectorizeUserFun(4, id))) o asVector(vectorWidth) o Get(1) $ p_7
                              ))),
                              toPrivate(MapSeq(MapSeq(id))) $
                                Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4)),
                              FunCall(Zip(2),
                                FunCall(Transpose(), p_3),
                                FunCall(Transpose(), p_4)))))))),
                    FunCall(Split(v__3),
                      /*FunCall(Gather(ReorderWithStride(N / v__3)), */Transpose() $ p_1)))))),
              FunCall(Split(v__4), Transpose() $ p_0)))))

    val code = Compile(f, 32, 8, 1, N/v__3, M/v__4,1, collection.immutable.Map())

    val (output: Array[Float], _) =
      Execute(32, 8, nSize/v__3, mSize/v__4,
        (true, true))(code, f, matrixA.transpose, matrixB)

    assertArrayEquals(mmGold, output, 0.0001f)
  }

  @Test
  def clblast_hawaii_sgemm_TN(): Unit = {

    val v__3 = 4
    val v__4 = 8

    val f = GEMM.clblast_hawaii

    val code = Compile(f, 32, 8, 1, N/v__3, M/v__4,1, collection.immutable.Map())

    val (output: Array[Float], _) =
      Execute(32, 8, nSize/v__3, mSize/v__4, (true, true))(
        code, f, matrixA.transpose, matrixB, matrixC,
        alpha, beta
      )

    assertArrayEquals(gemmGold, output, 0.0001f)
  }


  @Test
  def clblas_hawaii_sgemm_TN(): Unit = {

    val tileSizeM = 4
    val tileSizeN = 8

    val f = GEMM.clblas_hawaii

    val (output: Array[Float], _) =
      Execute(16, 16, nSize/tileSizeN, nSize/tileSizeM, (true, true))(
        f, matrixA.transpose, matrixB, matrixC, alpha, beta)
    assertArrayEquals(gemmGold, output, 0.0f)
  }

  @Test
  def clblas_kepler_mm_TN(): Unit = {

    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val tileSizeM = 8
    val tileSizeK = 4
    val tileSizeN = 8

    val multAndSumUp = UserFun("multAndSumUp", Array("acc", "l", "r"),
      "{ return acc + (l * r); }",
      Seq(VectorType(Float, tileSizeN), Float, VectorType(Float, tileSizeN)),
      VectorType(Float, tileSizeN))

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        // Undo the tiling
        Untile2D() o
          MapGlb(1)(fun( aRows =>
            MapGlb(0)(fun( bCols =>

              toGlobal(MapSeq(MapSeq(VectorizeUserFun(tileSizeN, id)))) o
                Join() o

                // Multiply all necessary combinations of tiles
                ReduceSeq(fun( (acc, pairOfTiles) =>

                  fun(pairOfTiles =>
                    Map(Join()) o
                      MapSeq( fun(rowA =>
                        MapSeq( fun( colB =>
                          ReduceSeq(fun((acc, y) =>
                            multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
                          ), Get(colB, 1)) $ Zip(Get(rowA, 0), Get(colB, 0))
                        )) $ Zip(Transpose() $ Get(pairOfTiles, 1), Get(rowA, 1))
                      )) $ Zip(Transpose() $ Get(pairOfTiles, 0), acc)
                  ) o

                    // Copy tiles to private memory
                    fun(pairOfTiles =>
                      Tuple(
                        toPrivate(MapSeq(
                          asScalar() o
                            MapSeq(VectorizeUserFun(tileSizeM, id)) o
                            asVector(tileSizeM)
                        )) $ Get(pairOfTiles, 0),

                        toPrivate(MapSeq(
                          MapSeq(VectorizeUserFun(tileSizeN, id)) o
                            asVector(tileSizeN)
                        )) $ Get(pairOfTiles, 1)
                      )) $ pairOfTiles
                )
                  , MapSeq(MapSeq(VectorizeUserFun(tileSizeN, id))) $
                    Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(VectorType(Float, tileSizeN), 1), tileSizeM))
                ) $ Zip(aRows, bCols)

            )) o Transpose() o Tile(tileSizeK, tileSizeN) $ B
            // Tile the matrices
          )) o Transpose() o Tile(tileSizeK, tileSizeM) $ A
      })

    val (output: Array[Float], _) =
      Execute(8, 8, nSize/tileSizeN, nSize/tileSizeM, (true, true))(f, matrixA.transpose, matrixB)
    assertArrayEquals(mmGold, output, 0.0f)
  }

  @Test
  def clblas_kepler_sgemm_TN(): Unit = {

    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val tileSizeM = 8
    val tileSizeN = 8

    val f = GEMM.clblas_kepler

    val (output: Array[Float], _) =
      Execute(8, 8, nSize/tileSizeN, nSize/tileSizeM, (true, true))(
        f, matrixA.transpose, matrixB, matrixC, alpha, beta)
    assertArrayEquals(gemmGold, output, 0.0f)
  }

}
