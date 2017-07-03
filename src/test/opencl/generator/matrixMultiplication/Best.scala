package opencl.generator.matrixMultiplication

import ir._
import ir.ast._
import lift.arithmetic.{ArithExpr, SizeVar}
import opencl.executor._
import opencl.ir._
import opencl.ir.ast._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.{AfterClass, BeforeClass, Test}

object Best {
  @BeforeClass def before(): Unit =
    Executor.loadAndInit()

  @AfterClass def after(): Unit =
    Executor.shutdown()
}

class Best {

  val N = SizeVar("N")
  val M = SizeVar("M")
  val K = SizeVar("K")

  @Test
  def mm_clblas(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val mSize = 512
    val kSize = 512
    val nSize = 512
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val tileSizeM = 4
    val tileSizeK = 4
    val tileSizeN = 8

    val multAndSumUp = UserFun("multAndSumUp", Array("acc", "l", "r"),
      "{ return acc + (l * r); }",
      Seq(VectorType(Float, tileSizeN), Float, VectorType(Float, tileSizeN)),
      VectorType(Float, tileSizeN))

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
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
                      )) $ Zip(Get(pairOfTiles, 0), acc)
                  ) o

                    // Copy tiles to private memory
                    fun(pairOfTiles =>
                      Tuple(
                        toPrivate(MapSeq(
                          asScalar() o
                            MapSeq(VectorizeUserFun(tileSizeK, id)) o
                            asVector(tileSizeK)
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
          )) o Tile(tileSizeM, tileSizeK) $ A
      })

    val (output: Array[Float], _) =
      ExecuteOld(tileSizeN, tileSizeM, nSize/tileSizeN, nSize/tileSizeM, (true, true))(f, matrixA, matrixB)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def mm_nn_intel(): Unit = {

    val tileSizeM = 1
    val tileGroupM = 16
    val tileSizeN = 128
    val tileGroupN = 1
    val tileSizeK = 8

    val mSize = 512
    val kSize = 512
    val nSize = 512
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).transpose.flatten

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), // column-major
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // column-major
      (A, B) =>
        TransposeW() o // column-major
          Untile2D() o
          MapGlb(0)(fun(aTile =>
            MapGlb(1)(fun(bTile =>
              Map(TransposeW()) o TransposeW() o
               toGlobal(MapSeq(MapSeq(MapSeq(id)))) o
                ReduceSeq(fun((acc, pair) =>
                  MapSeq(fun(aRow =>
                    Join() o MapSeq(fun(bCol =>
                      ReduceSeq(fun((acc, next) =>
                        toGlobal(add)(acc, toPrivate(mult)(next._0, next._1))), bCol._1)
                        $ Zip(aRow._0, bCol._0)
                    ))$ Zip(pair._1, aRow._1)
                  )) $ Zip(pair._0, acc)
                ),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSizeN), tileSizeM))
              ) $ Zip(aTile, bTile)
            )) o Tile(tileSizeN, tileSizeK) $ B
          )) o Tile(tileSizeM, tileSizeK) o Transpose() $ A
    )

    val (output: Array[Float], _) =
      ExecuteOld(tileGroupM, tileGroupN, mSize/tileSizeM, nSize/tileSizeN, (true, true))(f, matrixA.transpose, matrixB.transpose)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def vectorised(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val Msize = 16
    val Ksize = 16
    val Nsize = 16
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val f1 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // this is already transposed
      (A, B) => {
        MapGlb(0)(fun( Arow =>
          MapGlb(1)(fun( Bcol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o asScalar() o
              ReduceSeq(fun((acc, next) => VectorizeUserFun(4, multAndSumUp)(acc, Get(next, 0), Get(next, 1))), Value(0.0f).vectorize(4))
              $ Zip(asVector(4) $ Arow, asVector(4) $ Bcol)
          )) $ B
        )) $ A
      })

    // Derived. TODO: Actual one contains some empty MapSeqs.
    // High-level da55a60496191590d618057cadfe6d18b409f8b76cd31753123701e465a8ea4d
    // Low-level 330f47d76e559e4f466c49cde9d7bd9438b370f8cd032e22f20ee156db54bd9a
    val fd = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // this is already transposed
      (p_924477420, p_640363654) =>
      FunCall(MapGlb(0)(fun((p_317986356) =>
        FunCall(MapGlb(1)(fun((p_331510866) =>
          toGlobal(MapSeq(id)) $ FunCall(ReduceSeq(fun((p_1728790703, p_1227074340) =>
            FunCall(add, p_1728790703, p_1227074340))), Value("0.0f", Float),
            FunCall(asScalar(),
                FunCall(ReduceSeq(fun((p_929776179, p_1765250898) =>
                  FunCall(VectorizeUserFun(4,add), p_929776179, FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_1765250898), FunCall(Get(1), p_1765250898)))
                )),
                  FunCall(VectorizeUserFun(4, id), Value("0.0f", VectorType(Float, 4))),
                  FunCall(Zip(2), FunCall(asVector(4), p_317986356), FunCall(asVector(4), p_331510866)))
            ))
        )), p_640363654)
      )), p_924477420))

    val (output1: Array[Float], _) = ExecuteOld(16, 16, Msize, Nsize, (true, true))(f1, matrixA, matrixB.transpose)
    val (output2: Array[Float], _) = ExecuteOld(16, 16, Msize, Nsize, (true, true))(fd, matrixA, matrixB.transpose)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output1, 0.001f)
    assertArrayEquals(gold, output2, 0.001f)

  }

  @Test def vectorisedReuseA(): Unit = {

    val mSize = 8
    val kSize = 32
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val vectorLength = 4

    val mult = UserFun("mult", Array("l", "r"), "{ return l * r; }", Seq(Float, Float4), Float4)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) =>
        Map(Join()) o
          MapGlb(0)(fun(rowA => MapGlb(1)( fun(colsB =>
            toGlobal(MapSeq(VectorizeUserFun(4, id))) o Join() o ReduceSeq(fun((acc, elemRowPair) =>
              MapSeq(fun(partial => VectorizeUserFun(4,add)(Get(partial, 0), Get(partial, 1))))
                $ Zip(MapSeq(fun(b => mult(Get(elemRowPair, 0), b))) o asVector(vectorLength) $ Get(elemRowPair, 1), acc)
            ), toPrivate(MapSeq(VectorizeUserFun(4, id))) $ Value("0.0f", ArrayTypeWSWC(VectorType(Float, vectorLength), 1))) $ Zip(rowA, colsB)
          )) o Map(Transpose()) o Split(vectorLength) o Transpose() $ B
          )) $ A
    )

    val (output: Array[Float], _) = ExecuteOld(mSize, nSize)(f, matrixA, matrixB)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.001f)

  }

  @Test def partiallyVectorisedTiled(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    // Basic tiled matrix multiply without local memory
    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 2
    val vectorLength = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    // load the tile in local memory once (to avoid reloading the same value twice) and vectorized the summation in the reduction loop
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        // tile A
        A :>> Tile(tileSize, vectorLength) :>>
        MapGlb(0)(fun( aRows =>
          // tile B
          B :>> Tile(tileSize, vectorLength) :>>
          MapGlb(1)(fun( bCols =>
            Zip(aRows, bCols) :>>
            ReduceSeq(fun( (acc, pairOfTiles) => {
              // copy both tiles into private memory
              Zip(
                pairOfTiles._0 :>> MapSeq(fun(rowA =>
                  asVector(vectorLength)(rowA) :>> toPrivate(MapSeq(VectorizeUserFun(4, id))))),
                pairOfTiles._1 :>> MapSeq(fun(colB =>
                  asVector(vectorLength)(colB) :>> toPrivate(MapSeq(VectorizeUserFun(4, id)))))
              ) :>> fun(zippedPairOfTiles =>
                // for each row of the tile of A ...
                zippedPairOfTiles :>> MapSeq(fun( rowA =>
                  // ... and each column of the tile of B ...
                  zippedPairOfTiles :>> MapSeq(fun( colB =>
                    // ... perform vectorized multiplication ...
                    Zip(rowA._0, colB._1) :>>
                    MapSeq(VectorizeUserFun(4, mult)) :>>
                    // .. and scalar summation
                    asScalar() :>>
                    ReduceSeq(add, Value(0.0f) :>> id)
                  ))
                ))
              ) :>> fun(partial =>
                // reshape the data to be vectorized for performing the summation
                partial :>> Join() :>> Join() :>> asVector(4) :>>
                fun(xs =>
                  // perform the vectorized summation
                  Zip(acc, xs) :>> MapSeq(VectorizeUserFun(4, add))
                ))
              }), MapSeq(VectorizeUserFun(4, id))(Value(0.0f, ArrayTypeWSWC(VectorType(Float, 4), 1)))
            ) :>>
            // reshape the data and perform the copy back to global memory using a vector width of 2
            Map(asScalar() >>> asVector(2) >>> Split(1)) :>>
            toGlobal(MapSeq(MapSeq(MapSeq(VectorizeUserFun(2, id))))) :>>
            TransposeW() :>> Map(TransposeW() >>> Join() >>> asScalar())
          ))
        )) :>> Untile2D()
      }
    )

    // Derived. TODO: Actual one contains some empty MapSeqs.
    // High-level 7352181db558ca218caa8723936a115f8e30dd4e69a2686e11d5a0255bf5d8a4
    // Low-level 72ef373f103c33cf9c79741a95fdd9d15d605f757529151844690d685c173c19
    val fd = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) =>
      FunCall(Join(), FunCall(MapGlb(0)(fun((p_1545087375) =>
        FunCall(TransposeW(), FunCall(Join(), FunCall(MapGlb(1)(fun((p_668210649) =>
          FunCall(TransposeW(), FunCall(Map(fun((p_1434041222) =>
            FunCall(TransposeW(), p_1434041222)
          )), FunCall(TransposeW(), FunCall(MapSeq(fun((p_1308109015) =>
            FunCall(toGlobal(MapSeq(MapSeq(id))), p_1308109015))), FunCall(ReduceSeq(fun((p_2050404090, p_280265505) =>
            FunCall(MapSeq(fun((p_827084938) =>
              FunCall(Join(), FunCall(MapSeq(fun((p_306206744) =>
                FunCall(ReduceSeq(fun((p_1597655940, p_2619171) =>
                  FunCall(add, p_1597655940, p_2619171))),
                  FunCall(Get(0), p_306206744), FunCall(asScalar(), FunCall(MapSeq(fun((p_1983025922) =>
                    FunCall(VectorizeUserFun(4,mult), FunCall(Get(0), p_1983025922), FunCall(Get(1), p_1983025922))
                  )), FunCall(Zip(2), FunCall(asVector(4), FunCall(Get(1), p_827084938)), FunCall(asVector(4), FunCall(Get(1), p_306206744)))))))), FunCall(Zip(2), FunCall(Get(0), p_827084938), FunCall(Transpose(), FunCall(Get(1), p_280265505)))))
            )), FunCall(Zip(2), p_2050404090, FunCall(Transpose(), FunCall(Get(0), p_280265505))))
          )), FunCall(MapSeq(fun((p_824208363) =>
            FunCall(MapSeq(fun((p_500179317) =>
              FunCall(idfloat, p_500179317)
            )), p_824208363))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize))), FunCall(Zip(2), FunCall(Split(vectorLength), FunCall(Transpose(), p_1545087375)), FunCall(Split(vectorLength), FunCall(Transpose(), p_668210649))))))))
        )), FunCall(Split(tileSize), B))))
      )), FunCall(Split(tileSize), A))))

    val fdot = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) =>
      FunCall(Join(), FunCall(MapGlb(0)(fun((p_1545087375) =>
        FunCall(TransposeW(), FunCall(Join(), FunCall(MapGlb(1)(fun((p_668210649) =>
          FunCall(TransposeW(), FunCall(Map(fun((p_1434041222) =>
            FunCall(TransposeW(), p_1434041222)
          )), FunCall(TransposeW(), FunCall(MapSeq(fun((p_1308109015) =>
            FunCall(toGlobal(MapSeq(MapSeq(id))), p_1308109015))), FunCall(ReduceSeq(fun((p_2050404090, p_280265505) =>
            FunCall(MapSeq(fun((p_827084938) =>
              FunCall(Join(), FunCall(MapSeq(fun((p_306206744) =>
                FunCall(ReduceSeq(fun((p_1597655940, p_2619171) =>
                  FunCall(add, p_1597655940, p_2619171))),
                  FunCall(Get(0), p_306206744), FunCall(MapSeq(fun((p_1983025922) =>
                    FunCall(dot, FunCall(Get(0), p_1983025922), FunCall(Get(1), p_1983025922))
                  )), FunCall(Zip(2), FunCall(asVector(4), FunCall(Get(1), p_827084938)), FunCall(asVector(4), FunCall(Get(1), p_306206744))))))), FunCall(Zip(2), FunCall(Get(0), p_827084938), FunCall(Transpose(), FunCall(Get(1), p_280265505)))))
            )), FunCall(Zip(2), p_2050404090, FunCall(Transpose(), FunCall(Get(0), p_280265505))))
          )), FunCall(MapSeq(fun((p_824208363) =>
            FunCall(MapSeq(fun((p_500179317) =>
              FunCall(idfloat, p_500179317)
            )), p_824208363))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize))), FunCall(Zip(2), FunCall(Split(vectorLength), FunCall(Transpose(), p_1545087375)), FunCall(Split(vectorLength), FunCall(Transpose(), p_668210649))))))))
        )), FunCall(Split(tileSize), B))))
      )), FunCall(Split(tileSize), A))))


    val (output1: Array[Float], _) = ExecuteOld(2, 2, mSize/2, nSize/2, (true, true))(f, matrixA, matrixB.transpose)
    val (output2: Array[Float], _) = ExecuteOld(2, 2, mSize/2, nSize/2, (true, true))(fd, matrixA, matrixB.transpose)
    val (output3: Array[Float], _) = ExecuteOld(2, 2, mSize/2, nSize/2, (true, true))(fdot, matrixA, matrixB.transpose)

    assertArrayEquals(gold, output1, 0.0001f)
    assertArrayEquals(gold, output2, 0.0001f)
    assertArrayEquals(gold, output3, 0.0001f)
  }

  @Test
  def maliGEMM(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val maliFactory =
      (variables: Seq[ArithExpr]) => {
        val v_K_0 = variables(0)
        val v_M_1 = variables(1)
        val v_N_2 = variables(2)
        val v__3 = variables(3)
        val v__4 = variables(4)
        val v__5 = variables(5)

        val idfloat = UserFun("idfloat", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
        val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
        val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
        fun(
          ArrayTypeWSWC(ArrayTypeWSWC(Float, v_K_0), v_M_1),
          ArrayTypeWSWC(ArrayTypeWSWC(Float, v_K_0), v_N_2),
          ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_M_1),
          Float, Float,
          (p_0, p_1, C, alpha, beta) =>
            FunCall(Join(),
              FunCall(MapGlb(0)(fun((p_2) =>
                FunCall(TransposeW(),
                  FunCall(Join(),
                    FunCall(MapGlb(1)(fun((p_3) =>
                      FunCall(TransposeW(),
                        FunCall(Map(fun((p_4) =>
                          FunCall(TransposeW(), p_4))),
                          FunCall(TransposeW(),
                            FunCall(toGlobal(fun((p_5) =>
                              FunCall(MapSeq(fun((p_6) =>
                                FunCall(MapSeq(fun((p_7) =>
                                  FunCall(MapSeq(fun((p_8) =>
                                    add(mult(p_8._0, alpha),
                                      mult(p_8._1, beta))
                                  )), Zip(p_7._0, p_7._1)))),
                                  Zip(p_6, Transpose() $ p_3._1)))), p_5))),
                              FunCall(ReduceSeq(fun((p_9, p_10) =>
                                FunCall(fun((p_11) =>
                                  FunCall(MapSeq(fun((p_12) =>
                                    FunCall(Join(),
                                      FunCall(MapSeq(fun((p_13) =>
                                        FunCall(MapSeq(fun((p_14) => p_14)),
                                          FunCall(ReduceSeq(fun((p_15, p_16) =>
                                            FunCall(add, p_15, p_16))),
                                            FunCall(Get(0), p_13),
                                              FunCall(MapSeq(fun((p_17) =>
                                                FunCall(dot,
                                                  FunCall(Get(0), p_17),
                                                  FunCall(Get(1), p_17)))),
                                                FunCall(Zip(2),
                                                  FunCall(asVector(4),
                                                    FunCall(Get(1), p_12)),
                                                  FunCall(asVector(4),
                                                    FunCall(Get(1), p_13)))))))),
                                        FunCall(Zip(2),
                                          FunCall(Get(0), p_12),
                                          FunCall(Transpose(),
                                            FunCall(Get(1), p_11))))))),
                                    FunCall(Zip(2), p_9,
                                      FunCall(Transpose(),
                                        FunCall(Get(0), p_11))))), p_10))),
                                FunCall(MapSeq(fun((p_18) =>
                                  FunCall(MapSeq(fun((p_19) =>
                                    FunCall(idfloat, p_19))), p_18))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4))),
                                FunCall(Zip(2),
                                  FunCall(Split(v__5),
                                    FunCall(Transpose(), p_2._0)),
                                  FunCall(Split(v__5),
                                    FunCall(Transpose(), p_3._0)))))))))),
                      Zip(FunCall(Split(v__3), p_1), Split(v__3) o Transpose() $ p_2._1)))))),
                Zip(FunCall(Split(v__4), p_0), Split(v__4) $ C))))
      }

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val matrixC = Array.tabulate(nSize, mSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val alpha = 2.5f
    val beta = 1.5f

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB, matrixC, alpha, beta).flatten

    val f = maliFactory(Seq[ArithExpr](K, M, N, 2, 2, 4))

    val (output: Array[Float], _) =
      ExecuteOld(2, 2, mSize/2, nSize/2, (true, true))(
        f, matrixA, matrixB.transpose, matrixC, alpha, beta)

    assertArrayEquals(gold, output, 0.0001f)
  }


  @Test
  def hawaiiBest(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val factory = (variables: Seq[ArithExpr]) => {
      val v_M_0 = variables(0)
      val v_K_1 = variables(1)
      val v_N_2 = variables(2)
      val v__3 = variables(3)
      val v__4 = variables(4)
      val v__5 = variables(5)
      val v__6 = variables(6)
      val v__7 = variables(7)

      fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1), ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1),(p_0, p_1) => FunCall(Join(), FunCall(MapWrg(1)(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(MapWrg(0)(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(Map(fun((p_5) => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))), FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), FunCall(Map(fun((p_7) => FunCall(TransposeW(), p_7))), FunCall(TransposeW(), p_6))))), FunCall(TransposeW(), p_4))))))), FunCall(TransposeW(), FunCall(toGlobal(fun((p_8) => FunCall(MapSeq(fun((p_9) => FunCall(MapLcl(1)(fun((p_10) => FunCall(MapLcl(0)(fun((p_11) => FunCall(MapSeq(fun((p_12) => FunCall(MapSeq(fun((p_13) => FunCall(id, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))), FunCall(ReduceSeq(fun((p_14, p_15) => FunCall(fun((p_16) => FunCall(MapLcl(1)(fun((p_17) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_18) => FunCall(MapSeq(fun((p_19) => p_19)), FunCall(ReduceSeq(fun((p_20, p_21) => FunCall(fun((p_22) => FunCall(MapSeq(fun((p_23) => FunCall(MapSeq(fun((p_24) => FunCall(add, FunCall(Get(0), p_24), FunCall(mult, FunCall(Get(1), p_23), FunCall(Get(1), p_24))))), FunCall(Zip(2), FunCall(Get(0), p_23), FunCall(Get(1), p_22))))), FunCall(Zip(2), p_20, FunCall(Get(0), p_22)))), FunCall(toLocal(fun((p_25) => FunCall(fun((p_26) => FunCall(Tuple(2), FunCall(MapSeq(fun((p_27) => FunCall(id, p_27))), FunCall(Get(0), p_26)), FunCall(MapSeq(fun((p_28) => FunCall(id, p_28))), FunCall(Get(1), p_26)))), p_25))), FunCall(toPrivate(fun((p_29) => FunCall(fun((p_30) => FunCall(Tuple(2), FunCall(Get(0), p_30), FunCall(MapSeq(fun((p_31) => FunCall(id, p_31))), FunCall(Get(1), p_30)))), p_29))), p_21))))), FunCall(Get(0), p_18), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p_17)), FunCall(Transpose(), FunCall(Get(1), p_18))))))), FunCall(Zip(2), FunCall(Get(0), p_17), FunCall(Split(v__4), FunCall(Gather(ReorderWithStride(v__3 / v__4)), FunCall(Transpose(), FunCall(Get(1), p_16))))))))), FunCall(Zip(2), p_14, FunCall(Split(v__5), FunCall(Transpose(), FunCall(Get(0), p_16)))))), p_15))), FunCall(MapLcl(1)(fun((p_32) => FunCall(MapLcl(0)(fun((p_33) => FunCall(MapSeq(fun((p_34) => FunCall(MapSeq(fun((p_35) => FunCall(id, p_35))), p_34))), p_33))), p_32))), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3 * 1 /^ v__4), v__6 * 1 /^ v__5))), FunCall(Zip(2), p_2, p_3))))))))), FunCall(Transpose(), FunCall(Map(fun((p_36) => FunCall(Transpose(), p_36))), FunCall(Split(v__7), FunCall(Map(fun((p_37) => FunCall(Split(v__3), p_37))), p_1))))))))), FunCall(Transpose(), FunCall(Map(fun((p_38) => FunCall(Transpose(), p_38))), FunCall(Split(v__7), FunCall(Map(fun((p_39) => FunCall(Split(v__6), p_39))), p_0)))))))
    }

    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")

    val f = factory(Seq[ArithExpr](v_M_0, v_K_1, v_N_2,128,4,16, 64 ,256))

    val size = 1024

    val matrixA = Array.tabulate(size, size)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(size, size)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB)

    val (output: Array[Float], _) = ExecuteOld()(f, matrixA.transpose, matrixB)

    assertArrayEquals(gold.flatten, output, 0.001f)
  }

  @Test
  def hawaiiBestSgemm(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val factory = (variables: Seq[ArithExpr]) => {
      val v_M_0 = variables(0)
      val v_K_1 = variables(1)
      val v_N_2 = variables(2)
      val v__3 = variables(3)
      val v__4 = variables(4)
      val v__5 = variables(5)
      val v__6 = variables(6)
      val v__7 = variables(7)

      fun(
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1),
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1),
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_M_0),
        Float,
        Float,
        (p_0, p_1, C,alpha,beta) =>
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

                              FunCall(toGlobal(MapSeq(fun(x =>
                                MapLcl(1)(fun(y =>
                                  MapLcl(0)(fun( z =>
                                    MapSeq(fun(a =>
                                      MapSeq(fun(x =>
                                        add(
                                          toPrivate(mult)(Get(x, 0), alpha),
                                          toPrivate(mult)(Get(x, 1), beta)
                                        )
                                      )) $ Zip(Get(a, 0), Get(a, 1))
                                    )) $ Zip(Get(z, 0), Transpose() $ Get(z, 1))
                                  )) $ Zip(Get(y, 0), Split(v__4) o ReorderStride(v__3 / v__4) o Transpose() $ Get(y, 1))
                                )) $ Zip(x, Split(v__5) $ Get(p_3, 1))
                              ))),
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
                                              FunCall(toLocal(fun((p_25) =>
                                                FunCall(fun((p_26) =>
                                                  FunCall(Tuple(2),
                                                    FunCall(MapSeq(fun((p_27) =>
                                                      FunCall(id, p_27))),
                                                      FunCall(Get(0), p_26)),
                                                    FunCall(MapSeq(fun((p_28) =>
                                                      FunCall(id, p_28))),
                                                      FunCall(Get(1), p_26)))), p_25))),
                                                FunCall(toPrivate(fun((p_29) =>
                                                  FunCall(fun((p_30) =>
                                                    FunCall(Tuple(2),
                                                      FunCall(Get(0), p_30),
                                                      FunCall(MapSeq(fun((p_31) =>
                                                        FunCall(id, p_31))),
                                                        FunCall(Get(1), p_30)))), p_29))), p_21))))),
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
                                          FunCall(Get(0), p_16)))))), p_15))),
                                FunCall(MapLcl(1)(fun((p_32) =>
                                  FunCall(MapLcl(0)(fun((p_33) =>
                                    FunCall(MapSeq(fun((p_34) =>
                                      FunCall(MapSeq(fun((p_35) =>
                                        FunCall(id, p_35))), p_34))), p_33))), p_32))),
                                  Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3*1/^v__4), v__6*1/^v__5))),
                                FunCall(Zip(2), Get(p_2, 0), Get(p_3, 0)))))))))),
                    Zip(FunCall(Transpose(),
                      FunCall(Map(fun((p_36) =>
                        FunCall(Transpose(), p_36))),
                        FunCall(Split(v__7),
                          FunCall(Map(fun((p_37) =>
                            FunCall(Split(v__3), p_37))), p_1)))), Get(p_2,1))))))),
              Zip(FunCall(Transpose(),
                FunCall(Map(fun((p_38) => FunCall(Transpose(), p_38))),
                  FunCall(Split(v__7),
                    FunCall(Map(fun((p_39) =>
                      FunCall(Split(v__6), p_39))), p_0)))), Tile(v__6, v__3) $ C))))
    }

    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")

    val f = factory(Seq[ArithExpr](v_M_0, v_K_1, v_N_2,128,4,16, 64 ,256))

    val size = 1024

    val matrixA = Array.tabulate(size, size)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(size, size)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val matrixC = Array.tabulate(size, size)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val alpha = 1.5f
    val beta = 0.5f

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB, matrixC, alpha, beta)

    val (output: Array[Float], _) =
      ExecuteOld()(f, matrixA.transpose, matrixB, matrixC, alpha, beta)

    assertArrayEquals(gold.flatten, output, 0.001f)
  }
  @Test
  def clblast_TN_kepler(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

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
      fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1), ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1),(p_0, p_1) => FunCall(Join(), FunCall(MapWrg(1)(fun((p_2) => FunCall(TransposeW(), FunCall(Join(), FunCall(MapWrg(0)(fun((p_3) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_4) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(TransposeW(), FunCall(Map(fun((p_6) => FunCall(TransposeW(), p_6))), FunCall(TransposeW(), p_5))))), FunCall(TransposeW(), p_4)))))), FunCall(TransposeW(), FunCall(MapSeq(fun((p_7) => FunCall(toGlobal(fun((p_8) => FunCall(MapLcl(1)(fun((p_9) => FunCall(MapLcl(0)(fun((p_10) => FunCall(MapSeq(fun((p_11) => FunCall(asScalar(), FunCall(MapSeq(fun((p_12) => FunCall(VectorizeUserFun(4,idfloat), p_12))), FunCall(asVector(4), p_11))))), p_10))), p_9))), p_8))), p_7))), FunCall(ReduceSeq(fun((p_13, p_14) => FunCall(fun((p_15) => FunCall(MapLcl(1)(fun((p_16) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_17) => FunCall(MapSeq(fun((p_18) => p_18)), FunCall(ReduceSeq(fun((p_19, p_20) => FunCall(fun((p_21) => FunCall(MapSeq(fun((p_22) => FunCall(MapSeq(fun((p_23) => FunCall(add, FunCall(Get(0), p_23), FunCall(mult, FunCall(Get(1), p_22), FunCall(Get(1), p_23))))), FunCall(Zip(2), FunCall(Get(0), p_22), FunCall(Get(1), p_21))))), FunCall(Zip(2), p_19, FunCall(Get(0), p_21)))), FunCall(toPrivate(fun((p_24) => FunCall(fun((p_25) => FunCall(Tuple(2), FunCall(MapSeq(fun((p_26) => FunCall(idfloat, p_26))), FunCall(Get(0), p_25)), FunCall(MapSeq(fun((p_27) => FunCall(idfloat, p_27))), FunCall(Get(1), p_25)))), p_24))), p_20)))), FunCall(Get(0), p_17), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p_16)), FunCall(Transpose(), FunCall(Get(1), p_17))))))), FunCall(Zip(2), FunCall(Get(0), p_16), FunCall(Split(v__3), FunCall(Transpose(), FunCall(Get(1), p_15)))))))), FunCall(Zip(2), p_13, FunCall(Split(v__4), FunCall(Transpose(), FunCall(Get(0), p_15)))))), FunCall(toLocal(fun((p_28) => FunCall(fun((p_29) => FunCall(Tuple(2), FunCall(Split(v__5), FunCall(Join(), FunCall(MapLcl(1)(fun((p_30) => FunCall(asScalar(), FunCall(MapLcl(0)(fun((p_31) => FunCall(VectorizeUserFun(4,idfloat), p_31))), FunCall(asVector(4), p_30))))), FunCall(Split(v__6), FunCall(Join(), FunCall(Get(0), p_29)))))), FunCall(MapLcl(1)(fun((p_32) => FunCall(asScalar(), FunCall(MapLcl(0)(fun((p_33) => FunCall(VectorizeUserFun(4,idfloat), p_33))), FunCall(asVector(4), p_32))))), FunCall(Get(1), p_29)))), p_28))), p_14)))), FunCall(MapLcl(1)(fun((p_34) => FunCall(MapLcl(0)(fun((p_35) => FunCall(MapSeq(fun((p_36) => FunCall(MapSeq(fun((p_37) => FunCall(idfloat, p_37))), p_36))), p_35))), p_34))), Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4), v__7 * 1 /^ v__3), v__5 * 1 /^ v__4))), FunCall(Zip(2), p_2, p_3))))))))), FunCall(Transpose(), FunCall(Map(fun((p_38) => FunCall(Transpose(), p_38))), FunCall(Split(v__8), FunCall(Map(fun((p_39) => FunCall(Split(v__7), p_39))), p_1))))))))), FunCall(Transpose(), FunCall(Map(fun((p_40) => FunCall(Transpose(), p_40))), FunCall(Split(v__8), FunCall(Map(fun((p_41) => FunCall(Split(v__5), p_41))), p_0)))))))
    }

    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")

    val f = factory(Seq[ArithExpr](v_M_0, v_K_1, v_N_2,4,8,64, 128, 128, 16))

    val size = 1024

    val matrixA = Array.tabulate(size, size)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(size, size)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB)

    val (output: Array[Float], _) = ExecuteOld()(f, matrixA.transpose, matrixB)

    assertArrayEquals(gold.flatten, output, 0.001f)
  }

  @Test
  def keplerBest(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val factory = (variables: Seq[ArithExpr]) => {
      val v_M_0 = variables(0)
      val v_K_1 = variables(1)
      val v_N_2 = variables(2)
      val v__3 = variables(3)
      val v__4 = variables(4)
      val v__5 = variables(5)
      val v__6 = variables(6)
      val v__7 = variables(7)

      fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1), ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1), (p_0, p_1) =>
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
                                      FunCall(id, p_13))), p_12))), p_11))), p_10))), p_9))), p_8))),
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
                                                  FunCall(MapSeq(fun((p_27) =>
                                                    FunCall(id, p_27))),
                                                    FunCall(Get(0), p_26)),
                                                  FunCall(MapSeq(fun((p_28) =>
                                                    FunCall(id, p_28))),
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
                                FunCall(toLocal(fun((p_29) =>
                                  FunCall(fun((p_30) =>
                                    FunCall(Unzip(),
                                      FunCall(MapLcl(1)(fun((p_31) =>
                                        FunCall(Tuple(2),
                                          FunCall(MapLcl(0)(fun((p_32) =>
                                            FunCall(id, p_32))),
                                            FunCall(Get(0), p_31)),
                                          FunCall(MapLcl(0)(fun((p_33) =>
                                            FunCall(id, p_33))),
                                            FunCall(Get(1), p_31))))),
                                        FunCall(Zip(2),
                                          FunCall(Get(0), p_30),
                                          FunCall(Get(1), p_30))))), p_29))), p_15)))),
                              FunCall(MapLcl(1)(fun((p_34) =>
                                FunCall(MapLcl(0)(fun((p_35) =>
                                  FunCall(MapSeq(fun((p_36) =>
                                    FunCall(MapSeq(fun((p_37) =>
                                      FunCall(id, p_37))), p_36))), p_35))), p_34))), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3 * 1 /^ v__4), v__6 * 1 /^ v__5))),
                              FunCall(Zip(2), p_2, p_3))))))))),
                  FunCall(Transpose(),
                    FunCall(Map(fun((p_38) =>
                      FunCall(Transpose(), p_38))),
                      FunCall(Split(v__7),
                        FunCall(Map(fun((p_39) =>
                          FunCall(Split(v__3), p_39))), p_1))))))))),
            FunCall(Transpose(),
              FunCall(Map(fun((p_40) =>
                FunCall(Transpose(), p_40))),
                FunCall(Split(v__7),
                  FunCall(Map(fun((p_41) =>
                    FunCall(Split(v__6), p_41))), p_0)))))))
    }

    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")

    val f = factory(Seq[ArithExpr](v_M_0, v_K_1, v_N_2, 128, 4, 8, 64 ,8))

    val size = 1024

    val matrixA = Array.tabulate(size, size)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(size, size)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB)

    val (output: Array[Float], _) = ExecuteOld()(f, matrixA.transpose, matrixB)

    assertArrayEquals(gold.flatten, output, 0.001f)
  }

  @Test
  def keplerBestSgemm(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val factory = (variables: Seq[ArithExpr]) => {
      val v_M_0 = variables(0)
      val v_K_1 = variables(1)
      val v_N_2 = variables(2)
      val v__3 = variables(3)
      val v__4 = variables(4)
      val v__5 = variables(5)
      val v__6 = variables(6)
      val v__7 = variables(7)

      fun(
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1),
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1),
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_M_0),
        Float,
        Float,
        (p_0, p_1, C,alpha,beta) =>
          FunCall(Join(),
            FunCall(MapWrg(1)(fun((p_2) =>
              FunCall(TransposeW(),
                FunCall(Join(),
                  FunCall(MapWrg(0)(fun((p_3) =>
                    FunCall(TransposeW(),
                      FunCall(Join(),
                        FunCall(Map(fun((p_4)
                        => FunCall(Map(fun((p_5)
                          => FunCall(Scatter(ReorderWithStride(v__3 / v__4)), p_5))),
                            FunCall(TransposeW(),
                              FunCall(Join(),
                                FunCall(Map(fun((p_6) =>
                                  FunCall(TransposeW(),
                                    FunCall(Map(fun((p_7) =>
                                      FunCall(TransposeW(), p_7))),
                                      FunCall(TransposeW(), p_6))))),
                                  FunCall(TransposeW(), p_4))))))),
                          FunCall(TransposeW(),
                            FunCall(toGlobal(MapSeq(fun(x =>
                              MapLcl(1)(fun(y =>
                                MapLcl(0)(fun( z =>
                                  MapSeq(fun(a =>
                                    MapSeq(fun(x =>
                                      add(
                                        toPrivate(mult)(Get(x, 0), alpha),
                                        toPrivate(mult)(Get(x, 1), beta)
                                      )
                                    )) $ Zip(Get(a, 0), Get(a, 1))
                                  )) $ Zip(Get(z, 0), Transpose() $ Get(z, 1))
                                )) $ Zip(Get(y, 0), Split(v__4) o ReorderStride(v__3 / v__4) o Transpose() $ Get(y, 1))
                              )) $ Zip(x, Split(v__5) $ Get(p_3, 1))
                            ))),
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
                                                    FunCall(mult, FunCall(Get(1), p_23),
                                                      FunCall(Get(1), p_24))
                                                  ))
                                                ), FunCall(Zip(2),
                                                  FunCall(Get(0), p_23),
                                                  FunCall(Get(1), p_22))))),
                                                FunCall(Zip(2), p_20,
                                                  FunCall(Get(0), p_22)))),
                                              FunCall(toPrivate(fun((p_25) =>
                                                FunCall(fun((p_26) =>
                                                  FunCall(Tuple(2),
                                                    FunCall(MapSeq(fun((p_27) =>
                                                      FunCall(id, p_27))),
                                                      FunCall(Get(0), p_26)),
                                                    FunCall(MapSeq(fun((p_28) =>
                                                      FunCall(id, p_28))),
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
                                  FunCall(toLocal(fun((p_29) =>
                                    FunCall(fun((p_30) =>
                                      FunCall(Unzip(),
                                        FunCall(MapLcl(1)(fun((p_31) =>
                                          FunCall(Tuple(2),
                                            FunCall(MapLcl(0)(fun((p_32) =>
                                              FunCall(id, p_32))),
                                              FunCall(Get(0), p_31)),
                                            FunCall(MapLcl(0)(fun((p_33) =>
                                              FunCall(id, p_33))),
                                              FunCall(Get(1), p_31))))),
                                          FunCall(Zip(2),
                                            FunCall(Get(0), p_30),
                                            FunCall(Get(1), p_30))))), p_29))), p_15)))),
                                FunCall(MapLcl(1)(fun((p_34) =>
                                  FunCall(MapLcl(0)(fun((p_35) =>
                                    FunCall(MapSeq(fun((p_36) =>
                                      FunCall(MapSeq(fun((p_37) =>
                                        FunCall(id, p_37))), p_36))), p_35))), p_34))),
                                  Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3 * 1 /^ v__4), v__6 * 1 /^ v__5))),
                                FunCall(Zip(2), Get(p_2, 0), Get(p_3, 0)))))))))),
                    Zip(FunCall(Transpose(),
                      FunCall(Map(fun((p_38) =>
                        FunCall(Transpose(), p_38))),
                        FunCall(Split(v__7),
                          FunCall(Map(fun((p_39) =>
                            FunCall(Split(v__3), p_39))), p_1)))), Get(p_2, 1))))))),
              Zip(FunCall(Transpose(),
                FunCall(Map(fun((p_40) =>
                  FunCall(Transpose(), p_40))),
                  FunCall(Split(v__7),
                    FunCall(Map(fun((p_41) =>
                      FunCall(Split(v__6), p_41))), p_0)))), Tile(v__6, v__3) $ C))))
    }

    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")

    val f = factory(Seq[ArithExpr](v_M_0, v_K_1, v_N_2,128,4,8, 64 ,8))

    val size = 1024

    val matrixA = Array.tabulate(size, size)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(size, size)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val matrixC = Array.tabulate(size, size)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val alpha = 1.5f
    val beta = 0.5f

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB, matrixC, alpha, beta)

    val (output: Array[Float], _) =
      ExecuteOld()(f, matrixA.transpose, matrixB, matrixC, alpha, beta)

    assertArrayEquals(gold.flatten, output, 0.001f)
  }

}
