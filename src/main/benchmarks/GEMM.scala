package benchmarks

import apart.arithmetic._
import ir._
import ir.ast._
import opencl.executor.Utils
import opencl.ir._
import opencl.ir.pattern._
import org.clapper.argot.ArgotConverters._

class GEMM (override val f: Seq[(String, Array[Lambda])])
  extends Benchmark("Matrix Multiplication", Seq(1024, 1024, 1024), f, 0.1f, Array(16, 16, 1)) {

  val tileX = parser.option[Long](List("x", "tileX"), "size",
    "Tile size in the M and N dimension")

  val tileY = parser.option[Long](List("y", "tileY"), "size",
    "Tile size in the K dimension")

  val registerBlockM = parser.option[Long](List("bm", "blockM"), "size",
   "Register blocking factor in M dimension")

  val registerBlockN = parser.option[Long](List("bn", "blockN"), "size",
    "Register blocking factor in N dimension")

  val vectorWidth = parser.option[Long](List("vw", "vectorWidth"), "width",
    "Vector width for loading values")

  override def runScala(inputs: Any*): Array[Float] = {
    var A = inputs(0).asInstanceOf[Array[Array[Float]]]
    val B = inputs(1).asInstanceOf[Array[Array[Float]]]
    val C = inputs(2).asInstanceOf[Array[Array[Float]]]
    val alpha = inputs(3).asInstanceOf[Float]
    val beta = inputs(4).asInstanceOf[Float]

    val variant = variantOpt.value.getOrElse(0)
    if (variant != 0)
      A = A.transpose

    val res = Utils.matrixMatrixMultiply(A, B, C, alpha, beta)

    res.flatten
  }



  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes().head
    val inputSizeM = inputSizes()(1)
    val inputSizeK = inputSizes()(2)

    val matrixA = Array.tabulate(inputSizeM, inputSizeK)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val matrixB = Array.tabulate(inputSizeK, inputSizeN)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val matrixC = Array.tabulate(inputSizeK, inputSizeN)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)

    Seq(matrixA, matrixB, matrixC, 1.5f, 2.5f)
  }

  override def globalSize: Array[Int] = {
    val globalSizes = Array(inputSizes().head, inputSizes()(1), 1)
    globalSizeOpt.value.copyToArray(globalSizes)
    globalSizes
  }

  override protected def beforeBenchmark(): Unit = {
    f(1)._2(0) = GEMM.tiledAndBlockedBInnermost(Cst(tileX.value.getOrElse(16)),
      Cst(tileX.value.getOrElse(16)), Cst(tileY.value.getOrElse(8)), Cst(registerBlockN.value.getOrElse(4)),
      Cst(registerBlockM.value.getOrElse(4)))
  }

  override protected def printParams(): Unit = {
    println("Tile size: " + tileX.value.getOrElse(16) + " " + tileY.value.getOrElse(8))
    println("Work per thread: " +registerBlockN.value.getOrElse(4) + " " + registerBlockM.value.getOrElse(4))
  }

  override protected def printResults(time: Double): Unit = {
    val dNumOps = 2.0 * inputSizes().head.toDouble * inputSizes()(1).toDouble * inputSizes()(2).toDouble
    val gflops = 1.0e-6 * dNumOps/time
    println("THROUGHPUT: " + gflops + " GFlops/s")
  }
}

object GEMM {
  val N = SizeVar("N")
  val M = SizeVar("M")
  val K = SizeVar("K")

  val naive = fun(
    ArrayType(ArrayType(Float, K), N),
    ArrayType(ArrayType(Float, M), K),
    ArrayType(ArrayType(Float, M), N),
    Float,
    Float,
    (A, B, C, alpha, beta) => {
      MapGlb(fun( aRow =>
        Join() o  MapSeq(fun( bCol =>
          toGlobal(MapSeq(id)) o
            MapSeq(fun(x => multAndSumUp(x, beta, Get(bCol, 1)))) o
            MapSeq(fun(x => mult(x, alpha))) o
            ReduceSeq(fun((acc, y) =>
              multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
            ), 0.0f) $ Zip(Get(aRow, 0), Get(bCol, 0))
        )) $ Zip(Transpose() $ B, Get(aRow, 1))
      )) $ Zip(A, C)
    })

  // Currently the best for NVIDIA
  def tiledAndBlockedBInnermost(tileSizeN: ArithExpr, tileSizeM: ArithExpr, tileSizeK: ArithExpr,
                                workPerThreadN: ArithExpr, workPerThreadM: ArithExpr): Lambda = fun(
    ArrayType(ArrayType(Float, M), K), // Transposed
    ArrayType(ArrayType(Float, N), K),
    ArrayType(ArrayType(Float, N), M),
    Float,
    Float,
    (A, B, C, alpha, beta) => {
      // Undo the tiling
      Untile() o
        MapWrg(1)(fun( aRows =>
          MapWrg(0)(fun( bCols =>

            Map(Scatter(reorderStride(tileSizeM/workPerThreadM))) o Join() o
              Map(TransposeW() o Join() o Map(TransposeW())) o

              Join() o

              toGlobal(MapSeq(fun(x =>
                MapLcl(1)(fun(y =>
                  MapLcl(0)(fun( z =>
                    MapSeq(fun(a =>
                      MapSeq(fun(x =>
                        add(
                          mult(Get(x, 0), alpha),
                          mult(Get(x, 1),beta)
                        )
                      )) $ Zip(Get(a, 0), Get(a, 1))
                    )) $ Zip(Get(z, 0), Transpose() $ Get(z, 1))
                  )) $ Zip(Get(y, 0), Split(workPerThreadM) o ReorderStride(tileSizeM/workPerThreadM) o Transpose() $ Get(y, 1))
                )) $ Zip(x, Split(workPerThreadN) $ Get(bCols, 1))
              ))) o

              // Multiply all necessary combinations of tiles
              ReduceSeq(fun( (acc, pairOfTiles) =>

                fun(pairOfTiles =>

                  MapLcl(1)( fun(rowsA =>
                    MapLcl(0)( fun( colsB =>
                      Join() o ReduceSeq(fun((acc, rowElemPair) =>
                        MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1)))) o
                          fun(rowElemPair =>
                            Zip(
                              Join() o toPrivate(MapSeq(MapSeq(
                                fun(aArray => MapSeq(fun(b =>
                                  mult.apply(aArray, b)
                                )) $ Get(rowElemPair, 1))) o toPrivate(MapSeq(id))
                              )) o Split(1) $ Get(rowElemPair, 0),
                              acc
                            )
                          ) o fun(rowElemPair =>
                          Tuple(
                            Get(rowElemPair, 0),
                            toPrivate(MapSeq(id)) $ Get(rowElemPair, 1)
                          )) $ rowElemPair
                      ), Get(colsB, 1)
                      ) $ Zip(Transpose() $ Get(rowsA, 0), Transpose() $ Get(colsB, 0))

                    )) $ Zip(Split(workPerThreadM) o ReorderStride(tileSizeM/workPerThreadM) o Transpose() $ Get(pairOfTiles, 1), Get(rowsA, 1))
                  ))  $ Zip(Split(workPerThreadN) o Transpose() $ Get(pairOfTiles, 0), acc)

                ) o

                  // Copy tiles to local memory
                  Unzip() o toLocal(MapLcl(1)(fun(pair =>
                  Unzip() o MapLcl(0)(fun( pair =>
                    Tuple(id $ Get(pair, 0), id $ Get(pair, 1))
                  )) $ Zip(Get(pair, 0), Get(pair, 1))
                ))) $ Zip(Get(pairOfTiles, 0), Get(pairOfTiles, 1))
              )
                , MapLcl(1)(MapLcl(0)(MapSeq(MapSeq(id)))) $ Value(0.0f,
                  ArrayType(ArrayType(ArrayType(ArrayType(Float, workPerThreadM), workPerThreadN), tileSizeM/workPerThreadM), tileSizeN/workPerThreadN))
              ) $ Zip(Get(aRows, 0), Get(bCols, 0))

            // Tile the matrices
          )) $ Zip(Transpose() o Tile(tileSizeK, tileSizeN) $ B, Get(aRows, 1))
        )) $ Zip(Transpose() o Tile(tileSizeK, tileSizeM) $ A, Tile(tileSizeM, tileSizeN) $ C)
    })


  val clblas_kepler = {
    val tileSizeM = 8
    val tileSizeK = 4
    val tileSizeN = 8

    val multAndSumUp = UserFun("multAndSumUp", Array("acc", "l", "r"),
      "{ return acc + (l * r); }",
      Seq(VectorType(Float, tileSizeN), Float, VectorType(Float, tileSizeN)),
      VectorType(Float, tileSizeN))

    val mult = UserFun("mult", Array("x", "y"), "{ return x * y; }",
      Seq(VectorType(Float, tileSizeN), VectorType(Float, tileSizeN)), VectorType(Float, tileSizeN))

    fun(
      ArrayType(ArrayType(Float, M), K),
      ArrayType(ArrayType(Float, N), K),
      ArrayType(ArrayType(Float, N), M),
      VectorType(Float, tileSizeN),
      VectorType(Float, tileSizeN),
      (A, B, C, alpha, beta) => {
        // Undo the tiling
        Untile() o
          MapGlb(1)(fun( aRows =>
            MapGlb(0)(fun( bCols =>

              toGlobal(fun(x =>
                MapSeq(fun(y =>
                  MapSeq(fun(z =>
                    VectorizeUserFun(tileSizeN, add)(
                      toPrivate(mult)(z._0, alpha),
                      toPrivate(mult)(z._1, beta)
                    )
                  )) $ Zip(y._0, asVector(tileSizeN) $ y._1)
                )) $ Zip(x, bCols._1)
              )) o
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
                    Value(0.0f, ArrayType(ArrayType(VectorType(Float, tileSizeN), 1), tileSizeM))
                ) $ Zip(aRows._0, bCols._0)

            )) $ Zip(Transpose() o Tile(tileSizeK, tileSizeN) $ B, aRows._1)
            // Tile the matrices
          )) $ Zip(
          Transpose() o Tile(tileSizeK, tileSizeM) $ A,
          Tile(tileSizeN, tileSizeM) $ C
        )
      })
  }

  val clblast_kepler = {

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
      val mult2 = UserFun("mult2", Array("l", "r"), """|{ return l * r; }""".stripMargin,
        Seq(VectorType(Float, 2), VectorType(Float, 2)), VectorType(Float, 2))
      fun(
        ArrayType(ArrayType(Float, v_M_0), v_K_1),
        ArrayType(ArrayType(Float, v_N_2), v_K_1),
        ArrayType(ArrayType(Float, v_N_2), v_M_0),
        VectorType(Float, 2), VectorType(Float, 2),
        (p_0, p_1, C, alpha, beta) =>
          FunCall(Join(),
            FunCall(MapWrg(1)(fun((p_2) =>
              FunCall(TransposeW(),
                FunCall(Join(),
                  FunCall(MapWrg(0)(fun((p_3) =>
                    FunCall(TransposeW(),
                      FunCall(Join(),
                        FunCall(Map(fun((p_4) =>
                          Map(Join() o Scatter(ReorderWithStride(v__5/v__3 * 2)) o Split(2)) $
                            FunCall(TransposeW(),
                              FunCall(Join(),
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
                                          FunCall(VectorizeUserFun(2,add),
                                            toPrivate(mult2)(p_12._0, alpha),
                                            toPrivate(mult2)(p_12._1, beta)
                                          ))),
                                          Zip(FunCall(asVector(2), p_11._0), asVector(2) $ p_11._1))))),
                                      Zip(p_10._0, Transpose() $ p_10._1)))),
                                    Zip(p_9._0, Split(v__3) o Join() o Gather(ReorderWithStride(v__5/v__3* 2)) o Split(2) o Transpose() $ p_9._1)))),
                                  Zip(p_8, Split(v__4) $ p_3._1)
                                ))), p_7))),
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
                                            Join() o Gather(ReorderWithStride(v__5/v__3* 2)) o Split(2) $
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
                                                  FunCall(VectorizeUserFun(2,idfloat), p_31))),
                                                  FunCall(asVector(2), p_30))))),
                                              FunCall(Split(v__6),
                                                FunCall(Join(),
                                                  FunCall(Get(0), p_29)))))),
                                        FunCall(MapLcl(1)(fun((p_32) =>
                                          FunCall(asScalar(),
                                            FunCall(MapLcl(0)(fun((p_33) =>
                                              FunCall(VectorizeUserFun(2,idfloat), p_33))),
                                              FunCall(asVector(2), p_32))))),
                                          FunCall(Get(1), p_29)))), p_28))), p_14)))),
                                FunCall(MapLcl(1)(fun((p_34) =>
                                  FunCall(MapLcl(0)(fun((p_35) =>
                                    FunCall(MapSeq(fun((p_36) =>
                                      FunCall(MapSeq(fun((p_37) =>
                                        FunCall(idfloat, p_37))), p_36))), p_35))), p_34))), Value("0.0f", ArrayType(ArrayType(ArrayType(ArrayType(Float, v__3), v__4), v__7 * 1 /^ v__3), v__5 * 1 /^ v__4))),
                                FunCall(Zip(2), p_2._0, p_3._0))))))))),
                    Zip(FunCall(Transpose(),
                      FunCall(Map(fun((p_38) =>
                        FunCall(Transpose(), p_38))),
                        FunCall(Split(v__8),
                          FunCall(Map(fun((p_39) =>
                            FunCall(Split(v__7), p_39))), p_1)))),
                      p_2._1
                    )
                  ))))),
              Zip(
                FunCall(Transpose(),
                  FunCall(Map(fun((p_40) =>
                    FunCall(Transpose(), p_40))),
                    FunCall(Split(v__8),
                      FunCall(Map(fun((p_41) =>
                        FunCall(Split(v__5), p_41))), p_0)))),
                Tile(v__5, v__7) $ C
              ))
          ))
    }

    val param = 8

    factory(Seq[ArithExpr](M, K, N,param,8,64, 128, 128, 16))
  }

  val clblast_hawaii = {

    val v__3 = 4
    val v__4 = 8
    val vectorWidth = 4

    // TODO: the store isn't actually vectorised in the source
    val mult2 = UserFun("mult2", Array("x", "y"), "{ return x * y; }",
      Seq(VectorType(Float, vectorWidth), VectorType(Float, vectorWidth)), VectorType(Float, vectorWidth))
    fun(
      ArrayType(ArrayType(Float, M), K),
      ArrayType(ArrayType(Float, N), K),
      ArrayType(ArrayType(Float, N), M),
      VectorType(Float, vectorWidth),VectorType(Float, vectorWidth),
      (p_0, p_1, C, alpha, beta) =>
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
                          toGlobal(
                            MapSeq(fun(x =>
                              MapSeq(fun(z =>
                                asScalar() o MapSeq(fun(y =>
                                  VectorizeUserFun(4, add)(
                                    toPrivate(mult2)(y._0, alpha),
                                    toPrivate(mult2)(y._1, beta)
                                  )
                                )) $ Zip(asVector(4) $ z._0, asVector(4) $ z._1)
                              )) $ Zip(x, p_4._1)
                            ))
                          ) $
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
                                Value("0.0f", ArrayType(ArrayType(Float, v__3), v__4)),
                              FunCall(Zip(2),
                                FunCall(Transpose(), p_3._0),
                                FunCall(Transpose(), p_4._0)))))))),
                    Zip(FunCall(Split(v__3),
                      /*FunCall(Gather(ReorderWithStride(N / v__3)), */Transpose() $ p_1),
                    p_3._1))
                )))),
              Zip(FunCall(Split(v__4), Transpose() $ p_0), Tile(v__4, v__3) $ C))
          )))
  }

  val clblas_hawaii = {
    val tileSizeM = 4
    val tileSizeK = 4
    val tileSizeN = 8

    // TODO: Use mad instead of multAndSumUp?
    val multAndSumUp = UserFun("multAndSumUp", Array("acc", "l", "r"),
      "{ return acc + (l * r); }",
      Seq(VectorType(Float, tileSizeN), Float, VectorType(Float, tileSizeN)),
      VectorType(Float, tileSizeN))

    val mult = UserFun("mult", Array("x", "y"), "{ return x * y; }",
      Seq(VectorType(Float, tileSizeN), VectorType(Float, tileSizeN)), VectorType(Float, tileSizeN))

    fun(
      ArrayType(ArrayType(Float, M), K),
      ArrayType(ArrayType(Float, N), K),
      ArrayType(ArrayType(Float, N), M),
      VectorType(Float, tileSizeN),
      VectorType(Float, tileSizeN),
      (A, B, C, alpha, beta) => {
        // Undo the tiling
        Untile() o
          MapGlb(1)(fun( aRows =>
            MapGlb(0)(fun( bCols =>

              toGlobal(fun(z =>
                MapSeq(fun(x =>
                  MapSeq(fun(y =>
                    VectorizeUserFun(tileSizeN, add)(
                      toPrivate(mult)(y._0, alpha),
                      toPrivate(mult)(y._1, beta)
                    )
                  )) $ Zip(x._0, asVector(tileSizeN) $ x._1)
                )) $ Zip(z, bCols._1)
              )) o
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
                    Value(0.0f, ArrayType(ArrayType(VectorType(Float, tileSizeN), 1), tileSizeM))
                ) $ Zip(aRows._0, bCols._0)

            )) $ Zip(Transpose() o Tile(tileSizeK, tileSizeN) $ B, aRows._1)
            // Tile the matrices
          )) $ Zip(
          Transpose() o Tile(tileSizeK, tileSizeM) $ A,
          Tile(tileSizeM, tileSizeN) $ C
        )
      })
  }


  def apply() = new GEMM(
    Seq(("naive", Array[Lambda](naive)),
      ("tiledAndBlockedBInnermost", Array[Lambda](tiledAndBlockedBInnermost(16, 16, 8, 4, 4))),
      ("clblast_kepler", Array[Lambda](clblast_kepler)),
      ("clblast_hawaii", Array[Lambda](clblast_hawaii)),
      ("clblas_kepler", Array[Lambda](clblas_kepler)),
      ("clblas_hawaii", Array[Lambda](clblas_hawaii))

    ))


  def main(args: Array[String]): Unit = {
    GEMM().run(args)
  }
}
