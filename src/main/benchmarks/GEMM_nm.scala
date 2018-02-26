package benchmarks

import ir.ast.{FunCall, Get, Join, Lambda2, Lambda5, Map, Split, Transpose, TransposeW, UserFun, Value, Zip, asVector, fun}
import ir.{ArrayType, ArrayTypeWSWC}
import lift.arithmetic.{ArithExpr, SizeVar}
import opencl.executor.{Compile, Execute, Executor}
import opencl.ir._
import opencl.ir.ast.dot
import opencl.ir.pattern._


object GEMM_nm {
  val multAndSumUp = UserFun("multAndSumUp", Array("acc", "l", "r"),
    "{ return acc + (l * r); }",
    Seq(Float, Float, Float), Float)

  val N = SizeVar("N")
  val M = SizeVar("M")
  val K = SizeVar("K")
  
  val naiveNN: Lambda2 = fun(
    ArrayType(ArrayType(Float, K), M),
    ArrayType(ArrayType(Float, N), K),
    (A, B) => {
        MapGlb(0)(fun(aRow =>
        MapGlb(1)(fun(bCol =>
          toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) =>
            multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
          ), 0.0f) $ Zip(aRow, bCol)
        )) o Transpose() $ B
      )) $ A
    })

  val naiveNT: Lambda2 = fun(
    ArrayType(ArrayType(Float, K), M),
    ArrayType(ArrayType(Float, K), N),
    (A, B) => {
      MapGlb(0)(fun(aRow =>
        MapGlb(1)(fun(bCol =>
          toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) =>
            multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
          ), 0.0f) $ Zip(aRow, bCol)
        )) $ B
      )) $ A
    })

  val naiveTT: Lambda2 = fun(
    ArrayType(ArrayType(Float, M), K),
    ArrayType(ArrayType(Float, K), N),
    (A, B) => {
      TransposeW() o MapGlb(0)(fun(aRow =>
        MapGlb(1)(fun(bCol =>
          toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) =>
            multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
          ), 0.0f) $ Zip(aRow, bCol)
        )) $ B
      )) o Transpose() $ A
    })

  val naiveTTtiledB: Lambda2 = fun(
    ArrayType(ArrayType(Float, M), K),
    ArrayType(ArrayType(Float, K), N),
    (A, B) => {
      TransposeW() o MapGlb(0)(fun(aRow =>
        MapGlb(1)(fun(bColPair =>
          MapSeq(fun(aCol =>
            toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) =>
              multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
            ), 0.0f) $ Zip(aRow, aCol))) $ bColPair
        )) o Split(2) $ B
      )) o Transpose() $ A
    })

  val naiveTTtiledAB: Lambda2 = fun(
    ArrayType(ArrayType(Float, M), K),
    ArrayType(ArrayType(Float, K), N),
    (A, B) => {
      TransposeW() o MapGlb(0)(fun(aRowPair =>
        MapGlb(1)(fun(bColPair =>
          MapSeq(fun(aRow =>
            MapSeq(fun(aCol =>
              toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) =>
                multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
              ), 0.0f) $ Zip(aRow, aCol)
            )) o toPrivate(MapSeq(MapSeq(idF4))) $ bColPair
          )) o toPrivate(MapSeq(MapSeq(idF4))) $ aRowPair
        )) o Split(2) $ B
      )) o Split(2) o Transpose() $ A
    })

  val naiveTTVectorised: Lambda2 = fun(
    ArrayType(ArrayType(Float, M), K),
    ArrayType(ArrayType(Float, K), N),
    (A, B) => {
      TransposeW() o MapGlb(0)(fun(aRow =>
        MapGlb(1)(fun(bCol =>
          toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) =>
            add(acc, dot(Get(y, 0), Get(y, 1)))
          ), 0.0f) $ Zip(aRow, bCol)
        )) o Map(asVector(4)) $ B
      )) o Map(asVector(4)) o Transpose() $ A
    })

  val TTtiledVectorised: Lambda2 = fun(
    ArrayType(ArrayType(Float, M), K),
    ArrayType(ArrayType(Float, K), N),
    (A, B) => {
      val tileSizeA = 1
      val tileSizeB = 8
        TransposeW() o
        MapGlb(0)(fun(aRowTile =>
          MapGlb(1)(fun(bColTile =>
            toGlobal(MapSeq(MapSeq(MapSeq(id)))) o
              ReduceSeq(fun((acc, rowColVectors) =>
                MapSeq(fun((accRow, aRowVector) =>
                  MapSeq(fun((accCol, bColVector) =>
                    add(accCol, dot(aRowVector, bColVector))
                  )) $ Zip(accRow, toPrivate(MapSeq(idF4)) $ Get(rowColVectors, 1))
                )) $ Zip(acc, toPrivate(MapSeq(idF4)) $ Get(rowColVectors, 0))
              ), toPrivate(MapSeq(MapSeq(id))) $
                Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSizeB), tileSizeA))
              ) $ Zip(aRowTile, bColTile)
          )) o Map(Transpose()) o Split(tileSizeB) o Map(asVector(4)) $ B
        )) o Map(Transpose()) o Split(tileSizeA) o Map(asVector(4)) o
          Transpose() $ A
    })


  val NTtiledVectorised: Lambda2 = fun(
    ArrayType(ArrayType(Float, K), M),
    ArrayType(ArrayType(Float, K), N),
    (A, B) => {
      val tileSizeA = 1
      val tileSizeB = 8

        MapGlb(0)(fun(aRowTile =>
          MapGlb(1)(fun(bColTile =>
            toGlobal(MapSeq(MapSeq(MapSeq(id)))) o
              ReduceSeq(fun((acc, rowColVectors) =>
                MapSeq(fun((accRow, aRowVector) =>
                  MapSeq(fun((accCol, bColVector) =>
                    add(accCol, dot(aRowVector, bColVector))
                  )) $ Zip(accRow, toPrivate(MapSeq(idF4)) $ Get(rowColVectors, 1))
                )) $ Zip(acc, toPrivate(MapSeq(idF4)) $ Get(rowColVectors, 0))
              ), toPrivate(MapSeq(MapSeq(id))) $
                Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSizeB), tileSizeA))
              ) $ Zip(aRowTile, bColTile)
          )) o Map(Transpose()) o Split(tileSizeB) o Map(asVector(4)) $ B
        )) o Map(Transpose()) o Split(tileSizeA) o Map(asVector(4)) $ A
    })

  val maliGEMM: Lambda5 = {
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
          ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_1), v_K_0),
          ArrayTypeWSWC(ArrayTypeWSWC(Float, v_K_0), v_N_2),
          ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_M_1),
          Float, Float,
          (p_0, p_1, C, alpha, beta) =>
            FunCall(TransposeW() o Join(),
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
                Zip(FunCall(Split(v__4), Transpose() $ p_0), Split(v__4) $ C))))
      }

//    val mSize = 16
//    val kSize = 16
//    val nSize = 16
//    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
//    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
//    val matrixC = Array.tabulate(nSize, mSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
//
//    val alpha = 2.5f
//    val beta = 1.5f

//    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB, matrixC, alpha, beta).flatten

    maliFactory(Seq[ArithExpr](K, M, N, 2, 2, 4))

//    val (output, _) =
//      Execute(2, 2, mSize/2, nSize/2, (true, true))[Array[Float]](f, matrixA, matrixB.transpose, matrixC, alpha, beta)
//
//    assertArrayEquals(gold, output, 0.0001f)
  }

  def main(args: Array[String]): Unit = {
    println(Compile(NTtiledVectorised))

//    Executor.loadLibrary()
//    Executor.init(/*avus*/1, 1)
//    println(Compile(NTtiledVectorised))
//    Execute(1,1,1,1,1,1, (true, true))[Array[Float]](NTtiledVectorised, 0, 0)

//    Executor.shutdown()
  }
}