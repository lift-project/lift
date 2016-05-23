package benchmarks

import apart.arithmetic.{ArithExpr, Cst, Var}
import apart.arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.executor.Utils
import opencl.ir._
import opencl.ir.pattern._
import org.clapper.argot.ArgotConverters._

class GEMM (override val f: Seq[(String, Array[Lambda])])
  extends Benchmark("Matrix Multiplication", Seq(1024, 1024, 1024), f, 0.1f, Array(16, 16, 1)) {

  val tileX = parser.option[Int](List("x", "tileX"), "size",
    "Tile size in the M and N dimension")

  val tileY = parser.option[Int](List("y", "tileY"), "size",
    "Tile size in the K dimension")

  val registerBlockM = parser.option[Int](List("bm", "blockM"), "size",
   "Register blocking factor in M dimension")

  val registerBlockN = parser.option[Int](List("bn", "blockN"), "size",
    "Register blocking factor in N dimension")

  val vectorWidth = parser.option[Int](List("vw", "vectorWidth"), "width",
    "Vector width for loading values")

  override def runScala(inputs: Any*): Array[Float] = {
    var A = inputs(0).asInstanceOf[Array[Array[Float]]]
    val B = inputs(1).asInstanceOf[Array[Array[Float]]]
    val C = inputs(2).asInstanceOf[Array[Array[Float]]]
    val alpha = inputs(3).asInstanceOf[Float]
    val beta = inputs(4).asInstanceOf[Float]

    val variant = variantOpt.value.getOrElse(0)
    if (variant == 1)
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

  override protected def beforeBenchmark() = {
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


  def apply() = new GEMM(
    Seq(("naive", Array[Lambda](naive)),
      ("tiledAndBlockedBInnermost", Array[Lambda](tiledAndBlockedBInnermost(16, 16, 8, 4, 4)))
    ))


  def main(args: Array[String]): Unit = {
    GEMM().run(args)
  }
}
