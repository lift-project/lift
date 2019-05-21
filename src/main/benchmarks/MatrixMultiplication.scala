package benchmarks

import lift.arithmetic._
import ir._
import ir.ast._
import opencl.executor.Utils
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class MatrixMultiplication (override val f: Seq[(String, Array[Lambda])])
  extends DeprecatedBenchmark("Matrix Multiplication", Seq(1024, 1024, 1024), f, 0.1f, Array(16, 16, 1)) {

  case class MatMulConfig(tileX: Option[Long] = None,
                          tileY: Option[Long] = None,
                          registerBlockM: Option[Long] = None,
                          registerBlockN: Option[Long] = None,
                          vectorWidth: Option[Long] = None)

  var matMulCmdArgs = MatMulConfig()
  val matMulParser = new scopt.OptionParser[Unit]("MatrixMultiplication") {
    override val errorOnUnknownArgument = false

    opt[Long]('x', "tileX").text("Tile size in the M and N dimension").required()
      .foreach(arg => matMulCmdArgs = matMulCmdArgs.copy(tileX = Some(arg)))

    opt[Long]('y', "tileY").text("Tile size in the K dimension").required()
      .foreach(arg => matMulCmdArgs = matMulCmdArgs.copy(tileY = Some(arg)))

    opt[Long]("blockM").abbr("bm").text("Register blocking factor in M dimension").required()
      .foreach(arg => matMulCmdArgs = matMulCmdArgs.copy(registerBlockM = Some(arg)))

    opt[Long]("blockN").abbr("bn").text("Register blocking factor in N dimension").required()
      .foreach(arg => matMulCmdArgs = matMulCmdArgs.copy(registerBlockN = Some(arg)))

    opt[Long]("vectorWidth").abbr("vw").text("Vector width for loading values")
      .foreach(arg => matMulCmdArgs = matMulCmdArgs.copy(vectorWidth = Some(arg)))
  }

  override def runScala(inputs: Any*): Array[Float] = {
    var A = inputs(0).asInstanceOf[Array[Array[Float]]]
    val B = inputs(1).asInstanceOf[Array[Array[Float]]]

    val variant = cmdArgs.variant
    if (variant == 3 || variant == 4)
      A = A.transpose

    val res = Utils.matrixMatrixMultiply(A, B)

    res.flatten
  }



  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes()(0)
    val inputSizeM = inputSizes()(1)
    val inputSizeK = inputSizes()(2)

    val matrixA = Array.tabulate(inputSizeM, inputSizeK)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val matrixB = Array.tabulate(inputSizeK, inputSizeN)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)

    Seq(matrixA, matrixB)
  }

  override def globalSize: Array[Int] = {
    val globalSizes = Array(inputSizes()(0), inputSizes()(1), 1)
    cmdArgs.globalSize.copyToArray(globalSizes)
    globalSizes
  }

  override protected def beforeBenchmark(): Unit = {
    val temp = f(1)._2
    temp(0) = MatrixMultiplication.tiled(Cst(matMulCmdArgs.tileX.getOrElse(16)))
    val temp2 = f(2)._2
    temp2(0) = MatrixMultiplication.moreWorkPerThread(Cst(matMulCmdArgs.tileX.getOrElse(16)),
      Cst(matMulCmdArgs.registerBlockM.getOrElse(4)))
    val temp3 = f(3)._2
    temp3(0) = MatrixMultiplication.tiledAndBlockedBInnermost(Cst(matMulCmdArgs.tileX.getOrElse(16)),
      Cst(matMulCmdArgs.tileX.getOrElse(16)), Cst(matMulCmdArgs.tileY.getOrElse(8)), Cst(matMulCmdArgs.registerBlockN.getOrElse(4)),
      Cst(matMulCmdArgs.registerBlockM.getOrElse(4)))
    val temp4 = f(4)._2
    temp4(0) = MatrixMultiplication.vectorLoads(Cst(matMulCmdArgs.tileX.getOrElse(16)),
      Cst(matMulCmdArgs.tileX.getOrElse(16)), Cst(matMulCmdArgs.tileY.getOrElse(8)), Cst(matMulCmdArgs.registerBlockN.getOrElse(4)),
      Cst(matMulCmdArgs.registerBlockM.getOrElse(4)), Cst(matMulCmdArgs.vectorWidth.getOrElse(4)))
  }

  override protected def printParams(): Unit = {
    println("Tile size: " + matMulCmdArgs.tileX.getOrElse(16) + " " + matMulCmdArgs.tileY.getOrElse(8))
    println("Work per thread: " + matMulCmdArgs.registerBlockN.getOrElse(4) + " " + matMulCmdArgs.registerBlockM.getOrElse(4))
  }

  override protected def printResults(time: Double): Unit = {
    val dNumOps = 2.0 * inputSizes()(0).toDouble * inputSizes()(1).toDouble * inputSizes()(2).toDouble
    val gflops = 1.0e-6 * dNumOps/time
    println("THROUGHPUT: " + gflops + " GFlops/s")
  }
}

object MatrixMultiplication {
  val N = SizeVar("N")
  val M = SizeVar("M")
  val K = SizeVar("K")

  val naive = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
    (A, B) => {
      MapGlb(1)(fun( Arow =>
        MapGlb(0)(fun( Bcol =>
          toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
        )) o Transpose() $ B
      )) $ A
    })

  def tiled(tileSize: ArithExpr = 4) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
    (A, B) => {
      // Undo the tiling
      Untile2D() o
        MapWrg(1)(fun( aRows =>
          MapWrg(0)(fun( bCols =>

            toGlobal(MapLcl(1)(MapLcl(0)(id))) o
              Join() o

              // Multiply all necessary combinations of tiles
              ReduceSeq(fun( (acc, pairOfTiles) =>

                fun(pairOfTiles =>
                   fun(partial => MapLcl(1)(fun(pairOfRows => MapLcl(0)(add) $ Zip(Get(pairOfRows, 0), Get(pairOfRows, 1)))) $ Zip(acc, partial) ) o
                    Map(Join()) o
                    MapLcl(1)( fun(rowA =>
                      MapLcl(0)( fun( colB =>
                        ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(rowA, colB)
                      )) o Transpose() $ Get(pairOfTiles, 1)
                    )) $ Get(pairOfTiles, 0)
                ) o

                  // Copy tiles to local memory
                  fun(pairOfTiles =>
                    Tuple(
                       toLocal(MapLcl(1)(MapLcl(0)(id))) $ Get(pairOfTiles, 0),
                       toLocal(MapLcl(1)(MapLcl(0)(id))) $ Get(pairOfTiles, 1)
                    )) $ pairOfTiles
              )
                , MapLcl(1)(MapLcl(0)(id)) $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize))
              ) $ Zip(aRows, bCols)

          )) o Transpose() o Tile(tileSize) $ B
          // Tile the matrices
        )) o Tile(tileSize) $ A
    })

  def moreWorkPerThread(tileSize: ArithExpr = 8, workPerThread: ArithExpr = 4) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
    (A, B) => {
      // Undo the tiling
      Untile2D() o
        MapWrg(1)(fun( aRows =>
          MapWrg(0)(fun( bCols =>
            Join() o Map(TransposeW()) o
              toGlobal(MapLcl(1)(MapLcl(0)(MapSeq(id)))) o
              Join() o

              // Multiply all necessary combinations of tiles
              ReduceSeq(fun( (acc, pairOfTiles) =>

                fun(pairOfTiles =>
                   fun(partial =>
                    MapLcl(1)(fun(pairOfRows =>
                      MapLcl(0)(fun(x => MapSeq(add) $ Zip(Get(x, 0), Get(x, 1))
                      )) $ Zip(Get(pairOfRows, 0), Get(pairOfRows, 1))
                    )) $ Zip(acc, partial)
                  ) o

                    MapLcl(1)( fun(rowsA =>
                      MapLcl(0)( fun( colB =>
                        Join() o ReduceSeq(fun((acc, rowElemPair) =>
                          MapSeq(add) o fun(rowElemPair =>
                            Zip(
                              toPrivate(MapSeq(fun(a => mult.apply(a, Get(rowElemPair, 1)))
                              )) $ Get(rowElemPair, 0),
                              acc
                            )
                          ) $ rowElemPair
                        ), toPrivate(MapSeq(id)) $ Value("0.0f", ArrayTypeWSWC(Float, workPerThread))
                        ) $ Zip(Transpose() $ rowsA, colB)
                      )) o Transpose() $ Get(pairOfTiles, 1)
                    )) o Split(workPerThread) $ Get(pairOfTiles, 0)

                ) o

                  // Copy tiles to local memory
                  fun(pairOfTiles =>
                    Tuple(
                      toLocal(MapLcl(1)(MapLcl(0)(id))) $ Get(pairOfTiles, 0),
                      toLocal(MapLcl(1)(MapLcl(0)(id))) $ Get(pairOfTiles, 1)
                    )) $ pairOfTiles
              )
                , MapLcl(1)(MapLcl(0)(MapSeq(id))) $ Value(0.0f,
                  ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThread), tileSize), tileSize/workPerThread))
              ) $ Zip(aRows, bCols)

          )) o Transpose() o Tile(tileSize) $ B
          // Tile the matrices
        )) o Tile(tileSize) $ A
    })

  // Currently the best for NVIDIA
  def tiledAndBlockedBInnermost(tileSizeN: ArithExpr, tileSizeM: ArithExpr, tileSizeK: ArithExpr,
                                  workPerThreadN: ArithExpr, workPerThreadM: ArithExpr): Lambda = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), // Transposed
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
    (A, B) => {
      // Undo the tiling
      Untile2D() o
        MapWrg(1)(fun( aRows =>
          MapWrg(0)(fun( bCols =>

            Map(Scatter(reorderStride(tileSizeM/workPerThreadM))) o Join() o
              Map(TransposeW() o Join() o Map(TransposeW())) o

              toGlobal(MapLcl(1)(MapLcl(0)(MapSeq(MapSeq(id))))) o

              Join() o

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
                  ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThreadM), workPerThreadN), tileSizeM/workPerThreadM), tileSizeN/workPerThreadN))
              ) $ Zip(aRows, bCols)

            // Tile the matrices
          )) o Transpose() o Tile(tileSizeK, tileSizeN) $ B
        )) o Transpose() o Tile(tileSizeK, tileSizeM) $ A
    })

  def tiledAndBlockedBInnermost_(tileSizeN: ArithExpr, tileSizeM: ArithExpr, tileSizeK: ArithExpr,
                                workPerThreadN: ArithExpr, workPerThreadM: ArithExpr): Lambda =
    \(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), // Transposed
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        A :>> Tile(tileSizeK, tileSizeM) :>> Transpose() :>>
        MapWrg(1)(\( aRows =>
          B :>> Tile(tileSizeK, tileSizeN) :>> Transpose() :>>
          MapWrg(0)(\( bCols =>
            Zip(aRows, bCols) :>>
            ReduceSeq(\( (acc, pairOfTiles) =>

                Zip(pairOfTiles._0, pairOfTiles._1) :>>
                // copy tiles to local memory
                toLocal(MapLcl(1)(\(pair =>
                  Zip(pair._0, pair._1) :>>
                  MapLcl(0)(\(pair =>
                    Tuple(pair._0 :>> id, pair._1 :>> id)
                  )) :>>
                  Unzip()
                ))) :>>
                Unzip() :>>
                \(pairOfTiles =>
                  Zip(pairOfTiles._0 :>> Transpose() :>> Split(workPerThreadN), acc) :>>
                  MapLcl(1)(\(rowsA =>
                    Zip(pairOfTiles._1 :>> Transpose() :>> ReorderStride(tileSizeM/workPerThreadM) :>> Split(workPerThreadM), rowsA._1) :>>
                    MapLcl(0)(\( colsB =>
                      Zip(rowsA._0 :>> Transpose(), colsB._0 :>> Transpose()) :>>
                      ReduceSeq(
                        \( (acc, rowElemPair) =>
                          rowElemPair :>>
                          \(rowElemPair =>
                            Tuple(rowElemPair._0, rowElemPair._1 :>> toPrivate(MapSeq(id)))
                          ) :>>
                          \(rowElemPair =>
                            Zip(
                              rowElemPair._0 :>>
                              Split(1) :>>
                              toPrivate(MapSeq(
                                toPrivate(MapSeq(id)) >>>
                                MapSeq(\(aArray =>
                                  rowElemPair._1 :>> MapSeq(fun(b => mult.apply(aArray, b)))
                                ))
                              )) :>>
                              Join()
                              ,
                              acc
                            )
                          )
                        )
                        ,
                        colsB._1
                      ) :>>
                      Join()
                    ))
                  ))
                 )
              )
              ,
              Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThreadM), workPerThreadN), tileSizeM/workPerThreadM), tileSizeN/workPerThreadN)) :>>
              MapLcl(1)(MapLcl(0)(MapSeq(MapSeq(id))))
            ) :>>
            Join() :>>
            toGlobal(MapLcl(1)(MapLcl(0)(MapSeq(MapSeq(id))))) :>>
            Map(Map(TransposeW() >>> Join() >>> TransposeW())) :>>
            Join() :>>
            Map(Scatter(reorderStride(tileSizeM/workPerThreadM)))
          ))
        )) :>> Untile2D()
      }
    )

  // Currently the best for AMD
  def vectorLoads(tileSizeN: ArithExpr, tileSizeM: ArithExpr, tileSizeK: ArithExpr,
                                workPerThreadN: ArithExpr, workPerThreadM: ArithExpr,
                   vectorWidth: ArithExpr): Lambda = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), // Transposed
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
    (A, B) => {
      // Undo the tiling
      Untile2D() o
        MapWrg(1)(fun( aRows =>
          MapWrg(0)(fun( bCols =>

            Map(Scatter(reorderStride(tileSizeM/workPerThreadM))) o Join() o
              Map(TransposeW() o Join() o Map(TransposeW())) o

              toGlobal(MapLcl(1)(MapLcl(0)(MapSeq(MapSeq(id))))) o

              Join() o

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
                  fun(pair => Tuple(asScalar() $ Get(pair, 0), asScalar() $ Get(pair, 1))) o
                    Unzip() o MapLcl(0)(fun( pair =>
                    Tuple(id.vectorize(vectorWidth) $ Get(pair, 0), id.vectorize(vectorWidth) $ Get(pair, 1))
                  )) $ Zip(asVector(vectorWidth) $ Get(pair, 0), asVector(vectorWidth) $ Get(pair, 1))
                ))) $ Zip(Get(pairOfTiles, 0), Get(pairOfTiles, 1))
              )
                , MapLcl(1)(MapLcl(0)(MapSeq(MapSeq(id)))) $ Value(0.0f,
                  ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThreadM), workPerThreadN), tileSizeM/workPerThreadM), tileSizeN/workPerThreadN))
              ) $ Zip(aRows, bCols)

            // Tile the matrices
          )) o Transpose() o Tile(tileSizeK, tileSizeN) $ B
        )) o Transpose() o Tile(tileSizeK, tileSizeM) $ A
    })

  def apply() = new MatrixMultiplication(
    Seq(("naive", Array[Lambda](naive)),
      ("tiled", Array[Lambda](tiled(16))),
      ("moreWorkPerThread", Array[Lambda](moreWorkPerThread(16, 4))),
      ("tiledAndBlockedBInnermost", Array[Lambda](tiledAndBlockedBInnermost(16, 16, 8, 4, 4))),
      ("vectorLoads", Array[Lambda](vectorLoads(16, 16, 8, 4, 4, 4)))
    ))


  def main(args: Array[String]): Unit = {
    MatrixMultiplication().run(args)
  }
}
