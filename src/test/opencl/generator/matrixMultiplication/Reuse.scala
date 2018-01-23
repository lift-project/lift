package opencl.generator.matrixMultiplication

import benchmarks.{GEMM, MatrixMultiplication}
import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor, Utils}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.Test

object Reuse extends TestWithExecutor

class Reuse {

  private val N = SizeVar("N")
  private val M = SizeVar("M")
  private val K = SizeVar("K")

  @Test def rectangularTiles(): Unit = {

    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val mSize = 512
    val kSize = 512
    val nSize = 512
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val tileSizeM = 16
    val tileSizeN = tileSizeM
    val tileSizeK = 8
    val workPerThread = 2

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), // Transposed
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        // Undo the tiling
        Untile2D() o
          MapWrg(0)(fun( aRows =>
            MapWrg(1)(fun( bCols =>
              Join() o Map(TransposeW()) o
                Join() o
                toGlobal(MapSeq(MapLcl(1)(MapLcl(0)(MapSeq(id))))) o

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
                        )) o Transpose()$ Get(pairOfTiles, 1)
                      )) o Split(workPerThread) o Transpose() $ Get(pairOfTiles, 0)

                  ) o

                    // Copy tiles to local memory
                    fun(pairOfTiles =>
                      Tuple(
                        toLocal(MapLcl(1)(MapLcl(0)(id)))
                          $ Get(pairOfTiles, 0),
                        toLocal(MapLcl(1)(MapLcl(0)(id)))
                          $ Get(pairOfTiles, 1)
                      )) $ pairOfTiles
                )
                  , MapLcl(1)(MapLcl(0)(MapSeq(id))) $ Value(0.0f,
                    ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThread), tileSizeM), tileSizeN/workPerThread))
                ) $ Zip(aRows, bCols)

            )) o Transpose() o Tile(tileSizeK, tileSizeN) $ B
            // Tile the matrices
          )) o Transpose() o Tile(tileSizeK, tileSizeM) $ A // Transposed
      })

    val (output, _) = Execute(tileSizeM, tileSizeN / workPerThread,
      mSize, nSize / workPerThread, (true, true))[Array[Float]](f, matrixA.transpose, matrixB)

    assertArrayEquals(gold, output, 0.0001f)
  }

    @Test def mmReuseBothBInnermost(): Unit = {
    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val workPerThreadN = 4
    val workPerThreadM = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) =>
        Map(Scatter(reorderStride(workPerThreadM))) o Join() o Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( rowsA =>
            MapSeq(fun( colsB =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o ReduceSeq(fun((acc, rowElemPair) =>
                MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1)))) o fun(rowElemPair => Zip(toPrivate(MapSeq(fun(a => MapSeq(fun(b => mult.apply(a, b))) $ Get(rowElemPair, 1)))) $ Get(rowElemPair, 0), acc)) $ rowElemPair
              ), toPrivate(MapSeq(MapSeq(id))) $ Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThreadM), workPerThreadN))) $ Zip(Transpose() $ rowsA, Transpose() $ colsB)
            )) o Split(workPerThreadM) o ReorderStride(workPerThreadM) o Transpose() $ B
          )) o Split(workPerThreadN) $ A
    )

    val (output, _) = Execute(mSize * nSize)[Array[Float]](f, matrixA, matrixB)

    assertArrayEquals(gold, output, 0.0001f)
  }


  @Test def mmTiledAndBlockedBInnermost(): Unit = {

    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val mSize = 512
    val kSize = 512
    val nSize = 512
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val tileSizeM = 16
    val tileSizeN = tileSizeM
    val tileSizeK = 8
    val workPerThreadN = 2
    val workPerThreadM = 4

    val f = MatrixMultiplication.tiledAndBlockedBInnermost(tileSizeN, tileSizeM, tileSizeK, workPerThreadN, workPerThreadM)

    val (output, _) = Execute(tileSizeM / workPerThreadM, tileSizeN / workPerThreadN,
      mSize / workPerThreadM, nSize / workPerThreadN, (true, true))[Array[Float]](f, matrixA.transpose, matrixB)

    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def gemmTiledAndBlockedBInnermost(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val mSize = 512
    val kSize = 512
    val nSize = 512
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val matrixC = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val alpha = 1.5f
    val beta = 2.5f

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB, matrixC, alpha, beta).flatten

    val tileSizeM = 16
    val tileSizeN = tileSizeM
    val tileSizeK = 8
    val workPerThreadN = 2
    val workPerThreadM = 4

    val f = GEMM.tiledAndBlockedBInnermost(tileSizeN, tileSizeM, tileSizeK, workPerThreadN, workPerThreadM)

    val (output, _) = Execute(tileSizeM / workPerThreadM, tileSizeN / workPerThreadN,
      mSize / workPerThreadM, nSize / workPerThreadN, (true, true))[Array[Float]](f, matrixA.transpose, matrixB,
          matrixC, alpha, beta)

    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def mmVectorLoads(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val mSize = 512
    val kSize = 512
    val nSize = 512
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val tileSizeM = 16
    val tileSizeN = tileSizeM
    val tileSizeK = 8
    val workPerThreadN = 4
    val workPerThreadM = 4

    val f = MatrixMultiplication.vectorLoads(tileSizeN, tileSizeM, tileSizeK,
      workPerThreadN, workPerThreadM, 8)

    val (output, _) = Execute(tileSizeM / workPerThreadM, tileSizeN / workPerThreadN,
      mSize / workPerThreadM, nSize / workPerThreadN, (true, true))[Array[Float]](f, matrixA.transpose, matrixB)

    assertArrayEquals(gold, output, 0.0001f)
  }

    @Test def mmTiledAndBlockedAInnermost(): Unit = {
      assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val tileSizeM = 8
    val tileSizeN = tileSizeM
    val tileSizeK = 4
    val workPerThreadN = 2
    val workPerThreadM = 2

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), // Transposed
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        // Undo the tiling
        Untile2D() o
          MapWrg(0)(fun( aRows =>
            MapWrg(1)(fun( bCols =>

              Map(Scatter(reorderStride(tileSizeM/workPerThreadM))) o Join() o
                Map(TransposeW() o Join()) o

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
                                    )) $ Get(rowElemPair, 0))) o toPrivate(MapSeq(id))
                                  )) o Split(1) $ Get(rowElemPair, 1),
                                  acc
                                )
                              ) o fun(rowElemPair =>
                              Tuple(
                                toPrivate(MapSeq(id)) $ Get(rowElemPair, 0),
                                Get(rowElemPair, 1)
                              )) $ rowElemPair
                          ), Get(colsB, 1)
                          ) $ Zip(Transpose() $ Get(rowsA, 0), Transpose() $ Get(colsB, 0))

                        )) $ Zip(Split(workPerThreadM) o ReorderStride(tileSizeM/workPerThreadM) o Transpose() $ Get(pairOfTiles, 1), Get(rowsA, 1))
                      ))  $ Zip(Split(workPerThreadN) o Transpose() $ Get(pairOfTiles, 0), acc)

                  ) o

                    // Copy tiles to local memory
                    fun(pairOfTiles =>
                      Tuple(
                        toLocal(MapLcl(1)(MapLcl(0)(id)))
                          $ Get(pairOfTiles, 0),
                        toLocal(MapLcl(1)(MapLcl(0)(id)))
                          $ Get(pairOfTiles, 1)
                      )) $ pairOfTiles
                )
                  , MapLcl(1)(MapLcl(0)(MapSeq(MapSeq(id)))) $ Value(0.0f,
                    ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThreadM), workPerThreadN), tileSizeM/workPerThreadM), tileSizeN/workPerThreadN))
                ) $ Zip(aRows, bCols)

              // Tile the matrices
            )) o Transpose() o Tile(tileSizeK, tileSizeN) $ B
          )) o Transpose() o Tile(tileSizeK, tileSizeM) $ A
      })

    val (output, _) = Execute(tileSizeM / workPerThreadM, tileSizeN / workPerThreadN,
      mSize / workPerThreadM, nSize / workPerThreadN, (true, true))[Array[Float]](f, matrixA.transpose, matrixB)

    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def mmReuseB(): Unit = {
    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) =>
        Join() o Map(TransposeW()) o
          MapGlb(fun( rowsA =>
            MapSeq(fun( colB =>
              toGlobal(MapSeq(id)) o Join() o ReduceSeq(fun((acc, rowElemPair) =>
                MapSeq(add) o fun(rowElemPair => Zip(toPrivate(MapSeq(fun(a => mult.apply(a, Get(rowElemPair, 1))))) $ Get(rowElemPair, 0), acc)) $ rowElemPair
              ), toPrivate(MapSeq(id)) $ Value("0.0f", ArrayTypeWSWC(Float, tileSize))) $ Zip(Transpose() $ rowsA, colB)
            )) o Transpose() $ B
          )) o Split(tileSize) $ A
    )

    val (output, _) = Execute(mSize * nSize)[Array[Float]](f, matrixA, matrixB)
    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def mmReuseA(): Unit = {
    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) =>
        Map(Join()) o
          MapGlb(fun(rowA => MapSeq( fun(colsB =>
            toGlobal(MapSeq(id)) o Join() o ReduceSeq(fun((acc, elemRowPair) =>
              MapSeq(add) o fun(elemRowPair =>
                Zip(toPrivate(MapSeq(fun(a => mult.apply(a, Get(elemRowPair, 0))))) $ Get(elemRowPair, 1), acc)
              ) $ elemRowPair
            ), toPrivate(MapSeq(id)) $ Value("0.0f", ArrayTypeWSWC(Float, tileSize))) $ Zip(rowA, colsB)
          )) o Map(Transpose()) o Split(tileSize) o Transpose() $ B
        )) $ A
    )

    val (output, _) = Execute(mSize * nSize)[Array[Float]](f, matrixA, matrixB)

    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def mmTiledReuseB(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 8
    val blockSize = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val f = MatrixMultiplication.moreWorkPerThread(tileSize, blockSize)

    val (output, _) = Execute(tileSize, tileSize / blockSize,
      mSize, nSize / blockSize, (true, true))[Array[Float]](f, matrixA, matrixB)

    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def mmTiledReuseA(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 4
    val blockSize = 2

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        // Undo the tiling
        Untile2D() o
          MapWrg(1)(fun( aRows =>
            MapWrg(0)(fun( bCols =>


              toGlobal(MapLcl(1)(
                Scatter(reorderStride(blockSize)) o MapLcl(0)(id)
              )) o
                Join() o

                // Multiply all necessary combinations of tiles
                ReduceSeq(fun( (acc, pairOfTiles) =>

                  fun(pairOfTiles =>
                     fun(partial =>
                      MapLcl(1)(fun(pairOfRows =>
                        MapLcl(0)(add) $ Zip(Get(pairOfRows, 0), Get(pairOfRows, 1))
                      )) $ Zip(acc, partial)
                    ) o

                      Map(Join()) o // This reorders elements and needs the scatter at the end
                      MapLcl(1)( fun(rowA =>
                        MapLcl(0)( fun( colsB =>
                          Join() o ReduceSeq(fun((acc, elemRowPair) =>
                            MapSeq(add) o fun(elemRowPair =>
                              Zip(
                                toPrivate(MapSeq(fun(a => mult.apply(a, Get(elemRowPair, 0))))) $ Get(elemRowPair, 1),
                                acc
                              )) $ elemRowPair
                            ), toPrivate(MapSeq(id)) $ Value("0.0f", ArrayTypeWSWC(Float, blockSize))
                          ) $ Zip(rowA, colsB)

                          )) o Map(Transpose()) o Split(blockSize) o Transpose() $ Get(pairOfTiles, 1)
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

    val (output, _) = Execute(tileSize/blockSize, tileSize, mSize/blockSize, nSize, (true, false))[Array[Float]](f, matrixA, matrixB)
    assertArrayEquals(gold, output, 0.0001f)
  }
}
