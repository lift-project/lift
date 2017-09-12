package opencl.generator.matrixMultiplication

import benchmarks.MatrixMultiplication
import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor, Utils}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.Test

import scala.reflect.ClassTag

object Tiled extends TestWithExecutor

class Tiled {

  private val N = SizeVar("N")
  private val M = SizeVar("M")
  private val K = SizeVar("K")

  //noinspection MapFlatten
  @Test def tiledMultiplicationScala(): Unit = {
    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val transposedB = matrixB.transpose

    val tileSize = 4

    val tiledA = matrixA.grouped(tileSize).toArray.map(_.transpose.grouped(tileSize).toArray.map(_.transpose))
    val tiledB = transposedB.grouped(tileSize).toArray.map(_.transpose.grouped(tileSize).toArray.map(_.transpose))

    // Matrix-Multiplication and reduction over tiles fused
    val tiledC = tiledA.map(aRows => {
      tiledB.map(bCols => {
        (aRows, bCols).zipped.foldLeft(Array.ofDim[Float](tileSize, tileSize))((acc, tiles) => {
          val aTile = tiles._1
          val bTile = tiles._2

          val temp = aTile.map(aRow => bTile.map(bRow => (aRow, bRow).zipped.map(_ * _).sum))

          (temp, acc).zipped.map((x, y) => (x, y).zipped.map(_+_))
        })
      })
    })

    val matrixC = tiledC.flatMap(_.transpose.map(_.flatten)).flatten

    assertArrayEquals(gold, matrixC, 0.001f)

    // Matrix-Multiplication and reduction over tiles separate
    val tiledC2 = tiledA.map(aRows => {
      tiledB.map(bCols => {
        (aRows, bCols).zipped.map((aTile, bTile) => aTile.map(aRow => bTile.map(bRow => (aRow, bRow).zipped.map(_ * _).sum))).
          foldLeft(Array.ofDim[Float](tileSize, tileSize))((acc, tile) => {

            (tile, acc).zipped.map((x, y) => (x, y).zipped.map(_+_))
          })
      })
    })

    val matrixC2 = tiledC2.flatMap(_.transpose.map(_.flatten)).flatten

    assertArrayEquals(gold, matrixC2, 0.001f)

    // Matrix-Multiplication and reduction over tiles separate, reduction over last dimension, not first
    val tiledC3 = tiledA.map(aRows => {
      tiledB.map(bCols => {
        (aRows, bCols).zipped.map((aTile, bTile) => aTile.map(aRow => bTile.map(bRow => (aRow, bRow).zipped.map(_ * _).sum))).
          transpose.map(_.transpose.map(_.sum))
      })
    })

    val matrixC3 = tiledC3.flatMap(_.transpose.map(_.flatten)).flatten

    assertArrayEquals(gold, matrixC3, 0.001f)

    // Trying to reuse A
    val matrixC4 = matrixA.map(rowA => (rowA, matrixB).zipped.map((elemA, rowB) => rowB.map(_*elemA)))
      .transpose.fold(Array.ofDim[Float](16, 16))((a, b) =>  (a, b).zipped.map((a, b) => (a, b).zipped.map(_+_))).flatten

    assertArrayEquals(gold, matrixC4, 0.001f)


    val grouped = transposedB.grouped(4).toArray.map(_.transpose)

    val matrixC5 = matrixA.map(rowA => grouped.map(columnsB => (rowA, columnsB).zipped.
      foldLeft(Array.ofDim[Float](4))((acc, elemRowPair) => (elemRowPair._2.map(_*elemRowPair._1), acc).
        zipped.map(_+_)))).map(_.flatten)

    assertArrayEquals(gold, matrixC5.flatten, 0.001f)

    // Trying to reuse B
    val matrixC6 = matrixA.grouped(4).toArray.map(rowsA => transposedB.map(colB => (rowsA.transpose, colB).zipped.
      foldLeft(Array.ofDim[Float](4))((acc, rowElemPair) => (rowElemPair._1.map(_ * rowElemPair._2), acc).
        zipped.map(_ + _)))).map(_.transpose).flatten

    assertArrayEquals(gold, matrixC6.flatten, 0.001f)

    def reorder[T: ClassTag](array: Array[T], f: Int => Int): Array[T] = {
      val newArray = Array.ofDim[T](array.length)

      for (i <- array.indices) {
        newArray(i) = array(f(i))
      }

      newArray
    }

    val f: (Int) => Int = i => {
      val s = 4
      val n = matrixB.length / s
      (i / n) + s * (i % n)
    }

    // Trying to reuse both

    // B as innermost
    val matrixC7 = matrixA.grouped(4).toArray.map(rowsA =>
      reorder(matrixB.transpose, f).grouped(4).toArray.map(colsB => (rowsA.transpose, colsB.transpose).zipped.
        foldLeft(Array.ofDim[Float](4, 4))((acc, rowElemPair) => (rowElemPair._1.map(elem => rowElemPair._2.map(_ * elem)), acc).
          zipped.map((a, b) => (a, b).zipped.map(_+_))))
    ).map(_.map(_.transpose).flatten.transpose).flatten.map(reorder(_, f))

    assertArrayEquals(gold, matrixC7.flatten, 0.001f)

    // A as innermost
    val matrixC8 = matrixA.grouped(4).toArray.map(rowsA =>
      reorder(matrixB.transpose, f).grouped(4).toArray.map(colsB => (rowsA.transpose, colsB.transpose).zipped.
        foldLeft(Array.ofDim[Float](4, 4))((acc, rowElemPair) => (rowElemPair._2.map(elem => rowElemPair._1.map(_ * elem)), acc).
          zipped.map((a, b) => (a, b).zipped.map(_+_))))
    ).map(_.flatten.transpose).flatten.map(reorder(_, f))

    assertArrayEquals(gold, matrixC8.flatten, 0.001f)
  }

  @Test def tiledMatrixMultiply(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        // Undo the tiling
        Untile2D() o
        MapWrg(0)(fun( aRows =>
          MapWrg(1)(fun( bCols =>

            // Reduce the partial results (matrices), so that the reduce is innermost
             MapLcl(0)(Join() o MapLcl(1)(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join()) o Transpose()) o Transpose() o

            // Multiply all necessary combinations of tiles
            MapSeq(fun( tiles =>
               MapLcl(0)( fun(aTile =>
                MapLcl(1)( fun( bTile =>
                  toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(aTile, bTile)
                )) $ Get(tiles, 1)
              )) $ Get(tiles, 0)
            )) $ Zip(aRows, bCols)

          // Tile the matrices
          )) o Tile(tileSize) $ B
        )) o Tile(tileSize) $ A
      })

    val (output, _) = Execute(4, 4, mSize, nSize, (false, false))[Array[Float]](f, matrixA, matrixB.transpose)
    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def tiledMatrixMultiply2(): Unit = {
    // Basic tiled matrix multiply without local memory
    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        // Undo the tiling
        Untile2D() o
          MapWrg(0)(fun( aRows =>
            MapWrg(1)(fun( bCols =>

              toGlobal(MapLcl(1)(MapLcl(0)(id))) o
                Join() o

                // Multiply all necessary combinations of tiles
                ReduceSeq(fun( (acc, pairOfTiles) =>

                  fun(pairOfTiles =>
                     fun(partial => MapLcl(1)(fun(pairOfRows => MapLcl(0)(add) $ Zip(Get(pairOfRows, 0), Get(pairOfRows, 1)))) $ Zip(acc, partial) ) o
                      Map(Join()) o
                      MapLcl(1)( fun(rowA =>
                        MapLcl(0)( fun( colB =>
                          toGlobal(MapSeq(id) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)) $ Zip(rowA, colB)
                        )) $ Get(pairOfTiles, 1)
                      )) $ Get(pairOfTiles, 0)
                  ) $ pairOfTiles
                )
                  , toGlobal(MapLcl(1)(MapLcl(0)(id))) $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize))
                ) $ Zip(aRows, bCols)

              // Tile the matrices
            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output, _) = Execute(mSize, nSize)[Array[Float]](f, matrixA, matrixB.transpose)
    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def tiledMatrixMultiplyLocalMemory(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        // Undo the tiling
        Untile2D() o
          MapWrg(0)(fun( aRows =>
            MapWrg(1)(fun( bCols =>

              Map(Join()) o
              // Reduce the partial results (matrices), so that the reduce is innermost
               toGlobal(MapLcl(0)(MapLcl(1)(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)))) o
              Map(Map(Join()) o Transpose()) o Transpose() o

                // Multiply all necessary combinations of tiles
                toLocal(MapSeq(fun( tiles =>
                   MapLcl(0)( fun(aTile =>
                    MapLcl(1)( fun( bTile =>
                      toLocal(MapSeq(id)) o ReduceSeq(fun((acc, y) =>
                        multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(aTile, bTile)
                    )) $ Get(tiles, 1)
                  )) $ Get(tiles, 0)
                ) o

                // Copy tiles to local memory
                fun(tiles =>
                  Tuple(
                     toLocal(MapLcl(0)(MapLcl(1)(id))) $ Get(tiles, 0),
                     toLocal(MapLcl(0)(MapLcl(1)(id))) $ Get(tiles, 1)
                  ))
                )) $ Zip(aRows, bCols)

              // Tile the matrices
            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output, _) = Execute(4, 4, mSize, nSize, (true, true))[Array[Float]](f, matrixA, matrixB.transpose)
    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def tiledMatrixMultiplyLocalMemory2(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        // Undo the tiling
        Untile2D() o
          MapWrg(0)(fun( aRows =>
            MapWrg(1)(fun( bCols =>

              toGlobal(MapLcl(1)(MapLcl(0)(id))) o
                Join() o

                // Multiply all necessary combinations of tiles
                ReduceSeq(fun( (acc, pairOfTiles) =>

                  fun(pairOfTiles =>
                     fun(partial => MapLcl(1)(fun(pairOfRows => MapLcl(0)(add) $ Zip(Get(pairOfRows, 0), Get(pairOfRows, 1)))) $ Zip(acc, partial) ) o
                      Map(Join()) o
                      MapLcl(1)( fun(rowA =>
                        MapLcl(0)( fun( colB =>
                          toLocal(MapSeq(id) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)) $ Zip(rowA, colB)
                        )) $ Get(pairOfTiles, 1)
                      )) $ Get(pairOfTiles, 0)
                  ) o

                    // Copy tiles to local memory
                    fun(pairOfTiles =>
                      Tuple(
                         toLocal(MapLcl(1)(MapLcl(0)(id))) $ Get(pairOfTiles, 0),
                         toLocal(MapLcl(1)(MapLcl(0)(id))) $ Get(pairOfTiles, 1)
                      )) $ pairOfTiles
                )
                  , toLocal(MapLcl(1)(MapLcl(0)(id))) $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize))
                ) $ Zip(aRows, bCols)

              // Tile the matrices
            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output, _) = Execute(4, 4, mSize, nSize, (true, true))[Array[Float]](f, matrixA, matrixB.transpose)
    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def tiledMatrixMultiplyWithTranspose(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

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
      (A, B) => {
        // Undo the tiling
        Untile2D() o
          MapWrg(0)(fun( aRows =>
            MapWrg(1)(fun( bCols =>

              toGlobal(MapLcl(1)(MapLcl(0)(id))) o
                Join() o

                // Multiply all necessary combinations of tiles
                ReduceSeq(fun( (acc, pairOfTiles) =>

                  fun(pairOfTiles =>
                     fun(partial => MapLcl(1)(fun(pairOfRows => MapLcl(0)(add) $ Zip(Get(pairOfRows, 0), Get(pairOfRows, 1)))) $ Zip(acc, partial) ) o
                      Map(Join()) o
                      MapLcl(1)( fun(rowA =>
                        MapLcl(0)( fun( colB =>
                          toLocal(MapSeq(id) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)) $ Zip(rowA, colB)
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
                  , toLocal(MapLcl(1)(MapLcl(0)(id))) $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize))
                ) $ Zip(aRows, bCols)

            )) o Transpose() o Tile(tileSize) $ B
            // Tile the matrices
          )) o Tile(tileSize) $ A
      })

    val (output, _) = Execute(tileSize, tileSize, mSize, nSize, (true, true))[Array[Float]](f, matrixA, matrixB)

    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def tiledMatrixMultiplyWithTransposeAndPrivate(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val f = MatrixMultiplication.tiled(tileSize)

    val (output, _) = Execute(4, 4, mSize, nSize, (true, false))[Array[Float]](f, matrixA, matrixB)
    assertArrayEquals(gold, output, 0.0001f)
  }
}
