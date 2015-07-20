package exploration

import arithmetic.Var
import ir._
import ir.ast.UserFun._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir.ast.CompositePatterns._
import opencl.ir._
import opencl.ir.ast._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import opencl.ir.pattern._

object TestDerivingMatrixReuse {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }
}

class TestDerivingMatrixReuse {

  val mSize = 16
  val kSize = mSize
  val nSize = mSize

  val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
  val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
  val transposedMatrixB = matrixB.transpose

  val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

  val N = Var("N")
  val M = Var("M")
  val K = Var("K")

  val chunkSize = 4

  @Test
  def ruleTest(): Unit = {
    val size = 128
    val a = Array.fill(size)(util.Random.nextInt(5))
    val b = Array.fill(size)(util.Random.nextInt(5))

    // Split $ Zip( ... ) => Zip( Split $ ... )
    val gold = (a, b).zipped.map(_ * _)
    val test = (a, b).zipped.toArray.grouped(16).map(_.map(x => x._1 * x._2)).flatten.toArray
    val test2 = (a.grouped(16).toArray, b.grouped(16).toArray).zipped.map((x, y) => (x, y).zipped.map(_*_)).flatten

    assertArrayEquals(gold, test)
    assertArrayEquals(gold, test2)

    val A = Array.fill(size, size)(util.Random.nextInt(5))

    // Reduce => Reduce() o Reduce( ... $ Zip( ... ) )
    val gold2 = a.sum
    val test4 = a.grouped(16).toArray.reduce((x, y) => (x, y).zipped.map(_+_)).sum

    assertEquals(gold2, test4, 0.0f)

    // Reorder $ Zip( ... ) => Zip( Reorder $ ... )
    val goldReorderZip = (a, b).zipped.toArray.reverse
    val testReorderZip = (a.reverse, b.reverse).zipped.toArray

    assertArrayEquals(goldReorderZip.map(_._1), testReorderZip.map(_._1))
    assertArrayEquals(goldReorderZip.map(_._2), testReorderZip.map(_._2))

    // Map-Reduce interchange
    val goldSwapMapReduce = A.map(_.sum)
    val testSwapMapReduce = A.transpose.reduce((x, y) => (x, y).zipped.map(_+_))

    assertArrayEquals(goldSwapMapReduce, testSwapMapReduce)

    // Map-Map transpose, pulling zip out
    val goldMapMapPullZip = A.map(a => (a, b).zipped.map(_*_))
    val testMapMapPullZip = (A.transpose, b).zipped.map((a, bElem) => a.map(_ * bElem)).transpose

    assertArrayEquals(goldMapMapPullZip.flatten, testMapMapPullZip.flatten)

    // Map-Map transpose, pushing zip in
    val goldMapMapPushZip = (A, b).zipped.map((a, bElem) => a.map(_ * bElem))
    val testMapMapPushZip = A.transpose.map(a => (a, b).zipped.map(_ * _)).transpose

    assertArrayEquals(goldMapMapPushZip.flatten, testMapMapPushZip.flatten)

    // map(split) o transpose => transpose o map(transpose) o split
    val goldMapSplitTranspose = A.transpose.map(_.grouped(16).toArray)
    val testMapSplitTranspose = A.grouped(16).toArray.map(_.transpose).transpose

    assertArrayEquals(goldMapSplitTranspose.flatten.flatten, testMapSplitTranspose.flatten.flatten)
  }

  @Test
  def deriveReuseA(): Unit = {

    // Starting expression
    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          MapSeq(fun( bCol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val (output: Array[Float], _) = Execute(1, mSize)(f, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output, 0.0f)

    // split-join
    val f1 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            MapSeq(fun( bCol =>
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
            )) $ bCols
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output1: Array[Float], _) = Execute(1, mSize)(f1, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output1, 0.0f)

    // Map fission
    val f2 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            MapSeq(
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o
              MapSeq(fun(bCol => MapSeq(mult) $ Zip(aRow, bCol))) $ bCols
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output2: Array[Float], _) = Execute(1, mSize)(f2, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output2, 0.0f)

    // Map-Reduce interchange
    val f3 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) =>
                  MapSeq(add) $ Zip(acc, c)
              ), MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSize))) o Transpose()
              o MapSeq(fun(bCol => MapSeq(mult) $ Zip(aRow, bCol)
            )) $ bCols
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output3: Array[Float], _) = Execute(1, mSize)(f3, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output3, 0.0f)

    // Map-Map transpose, pulling the zip out
    val f4 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) =>
                MapSeq(add) $ Zip(acc, c)
              ), MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSize))) o Transpose()
              o Transpose() o MapSeq(fun(elemRowPair =>
              MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
              )) $ Get(elemRowPair, 1)
            )) $ Zip(aRow, Transpose() $ bCols)
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output4: Array[Float], _) = Execute(1, mSize)(f4, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output4, 0.0f)

    // Transpose o Transpose => id
    val f5 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) =>
                MapSeq(add) $ Zip(acc, c)
              ), MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSize)))
              o MapSeq(fun(elemRowPair =>
              MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
              )) $ Get(elemRowPair, 1)
            )) $ Zip(aRow, Transpose() $ bCols)
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output5: Array[Float], _) = Execute(1, mSize)(f5, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output5, 0.0f)

    // ReduceSeq o MapSeq fusion
    val f6 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) =>
                MapSeq(add) o fun(elemRowPair =>
                  Zip(toPrivate(MapSeq(fun(a => mult.apply(a, Get(elemRowPair, 0))))) $ Get(elemRowPair, 1), acc)
                ) $ c
              ), MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSize)))
              $ Zip(aRow, Transpose() $ bCols)
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output6: Array[Float], _) = Execute(1, mSize)(f6, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output6, 0.0f)
  }

  @Test
  def deriveReuseB(): Unit = {

    // Starting expression
    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          MapSeq(fun( bCol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val (output: Array[Float], _) = Execute(1, mSize)(f, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output, 0.0f)

    // split-join
    val f1 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          MapSeq(fun( aRow =>
            MapSeq(fun( bCol =>
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
            )) $ B
          )) $ aRows
        )) o Split(chunkSize) $ A
      })

    val (output1: Array[Float], _) = Execute(1, mSize)(f1, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output1, 0.0f)

    // Map-Map interchange
    val f2 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o MapSeq(fun( bCol =>
            MapSeq(fun( aRow =>
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
            )) $ aRows
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output2: Array[Float], _) = Execute(1, mSize)(f2, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output2, 0.0f)

    // Map fission
    val f3 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o MapSeq(fun( bCol =>
            Join() o MapSeq(fun( c => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) $ c)) o
              MapSeq(fun(aRow => MapSeq(mult) $ Zip(aRow, bCol))) $ aRows
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output3: Array[Float], _) = Execute(1, mSize)(f3, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output3, 0.0f)

    // Map-Reduce interchange
    val f4 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o Join() o MapSeq(fun( bCol =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) => MapSeq(add) $ Zip(acc, c)),
                MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSize))) o Transpose() o
              MapSeq(fun(aRow => MapSeq(mult) $ Zip(aRow, bCol))) $ aRows
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output4: Array[Float], _) = Execute(1, mSize)(f4, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output4, 0.0f)

    // Map-Map transpose, pulling the zip out
    val f5 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o Join() o MapSeq(fun( bCol =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) => MapSeq(add) $ Zip(acc, c)),
                MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSize))) o Transpose() o
              Transpose() o MapSeq(fun(rowElemPair => MapSeq(fun( aElem => mult.apply(aElem, Get(rowElemPair, 1)))) $ Get(rowElemPair, 0))) $ Zip(Transpose() $ aRows, bCol)
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output5: Array[Float], _) = Execute(1, mSize)(f5, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output5, 0.0f)

    // Transpose o Transpose => id
    val f6 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o Join() o MapSeq(fun( bCol =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) => MapSeq(add) $ Zip(acc, c)),
                MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSize)))
              o MapSeq(fun(rowElemPair =>
              MapSeq(fun( aElem => mult.apply(aElem, Get(rowElemPair, 1)))) $ Get(rowElemPair, 0)
            )) $ Zip(Transpose() $ aRows, bCol)
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output6: Array[Float], _) = Execute(1, mSize)(f6, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output6, 0.0f)

    // ReduceSeq o MapSeq fusion
    val f7 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o Join() o MapSeq(fun( bCol =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) => MapSeq(add) o fun(rowElemPair =>
                Zip(acc, toPrivate(MapSeq(fun( aElem => mult.apply(aElem, Get(rowElemPair, 1))))) $ Get(rowElemPair, 0))) $ c),
                MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSize))
              ) $ Zip(Transpose() $ aRows, bCol)
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output7: Array[Float], _) = Execute(1, mSize)(f7, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output7, 0.0f)
  }

  @Test
  def deriveReuseBothBInnermost(): Unit = {
    val chunkSizeN = 4
    val chunkSizeM = 4

    // Starting expression
    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          MapSeq(fun( bCol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val (output: Array[Float], _) = Execute(1, mSize)(f, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output, 0.0f)

    // Reorder both sides
    val f1 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Scatter(IndexFunction.reorderStride(chunkSizeM)) o MapSeq(fun( bCol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
          )) o ReorderStride(chunkSizeM) $ B
        )) $ A
      })

    val (output1: Array[Float], _) = Execute(1, mSize)(f1, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output1, 0.0f)

    // Map fission, pull reorder out
    val f2 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o
          MapGlb(fun( aRow =>
            MapSeq(fun( bCol =>
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
            )) o ReorderStride(chunkSizeM) $ B
          )) $ A
      })

    val (output2: Array[Float], _) = Execute(1, mSize)(f2, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output2, 0.0f)

    // Split-join
    val f3 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          MapGlb(fun( aRows =>
            MapSeq(fun( aRow =>
              MapSeq(fun( bCol =>
                toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
              )) o ReorderStride(chunkSizeM) $ B
            )) $ aRows
          )) o Split(chunkSizeN) $ A
      })

    val (output3: Array[Float], _) = Execute(1, mSize)(f3, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output3, 0.0f)

    // Map-Map interchange
    val f4 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          MapGlb(fun( aRows =>
            TransposeW() o MapSeq(fun( bCol =>
              MapSeq(fun( aRow =>
                toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
              )) $ aRows
            )) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output4: Array[Float], _) = Execute(1, mSize)(f4, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output4, 0.0f)

    // Split-join
    val f5 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          MapGlb(fun( aRows =>
            TransposeW() o Join() o
              MapSeq(fun( bCols =>
                MapSeq(fun( bCol =>
                  MapSeq(fun( aRow =>
                    toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ aRows
                )) $ bCols
              )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output5: Array[Float], _) = Execute(1, mSize)(f5, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output5, 0.0f)

    // Map-Map interchange
    val f6 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          MapGlb(fun( aRows =>
            TransposeW() o Join() o
              MapSeq(fun( bCols =>
                TransposeW() o MapSeq(fun( aRow =>
                  MapSeq(fun( bCol =>
                    toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ bCols
                )) $ aRows
              )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output6: Array[Float], _) = Execute(1, mSize)(f6, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output6, 0.0f)

    // Map fission, pull join and transposes out
    val f7 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(fun( aRow =>
                MapSeq(fun( bCol =>
                  toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
                )) $ bCols
              )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output7: Array[Float], _) = Execute(1, mSize)(f7, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output7, 0.0f)

    // Map fission, separate add and mult
    val f8 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(fun( aRow =>
                MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o
                  MapSeq(fun( bCol =>
                    MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ bCols
              )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output8: Array[Float], _) = Execute(1, mSize)(f8, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output8, 0.0f)

    // Map-Reduce interchange
    val f9 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(fun( aRow =>
                Join () o toGlobal(MapSeq(MapSeq(id))) o
                  ReduceSeq(fun((acc, c) =>
                    MapSeq(add) $ Zip(acc, c)
                  ), MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSizeM))) o Transpose()
                  o
                  MapSeq(fun( bCol =>
                    MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ bCols
              )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output9: Array[Float], _) = Execute(1, mSize)(f9, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output9, 0.0f)

    // Map-Map transpose, pulling the zip out
    val f10 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(fun( aRow =>
                Join () o toGlobal(MapSeq(MapSeq(id))) o
                  ReduceSeq(fun((acc, c) =>
                    MapSeq(add) $ Zip(acc, c)
                  ), MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSizeM))) o Transpose()
                  o Transpose() o MapSeq(fun(elemRowPair =>
                  MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
                  )) $ Get(elemRowPair, 1)
                )) $ Zip(aRow, Transpose() $ bCols)
              )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output10: Array[Float], _) = Execute(1, mSize)(f10, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output10, 0.0f)

    // Transpose() o Transpose() => id
    val f11 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(fun( aRow =>
                Join () o toGlobal(MapSeq(MapSeq(id))) o
                  ReduceSeq(fun((acc, c) =>
                    MapSeq(add) $ Zip(acc, c)
                  ), MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSizeM)))
                  o
                  MapSeq(fun(elemRowPair =>
                    MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
                    )) $ Get(elemRowPair, 1)
                  )) $ Zip(aRow, Transpose() $ bCols)
              )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output11: Array[Float], _) = Execute(1, mSize)(f11, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output11, 0.0f)

    // Map fission, separate reduce
    val f12 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(
                Join () o toGlobal(MapSeq(MapSeq(id))) o
                  ReduceSeq(fun((acc, c) =>
                    MapSeq(add) $ Zip(acc, c)
                  ), MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSizeM)))
              )
                o
                MapSeq(fun( aRow =>
                  MapSeq(fun(elemRowPair =>
                    MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
                    )) $ Get(elemRowPair, 1)
                  )) $ Zip(aRow, Transpose() $ bCols)
                )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output12: Array[Float], _) = Execute(1, mSize)(f12, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output12, 0.0f)

    // Map fission, between reduce and copy back
    val f13 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o MapSeq(
                ReduceSeq(fun((acc, c) =>
                  MapSeq(add) $ Zip(acc, c)
                ), MapSeq(id) $ Value(0.0f, ArrayType(Float, chunkSizeM)))
              )
                o
                MapSeq(fun( aRow =>
                  MapSeq(fun(elemRowPair =>
                    MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
                    )) $ Get(elemRowPair, 1)
                  )) $ Zip(aRow, Transpose() $ bCols)
                )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output13: Array[Float], _) = Execute(1, mSize)(f13, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output13, 0.0f)

    // Map-Reduce interchange
    val f14 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o
                ReduceSeq(fun((acc, c) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1)))) $ Zip(acc, c)
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayType(ArrayType(Float, chunkSizeM), chunkSizeN)))
                o Transpose()
                o
                MapSeq(fun( aRow =>
                  MapSeq(fun(elemRowPair =>
                    MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
                    )) $ Get(elemRowPair, 1)
                  )) $ Zip(aRow, Transpose() $ bCols)
                )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output14: Array[Float], _) = Execute(1, mSize)(f14, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output14, 0.0f)

    // Map-Map interchange, pulling the zip out
    val f15 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o
                ReduceSeq(fun((acc, c) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1)))) $ Zip(acc, c)
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayType(ArrayType(Float, chunkSizeM), chunkSizeN)))
                o Transpose()
                o Transpose() o
                MapSeq(fun( rowPair =>
                  MapSeq(fun(a =>
                    MapSeq(fun(bElem => mult.apply(bElem, a)
                    )) $ Get(rowPair, 1)
                  )) $ Get(rowPair, 0)
                )) $ Zip(Transpose() $ aRows, Transpose() $ bCols)
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output15: Array[Float], _) = Execute(1, mSize)(f15, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output15, 0.0f)

    // Transpose() o Transpose() => id
    val f16 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o
                ReduceSeq(fun((acc, c) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1)))) $ Zip(acc, c)
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayType(ArrayType(Float, chunkSizeM), chunkSizeN)))
                o
                MapSeq(fun( rowPair =>
                  MapSeq(fun(a =>
                    MapSeq(fun(bElem => mult.apply(bElem, a)
                    )) $ Get(rowPair, 1)
                  )) $ Get(rowPair, 0)
                )) $ Zip(Transpose() $ aRows, Transpose() $ bCols)
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output16: Array[Float], _) = Execute(1, mSize)(f16, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output16, 0.0f)

    /*
    MapSeq(fun( rowPair =>
      MapSeq(fun(a =>
        MapSeq(fun(bElem => mult.apply(bElem, a)
        )) $ Get(rowPair, 1)
      )) $ Get(rowPair, 0)
    )
    */

    // ReduceSeq-MapSeq fusion
    val f17 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o
                ReduceSeq(fun((acc, rowPair) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1))))
                    o fun(rowPair =>
                    Zip(acc, MapSeq(fun(a =>
                      MapSeq(fun(bElem => mult.apply(bElem, a)
                      )) $ Get(rowPair, 1)
                    )) $ Get(rowPair, 0))) $ rowPair
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayType(ArrayType(Float, chunkSizeM), chunkSizeN))
                ) $ Zip(Transpose() $ aRows, Transpose() $ bCols)
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output17: Array[Float], _) = Execute(1, mSize)(f17, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output17, 0.0f)

    // MapSeq(f) => toPrivate(MapSeq(f))
    val f18 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o
                ReduceSeq(fun((acc, rowPair) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1))))
                    o fun(rowPair =>
                    Zip(acc, toPrivate(MapSeq(fun(a =>
                      MapSeq(fun(bElem => mult.apply(bElem, a)
                      )) $ Get(rowPair, 1)
                    ))) $ Get(rowPair, 0))) $ rowPair
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayType(ArrayType(Float, chunkSizeM), chunkSizeN))
                ) $ Zip(Transpose() $ aRows, Transpose() $ bCols)
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output18: Array[Float], _) = Execute(1, mSize)(f18, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output18, 0.0f)

    // split-join + join o split => id
    val f19 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(IndexFunction.reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join()
                o
                ReduceSeq(fun((acc, rowPair) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1))))
                    o fun(rowPair =>
                    Zip(acc, toPrivate(MapSeq(fun(a =>
                      MapSeq(fun(bElem => mult.apply(bElem, a)
                      )) $ Get(rowPair, 1)
                    ))) $ Get(rowPair, 0))) $ rowPair
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayType(ArrayType(Float, chunkSizeM), chunkSizeN))
                ) $ Zip(Transpose() $ aRows, Transpose() $ bCols)
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output19: Array[Float], _) = Execute(1, mSize)(f19, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output19, 0.0f)
  }
}
