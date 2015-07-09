package exploration

import apart.arithmetic.Var
import ir.UserFunDef._
import ir._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestDerivingMatrixReuse {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestDerivingMatrixReuse {

  @Test
  def ruleTest(): Unit = {
    val size = 128
    val a = Array.fill(size)(util.Random.nextInt(5))
    val b = Array.fill(size)(util.Random.nextInt(5))

    val gold = (a, b).zipped.map(_ * _)

    val test = (a, b).zipped.toArray.grouped(16).map(_.map(x => x._1 * x._2)).flatten.toArray

    val test2 = (a.grouped(16).toArray, b.grouped(16).toArray).zipped.map((x, y) => (x, y).zipped.map(_*_)).flatten

    assertArrayEquals(gold, test)
    assertArrayEquals(gold, test2)

    val A = Array.fill(size, size)(util.Random.nextInt(5))

    val gold1 = A.map(_.sum)

    val test3 = A.grouped(16).toArray.flatMap(_.map(_.sum))

    assertArrayEquals(gold1, test3)

    val gold2 = a.sum

    val test4 = a.grouped(16).toArray.reduce((x, y) => (x, y).zipped.map(_+_)).sum

    assertEquals(gold2, test4, 0.0f)

    val goldReorderZip = (a, b).zipped.toArray.reverse
    val testReorderZip = (a.reverse, b.reverse).zipped.toArray

    assertArrayEquals(goldReorderZip.map(_._1), testReorderZip.map(_._1))
    assertArrayEquals(goldReorderZip.map(_._2), testReorderZip.map(_._2))

    val goldSwapMapReduce = A.map(_.sum)
    val testSwapMapReduce = A.transpose.reduce((x, y) => (x, y).zipped.map(_+_))

    assertArrayEquals(goldSwapMapReduce, testSwapMapReduce)
  }

  @Test
  def deriveReuseA(): Unit = {
    val mSize = 16
    val kSize = mSize
    val nSize = mSize

    val chunkSize = 4
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val transposedMatrixB = matrixB.transpose

    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

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
    val mSize = 16
    val kSize = mSize
    val nSize = mSize

    val chunkSize = 4
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val transposedMatrixB = matrixB.transpose

    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

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
}
