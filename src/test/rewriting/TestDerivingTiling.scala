package rewriting

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume._
import org.junit.{AfterClass, BeforeClass, Test}

object TestDerivingTiling {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    Executor.shutdown()
  }
}

class TestDerivingTiling {

  @Test
  def mapsOnly(): Unit = {
    val nSize = 16
    val mSize = 16
    val tileSizeM = 4
    val tileSizeN = 2

    val matrix = Array.fill(nSize, mSize)(util.Random.nextInt(nSize*mSize).toFloat)
    val gold = matrix.flatMap(_.map(_ + 1))

    val N = SizeVar("N")
    val M = SizeVar("M")

    // Starting expression
    def f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => MapGlb(MapSeq(plusOne)) $ input
    )

    val (output, _) = Execute(1, nSize)[Array[Float]](f, matrix)
    assertArrayEquals(gold, output, 0.0f)

    // split-join
    def f1 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Join() o MapGlb(MapSeq(MapSeq(plusOne))) o Split(tileSizeN) $ input
    )

    val (output1, _) = Execute(1, nSize)[Array[Float]](f1, matrix)
    assertArrayEquals(gold, output1, 0.0f)

    // Transpose both sides
    def f2 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Join() o MapGlb(TransposeW() o MapSeq(MapSeq(plusOne)) o Transpose()) o Split(tileSizeN) $ input
    )

    val (output2, _) = Execute(1, nSize)[Array[Float]](f2, matrix)
    assertArrayEquals(gold, output2, 0.0f)

    // split-join
    def f3 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Join() o MapGlb(TransposeW() o Join() o MapSeq(MapSeq(MapSeq(plusOne))) o Split(tileSizeM) o Transpose()) o Split(tileSizeN) $ input
    )

    val (output3, _) = Execute(1, nSize)[Array[Float]](f3, matrix)
    assertArrayEquals(gold, output3, 0.0f)

    // Transpose both sides
    def f4 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Join() o MapGlb(TransposeW() o Join() o MapSeq(TransposeW() o MapSeq(MapSeq(plusOne)) o Transpose()) o Split(tileSizeM) o Transpose()) o Split(tileSizeN) $ input
    )

    val (output4, _) = Execute(1, nSize)[Array[Float]](f4, matrix)
    assertArrayEquals(gold, output4, 0.0f)

    // Map fission, pull out splits, joins and transposes
    def f5 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Join() o Map(TransposeW() o Join() o Map(TransposeW())) o
        MapGlb(MapSeq(MapSeq(MapSeq(plusOne))))
        o Map(Map(Transpose()) o Split(tileSizeM) o Transpose()) o Split(tileSizeN) $ input
    )

    val (output5, _) = Execute(1, nSize)[Array[Float]](f5, matrix)
    assertArrayEquals(gold, output5, 0.0f)


    // Replace with predefined tile and untile
    def f6 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Untile2D() o
        MapGlb(MapSeq(MapSeq(MapSeq(plusOne))))
        o Tile(tileSizeN, tileSizeM) $ input
    )

    val (output6, _) = Execute(1, nSize)[Array[Float]](f6, matrix)
    assertArrayEquals(gold, output6, 0.0f)
  }

  @Test
  def mmSquareTiles(): Unit = {

    assumeFalse("Disabled on AMD GPUs. See issue #64.", Utils.isAmdGpu)

    val nSize = 16
    val mSize = 16
    val kSize = 16
    val tileSize = 4

    val matrixA = Array.fill(nSize, mSize)(util.Random.nextInt(nSize*mSize).toFloat)
    val matrixB = Array.fill(mSize, kSize)(util.Random.nextInt(kSize*mSize).toFloat)
    val transposedMatrixB = matrixB.transpose
    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    // Starting expression
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          MapSeq(fun( bCol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val (output, _) = Execute(1, mSize)[Array[Float]](f, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output, 0.0f)

    // split-join
    val f1 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          MapSeq(fun(aRow =>
            MapSeq(fun( bCol =>
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
            )) $ B
          )) $ aRows
        )) o Split(tileSize) $ A
      })

    val (output1, _) = Execute(1, mSize)[Array[Float]](f1, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output1, 0.0f)

    // Map-Map interchange
    val f2 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o MapSeq(fun(bCol =>
            MapSeq(fun( aRow =>
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
            )) $ aRows
          )) $ B
        )) o Split(tileSize) $ A
      })

    val (output2, _) = Execute(1, mSize)[Array[Float]](f2, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output2, 0.0f)

    // split-join
    val f3 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o Join() o MapSeq(fun(bCols =>
            MapSeq(fun( bCol =>
              MapSeq(fun( aRow =>
                toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
              )) $ aRows
            )) $ bCols
          )) o Split(tileSize) $ B
        )) o Split(tileSize) $ A
      })

    val (output3, _) = Execute(1, mSize)[Array[Float]](f3, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output3, 0.0f)

    // Map-Map interchange
    val f4 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o Join() o MapSeq(fun(bCols =>
            TransposeW() o MapSeq(fun( aRow =>
              MapSeq(fun( bCol =>
                toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
              )) $ bCols
            )) $ aRows
          )) o Split(tileSize) $ B
        )) o Split(tileSize) $ A
      })

    val (output4, _) = Execute(1, mSize)[Array[Float]](f4, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output4, 0.0f)

    // Map fission, pull out joins and transposes
    val f5 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(fun( aRow =>
                MapSeq(fun( bCol =>
                  toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
                )) $ bCols
              )) $ aRows
            )) o Split(tileSize) $ B
          )) o Split(tileSize) $ A
      })

    val (output5, _) = Execute(1, mSize)[Array[Float]](f5, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output5, 0.0f)

    // Replace with predefined untile
    val f6 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(fun( aRow =>
                MapSeq(fun( bCol =>
                  toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
                )) $ bCols
              )) $ aRows
            )) o Split(tileSize) $ B
          )) o Split(tileSize) $ A
      })

    val (output6, _) = Execute(1, mSize)[Array[Float]](f6, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output6, 0.0f)

    // Partial Reduce
    val f7 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(fun( aRow =>
                MapSeq(fun( bCol =>
                  toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o
                    Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                    ReduceSeq(add, 0.0f)) o Split(tileSize) o // partReduce
                    MapSeq(mult) $ Zip(aRow, bCol)
                )) $ bCols
              )) $ aRows
            )) o Split(tileSize) $ B
          )) o Split(tileSize) $ A
      })

    val (output7, _) = Execute(1, mSize)[Array[Float]](f7, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output7, 0.0f)

    // Map fission
    val f8 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(fun( aRow =>
                MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o
                  MapSeq(fun( bCol =>
                    Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)) o Split(tileSize) o // partReduce
                      MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ bCols
              )) $ aRows
            )) o Split(tileSize) $ B
          )) o Split(tileSize) $ A
      })

    val (output8, _) = Execute(1, mSize)[Array[Float]](f8, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output8, 0.0f)

    // Map-Reduce interchange
    val f9 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(fun( aRow =>
                toGlobal(MapSeq(MapSeq(id)) o ReduceSeq(fun((acc, p) => MapSeq(add) $ Zip(acc, p)),
                  MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, tileSize)))) o
                  Transpose() o
                  MapSeq(fun( bCol =>
                    Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)) o Split(tileSize) o // partReduce
                      MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ bCols
              )) $ aRows
            )) o Split(tileSize) $ B
          )) o Split(tileSize) $ A
      })

    val (output9, _) = Execute(1, mSize)[Array[Float]](f9, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output9, 0.0f)

    // Map fission
    val f10 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(MapSeq(id)) o ReduceSeq(fun((acc, p) => MapSeq(add) $ Zip(acc, p)),
                MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, tileSize))))) o
                MapSeq(fun( aRow =>
                  Transpose() o
                    MapSeq(fun( bCol =>
                      Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                        ReduceSeq(add, 0.0f)) o Split(tileSize) o // partReduce
                        MapSeq(mult) $ Zip(aRow, bCol)
                    )) $ bCols
                )) $ aRows
            )) o Split(tileSize) $ B
          )) o Split(tileSize) $ A
      })

    val (output10, _) = Execute(1, mSize)[Array[Float]](f10, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output10, 0.0f)

    // Map fission
    val f11 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(MapSeq(id)))) o MapSeq(ReduceSeq(fun((acc, p) => MapSeq(add) $ Zip(acc, p)),
                MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, tileSize)))) o
                MapSeq(fun( aRow =>
                  Transpose() o
                    MapSeq(fun( bCol =>
                      Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                        ReduceSeq(add, 0.0f)) o Split(tileSize) o // partReduce
                        MapSeq(mult) $ Zip(aRow, bCol)
                    )) $ bCols
                )) $ aRows
            )) o Split(tileSize) $ B
          )) o Split(tileSize) $ A
      })

    val (output11, _) = Execute(1, mSize)[Array[Float]](f11, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output11, 0.0f)

    // Map-Reduce interchange
    val f12 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o

                Transpose() o
                MapSeq(fun( aRow =>
                  Transpose() o
                    MapSeq(fun( bCol =>
                      Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                        ReduceSeq(add, 0.0f)) o Split(tileSize) o // partReduce
                        MapSeq(mult) $ Zip(aRow, bCol)
                    )) $ bCols
                )) $ aRows
            )) o Split(tileSize) $ B
          )) o Split(tileSize) $ A
      })

    val (output12, _) = Execute(1, mSize)[Array[Float]](f12, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output12, 0.0f)


    // Map fission
    val f13 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o

                Transpose() o
                MapSeq(fun( aRow =>
                  Transpose() o
                    MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)) o Split(tileSize)) o // partReduce)
                    MapSeq(fun( bCol =>
                      MapSeq(mult) $ Zip(aRow, bCol)
                    )) $ bCols
                )) $ aRows
            )) o Split(tileSize) $ B
          )) o Split(tileSize) $ A
      })

    val (output13, _) = Execute(1, mSize)[Array[Float]](f13, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output13, 0.0f)



        // Map-Map interchange, pulling the zip out
        val f14 = fun(
          ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
          ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
          (A, B) => {
            Untile2D() o
              MapGlb(fun( aRows =>
                MapSeq(fun(bCols =>
                  toGlobal(MapSeq(MapSeq(id))) o Join() o

                    ReduceSeq(fun((acc, p) =>
                      MapSeq(fun(part =>
                        MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                      )) $ Zip(acc, p)),
                      toGlobal(MapSeq(MapSeq(id)))
                        $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                    o
                    Transpose() o
                    MapSeq(fun( aRow =>
                      Transpose() o
                        MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                          ReduceSeq(add, 0.0f)) o Split(tileSize)) // partReduce)
                        o
                        Transpose() o MapSeq(fun( pair =>
                        MapSeq(fun( elem => mult.apply(Get(pair, 0), elem))) $ Get(pair, 1)
                      )) $ Zip(aRow, Transpose() $ bCols)
                    )) $ aRows
                )) o Split(tileSize) $ B
              )) o Split(tileSize) $ A
          })

        val (output14, _) = Execute(1, mSize)[Array[Float]](f14, matrixA, transposedMatrixB)
        assertArrayEquals(gold, output14, 0.0f)

        // Map fission
        val f15 = fun(
          ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
          ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
          (A, B) => {
            Untile2D() o
              MapGlb(fun( aRows =>
                MapSeq(fun(bCols =>
                  toGlobal(MapSeq(MapSeq(id))) o Join() o

                    ReduceSeq(fun((acc, p) =>
                      MapSeq(fun(part =>
                        MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                      )) $ Zip(acc, p)),
                      toGlobal(MapSeq(MapSeq(id)))
                        $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                    o
                    Transpose() o
                    MapSeq(
                      Transpose() o
                        MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                          ReduceSeq(add, 0.0f)) o Split(tileSize)) // partReduce)
                        o
                        Transpose()) o
                    MapSeq(fun( aRow =>
                      MapSeq(fun( pair =>
                        MapSeq(fun( elem => mult.apply(Get(pair, 0), elem))) $ Get(pair, 1)
                      )) $ Zip(aRow, Transpose() $ bCols)
                    )) $ aRows
                )) o Split(tileSize) $ B
              )) o Split(tileSize) $ A
          })

        val (output15, _) = Execute(1, mSize)[Array[Float]](f15, matrixA, transposedMatrixB)
        assertArrayEquals(gold, output15, 0.0f)

        // Map-Map interchange, pulling the zip out
        val f16 = fun(
          ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
          ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
          (A, B) => {
            Untile2D() o
              MapGlb(fun( aRows =>
                MapSeq(fun(bCols =>
                  toGlobal(MapSeq(MapSeq(id))) o Join() o

                    ReduceSeq(fun((acc, p) =>
                      MapSeq(fun(part =>
                        MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                      )) $ Zip(acc, p)),
                      toGlobal(MapSeq(MapSeq(id)))
                        $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                    o
                    Transpose() o
                    MapSeq(
                      Transpose() o
                        MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                          ReduceSeq(add, 0.0f)) o Split(tileSize)) // partReduce)
                        o
                        Transpose()) o
                    Transpose() o MapSeq(fun( pair =>
                    MapSeq(fun( aElem =>
                      MapSeq(fun( bElem => mult.apply(aElem, bElem))) $ Get(pair, 1)
                    )) $ Get(pair, 0)
                  )) $ Zip(Transpose() $ aRows, Transpose() $ bCols)
                )) o Split(tileSize) $ B
              )) o Split(tileSize) $ A
          })

        val (output16, _) = Execute(1, mSize)[Array[Float]](f16, matrixA, transposedMatrixB)
        assertArrayEquals(gold, output16, 0.0f)

    // Pull transposes out of the zip + map fission
    val f17 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                  Transpose() o
                    MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)) o Split(tileSize)) // partReduce)
                    o
                    Transpose()) o
                Transpose() o MapSeq(fun( pair =>
                MapSeq(fun( aElem =>
                  MapSeq(fun( bElem => mult.apply(aElem, bElem))) $ Get(pair, 1)
                )) $ Get(pair, 0)
              )) $ Zip(aRows, bCols)
            )) o Map(Transpose()) o Split(tileSize) $ B
          )) o Map(Transpose()) o Split(tileSize) $ A
      })

    val (output17, _) = Execute(1, mSize)[Array[Float]](f17, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output17, 0.0f)

    // split-join
    val f18 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                  Transpose() o
                    MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)) o Split(tileSize)) // partReduce)
                    o
                    Transpose()) o
                Transpose() o
                Join() o MapSeq(MapSeq(fun( pair =>
                  MapSeq(fun( aElem =>
                    MapSeq(fun( bElem => mult.apply(aElem, bElem))) $ Get(pair, 1)
                  )) $ Get(pair, 0)
                ))) o Split(tileSize) $ Zip(aRows, bCols)
            )) o Map(Transpose()) o Split(tileSize) $ B
          )) o Map(Transpose()) o Split(tileSize) $ A
      })

    val (output18, _) = Execute(1, mSize)[Array[Float]](f18, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output18, 0.0f)

    // split-join
    val f19 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                  Transpose() o
                    MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)) o Split(tileSize)) // partReduce)
                    o
                    Transpose()) o
                Transpose() o
                Join() o MapSeq(fun( p => MapSeq(fun( pair =>
                MapSeq(fun( aElem =>
                  MapSeq(fun( bElem => mult.apply(aElem, bElem))) $ Get(pair, 1)
                )) $ Get(pair, 0)
              )) $ Zip(Get(p, 0), Get(p, 1)) )) $ Zip(Split(tileSize) $ aRows, Split(tileSize) $ bCols)
            )) o Map(Transpose()) o Split(tileSize) $ B
          )) o Map(Transpose()) o Split(tileSize) $ A
      })

    val (output19, _) = Execute(1, mSize)[Array[Float]](f19, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output19, 0.0f)


    // move splits out from zip + fission + fusion
    val f20 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                  Transpose() o
                    MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)) o Split(tileSize)) // partReduce)
                    o
                    Transpose()) o
                Transpose() o
                Join() o MapSeq(fun( p => MapSeq(fun( pair =>
                MapSeq(fun( aElem =>
                  MapSeq(fun( bElem => mult.apply(aElem, bElem))) $ Get(pair, 1)
                )) $ Get(pair, 0)
              )) $ Zip(Get(p, 0), Get(p, 1)) )) $ Zip(aRows, bCols)
              //  ^                            ^ ^
              //  |                            | Arr(Tuple(Arr(Arr(float,4),4), Arr(Arr(float,4),4)),(K_2*1/(4)))
              //  |                            |
              //  |                            Tuple(Arr(Arr(float,4),4), Arr(Arr(float,4),4))
              //  Arr(Tuple(Arr(float,4), Arr(float,4)),4)
            )) o Map(Split(tileSize) o Transpose()) o Split(tileSize) $ B
          )) o Map(Split(tileSize) o Transpose()) o Split(tileSize) $ A
      })

    val (output20, _) = Execute(1, mSize)[Array[Float]](f20, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output20, 0.0f)

    // Map-Map transpose pushing zip inside
    val f21 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                  Transpose() o
                    MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)) o Split(tileSize)) // partReduce)
                    o
                    Transpose()) o
                Transpose() o
                Join() o MapSeq(fun( p => Transpose() o MapSeq(fun( pair =>
                MapSeq(fun( aElem =>
                  MapSeq(fun( bElem => mult.apply(Get(aElem, 0), bElem))) $ Get(aElem, 1)
                )) $ Zip(pair, Get(p, 1))
              )) o Transpose() $ Get(p, 0))) $ Zip(aRows, bCols)
            )) o Map(Split(tileSize) o Transpose()) o Split(tileSize) $ B
          )) o Map(Split(tileSize) o Transpose()) o Split(tileSize) $ A
      })

    val (output21, _) = Execute(1, mSize)[Array[Float]](f21, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output21, 0.0f)

    // Map-Map transpose pushing zip inside
    val f22 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                  Transpose() o
                    MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)) o Split(tileSize)) // partReduce)
                    o
                    Transpose()) o
                Transpose() o
                Join() o MapSeq(fun( p => Transpose() o MapSeq(fun( pair =>
                Transpose() o MapSeq(fun( aElem =>
                  MapSeq(mult) $ Zip(pair, aElem)
                  )) o Transpose() $ Get(p, 1)
              )) o Transpose() $ Get(p, 0))) $ Zip(aRows, bCols)
            )) o Map(Split(tileSize) o Transpose()) o Split(tileSize) $ B
          )) o Map(Split(tileSize) o Transpose()) o Split(tileSize) $ A
      })

    val (output22, _) = Execute(1, mSize)[Array[Float]](f22, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output22, 0.0f)

    // pull transposes to zip, fission
    val f23 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                  Transpose() o
                    MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)) o Split(tileSize)) // partReduce)
                    o
                    Transpose()) o
                Transpose() o
                Join() o MapSeq(fun( p => Transpose() o MapSeq(fun( pair =>
                Transpose() o MapSeq(fun( aElem =>
                  MapSeq(mult) $ Zip(pair, aElem)
                )) $ Get(p, 1)
              )) $ Get(p, 0))) $ Zip(Map(Transpose()) $ aRows, Map(Transpose()) $ bCols)
            )) o Map(Split(tileSize) o Transpose()) o Split(tileSize) $ B
          )) o Map(Split(tileSize) o Transpose()) o Split(tileSize) $ A
      })

    val (output23, _) = Execute(1, mSize)[Array[Float]](f23, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output23, 0.0f)

    // pull transposes outside zip + fusion
    val f24 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                  Transpose() o
                    MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)) o Split(tileSize)) // partReduce)
                    o
                    Transpose()) o
                Transpose() o
                Join() o MapSeq(fun( p => Transpose() o MapSeq(fun( pair =>
                Transpose() o MapSeq(fun( aElem =>
                  MapSeq(mult) $ Zip(pair, aElem)
                )) $ Get(p, 1)
              )) $ Get(p, 0))) $ Zip(aRows, bCols)
            )) o Map(Map(Transpose()) o Split(tileSize) o Transpose()) o Split(tileSize) $ B
          )) o Map(Map(Transpose()) o Split(tileSize) o Transpose()) o Split(tileSize) $ A
      })

    val (output24, _) = Execute(1, mSize)[Array[Float]](f24, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output24, 0.0f)

    // replace with predefined tile
    val f25 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              toGlobal(MapSeq(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
          //  ^
          //  Arr(Arr(Arr(float,4),4),(K_2*1/^(4)))
                MapSeq(
          //  ^
          //  Arr(Arr(Arr(float,4),(K_2*1/^(4))),4)
                  Transpose() o
             // ^
             // Arr(Arr(float,4),(K_2*1/^(4)))
                    MapSeq(  Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                // ^       ^
                // |       Arr(float,(K_2*1/^(4)))
                // Arr(Arr(float,(K_2*1/^(4))),4)
                      ReduceSeq(add, 0.0f)) o Split(tileSize) ) // partReduce)
                    //                       ^               ^
                    //                       |               Arr(float,K_2)
                    //                       Arr(Arr(float, 4),(K_2*1/^(4)))
                    o
                    Transpose()   ) o
                // ^           ^
                // |           Arr(Arr(float,4),K_2)
                // Arr(Arr(float,K_2),4)
                Transpose() o
           //  ^
           //  Arr(Arr(Arr(float,4),K_2),4)
                Join() o MapSeq(fun( pairOfTiles => Transpose() o MapSeq(fun( aRow =>
          //  ^         ^
          //  |         |
          //  |         Arr(Arr(Arr(Arr(float,4),4),4),(K_2*1/^(4)))
          //  Arr(Arr(Arr(float,4),4),K_2)
                Transpose() o MapSeq(fun( bCol =>
                  MapSeq(mult) $ Zip(aRow, bCol)
                )) $ Get(pairOfTiles, 1)
              )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output25, _) = Execute(1, mSize)[Array[Float]](f25, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output25, 0.0f)

    // Can't transpose both sides of a map if there is a split on one of those dimensions inside

    // map(split) o transpose => transpose o map(transpose) o split
    val f26 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                MapSeq(fun(part =>
                  MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                )) $ Zip(acc, p)),
                toGlobal(MapSeq(MapSeq(id)))
                  $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                  Transpose() o
                    MapSeq(  Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)))   // partReduce
                    o
                    Transpose() o Map(Transpose()) o Split(tileSize)   ) o
                Transpose() o
                  Join() o MapSeq(fun( pairOfTiles => Transpose() o MapSeq(fun( aRow =>
                Transpose() o MapSeq(fun( bCol =>
                  MapSeq(mult) $ Zip(aRow, bCol)
                )) $ Get(pairOfTiles, 1)
              )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output26, _) = Execute(1, mSize)[Array[Float]](f26, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output26, 0.0f)

    // transpose both sides + transpose o transpose => id
    val f27 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                    MapSeq(  Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                      ReduceSeq(add, 0.0f)))   // partReduce
                    o
                    Map(Transpose()) o Split(tileSize)   ) o
                Transpose() o
                Join() o MapSeq(fun( pairOfTiles => Transpose() o MapSeq(fun( aRow =>
                Transpose() o MapSeq(fun( bCol =>
                  MapSeq(mult) $ Zip(aRow, bCol)
                )) $ Get(pairOfTiles, 1)
              )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output27, _) = Execute(1, mSize)[Array[Float]](f27, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output27, 0.0f)

    // map(split) o transpose => transpose o map(transpose) o split
    val f28 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                  MapSeq(  Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                    ReduceSeq(add, 0.0f)))   // partReduce
                    o
                    Map(Transpose())   ) o
                Transpose() o Map(Transpose()) o Split(tileSize) o
                Join() o MapSeq(fun( pairOfTiles => Transpose() o MapSeq(fun( aRow =>
                Transpose() o MapSeq(fun( bCol =>
                  MapSeq(mult) $ Zip(aRow, bCol)
                )) $ Get(pairOfTiles, 1)
              )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output28, _) = Execute(1, mSize)[Array[Float]](f28, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output28, 0.0f)

    // split o join => id
    val f29 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                Transpose() o
                MapSeq(
                  MapSeq(  Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                    ReduceSeq(add, 0.0f)))   // partReduce
                    o
                    Map(Transpose())   ) o
                Transpose() o Map(Transpose()) o
                MapSeq(fun( pairOfTiles => Transpose() o MapSeq(fun( aRow =>
                Transpose() o MapSeq(fun( bCol =>
                  MapSeq(mult) $ Zip(aRow, bCol)
                )) $ Get(pairOfTiles, 1)
              )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output29, _) = Execute(1, mSize)[Array[Float]](f29, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output29, 0.0f)

    // transpose both sides + transpose o transpose => id
    val f30 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                MapSeq(
                  MapSeq(  Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                    ReduceSeq(add, 0.0f)))   // partReduce
                    o
                    Map(Transpose())   ) o
                Map(Transpose()) o
                MapSeq(fun( pairOfTiles => Transpose() o MapSeq(fun( aRow =>
                  Transpose() o MapSeq(fun( bCol =>
                    MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ Get(pairOfTiles, 1)
                )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output30, _) = Execute(1, mSize)[Array[Float]](f30, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output30, 0.0f)

    // Map fusion + transpose o transpose => id
    val f31 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                MapSeq(
                  MapSeq(  Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                    ReduceSeq(add, 0.0f)))   // partReduce
                    o
                    Map(Transpose())   ) o
                MapSeq(fun( pairOfTiles => MapSeq(fun( aRow =>
                  Transpose() o MapSeq(fun( bCol =>
                    MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ Get(pairOfTiles, 1)
                )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output31, _) = Execute(1, mSize)[Array[Float]](f31, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output31, 0.0f)

    // Map fission + Map fusion + transpose o transpose => id
    val f32 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                MapSeq(
                  MapSeq(  Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                    ReduceSeq(add, 0.0f)))   // partReduce
                    ) o
                MapSeq(fun( pairOfTiles => MapSeq(fun( aRow =>
                  MapSeq(fun( bCol =>
                    MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ Get(pairOfTiles, 1)
                )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output32, _) = Execute(1, mSize)[Array[Float]](f32, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output32, 0.0f)

    // Map fusion
    val f33 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                MapSeq(fun( pairOfTiles =>
                  MapSeq(  Join() o MapSeq(toGlobal(MapSeq(id)) o // partReduce
                    ReduceSeq(add, 0.0f)))   // partReduce
                 o
                MapSeq(fun( aRow =>
                  MapSeq(fun( bCol =>
                    MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ Get(pairOfTiles, 1)
                )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output33, _) = Execute(1, mSize)[Array[Float]](f33, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output33, 0.0f)

    // Map fusion
    val f34 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                MapSeq(fun( pairOfTiles =>
                  MapSeq(  Join() o fun( aRow => MapSeq(toGlobal(MapSeq(id)) o
                    ReduceSeq(add, 0.0f))
                    o
                      MapSeq(fun( bCol =>
                        MapSeq(mult) $ Zip(aRow, bCol)
                      )) $ Get(pairOfTiles, 1)
                    )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output34, _) = Execute(1, mSize)[Array[Float]](f34, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output34, 0.0f)

    // Map fusion
    val f35 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                MapSeq(fun( pairOfTiles =>
                  MapSeq(  Join() o fun( aRow => MapSeq(
                    fun( bCol => toGlobal(MapSeq(id)) o
                    ReduceSeq(add, 0.0f)
                    o
                      MapSeq(mult) $ Zip(aRow, bCol)
                    )) $ Get(pairOfTiles, 1)
                  )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output35, _) = Execute(1, mSize)[Array[Float]](f35, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output35, 0.0f)

    // ReduceSeq o MapSeq fusion
    val f36 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                MapSeq(fun( pairOfTiles =>
                  MapSeq(Join() o fun( aRow => MapSeq(
                    fun( bCol => toGlobal(MapSeq(id)) o
                      ReduceSeq(fun((acc, y) =>
                        multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(aRow, bCol)
                    )) $ Get(pairOfTiles, 1)
                  )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output36, _) = Execute(1, mSize)[Array[Float]](f36, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output36, 0.0f)

    // Map fission, separate join
    val f37 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>
                  MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, p)),
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize)))
                o
                MapSeq(fun( pairOfTiles =>
                  Map(Join()) o MapSeq(fun( aRow => MapSeq(
                    fun( bCol => toGlobal(MapSeq(id)) o
                      ReduceSeq(fun((acc, y) =>
                        multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(aRow, bCol)
                    )) $ Get(pairOfTiles, 1)
                  )) $ Get(pairOfTiles, 0))) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output37, _) = Execute(1, mSize)[Array[Float]](f37, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output37, 0.0f)

    // ReduceSeq o MapSeq fusion
    val f38 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Untile2D() o
          MapGlb(fun( aRows =>
            MapSeq(fun(bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join() o

                ReduceSeq(fun((acc, p) =>

                  fun( pairOfTiles =>

                  fun(partial => MapSeq(fun(part =>
                    MapSeq(add) $ Zip(Get(part, 0), Get(part, 1))
                  )) $ Zip(acc, partial) )
                  o Map(Join()) o MapSeq(fun( aRow => MapSeq(
                  fun( bCol => toGlobal(MapSeq(id)) o
                    ReduceSeq(fun((acc, y) =>
                      multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(aRow, bCol)
                  )) $ Get(pairOfTiles, 1)
                )) $ Get(pairOfTiles, 0)) $ p)
                  ,
                  toGlobal(MapSeq(MapSeq(id)))
                    $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, tileSize), tileSize))
                ) $ Zip(aRows, bCols)

            )) o Tile(tileSize) $ B
          )) o Tile(tileSize) $ A
      })

    val (output38, _) = Execute(1, mSize)[Array[Float]](f38, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output38, 0.0f)

    // Goal reached, same as TestMatrixMatrix.tiledMatrixMultiply2
    // Basic tiled matrix multiply without local memory
  }
}
