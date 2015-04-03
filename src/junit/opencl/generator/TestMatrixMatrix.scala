package opencl.generator

import opencl.executor._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test, Ignore}
import opencl.ir._
import ir._
import ir.UserFunDef._

object TestMatrixMatrix {
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

class TestMatrixMatrix {

  def matrixMatrixPatternMultiply(A: Array[Array[Float]], B: Array[Array[Float]]): Array[Array[Float]] = {
    val Bt = B.transpose
    A.map( Arow =>
      Bt.map( Bcol => (Arow, Bcol).zipped.map(_ * _).sum )
    )
  }

  def matrixMatrixPatternMultiply2(A: Array[Array[Float]], B: Array[Array[Float]]): Array[Array[Float]] = {
    val Bt = B.transpose
    A.map( Arow =>
      Bt.map( Bcol => (Arow, Bcol).zipped )
    ).map(_.map(_.map(_ * _).sum))
  }

  def matrixMatrixMultiply(A: Array[Array[Float]], B: Array[Array[Float]]) :  Array[Array[Float]] = {
    val aCols = A(0).length
    val aRows = A.length
    val bCols = B(0).length
    val res =  Array.ofDim[Float](aRows, bCols)

    @inline def computeRow(row: Int) {
      // while statements are much faster than for statements
      var col = 0
      while(col < bCols) { var i = 0; var sum = 0.0f
        while(i < aCols) {
          sum += A(row)(i) * B(i)(col)
          i += 1
        }

        res(row)(col) = sum
        col += 1
      }
    }

    (0 until aRows).par.foreach( computeRow )

    res
  }

  /*
  @Test def TestScalaPatternImplementation(): Unit = {

    val inputSize = 512
    val matrixA = Array.tabulate(inputSize, inputSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(inputSize, inputSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val t0 = System.nanoTime()
    val matrixC = matrixMatrixPatternMultiply(matrixA, matrixB)
    val t1 = System.nanoTime()
    val gold = matrixMatrixMultiply(matrixA, matrixB)
    val t2 = System.nanoTime()

    println("patterns: " + ((t1 - t0) * 1.0e-9) + "secs")
    println("multiply: " + ((t2 - t1) * 1.0e-9) + "secs")

    (gold, matrixC).zipped.map(
      (goldRow, cRow) => (goldRow, cRow).zipped.map(assertEquals(_,_,0.0))
    )

  }
  */

  @Test def MATRIX_MATRIX_SIMPLE() {

    val Msize = 256
    val Ksize = 64
    val Nsize = 512
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N),
      (A, B) => {
        MapWrg(fun( Arow =>
          Barrier() o MapLcl(fun( Bcol =>
            ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
          )) $ B
        )) $ A
      })

    val (output, runtime) = Execute(Msize * Nsize)(f, matrixA, matrixB.transpose, Msize, Ksize, Nsize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def MATRIX_MATRIX_SIMPLER() {

    val Msize = 64
    val Ksize = 128
    val Nsize = 256
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N),
      (A, B) => {
        MapGlb(fun( Arow =>
          MapSeq(fun( Bcol =>
            ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(Arow, Bcol)
          )) $ B
        )) $ A
      })

    val (output, runtime) = Execute(Msize * Nsize)(f, matrixA, matrixB.transpose, Msize, Ksize, Nsize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.0f)
  }

  @Ignore
  @Test def MATRIX_MATRIX_Christophe() {

    val Msize = 32
    val Ksize = 32
    val Nsize = 32
    //val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    //val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => 2.0f)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val r = 2 // number of rows a single workgroup computes
    val c = 4 // number of columns a single workgroup computes
    val d = 16 // chunk size

    //val dotProd = fun(rowPair => ReduceSeq(multAndSumUp, 0.0f) o rowPair)
    val dotProd = fun(rowPair => ReduceSeq(add,0.0f) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ rowPair)

    val f1 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Map(fun(rowA =>
          Map(fun(colB =>
            dotProd(Zip(rowA, colB))
          )) $ B
        )) $ A
      }
    )

    val t1 = Type.check(f1.body)


    // inline
    val f12 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Map(fun(rowA =>
          Map(fun(colB =>
            ReduceSeq(add,0.0f) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
          )) $ B
        )) $ A
      }
    )
    val t12 = Type.check(f12.body)


//    // partial reduction
//    val f13 = fun(
//      ArrayType(ArrayType(Float, K), M),
//      ArrayType(ArrayType(Float, K), N), // this is already transposed
//      (A, B) => {
//        Map(fun(rowA =>
//          Map(fun(colB =>
//            ReduceSeq(add,0.0f) o Join() o Map(fun(pairChunk => ReduceSeq(add,0.0f) o pairChunk )) o Split(d) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) o Zip(rowA, colB)
//          )) o B
//        )) o A
//      }
//    )
//    val t13 = Type.check(f13.body)

    // map distribution
    val f14 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Map(fun(rowA =>
          Map(fun(x => ReduceSeq(add, 0.0f) $ x)) o
          Map(fun(colB =>
            MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
            //Map(fun(pairChunk => ReduceSeq(add,0.0f) o pairChunk )) o Split(d) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
          )) $ B
        )) $ A
      }
    )
    val t14 = Type.check(f14.body)

    // split-join
    val f15 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Map(fun(rowA =>
          Map(fun(x => ReduceSeq(add,0.0f) $ x)) o
          Join() o Map(fun(colsB =>
            Map(fun(colB =>
              MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
              //Map(fun(pairChunk => ReduceSeq(add,0.0f) o pairChunk )) o Split(d) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
            )) $ colsB
          )) o Split(c) $ B
        )) $ A
      }
    )
    val t15 = Type.check(f15.body)

    // split-join
    val f16 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Map(fun(rowA =>
          Map(fun(x => ReduceSeq(add,0.0f) $ x)) o
            Join() o
            Map(fun(colsB =>
              Map(fun(colB =>
                Join() o Map(fun(pairChunk =>
                  MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ pairChunk
                )) o Split(d) $ Zip(rowA, colB)
                //Map(fun(pairChunk => ReduceSeq(add,0.0f) o pairChunk )) o Split(d) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
              )) $ colsB
            )) o Split(c) $ B
        )) $ A
      }
    )
    val t16 = Type.check(f16.body)

    // map distribute
    val f17 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Map(fun(rowA =>
          Map(fun(x => ReduceSeq(add,0.0f) $ x)) o
            Join() o
            Map(fun(colsB =>
              Map(fun(y => Join() $ y)) o
              Map(fun(colB =>
                Map(fun(pairChunk =>
                  MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ pairChunk
                )) o Split(d) $ Zip(rowA, colB)
                //Map(fun(pairChunk => ReduceSeq(add,0.0f) o pairChunk )) o Split(d) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
              )) $ colsB
            )) o Split(c) $ B
        )) $ A
      }
    )
    val t17 = Type.check(f17.body)

    // map transposition
    val f18 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Map(fun(rowA =>
          Map(fun(x => ReduceSeq(add,0.0f) $ x)) o
            Join() o
            Map(fun(colsB =>
              Map(fun(y => Join() $ y)) o
                Transpose() o
                Map(fun(colB =>
                  Map(fun(pairChunk =>
                    MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ pairChunk
                    // MapSeq(mult) $ pairChunk
                  )) o Split(d) $ Zip(rowA, colB)
                  //Map(fun(pairChunk => ReduceSeq(add,0.0f) o pairChunk )) o Split(d) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
                )) o
                Transpose() $ colsB
            )) o Split(c) $ B
        )) $ A
      }
    )
    val t18 = Type.check(f18.body)


    val f21 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(rowsA =>
          Map(fun(rowA =>
            Map(fun(colB =>
              dotProd(Zip(rowA, colB))
            )) $ B
          )) $ rowsA
        )) o Split(r) $ A
      }
    )


    val t21 = Type.check(f21.body)

    // split-join
    val f22 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(rowsA =>
          Map(fun(rowA =>
            Join() o Map(fun(colsB =>
              Map(fun(colB =>
                dotProd(Zip(rowA, colB))
              )) $ colsB
            )) o Split(c) $ B
          )) $ rowsA
        )) o Split(r) $ A
      }
    )

    val t22 = Type.check(f22.body)

    // map distribute
    val f23 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(rowsA =>
          Map(fun(x => Join() $ x)) o
          Map(fun(rowA =>
            Map(fun(colsB =>
              Map(fun(colB =>
                dotProd(Zip(rowA, colB))
              )) $ colsB
            )) o Split(c) $ B
          )) $ rowsA
        )) o Split(r) $ A
      }
    )
    val t23 = Type.check(f23.body)

    // map distribute
    val f24 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Map(fun(rowA =>
              Map(fun(colsB =>
                Map(fun(colB =>
                  dotProd(Zip(rowA, colB))
                )) $ colsB
              )) o Split(c) $ B
            )) $ rowsA
        )) o Split(r) $ A
      }
    )
    val t24 = Type.check(f24.body)

    // map interchange
    val f25 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, N), K), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Transpose() o
            Map(fun(colsB =>

              Map(fun(rowA =>
                Map(fun(colB =>
                  dotProd(Zip(rowA, colB))
                )) $ colsB
              )) $ rowsA

            )) o Split(c) o  Transpose() $ B
          )) o Split(r) $ A
      }
    )
    val t25 = Type.check(f25.body)




    // inline
    val f26 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Transpose() o
              Map(fun(colsB =>
                Map(fun(rowA =>
                  Map(fun(colB =>
                    ReduceSeq(add,0.0f) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
                  )) $ colsB
                )) $ rowsA
              )) o Split(c) $ B
          )) o Split(r) $ A
      }
    )
    val t26 = Type.check(f26.body)

    // Map distribution
    val f26a1 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Transpose() o
              Map(fun(colsB =>

                Map(fun(x =>
                  Map(fun(y =>
                    ReduceSeq(add,0.0f) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ y
                  )) $ x
                )) o

                Map(fun(rowA =>
                  Map(fun(colB =>
                    Zip(rowA, colB)
                  )) $ colsB
                )) $ rowsA

              )) o Split(c) $ B
          )) o Split(r) $ A
      }
    )
    val t26a1 = Type.check(f26a1.body)

    // partial reduce
    val f26a2 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Transpose() o
              Map(fun(colsB =>

                Map(fun(x =>
                  Map(fun(y =>
                    ReduceSeq(add,0.0f) o Join() o Map(ReduceSeq(add,0.0f)) o Split(d) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ y
                  )) $ x
                )) o

                  Map(fun(rowA =>
                    Map(fun(colB =>
                      Zip(rowA, colB)
                    )) $ colsB
                  )) $ rowsA

              )) o Split(c) $ B
          )) o Split(r) $ A
      }
    )
    val t26a2 = Type.check(f26a2.body)

    // split join (+ join-split deletion + map fusion)
    val f26a3 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Transpose() o
              Map(fun(colsB =>

                Map(fun(x =>
                  Map(fun(y =>
                    ReduceSeq(add,0.0f) o Join() o Map(ReduceSeq(add,0.0f) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1))))) o Split(d) $ y
                  )) $ x
                )) o

                  Map(fun(rowA =>
                    Map(fun(colB =>
                      Zip(rowA, colB)
                    )) $ colsB
                  )) $ rowsA

              )) o Split(c) $ B
          )) o Split(r) $ A
      }
    )
    val t26a3 = Type.check(f26a3.body)



    // map distribution
    val f27 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Transpose() o
              Map(fun(colsB =>
                Map(fun(rowA =>
                  Map(fun(x => ReduceSeq(add,0.0f) $ x)) o
                  Map(fun(colB =>
                     MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
                  )) $ colsB
                )) $ rowsA
              )) o Split(c) $ B
          )) o Split(r) $ A
      }
    )
    val t27 = Type.check(f27.body)

    // split-join
    val f28 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Transpose() o
              Map(fun(colsB =>

                Map(fun(rowA =>
                  Map(fun(x => ReduceSeq(add,0.0f) $ x)) o
                    Map(fun(colB =>
                      Join() o Map(fun(chunkPair =>
                        MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ chunkPair
                      )) o Split(r) $ Zip(rowA, colB)
                    )) $ colsB
                )) $ rowsA

              )) o Split(c) $ B
          )) o Split(r) $ A
      }
    )
    val t28 = Type.check(f28.body)

    // map distribution
    val f29 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Transpose() o
              Map(fun(colsB =>

                Map(fun(y => Map(fun(x => ReduceSeq(add,0.0f) $ x)) $ y)) o

                Map(fun(rowA =>
                    Map(fun(colB =>
                      Join() o Map(fun(chunkPair =>
                        MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ chunkPair
                      )) o Split(r) $ Zip(rowA, colB)
                    )) $ colsB
                )) $ rowsA

              )) o Split(c) $ B
          )) o Split(r) $ A
      }
    )
    val t29 = Type.check(f29.body)

    // map distribution
    val f210 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Transpose() o
              Map(fun(colsB =>

                Map(fun(y => Map(fun(x => ReduceSeq(add,0.0f) $ x)) $ y)) o
                Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o

                  Map(fun(rowA =>
                    Map(fun(colB =>
                     Map(fun(chunkPair =>
                        MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ chunkPair
                      )) o Split(r) $ Zip(rowA, colB)
                    )) $ colsB
                  )) $ rowsA

              )) o Split(c) $ B
          )) o Split(r) $ A
      }
    )
    val t210 = Type.check(f210.body)

    // map distribution
    val f211 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Transpose() o
              Map(fun(colsB =>

                Map(fun(y => Map(fun(x => ReduceSeq(add,0.0f) $ x)) $ y)) o
                Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o

                Map(fun(y => Map(fun(x =>
                  Map(fun(chunkPair =>
                    MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ chunkPair
                  )) $ x)) $ y)) o


                Map(fun(rowA =>
                  Map(fun(colB =>
                     Split(r) $ Zip(rowA, colB)
                  )) $ colsB
                )) $ rowsA

              )) o Split(c) $  B
          )) o Split(r) $ A
      }
    )
    val t211 = Type.check(f211.body)

    //
    val f212 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o
          Map(fun(rowsA =>
            Transpose() o
              Map(fun(colsB =>

                Map(fun(y => Map(fun(x => ReduceSeq(add,0.0f) $ x)) $ y)) o
                  Map(fun(y => Map(fun(x => Join() $ x)) $ y)) o

                  Map(fun(y =>
                    Map(fun(x =>
                      Map(fun(chunkPair =>
                        MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ chunkPair
                      )) o Split(r) $ x)) $ y)) o

                  Map(fun(rowA =>
                    Map(fun(colB =>
                      Zip(rowA, colB)
                    )) $ colsB
                  )) $ rowsA

              )) o Split(c) $ B
          )) o Split(r) $ A
      }
    )
    val t212 = Type.check(f212.body)


    // interchange
    val f3 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(rowsA =>
          Transpose() o Map(fun(colB =>
            Map(fun(rowA =>
              dotProd(Zip(rowA, colB))
            )) $ rowsA
          )) $ B
        )) o Split(r) $ A
      }
    )


    val t3 = Type.check(f3.body)


    // split-join
    val f4 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(rowsA =>
          Transpose() o Join() o Map(fun(colsB =>
            Map(fun(colB =>
              Map(fun(rowA =>
                dotProd(Zip(rowA, colB))
              )) $ rowsA
            )) $ colsB
          ))  o Split(c) $ B
        )) o Split(r) $ A
      }
    )

    val t4 = Type.check(f4.body)


    // inline
    val f5 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(rowsA =>
          Transpose() o Join() o Map(fun(colsB =>
            Map(fun(colB =>
              Map(fun(rowA =>
                ReduceSeq(add,0.0f) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
              )) $ rowsA
            )) $ colsB
          ))  o Split(c) $ B
        )) o Split(r) $ A
      }
    )

    val t5 = Type.check(f5.body)


    // split-join
    val f6 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(rowsA =>
          Transpose() o Join() o Map(fun(colsB =>
            Map(fun(colB =>
              Map(fun(rowA =>
                ReduceSeq(add,0.0f) o Join() o Map(fun(pairChunk =>
                  MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ pairChunk
                )) o Split(d) $ Zip(rowA, colB)
              )) $ rowsA
            )) $ colsB
          ))  o Split(c) $ B
        )) o Split(r) $ A
      }
    )

    val t6 = Type.check(f6.body)

    // split-join
    val f7 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o Map(fun(rowsA =>
          Transpose() o Join() o Map(fun(colsB =>
            Map(fun(colB =>
              Map(fun(rowA =>
                ReduceSeq(add,0.0f) o Join() o Map(fun(pairChunk =>
                  MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ pairChunk
                )) o Split(d) $ Zip(rowA, colB)
              )) $ rowsA
            )) $ colsB
          ))  o Split(c) $ B
        )) o Split(r) $ A
      }
    )

    val t7 = Type.check(f7.body)


    //
    val fa = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o MapWrg(0)(fun(rowsA =>
          Transpose() o Join() o MapWrg(1)(fun(colsB =>
            Barrier() o MapLcl(0)(fun(colB =>
              Barrier() o MapLcl(1)(fun(rowA =>
                ReduceSeq(add,0.0f) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1)))) $ Zip(rowA, colB)
              )) $ rowsA
            )) $ colsB
          ))  o Split(c) $ B
        )) o Split(r) $ A
      }
    )

    val ta = Type.check(fa.body)


   /* val f6 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        MapWrg(0)(fun(rowsA =>
          MapWrg(1)(fun(colsB =>
            Barrier() o MapLcl(0)(fun(rowA =>
              Barrier() o MapLcl(1)(fun(colB =>
                ReduceSeq(add,0.0f) o MapSeq(fun(pair => mult(Get(pair,0),Get(pair,1))
                )) o Zip(rowA, colB)
              )) o colsB
            )) o rowsA
          )) o Split(c) o B
        )) o Split(r) o A
      }
    )*/


    val (output, runtime) = Execute(Msize * Nsize)(f5, matrixA, matrixB.transpose, Msize, Ksize, Nsize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    (gold, output).zipped.map(assertEquals(_,_,0.0))
  }

  @Test def MATRIX_MATRIX_2D_GLOBAL_ID() {

    val Msize = 512
    val Ksize = 512
    val Nsize = 512
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f1 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        MapGlb(0)(fun( Arow =>
          MapGlb(1)(fun( Bcol =>
            ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
          )) $ B
        )) $ A
      })

    val f2 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        MapGlb(0)(MapGlb(1)(ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f))) o
        Map(fun( Arow =>
          Map(fun( Bcol =>
            Zip(Arow, Bcol)
          )) $ B
        )) $ A
      })

    val (output, runtime) = Execute(Msize * Nsize)(f1, matrixA, matrixB.transpose, Msize, Ksize, Nsize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.001f)

    val (output2, runtime2) = Execute(Msize * Nsize)(f2, matrixA, matrixB.transpose, Msize, Ksize, Nsize)

    println("output.size = " + output2.length)
    println("output(0) = " + output2(0))
    println("runtime = " + runtime2)

    assertArrayEquals(gold, output2, 0.001f)
  }

  @Test def tiledMultiplicationScala(): Unit = {
    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

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

    val matrixC = tiledC.map(_.transpose.map(_.flatten)).flatten.flatten

    assertArrayEquals(gold, matrixC, 0.001f)

    // Matrix-Multiplication and reduction over tiles separate
    val tiledC2 = tiledA.map(aRows => {
      tiledB.map(bCols => {
        (aRows, bCols).zipped.map((aTile, bTile) => aTile.map(aRow => bTile.map(bRow => (aRow, bRow).zipped.map(_ * _).sum))).
          reduce((acc, tile) => {

          (tile, acc).zipped.map((x, y) => (x, y).zipped.map(_+_))
        })
      })
    })

    val matrixC2 = tiledC2.map(_.transpose.map(_.flatten)).flatten.flatten

    assertArrayEquals(gold, matrixC2, 0.001f)

    // Matrix-Multiplication and reduction over tiles separate, reduction over last dimension, not first
    val tiledC3 = tiledA.map(aRows => {
      tiledB.map(bCols => {
        (aRows, bCols).zipped.map((aTile, bTile) => aTile.map(aRow => bTile.map(bRow => (aRow, bRow).zipped.map(_ * _).sum))).
          transpose.map(_.transpose.map(_.sum))
      })
    })

    val matrixC3 = tiledC3.map(_.transpose.map(_.flatten)).flatten.flatten

    assertArrayEquals(gold, matrixC3, 0.001f)
  }

  @Test def tiledMatrixMultiply(): Unit = {
    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 4

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    val n = new Var("N")
    val m = new Var("M")
    val k = new Var("K")

    def Tile = Map(Map(Transpose()) o Split(tileSize) o Transpose()) o Split(tileSize)

    val f = fun(
      ArrayType(ArrayType(Float, k), m),
      ArrayType(ArrayType(Float, k), n),
      (A, B) => {
        // Undo the tiling
        Join() o MapWrg(0)(Map(Join()) o TransposeW() o fun( aRows =>
          MapWrg(1)(fun( bCols =>

            // Reduce the partial results (matrices), so that the reduce is innermost
            Barrier() o MapLcl(0)(Join() o MapLcl(1)(ReduceSeq(add, 0.0f) o Join()) o Transpose()) o Transpose() o

            // Multiply all necessary combinations of tiles
            MapSeq(fun( tiles =>
              Barrier() o MapLcl(0)( fun(aTile =>
                MapLcl(1)( fun( bTile =>
                  ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(aTile, bTile)
                )) $ Get(tiles, 1)
              )) $ Get(tiles, 0)
            )) $ Zip(aRows, bCols)

          // Tile the matrices
          )) o Tile $ B
        )) o Tile $ A
      })

    val (output, runtime) = Execute(mSize * nSize)(f, matrixA, matrixB.transpose, mSize, kSize, nSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def addArrayOfMatrices(): Unit = {
    val mSize = 16
    val kSize = 16
    val numMatrices = 16
    val matrices = Array.fill(numMatrices, mSize, kSize)(util.Random.nextInt(5).toFloat)

    val gold = matrices.reduce((x, y) => (x, y).zipped.map((x, y) => (x, y).zipped.map(_+_))).flatten

    val test = matrices.transpose.map(_.transpose.map(_.sum)).flatten

    assertArrayEquals(gold, test, 0.001f)

    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, new Var("M")), new Var("K")), new Var("N")),
      input => MapGlb(0)(MapGlb(1)(ReduceSeq(add, 0.0f)) o Transpose()) o Transpose() $ input
    )

    val (output, _) = Execute(mSize*kSize)(f, matrices, numMatrices, kSize, mSize)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test def MATRIX_MATRIX_2D_GLOBAL_ID_WITH_TRANSPOSE() {

    val Msize = 512
    val Ksize = 512
    val Nsize = 512
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, N), K),
      (A, B) => {
        MapGlb(0)(fun( Arow =>
          MapGlb(1)(fun( Bcol =>
            ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
          )) o Transpose() $ B
        )) $ A
      })

    val (output, runtime) = Execute(Msize * Nsize)(f, matrixA, matrixB, Msize, Ksize, Nsize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.0f)
  }

/*
  @Test def MATRIX_MATRIX_2D_TESTS_1() {

    val Msize = 32
    val Ksize = 32
    val Nsize = 32
    //val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    //val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => 2.0f)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val r = 2 // number of rows a single workgroup computes
    val c = 4 // number of columns a single workgroup computes
    val d = 16 // chunk size

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o MapWrg(0)(fun( Arows =>
          Join() o MapWrg(1)(fun( Bcols =>

            Join() o MapSeq(fun( (zippedChunk) => //acc,

              Barrier() o MapLcl(0)(fun( Arow =>
                Barrier() o MapLcl(1)(fun( Bcol =>

                  ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)

                )) o Transpose() o fun(p => Get(p, 1)) $ zippedChunk
              )) o Transpose() o fun(p => Get(p, 0)) $ zippedChunk

            )) o Split(d) $ Zip(Transpose() $ Arows, Transpose() $ Bcols) // ,0.0f*r*c

          )) o Split(c) $ B
        )) o Split(r) $ A
      })

    val (output, runtime) = Execute(Msize * Nsize)(f, matrixA, matrixB.transpose, Msize, Ksize, Nsize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    (gold, output).zipped.map(assertEquals(_,_,0.0))
  }
*/


  /* TODO: Add support for writing transposed, as in: Transpose() o Join() o MapWrg(1)( ...) o Split(c) o B
  @Test def MATRIX_MATRIX_2D_TESTS_2() {

    val Msize = 8
    val Ksize = 8
    val Nsize = 8
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 1 + c * 0) % 10) + 1) * 1.0f)
    //val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 2 + c * 3) % 10) + 1) * 1.0f)
    //val matrixA = Array.tabulate(Msize, Ksize)((r, c) => 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => 1.0f)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val r = 2 // number of rows a single workgroup computes
    val c = 4 // number of columns a single workgroup computes
    val d = 16 // chunk size

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o MapWrg(0)(fun( Arows =>
          Transpose() o Join() o MapWrg(1)(fun( Bcols =>
            Barrier() o MapLcl(0)(fun( Bcol =>
              Barrier() o MapLcl(1)(fun( Arow =>
                ReduceSeq(multAndSumUp, 0.0f) o Zip(Arow, Bcol)
              )) o Arows
            )) o Bcols
          )) o Split(c) o B
        )) o Split(r) o A
      })

    val (output, runtime) = Execute(8, Msize * Nsize)(f, matrixA, matrixB.transpose, Msize, Ksize, Nsize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    println("gold:")
    PrintUtils.myPrint(gold, Msize)
    println("output:")
    PrintUtils.myPrint(output, Msize)

    assertArrayEquals(gold,output,0.0f)

  }
*/

}
