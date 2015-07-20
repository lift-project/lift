package opencl.generator

import arithmetic.Var
import ir._
import ir.ast._
import ir.ast.UserFun._
import opencl.executor.{Execute, Executor, Utils}
import opencl.ir._
import opencl.ir.ast._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}
import opencl.ir.pattern._

object TestMatrixMatrixChristophe {
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

class TestMatrixMatrixChristophe {
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


    val (output: Array[Float], runtime) = Execute(Msize * Nsize)(f5, matrixA, matrixB.transpose)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    (gold, output).zipped.map(assertEquals(_,_,0.0))
  }
}
