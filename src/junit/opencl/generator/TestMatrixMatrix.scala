package junit.opencl.generator

import opencl.executor._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import opencl.ir._
import ir._

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

  val id = UserFunDef("id", "x", "{ return x; }", Float, Float)

  val sumUp = UserFunDef("sumUp", Array("x", "y"), "{ return x+y; }", TupleType(Float, Float), Float)

  val add = UserFunDef("add", Array("x", "y"), "{ return x+y; }", TupleType(Float, Float), Float)

  val mult = UserFunDef("mult", Array("l", "r"), "{ return l * r; }", TupleType(Float, Float), Float)

  val multAndSumUp = UserFunDef("multAndSumUp", Array("acc", Array("l", "r")),
    "{ return acc + (l * r); }",
    TupleType(Float, TupleType(Float, Float)), Float)

  val multAndSumUp3 = UserFunDef("multAndSumUp3", Array("acc", "l", "r"),
    "{ return acc + (l * r); }",
    TupleType(Float, Float, Float), Float)

  def matrixMatrixPatternMultiply(A: Array[Array[Float]], B: Array[Array[Float]]): Array[Array[Float]] = {
    val Bt = B.transpose
    A.map( Arow =>
      Bt.map( Bcol => (Arow, Bcol).zipped.map(_ * _).reduce(_ + _) )
    )
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

    val inputSize = 1024
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

    val Msize = 512
    val Ksize = 512
    val Nsize = 512
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(Float, M), K),
      ArrayType(ArrayType(Float, K), N),
      (A, B) => {
        MapWrg(fun( Arow =>
          MapLcl(fun( Bcol =>
              ReduceSeq(multAndSumUp, 0.0f) o Zip(Arow, Bcol)
          )) o B
        )) o A
      })

    val (output, runtime) = Execute(Msize * Nsize)(f, matrixA, matrixB.transpose, Msize, Ksize, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    (gold, output).zipped.map(assertEquals(_,_,0.0))

    (output, runtime)

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

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // this is already transposed
      (A, B) => {
        MapGlb(0)(fun( Arow =>
          MapGlb(1)(fun( Bcol =>
            ReduceSeq(multAndSumUp, 0.0f) o Zip(Arow, Bcol)
          )) o B
        )) o A
      })

    val (output, runtime) = Execute(Msize * Nsize)(f, matrixA, matrixB.transpose, Msize, Ksize, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    (gold, output).zipped.map(assertEquals(_,_,0.0))

    (output, runtime)

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
            ReduceSeq(multAndSumUp, 0.0f) o Zip(Arow, Bcol)
          )) o Transpose() o B
        )) o A
      })

    val (output, runtime) = Execute(Msize * Nsize)(f, matrixA, matrixB, Msize, Ksize, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    (gold, output).zipped.map(assertEquals(_,_,0.0))

    (output, runtime)

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

              MapLcl(0)(fun( Arow =>
                MapLcl(1)(fun( Bcol =>

                  ReduceSeq(multAndSumUp, 0.0f) o Zip(Arow, Bcol)

                )) o Transpose() o fun(p => Get(p, 1)) o Unzip() o zippedChunk
              )) o Transpose() o fun(p => Get(p, 0)) o Unzip() o zippedChunk

            )) o Split(d) o Zip(Transpose() o Arows, Transpose() o Bcols) // ,0.0f*r*c

          )) o Split(c) o B
        )) o Split(r) o A
      })

    val (output, runtime) = Execute(Msize * Nsize)(f, matrixA, matrixB.transpose, Msize, Ksize, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    (gold, output).zipped.map(assertEquals(_,_,0.0))

    (output, runtime)

  }
  */

  @Test def MATRIX_MATRIX_2D_TESTS_2() {

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

            // Join() o MapSeq(fun( (zippedChunk) => //acc,

              MapLcl(0)(fun( Arow =>
                MapLcl(1)(fun( Bcol =>

                  ReduceSeq(multAndSumUp, 0.0f) o Zip(Arow, Bcol)

                )) o Bcols // o Transpose() o fun(p => Get(p, 1)) o Unzip() o zippedChunk
              )) o Arows // o Transpose() o fun(p => Get(p, 0)) o Unzip() o zippedChunk

            // )) o Split(d) o Zip(Transpose() o Arows, Transpose() o Bcols) // ,0.0f*r*c

          )) o Split(c) o B
        )) o Split(r) o A
      })

    val (output, runtime) = Execute(Msize * Nsize)(f, matrixA, matrixB.transpose, Msize, Ksize, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    val gold = matrixMatrixMultiply(matrixA, matrixB).flatten

    (gold, output).zipped.map(assertEquals(_,_,0.0))

    (output, runtime)

  }

}
