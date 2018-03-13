package opencl.generator.matrixMultiplication

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor, Utils}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

object Basic extends TestWithExecutor

class Basic {

  private val N = SizeVar("N")
  private val M = SizeVar("M")
  private val K = SizeVar("K")

  @Test def MATRIX_MATRIX_SIMPLE(): Unit = {

    val Msize = 256
    val Ksize = 64
    val Nsize = 512
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        MapWrg(fun( Arow =>
          Join() o  MapLcl(fun( Bcol =>
            toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
          )) $ B
        )) $ A
      })

    val (output, _) = Execute(Msize * Nsize)[Array[Float]](f, matrixA, matrixB.transpose)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test def MATRIX_MATRIX_SIMPLER(): Unit = {

    val Msize = 64
    val Ksize = 128
    val Nsize = 256
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        MapGlb(fun( Arow =>
          MapSeq(fun( Bcol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)  $ Zip(Arow, Bcol)
          )) $ B
        )) $ A
      })

    val (output, _) = Execute(Msize * Nsize)[Array[Float]](f, matrixA, matrixB.transpose)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def MATRIX_MATRIX_2D_GLOBAL_ID(): Unit = {

    val Msize = 512
    val Ksize = 512
    val Nsize = 512
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val f1 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // this is already transposed
      (A, B) => {
        MapGlb(0)(fun( Arow =>
          MapGlb(1)(fun( Bcol =>
            toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
          )) $ B
        )) $ A
      })

    val f2 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // this is already transposed
      (A, B) => {
        MapGlb(0)(MapGlb(1)(toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f))) o
          Map(fun( Arow =>
            Map(fun( Bcol =>
              Zip(Arow, Bcol)
            )) $ B
          )) $ A
      })

    val (output, _) = Execute(Msize * Nsize)[Array[Float]](f1, matrixA, matrixB.transpose)
    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.001f)

    val (output2, _) = Execute(Msize * Nsize)[Array[Float]](f2, matrixA, matrixB.transpose)
    assertArrayEquals(gold, output2, 0.001f)
  }

  @Test def addArrayOfMatrices(): Unit = {
    val mSize = 16
    val kSize = 16
    val numMatrices = 16
    val matrices = Array.fill(numMatrices, mSize, kSize)(util.Random.nextInt(5).toFloat)

    val gold = matrices.reduce((x, y) => (x, y).zipped.map((x, y) =>
      (x, y).zipped.map(_+_))).flatten

    val test = matrices.transpose.flatMap(_.transpose.map(_.sum))

    assertArrayEquals(gold, test, 0.001f)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("K")), SizeVar("N")),
      input => {
        MapGlb(0)(MapGlb(1)(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Transpose()) o
          Transpose() $ input
      }
    )

    val (output, _) = Execute(mSize*kSize)[Array[Float]](f, matrices)
    assertArrayEquals(gold, output, 0.001f)
  }

  @Test def MATRIX_MATRIX_2D_GLOBAL_ID_WITH_TRANSPOSE(): Unit = {

    val Msize = 512
    val Ksize = 512
    val Nsize = 512
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        MapGlb(0)(fun( Arow =>
          MapGlb(1)(fun( Bcol =>
            toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
          )) o Transpose() $ B
        )) $ A
      })

    val (output, _) = Execute(Msize * Nsize)[Array[Float]](f, matrixA, matrixB)
    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def MATRIX_MATRIX_2D_TEST(): Unit = {

    val mSize = 8
    val kSize = 8
    val nSize = 8
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 1 + c * 0) % 10) + 1) * 1.0f)
    //val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 2 + c * 3) % 10) + 1) * 1.0f)
    //val matrixA = Array.tabulate(mSize, kSize)((r, c) => 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => 1.0f)

    val r = 2 // number of rows a single workgroup computes
    val c = 4 // number of columns a single workgroup computes

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // this is already transposed
      (A, B) => {
        Join() o MapWrg(0)(fun( aRows =>
          TransposeW() o Join() o MapWrg(1)(fun( bCols =>
            MapLcl(0)(fun( bCol =>
              MapLcl(1)(fun( aRow =>
                toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(aRow, bCol)
              )) $ aRows
            )) $ bCols
          )) o Split(c) $ B
        )) o Split(r) $ A
      })

    val (output, _) = Execute(8, mSize * nSize)[Array[Float]](f, matrixA, matrixB.transpose)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold,output,0.0f)

  }
}
