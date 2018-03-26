package tutorial.applications

import ir.ArrayTypeWSWC
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.ir.pattern._
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import org.junit.Assert.assertArrayEquals
import org.junit.Test

object MatrixUtensils
{

  def matrixVector(matrix: Array[Array[Float]], vector: Array[Float]): Array[Float] = {
    matrix.map(
      (row) => (row, vector).zipped.map(_ * _).sum
    )
  }

  def matrixMatrixMultiplyStandard(A: Array[Array[Float]], B: Array[Array[Float]]) :  Array[Array[Float]] =
  {
    val aCols = A(0).length
    val aRows = A.length
    val bCols = B(0).length
    val result =  Array.ofDim[Float](aRows, bCols)

    if (A.head.length != B.length)
      throw new IllegalArgumentException

    def computeRow(row: Int): Unit = {
        for (col <- 0 until bCols)
        {
           var sum = 0.0f
           for (i <- 0 until aCols)
           {
             sum += A(row)(i) * B(i)(col)
           }

           result(row)(col) = sum

        }
    }

    (0 until aRows).foreach( computeRow )

    result
  }

}

object MatrixMatrixMultiplication extends TestWithExecutor

/**
  * Here are some examples of matrix multiplication
  */
class MatrixMatrixMultiplication
{

  @Test def matrixVectorMultiplication(): Unit = {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r,c) => 1.0f)
    val vector = Array.fill(inputSize)(2.0f)

    val M = SizeVar("M")
    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      ArrayTypeWSWC(Float, N),
      (matrix, vector) => {
        Join() o MapWrg(
          MapLcl( fun( (r) => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(vector, r) ) )
        ) o Split(128) $ matrix
      })

    val (output, runtime) = Execute(inputSize * inputSize)[Array[Float]](f, matrix, vector)

    assertArrayEquals(MatrixUtensils.matrixVector(matrix, vector), output, 0.0f)
  }

  @Test def matrixMatrixMultiplication(): Unit =
  {

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val Msize = 64
    val Ksize = 128
    val Nsize = 256

    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (r + c + 1).toFloat)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (r * c * 3).toFloat)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (matA, matB) => {
        MapGlb(fun( Arow =>
          MapSeq(fun( Bcol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(Arow, Bcol)
          )) $ matB
        )) $ matA
      })

    val (output, _) = Execute(Msize * Nsize)[Array[Float]](f, matrixA, matrixB.transpose)

    val gold = MatrixUtensils.matrixMatrixMultiplyStandard(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.0f)

  }

}
