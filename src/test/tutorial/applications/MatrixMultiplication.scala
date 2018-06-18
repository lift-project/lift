package tutorial.applications

import ir.ArrayTypeWSWC
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.ir.pattern._
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import org.junit.Assert.assertArrayEquals
import org.junit.Test


/*
 * helper class for matrix multiplication
 */

object MatrixUtensils
{

  /*
   * scala version of matrix-vector multiplication
   */

  def matrixVector(matrix: Array[Array[Float]], vector: Array[Float]): Array[Float] = {
    matrix.map(
      (row) => (row, vector).zipped.map(_ * _).sum
    )
  }

  /*
   * scala version of matrix-matrix multiplication
   */

  def matrixMatrixMultiplyStandard(A: Array[Array[Float]], B: Array[Array[Float]]) :  Array[Array[Float]] =
  {
    val aCols = A(0).length
    val aRows = A.length
    val bCols = B(0).length
    val result =  Array.ofDim[Float](aRows, bCols)

    // throw an exemption if matrices cannot be multiplied
    if (A.head.length != B.length)
      throw new IllegalArgumentException

    def computeRow(row: Int): Unit = {
        //loop over columns of B
        for (col <- 0 until bCols)
        {
           var sum = 0.0f
          //loop over columns of A
           for (i <- 0 until aCols)
           {
             sum += A(row)(i) * B(i)(col)
           }

           result(row)(col) = sum

        }
    }

    // loop over rows of A to compute each row
    (0 until aRows).foreach( computeRow )

    result
  }

}

/**
  * Here are some tests showing examples of matrix multiplication
  * Included is a matrix-vector multiplication test as well as a matrix-matrix multiplication example
  */

object MatrixMultiplication extends TestWithExecutor

class MatrixMultiplication
{

  @Test def matrixVectorMultiplication(): Unit =
  {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r,c) => 1.0f) // fill matrix with 1.0fs
    val vector = Array.fill(inputSize)(2.0f) // fill vector with 2.0fs

    // size placeholders in LIFT
    val M = SizeVar("M")
    val N = SizeVar("N")

    // define our lambda function - this gets compiled to a kernel

    val f = fun(
      // takes in two inputs - (matrix, vector) - one two dimensional matrix and one one dimensional vector
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      ArrayTypeWSWC(Float, N),
      (matrix, vector) => {
        Join() o MapGlb(( fun( (r) => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(vector, r) ) )
        ) $ matrix
      })

    // execute the kernel - returns output (the resulting array) and runtime (length of computation)
    val (output, runtime) = Execute(inputSize * inputSize)[Array[Float]](f, matrix, vector)

    // ensure the kernel calculated the correct value - comparison to scala version of matrix Vector
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
        MapGlb(fun( Arow =>  // map over function with "Arow" as the parameter
          MapSeq(fun( Bcol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(Arow, Bcol)
          )) $ matB
        )) $ matA
      })

    val (output, _) = Execute(Msize * Nsize)[Array[Float]](f, matrixA, matrixB.transpose)

    val comparisonData = MatrixUtensils.matrixMatrixMultiplyStandard(matrixA, matrixB).flatten // calculate our comparison array (and then flatten it)

    assertArrayEquals(comparisonData, output, 0.0f)

  }

}
