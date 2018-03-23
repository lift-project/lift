package tutorial.applications

import ir.ArrayTypeWSWC
import ir.ast.{Get, Transpose, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import opencl.executor.{Execute, TestWithExecutor, Utils}
import opencl.ir._
import org.junit.Assert.assertArrayEquals
import org.junit.Test

object MatrixUtensils
{

  def matrixMatrixMultiplyStandard(A: Array[Array[Float]], B: Array[Array[Float]]) :  Array[Array[Float]] =
  {
    val aCols = A(0).length
    val aRows = A.length
    val bCols = B(0).length
    val result =  Array.ofDim[Float](aRows, bCols)

    if (A.head.length != B.length)
      throw new IllegalArgumentException

    def computeRow(row: Int): Unit = {
      var col = 0
      while(col < bCols) {
        var i = 0;
        var sum = 0.0f
         while(i < aCols) {
           sum += A(row)(i) * B(i)(col)
           i += 1
        }

        result(row)(col) = sum
        col += 1

      }
    }

    (0 until aRows).par.foreach( computeRow )

    result
  }

}

object  MatrixMatrixMultiplication extends TestWithExecutor

/**
  * Here are some examples of matrix matrix multiplication
  */
class MatrixMatrixMultiplication
{


  @Test def matrixMatrixMultiplication2D(): Unit =
  {

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val Msize = 64
    val Ksize = 128
    val Nsize = 256

    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

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
