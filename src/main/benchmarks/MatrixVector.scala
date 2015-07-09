package benchmarks

import apart.arithmetic.SizeVar
import ir.UserFunDef._
import ir._
import opencl.ir._
import opencl.ir.CompositePatterns._

class MatrixVector (override val f: Seq[(String, Array[Lambda])]) extends Benchmark("Matrix Vector Multiplication (gemv)", Seq(4096, 4096), f, 0.0f) {

  override def runScala(inputs: Any*): Array[Float] = {
    val matrix = inputs(0).asInstanceOf[Array[Array[Float]]]
    val vectorX = inputs(1).asInstanceOf[Array[Float]]
    val vectorY = inputs(2).asInstanceOf[Array[Float]]
    val alpha = inputs(3).asInstanceOf[Float]
    val beta = inputs(4).asInstanceOf[Float]


    val tmp = matrix.map(
      (row) => (row, vectorX).zipped.map(_ * _).sum * alpha
    )

    val scaledY = vectorY.map(_ * beta)

    (tmp, scaledY).zipped.map(_ + _)
  }

  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes()(0)
    val inputSizeM = inputSizes()(1)

    val matrix = Array.tabulate(inputSizeN, inputSizeM)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val vectorX = Array.tabulate(inputSizeN)(i => ((i % 10) + 1) * 2.0f)
    val vectorY = Array.tabulate(inputSizeN)(i => ((i*3 % 10) + 1) + 1.5f)

    val alpha = 2.5f
    val beta = 1.5f

    Seq(matrix, vectorX, vectorY, alpha, beta)
  }
}

object MatrixVector {

  val N = SizeVar("N")
  val M = SizeVar("M")

  val fullMatrixVectorFusedOpenCL = fun(
    ArrayType(ArrayType(Float, N), M),
    ArrayType(Float, N),
    ArrayType(ArrayType(Float, 1), M),
    Float,
    Float,
    (matrix, vectorX, vectorY, alpha, beta) => {
      MapWrg(
        Join() o Barrier() o toGlobal(MapLcl(MapSeq(fun( x => multAndSumUp(Get(x, 0), Get(x, 1), beta))))) o Split(1) o
          fun( t => Zip(
            Join() o Barrier() o MapLcl(MapSeq(fun( x => mult(alpha, x) ))) o Split(1) o
              Join() o Barrier() o toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f))) o Split(N) $ Zip(vectorX, Get(t, 0)),
            Get(t, 1)) )
      ) $ Zip(matrix, vectorY)
    })

  val fullMatrixVectorFusedOpenCLAMD = fun(
    ArrayType(ArrayType(Float, N), M),
    ArrayType(Float, N),
    ArrayType(ArrayType(Float, 1), M),
    Float,
    Float,
    (matrix, vectorX, vectorY, alpha, beta) => {
      MapWrg(
        Join() o Barrier() o toGlobal(MapLcl(MapSeq(fun( x => multAndSumUp(Get(x, 0), Get(x, 1), beta))))) o Split(1) o
          fun( t => Zip(
            Join() o Barrier() o MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(128) o
              Join() o Barrier() o MapLcl(MapSeq(fun( x => mult(alpha, x) ))) o Split(1) o
              Join() o Barrier() o toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f))) o Split(N/^128) o ReorderStride(128) $ Zip(vectorX, Get(t, 0)),
            Get(t, 1)) )
      ) $ Zip(matrix, vectorY)
    })

  def apply() = new MatrixVector(Seq(
    ("FULL_MATRIX_VECTOR_FUSED_OPENCL", Array[Lambda](fullMatrixVectorFusedOpenCL)),
    ("FULL_MATRIX_VECTOR_FUSED_OPENCL_AMD", Array[Lambda](fullMatrixVectorFusedOpenCLAMD))))

  def main(args: Array[String]): Unit = {
    MatrixVector().run(args)
  }
}
