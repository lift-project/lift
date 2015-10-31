package benchmarks

import arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

class MatrixVector (override val f: Seq[(String, Array[Lambda])]) extends Benchmark("Matrix Vector Multiplication (gemv)", Seq(4096, 4096), f, 0.0f) {

  override def runScala(inputs: Any*): Array[Float] = {
    val matrix = inputs(0).asInstanceOf[Array[Array[Float]]]
    val vectorX = inputs(1).asInstanceOf[Array[Float]]
    val vectorY = inputs(2).asInstanceOf[Array[Array[Float]]]
    val alpha = inputs(3).asInstanceOf[Float]
    val beta = inputs(4).asInstanceOf[Float]


    val tmp = matrix.map(
      (row) => (row, vectorX).zipped.map(_ * _).sum * alpha
    )

    val scaledY = vectorY.map(_.head * beta)

    (tmp, scaledY).zipped.map(_ + _)
  }

  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes()(0)
    val inputSizeM = inputSizes()(1)

    val matrix = Array.fill(inputSizeN, inputSizeM)(util.Random.nextInt(5).toFloat)
    val vectorX = Array.fill(inputSizeM)(util.Random.nextInt(5).toFloat)
    val vectorY = Array.fill(inputSizeN, 1)(util.Random.nextInt(5).toFloat)

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
        Join() o  toGlobal(MapLcl(MapSeq(fun( x => multAndSumUp(Get(x, 0), Get(x, 1), beta))))) o Split(1) o
          fun( t => Zip(
            Join() o  MapLcl(MapSeq(fun( x => mult(alpha, x) ))) o Split(1) o
              Join() o  toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f))) o Split(N) $ Zip(vectorX, Get(t, 0)),
            Get(t, 1)) )
      ) $ Zip(matrix, vectorY)
    })

  val fullMatrixVectorFusedOpenCL_ =
    fun(ArrayType(ArrayType(Float, N), M),
        ArrayType(Float, N),
        ArrayType(ArrayType(Float, 1), M),
        Float,
        Float,
        (matrix, vectorX, vectorY, alpha, beta) => {
    Zip(matrix, vectorY) :>>
    MapWrg(
      fun(t =>
        Zip(
          Zip(vectorX, t._0) :>>
          Split(N) :>>
          toLocal(MapLcl(
            ReduceSeq(fun((acc, y) => multAndSumUp(acc, y._0, y._1)), 0.0f) >>>
            toLocal(MapSeq(id))
          )) :>>
          Join() :>>
          Split(1) :>>
          MapLcl(
            MapSeq(fun(x => mult(alpha, x)))
          ) :>>
          Join()
          ,
          Get(t, 1)
        )
      ) >>>
      Split(1) >>>
      toGlobal(MapLcl(MapSeq(fun(x => multAndSumUp(x._0, x._1, beta))))) >>>
      Join()
    )
  })

  val fullMatrixVectorFusedOpenCLAMD = fun(
    ArrayType(ArrayType(Float, N), M),
    ArrayType(Float, N),
    ArrayType(ArrayType(Float, 1), M),
    Float,
    Float,
    (matrix, vectorX, vectorY, alpha, beta) => {
      MapWrg(
        Join() o  toGlobal(MapLcl(MapSeq(fun( x => multAndSumUp(Get(x, 0), Get(x, 1), beta))))) o Split(1) o
          fun( t => Zip(
            Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(128) o
              Join() o  MapLcl(MapSeq(fun( x => mult(alpha, x) ))) o Split(1) o
              Join() o  toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f))) o Split(N/^128) o ReorderStride(128) $ Zip(vectorX, Get(t, 0)),
            Get(t, 1)) )
      ) $ Zip(matrix, vectorY)
    })

  // The same expression as 'fullMatrixVectorFusedOpenCLAMD' but written in a
  // dataflow / more imperative style
  val fullMatrixVectorFusedOpenCLAMD_ = fun(
     ArrayType(ArrayType(Float, N), M),
     ArrayType(Float, N),
     ArrayType(ArrayType(Float, 1), M),
     Float,
     Float,
     (matrix, vectorX, vectorY, alpha, beta) => {
       Zip(matrix, vectorY) :>>
       MapWrg(
         \(pair => {
           val matrixRow = pair._0
           val y_i       = pair._1

           val partialDotProdcut = {
             Zip(vectorX, matrixRow) :>>
             ReorderStride(128) :>>
             Split(N /^ 128) :>>
             toLocal(MapLcl(
               ReduceSeq(\((acc, y) => multAndSumUp(acc, y._0, y._1)), 0.0f) >>>
               toLocal(MapSeq(id))
             )) :>>
             Join()
           }

           val timesAlpha = {
             partialDotProdcut :>>
             Split(1) :>> MapLcl(MapSeq(\(x => mult(alpha, x)))) :>> Join()
           }

           val fullDotProdcut = {
             timesAlpha  :>>
             Split(128) :>> MapLcl(ReduceSeq(add, 0.0f) >>> toLocal(MapSeq(id))) :>> Join()
           }

           Zip(fullDotProdcut, y_i)
         }) >>>
         Split(1) >>>
         toGlobal(MapLcl(MapSeq(fun(x => multAndSumUp(x._0, x._1, beta))))) >>>
         Join()
       )
     })

  def apply() = new MatrixVector(Seq(
    ("FULL_MATRIX_VECTOR_FUSED_OPENCL", Array[Lambda](fullMatrixVectorFusedOpenCL)),
    ("FULL_MATRIX_VECTOR_FUSED_OPENCL_AMD", Array[Lambda](fullMatrixVectorFusedOpenCLAMD)),
    ("FULL_MATRIX_VECTOR_FUSED_OPENCL_AMD_", Array[Lambda](fullMatrixVectorFusedOpenCLAMD_))))

  def main(args: Array[String]): Unit = {
    MatrixVector().run(args)
  }
}
