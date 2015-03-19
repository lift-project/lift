package benchmarks

import ir.UserFunDef._
import ir.{ArrayType, fun, Var, Lambda}
import opencl.ir.{Transpose, MapGlb, Float}

class MatrixTransposition (override val f: Seq[(String, Seq[Lambda])])
  extends Benchmark("Matrix Transposition)", Seq(1024, 1024), f, 0.0f, Array(16, 16, 1)) {

  override def runScala(inputs: Any*): Array[Float] = {
    val matrix = inputs(0).asInstanceOf[Array[Array[Float]]]

    matrix.transpose.flatten
  }

  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes()(0)
    val inputSizeM = inputSizes()(1)

    val matrix = Array.tabulate(inputSizeN, inputSizeM)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)

    Seq(matrix)
  }

  override def globalSize: Array[Int] = {
    val globalSizes = Array(inputSizes()(0), inputSizes()(1), 1)
    globalSizeOpt.value.copyToArray(globalSizes)
    globalSizes
  }
}

object MatrixTransposition {
  val N = Var("N")
  val M = Var("M")

  val naive = fun(
    ArrayType(ArrayType(Float, M), N),
    (matrix) => {
      MapGlb(0)(MapGlb(1)(id)) o Transpose() $ matrix
    })

  def apply() = new MatrixTransposition(Seq(("naive", Seq(naive))))

  def main(args: Array[String]): Unit = {
    MatrixTransposition().run(args)
  }
}
