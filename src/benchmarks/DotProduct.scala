package benchmarks

import ir.UserFunDef._
import ir._
import opencl.ir._

class DotProduct(override val name: String,
                 override val defaultSizes: Seq[Int],
                 override val delta: Float,
                 override val f: Seq[(String, Lambda)]) extends Benchmark(name, defaultSizes, f, delta) {

  override def runScala(inputs: Any*): Array[Float] = {
    Array((inputs(0).asInstanceOf[Array[Float]], inputs(1).asInstanceOf[Array[Float]]).zipped.map(_*_).sum)
  }

  override def generateInputs(): Seq[Any] = {
    val inputSize = inputSizes()(0)

    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    Seq(leftInputData, rightInputData)
  }

  override def inputSizes(): Seq[Int] = {
    if (size.value.length == 1) size.value else defaultSizes
  }

  override def runOpenCL(inputs: Any*): (Array[Float], Double) = {
    val (output, runtime) = super.runOpenCL(inputs:_*)
    (Array(output.sum), runtime)
  }
}

object DotProduct {

  val f1 = fun(ArrayType(Float, Var("N")),
    ArrayType(Float, Var("N")), (left, right) => {
      Join() o MapWrg(
        Join() o MapLcl(ReduceSeq(add, 0.0f) o MapSeq(mult)) o Split(4)
      ) o Split(1024) $ Zip(left, right)
    })

  val f2 = fun(ArrayType(Float, Var("N")),
    ArrayType(Float, Var("N")),(left, right) => {
      Join() o Join() o MapWrg(
        toGlobal(MapLcl(ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)))
      ) o Split(128) o Split(2048) $ Zip(left, right)
    })

  val f3 = fun(ArrayType(Float, Var("N")),
    ArrayType(Float, Var("N")), (left, right) => {
      Join() o Join() o MapWrg(
        toGlobal(MapLcl(ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)))
      ) o Split(128) o ReorderStride(2048/128) o Split(2048) $ Zip(left, right)
    })

  def apply() = new VectorScaling("Dot Product",
    Seq(1024),
    0.001f,
    Seq(("simple", f1), ("cpu", f2), ("gpu", f3)))

  def main(args: Array[String]) = {
    DotProduct().run(args)
  }
}
