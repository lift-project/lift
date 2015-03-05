package benchmarks

import ir.UserFunDef._
import ir._
import opencl.ir._

class DotProduct(override val name: String,
                 override val defaultSizes: Seq[Int],
                 override val delta: Float,
                 override val f: Seq[(String, Seq[Lambda])]) extends Benchmark(name, defaultSizes, f, delta) {

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

  val N = Var("N")

  val dotProductSimple = fun(ArrayType(Float, N),
    ArrayType(Float, N), (left, right) => {
      Join() o MapWrg(
        Join() o MapLcl(ReduceSeq(add, 0.0f) o MapSeq(mult)) o Split(4)
      ) o Split(1024) $ Zip(left, right)
    })

  val dotProductCPU1 = fun(ArrayType(Float, N),
    ArrayType(Float, N),(left, right) => {
      Join() o Join() o MapWrg(
        toGlobal(MapLcl(ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f))) o Split(2048)
      )o  Split(2048*128) $ Zip(left, right)
    })

  val dotProductCPU2 = fun (ArrayType(Float, N),(in) => {
    Join() o MapWrg(
      Join() o MapLcl(ReduceSeq(add, 0.0f)) o Split(128)
    ) o Split(128) $ in

  })

  val dotProduct1 = fun(ArrayType(Float, N),
    ArrayType(Float, N), (left, right) => {
      Join() o Join() o MapWrg(
        toGlobal(MapLcl(ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)))
          o Split(2048) o ReorderStride(128)
      ) o Split(2048*128) $ Zip(left, right)
    })

  val dotProduct2 = fun (ArrayType(Float, N), (in) => {

    Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Iterate(6)(Join() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2)) o
        Join() o toLocal(MapLcl(ReduceSeq(add, 0.0f))) o Split(2)
    ) o Split(128) $ in

  })

  def apply() = new DotProduct("Dot Product",
    Seq(1024),
    0.001f,
    Seq(("simple", Seq(dotProductSimple)),
        ("cpu", Seq(dotProductCPU1, dotProductCPU2)),
        ("gpu", Seq(dotProduct1, dotProduct2))))

  def main(args: Array[String]) = {
    DotProduct().run(args)
  }
}
