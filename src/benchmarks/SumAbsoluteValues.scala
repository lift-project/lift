package benchmarks

import ir.UserFunDef._
import ir._
import opencl.ir._

class SumAbsoluteValues(override val name: String,
                 override val defaultSizes: Seq[Int],
                 override val delta: Float,
                 override val f: Seq[(String, Seq[Lambda])]) extends Benchmark(name, defaultSizes, f, delta) {

  override def runScala(inputs: Any*): Array[Float] = {
    Array(inputs(0).asInstanceOf[Array[Float]].sum)
  }

  override def generateInputs(): Seq[Any] = {
    val inputSize = inputSizes()(0)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)
    Seq(inputData)
  }
}

object SumAbsoluteValues {

  val intelDerivedNoWarp1 = fun(ArrayType(Float, Var("N")), (in) => {
    Join() o MapWrg(
      asScalar() o Join() o MapLcl(
        MapSeq(Vectorize(4)(id)) o ReduceSeq(Vectorize(4)(absAndSumUp), Vectorize(4)(0.0f))
      ) o Split(8192) o asVector(4)
    ) o Split(32768) $ in
  })

  val intelDerived2 = fun(ArrayType(Float, Var("N")), (in) => {
    Join() o MapWrg(
      Join() o MapLcl(
        ReduceSeq(add, 0.0f)
      ) o Split(2048)
    ) o Split(2048) $ in
  })

  val nvidiaDerived1 = fun(ArrayType(Float, Var("N")), (in) => {
    // the original derived one does not generate correct code ...
    Join() o MapWrg( Join() o
      MapLcl(ReduceSeq(add, 0.0f)) o Split(2048) o ReorderStride(128)
      //toGlobal(MapLcl(Iterate(7)(MapSeq(id) o ReduceSeq(sumUp, 0.0f)) o ReduceSeq(sumUp, 0.0f))) o ReorderStride()
    ) o Split(2048*128) $ in
  })

  val amdNvidiaDerived2 = fun(ArrayType(Float, Var("N")), (in) => {
    Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Iterate(6)( Join() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2) ) o
        Join() o toLocal(MapLcl(ReduceSeq(add, 0.0f))) o Split(128)
    ) o Split(8192) $ in
  })
  
  val amdDerived1 = fun(ArrayType(Float, Var("N")), (in) => {
    Join() o MapWrg(
      asScalar() o Join() o
        MapLcl(ReduceSeq(Vectorize(2)(add), Vectorize(2)(0.0f)))
        o Split(2048) o ReorderStride(64) o asVector(2)
    ) o Split(4096*128) $ in
  })

  def apply() = new SumAbsoluteValues("Sum of absolute values",
    Seq(16777216),
    0.001f,
    Seq(("INTEL_DERIVED_NO_WARP", Seq(intelDerivedNoWarp1, intelDerived2)),
        ("NVIDIA_DERIVED", Seq(nvidiaDerived1, amdNvidiaDerived2)),
        ("AMD_DERIVED", Seq(amdDerived1, amdNvidiaDerived2))))

  def main(args: Array[String]) = {
    SumAbsoluteValues().run(args)
  }
}
