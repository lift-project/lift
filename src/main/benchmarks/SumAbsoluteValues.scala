package benchmarks

import lift.arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class SumAbsoluteValues(override val name: String,
                 override val defaultInputSizes: Seq[Int],
                 override val delta: Float,
                 override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark(name, defaultInputSizes, f, delta) {

  override def runScala(inputs: Any*): Array[Float] = {
    Array(inputs(0).asInstanceOf[Array[Float]].sum)
  }

  override def generateInputs(): Seq[Any] = {
    val inputSize = inputSizes().head
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)
    Seq(inputData)
  }
}

object SumAbsoluteValues {

  val intelDerivedNoWarp1 = fun(ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
    Join() o MapWrg(
      asScalar() o Join() o  MapLcl(
        toGlobal(MapSeq(id.vectorize(4))) o ReduceSeq(absAndSumUp.vectorize(4), Value(0.0f).vectorize(4))
      ) o Split(8192) o asVector(4)
    ) o Split(32768) $ in
  })

  val intelDerivedNoWarp1_ = fun(ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
    in :>>
    Split(32768) :>>
    MapWrg(
      asVector(4) >>>
      Split(8192) >>>
      MapLcl(
        ReduceSeq(absAndSumUp.vectorize(4), Value(0.0f).vectorize(4)) >>>
        toGlobal(MapSeq(id.vectorize(4)))
      ) >>>
      Join() >>>
      asScalar()
    ) :>>
    Join()
  })

  val intelDerived2 = fun(ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
    Join() o MapWrg(
      Join() o  MapLcl(
        toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)
      ) o Split(2048)
    ) o Split(2048) $ in
  })

  val intelDerived2_ = fun(ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
    in :>>
    Split(2048) :>>
    MapWrg(
      Split(2048) >>>
      MapLcl(
        ReduceSeq(add, 0.0f) >>>
        toGlobal(MapSeq(id))
      ) >>>
      Join()
    ) :>>
    Join()
  })

  val nvidiaDerived1 = fun(ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
    // the original derived one does not generate correct code ...
    Join() o MapWrg( Join() o
       MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2048) o ReorderStride(128)
      // toGlobal(MapLcl(Iterate(7)(MapSeq(id) o ReduceSeq(sumUp, 0.0f)) o ReduceSeq(sumUp, 0.0f))) o ReorderStride()
    ) o Split(2048*128) $ in
  })

  val nvidiaDerived1_ = fun(ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
    in :>>
    Split(2048*128) :>>
    MapWrg(
      ReorderStride(128) >>>
      Split(2048) >>>
      MapLcl(
        ReduceSeq(add, 0.0f) >>>
        toGlobal(MapSeq(id))
      ) >>>
      Join()
    ) :>>
    Join()
  })

  val amdNvidiaDerived2 = fun(ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
    Join() o MapWrg(
      Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Iterate(6)( Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2) ) o
        Join() o  toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o Split(128)
    ) o Split(8192) $ in
  })

  val amdNvidiaDerived2_ = fun(ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
    in :>>
    Split(8192) :>>
    MapWrg(
      Split(128) >>>
      toLocal(MapLcl(
        ReduceSeq(add, 0.0f) >>>
        toLocal(MapSeq(id))
      )) >>>
      Join() >>>
      Iterate(6)(
        Split(2) >>>
        MapLcl(
          ReduceSeq(add, 0.0f) >>>
          toLocal(MapSeq(id))
        ) >>>
        Join()
      ) >>>
      Split(1) >>>
      toGlobal(MapLcl(MapSeq(id))) >>>
      Join()
    ) :>>
    Join()
  })
  
  val amdDerived1 = fun(ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
    Join() o MapWrg(
      asScalar() o Join() o
         MapLcl(toGlobal(MapSeq(id.vectorize(2))) o ReduceSeq(add.vectorize(2), Value(0.0f).vectorize(2)))
        o Split(2048) o ReorderStride(64) o asVector(2)
    ) o Split(4096*128) $ in
  })

  val amdDerived1_ = fun(ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
    in :>>
    Split(4096*128) :>>
    MapWrg(
      asVector(2) >>>
      ReorderStride(64) >>>
      Split(2048) >>>
      MapLcl(
        ReduceSeq(add.vectorize(2), Value(0.0f).vectorize(2)) >>>
        toGlobal(MapSeq(id.vectorize(2)))
      ) >>>
      Join() >>>
      asScalar()
    ) :>>
    Join()
  })

  def apply() = new SumAbsoluteValues("Sum of absolute values",
    Seq(16777216),
    0.001f,
    Seq(("INTEL_DERIVED_NO_WARP", Array[Lambda](intelDerivedNoWarp1, intelDerived2)),
        ("NVIDIA_DERIVED", Array[Lambda](nvidiaDerived1, amdNvidiaDerived2)),
        ("AMD_DERIVED", Array[Lambda](amdDerived1, amdNvidiaDerived2))))

  def main(args: Array[String]): Unit = {
    SumAbsoluteValues().run(args)
  }
}
