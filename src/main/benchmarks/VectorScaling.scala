package benchmarks

import lift.arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class VectorScaling(override val name: String,
                    override val defaultInputSizes: Seq[Int],
                    override val delta: Float,
                    override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark(name, defaultInputSizes, f, delta) {

  override def runScala(inputs: Any*): Array[Float] = {
    inputs(0).asInstanceOf[Array[Float]].map(_ * inputs(1).asInstanceOf[Float])
  }

  override def generateInputs(): Seq[Any] = {
    val inputSize = inputSizes().head

    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f

    Seq(inputArray, alpha)
  }
}

object VectorScaling {

  val vectorScal = fun( ArrayTypeWSWC(Float, SizeVar("N")), Float, (input, alpha) =>
    Join() o MapWrg(
      Join() o MapLcl(MapSeq(
        fun( x => mult(alpha, x) )
      )) o Split(4)
    ) o Split(1024) $ input
  )

  val vectorScal_ = fun( ArrayTypeWSWC(Float, SizeVar("N")), Float, (input, alpha) =>
    input :>>
    Split(1024) :>>
    MapWrg(
      Split(4) >>>
      MapLcl(
        MapSeq(fun( x => mult(alpha, x) ))
      ) >>>
      Join()
    ) :>>
    Join()
  )

  val scalAMD = fun( ArrayTypeWSWC(Float, SizeVar("N")), Float, (input, alpha) =>
    Join() o MapWrg(
      Join() o  MapLcl(MapSeq(
        fun( x => mult(alpha, x) )
      )) o Split(1)
    ) o Split(128) $ input
  )

  val scalAMD_ = fun( ArrayTypeWSWC(Float, SizeVar("N")), Float, (input, alpha) =>
    input :>>
    Split(128) :>>
    MapWrg(
      Split(1) >>>
      MapLcl(
        MapSeq(fun(x => mult(alpha, x)))
      ) >>>
      Join()
    ) :>>
    Join()
  )

  val scalNVIDIA = fun( ArrayTypeWSWC(Float, SizeVar("N")), Float, (input, alpha) =>
    Join() o MapWrg(
      Join() o  MapLcl(MapSeq(
        fun( x => mult(alpha, x) )
      )) o Split(1)
    ) o Split(2048) $ input
  )

  val scalNVIDIA_ = fun( ArrayTypeWSWC(Float, SizeVar("N")), Float, (input, alpha) =>
    input :>>
    Split(2048) :>>
    MapWrg(
      Split(1) >>>
      MapLcl(
        MapSeq(fun(x => mult(alpha, x)))
      ) >>>
      Join()
    ) :>>
    Join()
  )

  val scalINTEL = fun( ArrayTypeWSWC(Float, SizeVar("N")), Float, (input, alpha) =>
    Join() o MapWrg(
      Join() o  MapLcl(MapSeq(
        fun( x => mult.vectorize(4).apply(alpha.vectorize(4), x) )
      )) o Split(128) o asVector(4)
    ) o Split(4*128*128) $ input
  )

  val scalINTEL_ = fun( ArrayTypeWSWC(Float, SizeVar("N")), Float, (input, alpha) =>
    input :>>
    Split(4*128*128) :>>
    MapWrg(
      asVector(4) >>>
      Split(128) >>>
      MapLcl(
        MapSeq( fun(x => mult.vectorize(4).apply(alpha.vectorize(4), x)) )
      ) >>>
      Join()
    ) :>>
    Join()
  )

  def apply() = new VectorScaling("Vector Scaling",
    Seq(1024),
    0.001f,
    Seq(("simple", Array[Lambda](vectorScal)),
        ("SCAL_NVIDIA", Array[Lambda](scalNVIDIA)),
        ("SCAL_AMD", Array[Lambda](scalAMD)),
        ("SCAL_INTEL", Array[Lambda](scalINTEL))))

  def main(args: Array[String]): Unit = {
    VectorScaling().run(args)
  }
}
