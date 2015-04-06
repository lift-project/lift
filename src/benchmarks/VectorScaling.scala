package benchmarks

import ir.UserFunDef._
import ir._
import opencl.ir._

class VectorScaling(override val name: String,
                    override val defaultInputSizes: Seq[Int],
                    override val delta: Float,
                    override val f: Seq[(String, Array[Lambda])]) extends Benchmark(name, defaultInputSizes, f, delta) {

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

  val vectorScal = fun( ArrayType(Float, Var("N")), Float, (input, alpha) =>
    Join() o MapWrg(
      Join() o Barrier() o MapLcl(MapSeq(
        fun( x => mult(alpha, x) )
      )) o Split(4)
    ) o Split(1024) $ input
  )

  val scalAMD = fun( ArrayType(Float, Var("N")), Float, (input, alpha) =>
    Join() o MapWrg(
      Join() o Barrier() o MapLcl(MapSeq(
        fun( x => mult(alpha, x) )
      )) o Split(1)
    ) o Split(128) $ input
  )

  val scalNVIDIA = fun( ArrayType(Float, Var("N")), Float, (input, alpha) =>
    Join() o MapWrg(
      Join() o Barrier() o MapLcl(MapSeq(
        fun( x => mult(alpha, x) )
      )) o Split(1)
    ) o Split(2048) $ input
  )

  val scalINTEL = fun( ArrayType(Float, Var("N")), Float, (input, alpha) =>
    Join() o MapWrg(
      Join() o Barrier() o MapLcl(MapSeq(
        fun( x => Vectorize(4)(mult).apply(Vectorize(4)(alpha), x) )
      )) o Split(128) o asVector(4)
    ) o Split(4*128*128) $ input
  )

  def apply() = new VectorScaling("Vector Scaling",
    Seq(1024),
    0.001f,
    Seq(("simple", Array[Lambda](vectorScal)),
        ("SCAL_NVIDIA", Array[Lambda](scalNVIDIA)),
        ("SCAL_AMD", Array[Lambda](scalAMD)),
        ("SCAL_INTEL", Array[Lambda](scalINTEL))))

  def main(args: Array[String]) = {
    VectorScaling().run(args)
  }
}
