package benchmarks

import ir.UserFunDef._
import ir._
import opencl.ir.{MapSeq, MapLcl, MapWrg, Float}

class VectorScaling(override val name: String,
                    override val defaultSizes: Seq[Int],
                    override val delta: Float,
                    override val f: Seq[(String, Seq[Lambda])]) extends Benchmark(name, defaultSizes, f, delta) {

  override def runScala(inputs: Any*): Array[Float] = {
    inputs(0).asInstanceOf[Array[Float]].map(_ * inputs(1).asInstanceOf[Float])
  }

  override def generateInputs(): Seq[Any] = {
    val inputSize = inputSizes()(0)

    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f

    Seq(inputArray, alpha)
  }

  override def inputSizes(): Seq[Int] = {
    if (size.value.length == 1) size.value else defaultSizes
  }
}

object VectorScaling {

  val vectorScal = fun( ArrayType(Float, Var("N")), Float, (input, alpha) =>
    Join() o MapWrg(
      Join() o MapLcl(MapSeq(
        fun( x => mult(alpha, x) )
      )) o Split(4)
    ) o Split(1024) $ input
  )

  def apply() = new VectorScaling("Vector Scaling",
    Seq(1024),
    0.001f,
    Seq(("simple", Seq(vectorScal))))

  def main(args: Array[String]) = {
    VectorScaling().run(args)
  }
}
