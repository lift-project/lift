package benchmarks

import ir.UserFunDef._
import ir._
import opencl.ir.{MapSeq, MapLcl, MapWrg, Float}

class VectorScaling extends Benchmark {

  override val name: String = "Vector Scaling"
  override val defaultSizes: Seq[Int] = Seq(1024)
  override val delta = 0.001f
  override val f: Lambda = fun( ArrayType(Float, Var("N")), Float, (input, alpha) =>
    Join() o MapWrg(
      Join() o MapLcl(MapSeq(
        fun( x => mult(alpha, x) )
      )) o Split(4)
    ) o Split(1024) $ input
  )

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

object VectorScaling extends App{
  def apply() = new VectorScaling

  VectorScaling().run(args)
}
