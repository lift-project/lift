package benchmarks

import apart.arithmetic.{StartFromRange, Var}
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

import scala.util.Random

class Hotspot3D(override val f: Seq[(String, Array[Lambda])]) extends Benchmark("Hotspot3D", Seq(512, 512, 8), f, 0.01f) {

  override def generateInputs(): Seq[Any] = {
    val input = Array.tabulate(512, 512, 8) { (i, j, k) => Random.nextFloat() }

    Seq(input)
  }

  override def globalSize: Array[Int] = {
    Array(512,512,1) // 8192
  }

  override def localSize: Array[Int] = {
    Array(64,4,1)
  }
  // no scala checks
  override def runScala(inputs: Any*): Array[Float] = {
    throw new IllegalArgumentException("no scala check defined for this benchmark")
  }

  override def runOpenCL(inputs: Any*): (Array[Float], Double) = {
    val (output, runtime) = super.runOpenCL(inputs:_*)
    (Array(output.sum), runtime)
  }
}

object Hotspot3D{

  //ArrayType(ArrayType(Float, N), N),
  def hotspot(): Lambda = {
    val N = Var("N", StartFromRange(2))
    fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      (input) => {
        MapSeq(MapGlb(1)(MapGlb(0)( \(nbh =>
          toGlobal(MapSeq(id)) o
            ReduceSeq(add, 0.0f) o Join() o Join() $ nbh)
        ))) o Slide3D(3,1) o Pad3D(1,1,1, Pad.Boundary.Clamp) $ input
      }
    )
  }

  def apply() = new Hotspot3D(
    Seq(
      ("HOTSPOT", Array[Lambda](hotspot()))
    ))

  def main(args: Array[String]): Unit = {
    Hotspot3D().run(args)
  }
}
