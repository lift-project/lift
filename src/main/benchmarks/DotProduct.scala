package benchmarks

import java.io.{PrintWriter, File, OutputStreamWriter}

import apart.arithmetic.Var
import ir._
import ir.ast._
import ir.printer.{DotPrinter, TikzPrinter}
import opencl.ir._
import opencl.ir.pattern._

class DotProduct(override val name: String,
                 override val defaultInputSizes: Seq[Int],
                 override val delta: Float,
                 override val f: Seq[(String, Array[Lambda])]) extends Benchmark(name, defaultInputSizes, f, delta) {

  override def runScala(inputs: Any*): Array[Float] = {
    Array((inputs(0).asInstanceOf[Array[Float]], inputs(1).asInstanceOf[Array[Float]]).zipped.map(_*_).sum)
  }

  override def generateInputs(): Seq[Any] = {
    val inputSize = inputSizes().head

    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    Seq(leftInputData, rightInputData)
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
        Join() o  MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o Split(4)
      ) o Split(1024) $ Zip(left, right)
    })

  val dotProductCPU1 = fun(ArrayType(Float, N),
    ArrayType(Float, N),(left, right) => {
      Join() o Join() o MapWrg(
         toGlobal(MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f))) o Split(2048)
      )o  Split(2048*128) $ Zip(left, right)
    })

  val dotProductCPU2 = fun (ArrayType(Float, N),(in) => {
    Join() o MapWrg(
      Join() o  MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(128)
    ) o Split(128) $ in

  })

  val dotProduct1 = fun(ArrayType(Float, N),
    ArrayType(Float, N), (left, right) => {
      Join() o Join() o MapWrg(
         toGlobal(MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)))
          o Split(2048) o ReorderStride(128)
      ) o Split(2048*128) $ Zip(left, right)
    })

  val dotProduct2 = fun (ArrayType(Float, N), (in) => {

    Join() o MapWrg(
      Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Iterate(6)(Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)) o
        Join() o  toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o Split(2)
    ) o Split(128) $ in

  })

  val dotProduct3 = fun (ArrayType(Float, N), ArrayType(Float, N), (left, right) => {

    Join() o MapWrg(
      Join() o  MapLcl(toGlobal(MapSeq(id))) o Split(1) o
        Iterate(6)(Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)) o
        Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)
    ) o Split(128) $ Zip(left, right)
  })

  val dpMapWrg =
    Join() o MapWrg(
      id
    ) o Split(128)

  val dpReadGlbToLcl =
    Join() o toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o Split(2)
  val dpWriteLclToGlb =
    Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1)
  val dpIterate =
     Iterate(6)(Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2))

  def apply() = new DotProduct("Dot Product",
    Seq(1024),
    0.001f,
    Seq(("simple", Array[Lambda](dotProductSimple)),
        ("cpu", Array[Lambda](dotProductCPU1, dotProductCPU2)),
        ("gpu", Array[Lambda](dotProduct1, dotProduct2))))

  def main(args: Array[String]) = {
   // DotProduct().run(args)
    println(dotProduct3)
    //new TikzPrinter(new PrintWriter(new File("/home/cdubach/svn/cdubach/papers/pact2016_michel/graph.tkz"))).print(ReduceSeq(add, 0.0f))
    //new DotPrinter(new PrintWriter(new File("/home/cdubach/svn/cdubach/papers/pact2016_michel/graph.dot"))).print(ReduceSeq(add, 0.0f))
    new DotPrinter(new PrintWriter(new File("/home/cdubach/svn/cdubach/papers/pact2016_michel/figures/dpGraph.dot"))).print(dotProduct3)

    new DotPrinter(new PrintWriter(new File("/home/cdubach/svn/cdubach/papers/pact2016_michel/figures/dpMapWrg.dot"))).print(dpMapWrg)
    new DotPrinter(new PrintWriter(new File("/home/cdubach/svn/cdubach/papers/pact2016_michel/figures/dpReadGlbToLcl.dot"))).print(dpReadGlbToLcl)
    new DotPrinter(new PrintWriter(new File("/home/cdubach/svn/cdubach/papers/pact2016_michel/figures/dpWriteLclToGlb.dot"))).print(dpWriteLclToGlb)
    new DotPrinter(new PrintWriter(new File("/home/cdubach/svn/cdubach/papers/pact2016_michel/figures/dpIterate.dot"))).print(dpIterate)
  }
}
