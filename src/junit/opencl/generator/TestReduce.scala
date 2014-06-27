package junit.opencl.generator

import org.junit._
import org.junit.Assert._
import opencl.generator._
import opencl.ir._
import ir._

import opencl.executor._

object TestReduce {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestReduce {

  val sumUp = UserFun("sumUp", "float sumUp(float x, float y) { return x+y; }", TupleType(Float, Float), Float)

  val id = UserFun("id", "float id(float x) { return x; }", Float, Float)

  val N = Var("N")
  val input = Input(Var("x"), ArrayType(Float, N))

  @Test def SIMPLE_REDUCE_FIRST() {

    val kernel = Join() o MapWrg(
      Join() o MapLcl(ReduceSeq(sumUp)) o Split(2048)
    ) o Split(262144) o input

    val kernelCode = OpenCLGenerator.compile(kernel)
    println(kernelCode)

    val outputArray = testSimplePartialReduction(kernelCode, 4194304)

    val finalResult = outputArray.reduce( _ + _)

    println( "Final finished result: " +  finalResult)

  }

  @Test def SIMPLE_REDUCE_SECOND() {

    val kernel = Join() o Join() o  MapWrg(
      MapLcl(ReduceSeq(sumUp))
    ) o Split(128) o Split(2048) o input

    val kernelCode = OpenCLGenerator.compile(kernel)
    println(kernelCode)

    val outputArray = testSimplePartialReduction(kernelCode, 4194304)

    val finalResult = outputArray.reduce( _ + _)

    println( "Final finished result: " +  finalResult)

  }

  @Test def NVIDIA_A() {

    val firstKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
      Iterate(7)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2) ) o
      Join() o toLocal(MapLcl(MapSeq(id))) o Split(1)
    ) o Split(128) o input

    val firstKernelCode = OpenCLGenerator.compile(firstKernel)
    println("Kernel code:")
    println(firstKernelCode)



    val tmp = Input(Var("tmp"), ArrayType(Float, N / 128))

    val secondKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
      Iterate(3)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2)) o
      Join() o toLocal(MapLcl(MapSeq(id))) o Split(1)
    ) o Split(8) o tmp

    //val secondKernelCode = OpenCLGenerator.compile(secondKernel)
    //println(secondKernelCode)

  }

  @Test def NVIDIA_B() {

    val kernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
      Iterate(7)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2)) o
      Join() o toLocal(MapLcl(ReduceSeq(sumUp))) o Split(2)
    ) o Split(256) o input

    val kernelCode = OpenCLGenerator.compile(kernel)

  }

  @Test def NVIDIA_C() {

    val firstKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Join() o MapWarp(Iterate(5)(Join() o MapLane(ReduceSeq(sumUp)) o Split(2))) o Split(32) o
        Iterate(2)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2)) o
        Join() o toLocal(MapLcl(ReduceSeq(sumUp))) o ReorderStride() o Split(2048)
    ) o Split(262144) o input

    val tmp = Input(Var("tmp"), ArrayType(Float, N / 262144))

    val secondKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Iterate(5)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2)) o
        Join() o toLocal(MapLcl(ReduceSeq(sumUp))) o Split(2)
    ) o Split(64) o tmp

    val firstKernelCode = OpenCLGenerator.compile(firstKernel)
    val secondKernelCode = OpenCLGenerator.compile(secondKernel)

  }

  @Test def NVIDIA_C_NO_WARPS() {

    val firstKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Iterate(7)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2)) o
        Join() o toLocal(MapLcl(ReduceSeq(sumUp))) o ReorderStride() o Split(2048)
    ) o Split(262144) o input

    val tmp = Input(Var("tmp"), ArrayType(Float, N / 262144))

    val secondKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Iterate(5)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2)) o
        Join() o toLocal(MapLcl(ReduceSeq(sumUp))) o Split(2)
    ) o Split(64) o tmp

    val firstKernelCode = OpenCLGenerator.compile(firstKernel)
    val secondKernelCode = OpenCLGenerator.compile(secondKernel)

  }

  @Test def AMD_A() {

    val firstKernel = Join() o asScalar() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(Vectorize(4)(id)))) o Split(1) o
      Iterate(8)(Join() o MapLcl(ReduceSeq(Vectorize(4)(sumUp))) o Split(2)) o
      Join() o toLocal(MapLcl(ReduceSeq(Vectorize(4)(sumUp)))) o Split(2)
    ) o asVector(4) o Split(2048) o input

    val tmp = Input(Var("tmp"), ArrayType(Float, N / 512))

    val secondKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Iterate(5)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2)) o
        Join() o toLocal(MapLcl(ReduceSeq(sumUp))) o Split(2)
    ) o Split(64) o tmp

    val firstKernelCode = OpenCLGenerator.compile(firstKernel)
    val secondKernelCode = OpenCLGenerator.compile(secondKernel)

  }

  @Test def NVIDIA_DERIVED() {

    val firstKernel = Join() o Join() o MapWrg(
      toGlobal(MapLcl(Iterate(7)(MapSeq(id) o ReduceSeq(sumUp))) o ReduceSeq(sumUp)) o ReorderStride()
    ) o Split(128) o Split(2048) o input

    val tmp = Input(Var("tmp"), ArrayType(Float, N / 2048))

    val secondKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Iterate(6)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2)) o
        Join() o toLocal(MapLcl(ReduceSeq(sumUp))) o Split(128)
    ) o Split(8192) o tmp

    val firstKernelCode = OpenCLGenerator.compile(firstKernel)
    val secondKernelCode = OpenCLGenerator.compile(secondKernel)

  }

  @Test def AMD_DERIVED() {

    val firstKernel = Join() o asScalar() o Join() o MapWrg(
      MapLcl(MapSeq(Vectorize(2)(id)) o ReduceSeq(Vectorize(2)(sumUp)) o ReorderStride())
    ) o Split(128) o asVector(2) o Split(4096) o input

    val tmp = Input(Var("tmp"), ArrayType(Float, N / 2048))

    val secondKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
        Iterate(6)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2)) o
        Join() o toLocal(MapLcl(ReduceSeq(sumUp))) o Split(128)
    ) o Split(8192) o tmp

    val firstKernelCode = OpenCLGenerator.compile(firstKernel)
    val secondKernelCode = OpenCLGenerator.compile(secondKernel)

  }

  @Test def INTEL_DERIVED() {

    val firstKernel = Join() o MapWrg(
      Join() o asScalar() o Join() o MapWarp(
        MapLane(MapSeq(Vectorize(4)(id)) o ReduceSeq(Vectorize(4)(sumUp)))
      ) o Split(1) o asVector(4) o Split(32768)
    ) o Split(32768) o input

    val tmp = Input(Var("tmp"), ArrayType(Float, N / 8192))

    val secondKernel = Join() o MapWrg(
      Join() o MapLcl(
        ReduceSeq(sumUp)
      ) o Split(2048)
    ) o Split(2048) o tmp

    val firstKernelCode = OpenCLGenerator.compile(firstKernel)
    val secondKernelCode = OpenCLGenerator.compile(secondKernel)

  }

  @Test def INTEL_DERIVED_NO_WARP() {

    val firstKernel = Join() o MapWrg(
      Join() o asScalar() o MapLcl(
        MapSeq(Vectorize(4)(id)) o ReduceSeq(Vectorize(4)(sumUp))
      ) o asVector(4) o Split(32768)
    ) o Split(32768) o input

    val tmp = Input(Var("tmp"), ArrayType(Float, N / 8192))

    val secondKernel = Join() o MapWrg(
      Join() o MapLcl(
        ReduceSeq(sumUp)
      ) o Split(2048)
    ) o Split(2048) o tmp

    val firstKernelCode = OpenCLGenerator.compile(firstKernel)
    val secondKernelCode = OpenCLGenerator.compile(secondKernel)

  }


  private def testSimplePartialReduction(kernelCode: String, inputSize: Int) : Array[Float] = {
    val inputArray = Array.fill(inputSize)(1.0f)
    val inputData = global.input(inputArray)
    val outputData = global.output[Float](inputSize / 2048)

    val args = Array(inputData, outputData, value(inputSize))

    Executor.execute(kernelCode, 128, inputSize, args)

    val outputArray = outputData.asFloatArray()

    val finalResult = outputArray.reduce( _ + _)

    assertEquals(inputArray.reduce( _ + _ ), finalResult, 0.0f)

    args.foreach(_.dispose) // free c++ memory (important!)

    outputArray
  }

}
