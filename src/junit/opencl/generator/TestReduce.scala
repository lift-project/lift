package junit.opencl.generator

import org.junit.Test
import opencl.generator._
import opencl.ir._
import ir._

class TestReduce {

  implicit def IntToCst(cst: Int) : Cst = new Cst(cst) // try to get this away from here ...

  val sumUp = UserFun("sumUp", "int sumUp(int x, int y) { return x+y; }", TupleType(Int, Int), Int)

  val id = UserFun("id", "int id(int x) { return x; }", Int, Int)

  val N = Cst(1048576) // Var("N")
  val input = Input(Var("x"), ArrayType(Int, N))

  @Test def SIMPLE_REDUCE() {

    val kernel = Join() o Join() o MapWrg(
      MapLcl(ReduceSeq(sumUp))
    ) o Split(Cst(128)) o Split(Cst(2048)) o input

    val kernelCode = OpenCLGenerator.compile(kernel)
    println(kernelCode)

  }

  @Test def NVIDIA_A() {

    val firstKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
      Iterate(7)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2) ) o
      Join() o toLocal(MapLcl(MapSeq(id))) o Split(1)
    ) o Split(128) o input

    val tmp = Input(Var("tmp"), ArrayType(Int, N / 128))

    val secondKernel = Join() o MapWrg(
      Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
      Iterate(3)(Join() o MapLcl(ReduceSeq(sumUp)) o Split(2)) o
      Join() o toLocal(MapLcl(MapSeq(id))) o Split(1)
    ) o Split(8) o tmp

    val firstKernelCode = OpenCLGenerator.compile(firstKernel)
    //println(firstKernelCode)

    val secondKernelCode = OpenCLGenerator.compile(secondKernel)
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

    val tmp = Input(Var("tmp"), ArrayType(Int, N / 262144))

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

    val tmp = Input(Var("tmp"), ArrayType(Int, N / 262144))

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

    val tmp = Input(Var("tmp"), ArrayType(Int, N / 512))

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

    val tmp = Input(Var("tmp"), ArrayType(Int, N / 2048))

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

    val tmp = Input(Var("tmp"), ArrayType(Int, N / 2048))

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

    val tmp = Input(Var("tmp"), ArrayType(Int, N / 8192))

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

    val tmp = Input(Var("tmp"), ArrayType(Int, N / 8192))

    val secondKernel = Join() o MapWrg(
      Join() o MapLcl(
        ReduceSeq(sumUp)
      ) o Split(2048)
    ) o Split(2048) o tmp

    val firstKernelCode = OpenCLGenerator.compile(firstKernel)
    val secondKernelCode = OpenCLGenerator.compile(secondKernel)

  }

}
