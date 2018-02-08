package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

object TestAddressSpaces extends TestWithExecutor

class TestAddressSpaces {

  @Test
  def simple(): Unit = {
    val N = SizeVar("N")
    val f = fun(
      ArrayTypeWSWC(Float, N),
      in =>
         MapGlb(id) $ in
    )

    TypeChecker(f)
    InferOpenCLAddressSpace(f)
    OpenCLMemoryAllocator(f)

    assertEquals(GlobalMemory, f.body.addressSpace)
  }

  @Test
  def globalPrivate(): Unit = {
    val N = SizeVar("N")
    val f = fun(
      ArrayTypeWSWC(Float, N),
      in =>
        MapGlb(toGlobal(id) o toPrivate(id)) $ in
    )

    TypeChecker(f)
    InferOpenCLAddressSpace(f)
    OpenCLMemoryAllocator(f)

    assertEquals(GlobalMemory, f.body.addressSpace)
  }

  @Test
  def globalPrivateFixedSizeArray(): Unit = {
    val N = SizeVar("N")
    val f = fun(
      ArrayTypeWSWC(Float, N),
      in =>
        Join() o MapGlb(toGlobal(MapSeq(id)) o toPrivate(MapSeq(id))) o Split(16) $ in
    )

    TypeChecker(f)
    InferOpenCLAddressSpace(f)

    assertEquals(GlobalMemory, f.body.addressSpace)
  }

  @Test
  def globalPrivateFixedSizeWithReorder(): Unit = {
        val N = SizeVar("N")
    val f = fun(
      ArrayTypeWSWC(Float, N),
      in =>
        Join() o MapGlb(Scatter(reverse) o toGlobal(MapSeq(id)) o toPrivate(MapSeq(id))) o Split(16) $ in
    )

    TypeChecker(f)
    InferOpenCLAddressSpace(f)

    assertEquals(GlobalMemory, f.body.addressSpace)
  }

  @Test
  def globalPrivateFixedSizeWithReorder2(): Unit = {
    val N = SizeVar("N")
    val f = fun(
      ArrayTypeWSWC(Float, N),
      in =>
        Join() o MapGlb(toGlobal(Scatter(reverse) o MapSeq(id)) o toPrivate(MapSeq(id))) o Split(16) $ in
    )

    TypeChecker(f)
    InferOpenCLAddressSpace(f)

    assertEquals(GlobalMemory, f.body.addressSpace)
  }

  @Test def localGlobalMemory(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg( toGlobal(MapLcl(plusOne)) o
         toLocal(MapLcl(id)))
        o Split(4) $ in
    )

    val (output, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def staticLocalMemory(): Unit = {
    val orig = AllocateLocalMemoryStatically()
    AllocateLocalMemoryStatically(allocateStatically = true)

    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg( toGlobal(MapLcl(plusOne)) o
         toLocal(MapLcl(id)))
        o Split(4) $ in
    )

    val kernel = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel, f, input)

    val memories = OpenCLGeneratorNew.getMemories(f)._2

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, memories.length)

    AllocateLocalMemoryStatically(orig)
  }

  @Test def nonStaticLocalMemory(): Unit = {
    val orig = AllocateLocalMemoryStatically()
    AllocateLocalMemoryStatically(allocateStatically = false)

    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg( toGlobal(MapLcl(plusOne)) o
         toLocal(MapLcl(id)))
        o Split(4) $ in
    )

    val kernel = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel, f, input)
    val memories = OpenCLGeneratorNew.getMemories(f)._2

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(3, memories.length)

    AllocateLocalMemoryStatically(orig)
  }

  @Test def privateGlobalMemory(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg(Join() o  MapLcl(toGlobal(MapSeq(id))
        o toPrivate(MapSeq(plusOne))) o Split(1))
        o Split(4) $ in
    )

    val (output, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def privateArray(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg( Join() o MapLcl(toGlobal(MapSeq(id)) o toPrivate(MapSeq(plusOne))) o Split(4))
        o Split(128) $ in
    )

    val (output, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold: Array[Float], output, 0.0f)
  }

  @Test def privateLocalGlobal(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg( toGlobal(MapLcl(id)) o Join() o 
        MapLcl(toLocal(MapSeq(id)) o toPrivate(MapSeq(plusOne))) o Split(4) o
         toLocal(MapLcl(id))
      ) o Split(128) $ in
    )

    val (output, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold: Array[Float], output, 0.0f)
  }

  @Test def privateLocalGlobal2(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg( Join() o
        MapLcl(toGlobal(MapSeq(id)) o toPrivate(MapSeq(plusOne))) o Split(4) o
         toLocal(MapLcl(id))
      ) o Split(128) $ in
    )

    val (output, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def reduceOnPrivateArray(): Unit = {
    val inputSize = 512
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
      Join() o MapGlb(
        toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o toPrivate(MapSeq(id))
      ) o Split(4) $ in
    })

    val (output, _) = Execute(inputData.length)[Array[Float]]( l, inputData)

    assertEquals(inputData.sum, output.sum, 0.0)
  }

  @Test def initArrayValuePrivate2(): Unit = {
    val inputSize = 512
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
      Join() o MapGlb( fun(x =>
        toGlobal(MapSeq(id)) o toPrivate(MapSeq(id)) $ Value("0.0f", ArrayTypeWSWC(Float, 4)))
      ) o Split(4) $ in
    })

    val (output, _) = Execute(inputData.length)[Array[Float]]( l, inputData)

    assertEquals(0.0f, output.sum, 0.0)
  }

  @Test def privateGlobalMemory2(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg(Join() o  MapLcl(toGlobal(MapSeq(id)))
        o MapLcl(toPrivate(MapSeq(plusOne))) o Split(1))
        o Split(4) $ in
    )

    val (output, _) = Execute(4, inputSize, (true, false))[Array[Float]](f, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def privateGlobalMemoryLessThreads(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg(Join() o  MapLcl(toGlobal(MapSeq(id)))
        o MapLcl(toPrivate(MapSeq(plusOne))) o Split(1))
        o Split(4) $ in
    )

    val (output, _) = Execute(2, inputSize, (true, false))[Array[Float]](f, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test(expected = classOf[IllegalKernel])
  def privateGlobalMemoryThreadsNotSpecified(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val  f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg(Join() o  MapLcl(toGlobal(MapSeq(id)))
        o MapLcl(toPrivate(MapSeq(plusOne))) o Split(1))
        o Split(4) $ in
    )

    Execute(inputSize)[Array[Float]](f, input, inputSize)
  }

  @Test def initArrayValuePrivate(): Unit = {
    val inputSize = 512
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayTypeWSWC(Float, SizeVar("N")), (in) => {
      Join() o MapWrg( fun(x =>
        toGlobal(MapLcl(id)) o toPrivate(MapLcl(id)) $ Value("1.0f", ArrayTypeWSWC(Float, 4)))
      ) o Split(4) $ in
    })

    val (output, _) = Execute(4, inputData.length, (true, false))[Array[Float]]( l, inputData)

    assertArrayEquals(Array.fill(inputSize)(1.0f), output, 0.0f)
  }
}
