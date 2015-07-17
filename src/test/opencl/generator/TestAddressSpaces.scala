package opencl.generator

import arithmetic._
import ir._
import ir.ast.UserFunDef._
import ir.ast._
import opencl.executor.{Compile, Execute, Executor}
import opencl.ir._
import opencl.ir.ast._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestAddressSpaces {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }
}

class TestAddressSpaces {
  @Test def localGlobalMemory(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(Barrier() o toGlobal(MapLcl(plusOne)) o
        Barrier() o toLocal(MapLcl(id)))
        o Split(4) $ in
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def staticLocalMemory(): Unit = {
    val orig = AllocateLocalMemoryStatically()
    AllocateLocalMemoryStatically(allocateStatically = true)

    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(Barrier() o toGlobal(MapLcl(plusOne)) o
        Barrier() o toLocal(MapLcl(id)))
        o Split(4) $ in
    )

    val code = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, OpenCLGenerator.Kernel.memory.length)

    AllocateLocalMemoryStatically(orig)
  }

  @Test def nonStaticLocalMemory(): Unit = {
    val orig = AllocateLocalMemoryStatically()
    AllocateLocalMemoryStatically(allocateStatically = false)

    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(Barrier() o toGlobal(MapLcl(plusOne)) o
        Barrier() o toLocal(MapLcl(id)))
        o Split(4) $ in
    )

    val code = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(3, OpenCLGenerator.Kernel.memory.length)

    AllocateLocalMemoryStatically(orig)
  }

  @Test def privateGlobalMemory(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(Join() o Barrier() o MapLcl(toGlobal(MapSeq(id))
        o toPrivate(MapSeq(plusOne))) o Split(1))
        o Split(4) $ in
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def privateArray(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg( Join() o MapLcl(toGlobal(MapSeq(id)) o toPrivate(MapSeq(plusOne))) o Split(4))
        o Split(128) $ in
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold: Array[Float], output, 0.0f)
  }

  @Test def privateLocalGlobal(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg( toGlobal(MapLcl(id)) o Join() o Barrier() o
        MapLcl(toLocal(MapSeq(id)) o toPrivate(MapSeq(plusOne))) o Split(4) o
        Barrier() o toLocal(MapLcl(id))
      ) o Split(128) $ in
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold: Array[Float], output, 0.0f)
  }

  @Test def privateLocalGlobal2(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg( Join() o
        MapLcl(toGlobal(MapSeq(id)) o toPrivate(MapSeq(plusOne))) o Split(4) o
        Barrier() o toLocal(MapLcl(id))
      ) o Split(128) $ in
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def reduceOnPrivateArray(): Unit = {
    val inputSize = 512
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayType(Float, Var("N")), (in) => {
      Join() o MapGlb(
        toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o toPrivate(MapSeq(id))
      ) o Split(4) $ in
    })

    val (output: Array[Float], _) = Execute(inputData.length)( l, inputData)

    assertEquals(inputData.sum, output.sum, 0.0)
  }

  @Test def initArrayValuePrivate2(): Unit = {
    val inputSize = 512
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayType(Float, Var("N")), (in) => {
      Join() o MapGlb( fun(x =>
        toGlobal(MapSeq(id)) o toPrivate(MapSeq(id)) $ Value("0.0f", ArrayType(Float, 4)))
      ) o Split(4) $ in
    })

    val (output: Array[Float], _) = Execute(inputData.length)( l, inputData)

    assertEquals(0.0f, output.sum, 0.0)
  }

  @Test def privateGlobalMemory2(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(Join() o Barrier() o MapLcl(toGlobal(MapSeq(id)))
        o MapLcl(toPrivate(MapSeq(plusOne))) o Split(1))
        o Split(4) $ in
    )

    val (output: Array[Float], _) = Execute(4, inputSize, (true, false))(f, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def privateGlobalMemoryLessThreads(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(Join() o Barrier() o MapLcl(toGlobal(MapSeq(id)))
        o MapLcl(toPrivate(MapSeq(plusOne))) o Split(1))
        o Split(4) $ in
    )

    val (output: Array[Float], _) = Execute(2, inputSize, (true, false))(f, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test(expected = classOf[NotEvaluableException])
  def privateGlobalMemoryThreadsNotSpecified(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(Join() o Barrier() o MapLcl(toGlobal(MapSeq(id)))
        o MapLcl(toPrivate(MapSeq(plusOne))) o Split(1))
        o Split(4) $ in
    )

    Execute(inputSize)(f, input, inputSize)
  }

  @Test def initArrayValuePrivate(): Unit = {
    val inputSize = 512
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayType(Float, Var("N")), (in) => {
      Join() o MapWrg( fun(x =>
        toGlobal(MapLcl(id)) o toPrivate(MapLcl(id)) $ Value("1.0f", ArrayType(Float, 4)))
      ) o Split(4) $ in
    })

    val (output: Array[Float], _) = Execute(4, inputData.length, (true, false))( l, inputData)

    assertArrayEquals(Array.fill(inputSize)(1.0f), output, 0.0f)
  }
}
