package opencl.generator

import arithmetic.Var
import ir.UserFunDef._
import ir._
import opencl.executor.{Executor, Compile, Execute}
import opencl.ir._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestAddressSpaces {
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

    val (output, _) = Execute(inputSize)(f, input, inputSize)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def staticLocalMemory(): Unit = {
    val orig = AllocateStatically()
    AllocateStatically(allocateStatically = true)

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

    val (output, _) = Execute(inputSize)(code, f, input, inputSize)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, OpenCLGenerator.Kernel.memory.length)

    AllocateStatically(orig)
  }

  @Test def nonStaticLocalMemory(): Unit = {
    val orig = AllocateStatically()
    AllocateStatically(allocateStatically = false)

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

    val (output, _) = Execute(inputSize)(code, f, input, inputSize)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(3, OpenCLGenerator.Kernel.memory.length)

    AllocateStatically(orig)
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

    val (output, _) = Execute(inputSize)(f, input, inputSize)

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

    val (output, _) = Execute(inputSize)(f, input, inputSize)

    assertArrayEquals(gold, output, 0.0f)
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

    val (output, _) = Execute(inputSize)(f, input, inputSize)

    assertArrayEquals(gold, output, 0.0f)
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

    val (output, _) = Execute(inputSize)(f, input, inputSize)

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

    val (output, _) = Execute(inputData.length)( l, inputData, inputData.length )

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

    val (output, _) = Execute(inputData.length)( l, inputData, inputData.length )

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

    val (output, _) = Execute(4, inputSize, (true, false))(f, input, inputSize)

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

    val (output, _) = Execute(2, inputSize, (true, false))(f, input, inputSize)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def initArrayValuePrivate(): Unit = {
    val inputSize = 512
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayType(Float, Var("N")), (in) => {
      Join() o MapWrg( fun(x =>
        toGlobal(MapLcl(id)) o MapLcl(toPrivate(id)) $ Value("1.0f", ArrayType(Float, 4)))
      ) o Split(4) $ in
    })

    val (output, _) = Execute(4, inputData.length, (true, false))( l, inputData, inputData.length )

    assertArrayEquals(Array.fill(inputSize)(1.0f), output, 0.0f)
  }
}
