package opencl.generator

import arithmetic.Var
import ir.UserFunDef._
import ir.{Split, Join, ArrayType, fun}
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

    val (output, runtime) = Execute(inputSize)(f, input, inputSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

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

    val (output, runtime) = Execute(inputSize)(code, f, input, inputSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

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

    val (output, runtime) = Execute(inputSize)(code, f, input, inputSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

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
      in => Join() o MapWrg(Join() o Barrier() o MapLcl(toGlobal(MapUnroll(id))
        o toPrivate(MapUnroll(plusOne))) o Split(1))
        o Split(4) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, input, inputSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def mapUnroll(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg( Join() o MapLcl(MapUnroll(plusOne)) o Split(4))
        o Split(128) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, input, inputSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def privateArray(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg( Join() o MapLcl(toGlobal(MapUnroll(id)) o toPrivate(MapUnroll(plusOne))) o Split(4))
        o Split(128) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, input, inputSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def privateLocalGlobal(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg( toGlobal(MapLcl(id)) o Join() o Barrier() o
        MapLcl(toLocal(MapUnroll(id)) o toPrivate(MapUnroll(plusOne))) o Split(4) o
        Barrier() o toLocal(MapLcl(id))
      ) o Split(128) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, input, inputSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def privateLocalGlobal2(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg( Join() o
        MapLcl(toGlobal(MapUnroll(id)) o toPrivate(MapUnroll(plusOne))) o Split(4) o
        Barrier() o toLocal(MapLcl(id))
      ) o Split(128) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, input, inputSize)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }
}
