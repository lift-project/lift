package opencl.generator

import ir.UserFunDef._
import ir._
import opencl.executor.{Execute, Executor, Compile}
import opencl.ir._
import opencl.ir.IndexFunction._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestBarrier {
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

class TestBarrier {
  @Test def basicBarrier(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    // Barrier should be removed
    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join() o MapWrg(Barrier() o MapLcl(id)) o Split(128) $ input
    )

    val code = Compile(f)
    val (output, _) = Execute(inputSize)(code, f, input, inputSize)

    assertFalse(code.containsSlice("barrier"))
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def reorderGlobalLast(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // Last barrier should be removed
    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join() o MapWrg(Barrier() o Gather(reverse)(MapLcl(id)) o Barrier() o MapLcl(id)) o Split(128) $ input
    )

    val code = Compile(f)
    val (output, _) = Execute(inputSize)(code, f, input, inputSize)

    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def reorderGlobalFirst(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // All barriers should be removed
    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join() o MapWrg(Barrier() o MapLcl(id) o Barrier() o Gather(reverse)(MapLcl(id))) o Split(128) $ input
    )

    val code = Compile(f)
    val (output, _) = Execute(inputSize)(code, f, input, inputSize)

    assertEquals(0, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def reorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // First barrier should be eliminated
    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join() o MapWrg(Barrier() o toGlobal(MapLcl(id)) o Barrier() o toLocal(Gather(reverse)(MapLcl(id)))) o Split(128) $ input
    )

    val code = Compile(f)
    val (output, _) = Execute(inputSize)(code, f, input, inputSize)

    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def reorderLastLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // No barriers should be eliminated
    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join() o MapWrg(Barrier() o toGlobal(Gather(reverse)(MapLcl(id))) o Barrier() o toLocal(MapLcl(id))) o Split(128) $ input
    )

    val code = Compile(f)
    val (output, _) = Execute(inputSize)(code, f, input, inputSize)

    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def reorderWriteLastLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // First barrier should be eliminated
    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join() o MapWrg(Barrier() o toGlobal(Scatter(reverse)(MapLcl(id))) o Barrier() o toLocal(MapLcl(id))) o Split(128) $ input
    )

    val code = Compile(f)
    val (output, _) = Execute(inputSize)(code, f, input, inputSize)

    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }


  @Test def noReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // First barrier should be eliminated
    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join() o MapWrg(Barrier() o toGlobal(MapLcl(id)) o Barrier() o toLocal(MapLcl(id))) o Split(128) $ input
    )

    val code = Compile(f)
    val (output, _) = Execute(inputSize)(code, f, input, inputSize)

    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def noLoopNoReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // All barriers should be eliminated
    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join() o MapWrg(Barrier() o toGlobal(MapLcl(id)) o Barrier() o toLocal(MapLcl(id))) o Split(128) $ input
    )

    val inputs = Seq(input, inputSize)
    val code = TestUtils.compile(f, inputs, 128, inputSize, (true, true))
    val (output, _) = Execute(128, inputSize)(
      code, f, inputs:_*)

    assertEquals(0, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def noLoopReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // All barriers should be eliminated
    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join() o MapWrg(Barrier() o toGlobal(MapLcl(id)) o Barrier() o toLocal(Gather(reverse)(MapLcl(id)))) o Split(128) $ input
    )

    val inputs = Seq(input, inputSize)
    val code = TestUtils.compile(f, inputs, 128, inputSize, (true, true))
    val (output, _) = Execute(128, inputSize)(
      code, f, inputs:_*)

    assertEquals(0, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def noLoopReorderLastLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // Last barrier should be eliminated
    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join() o MapWrg(Barrier() o toGlobal(Gather(reverse)(MapLcl(id))) o Barrier() o toLocal(MapLcl(id))) o Split(128) $ input
    )

    val inputs = Seq(input, inputSize)
    val code = TestUtils.compile(f, inputs, 128, inputSize, (true, true))
    val (output, _) = Execute(128, inputSize)(
      code, f, inputs:_*)

    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }
}
