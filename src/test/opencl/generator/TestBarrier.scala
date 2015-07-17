package opencl.generator

import arithmetic.Var
import ir._
import ir.ast.UserFunDef._
import ir.ast.IndexFunction._
import ir.ast._
import opencl.executor.{Compile, Execute, Executor, Utils}
import opencl.ir._
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
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

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
      input =>
        Join() o MapWrg(
          Barrier() o MapLcl(id) o Gather(reverse) o Barrier() o MapLcl(id)
        ) o Split(128) $ input
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

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
      input =>
        Join() o MapWrg(
          Barrier() o MapLcl(id) o Barrier() o MapLcl(id) o Gather(reverse)
        ) o Split(128) $ input
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

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
      input =>
        Join() o MapWrg(
          Barrier() o toGlobal(MapLcl(id)) o Barrier() o toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

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
      input =>
        Join() o MapWrg(
          Barrier() o toGlobal(MapLcl(id)) o Gather(reverse) o Barrier() o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

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
      input =>
        Join() o MapWrg(
          Barrier() o toGlobal(Scatter(reverse) o MapLcl(id)) o Barrier() o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

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
      input =>
        Join() o MapWrg(
          Barrier() o toGlobal(MapLcl(id)) o Barrier() o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(code, f, input)

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
      input =>
        Join() o MapWrg(
          Barrier() o toGlobal(MapLcl(id)) o Barrier() o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

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
      input =>
        Join() o MapWrg(
          Barrier() o toGlobal(MapLcl(id)) o Barrier() o toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

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
      input =>
        Join() o MapWrg(
          Barrier() o toGlobal(MapLcl(id)) o Gather(reverse) o Barrier() o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def noLoopReorder2Local(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // Last and middle barriers should be eliminated
    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join() o MapWrg(
          Barrier() o toGlobal(MapLcl(id)) o Barrier() o MapLcl(id) o Gather(reverse) o Barrier() o
          toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertEquals(1, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def noLoopReorder3Local(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // Last barrier should be eliminated
    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join() o MapWrg(
          Barrier() o toGlobal(MapLcl(id)) o Gather(reverse) o Barrier() o
          MapLcl(id) o Gather(reverse) o Barrier() o toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def loopReorder2Local(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // Middle barrier should be eliminated
    val f = fun(
      ArrayType(Float, new Var("N")),
      input =>
        Join() o MapWrg(
          Barrier() o toGlobal(MapLcl(id)) o Barrier() o MapLcl(id) o Gather(reverse) o Barrier() o
          toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val code = Compile(f)
    val (output: Array[Float], _) = Execute(128, inputSize)(f, input)

    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def copyToLocalInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.map(_+_))

    val N = Var("N")

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), N),
      (a, b) => {
        MapWrg(Barrier() o toGlobal(MapLcl(add)) o fun(pairArrays => {
          Zip(Barrier() o toLocal(MapLcl(id)) $ Get(pairArrays, 0), Barrier() o
          toLocal(MapLcl(id)) $ Get(pairArrays, 1))
        })) $ Zip(a, b)
      }
    )

    val code = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(code, f, input, input)

    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def copyOneToLocalInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.map(_+_))

    val N = Var("N")

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), N),
      (a, b) => MapWrg(Barrier() o toGlobal(MapLcl(add)) o fun(pairArrays =>
        Zip(Barrier() o toLocal(MapLcl(id)) $ Get(pairArrays, 0), Get(pairArrays, 1))
      )) $ Zip(a, b)
    )

    val code = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(code, f, input, input)

    assertEquals(2, "barrier".r.findAllMatchIn(code).length)
    assertArrayEquals(gold.flatten, output, 0.0f)
  }
}
