package opencl.generator

import arithmetic.Var
import ir._
import ir.ast._
import opencl.executor.{Executor, Utils}
import opencl.ir._
import opencl.ir.ast.{Barrier, MapLcl, MapWrg}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestInject {
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

class TestInject {
  @Test def injectExactlyOneIteration(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapWrg(Barrier() o MapLcl(id)) o Split(128) $ in
    )

    val inputs = Seq(input)
    val (output, runtime, code) = Utils.execute(f, inputs, 128, inputSize, (true, false))


    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(1, "for".r.findAllMatchIn(code).length)
    assertEquals(0, "if".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectLessThanOneIteration(): Unit ={
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapWrg(Barrier() o MapLcl(id)) o Split(64) $ in
    )

    val inputs = Seq(input)
    val (output, runtime, code) = Utils.execute(f, inputs, 128, inputSize, (true, false))

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(1, "for".r.findAllMatchIn(code).length)
    assertEquals(1, "if".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectMoreThanOneIteration(): Unit ={
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapWrg(Barrier() o MapLcl(id)) o Split(256) $ in
    )

    val inputs = Seq(input)
    val (output, runtime, code) = Utils.execute(f, inputs, 128, inputSize, (true, false))

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(2, "for".r.findAllMatchIn(code).length)
    assertEquals(0, "if".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectGroupExactlyOneIteration(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapWrg(Barrier() o MapLcl(id)) o Split(128) $ in
    )

    val inputs = Seq(input)
    val (output, runtime, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(0, "for".r.findAllMatchIn(code).length)
    assertEquals(0, "if".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectGroupLessThanOneIteration(): Unit ={
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapWrg(Barrier() o MapLcl(id)) o Split(128) $ in
    )

    val inputs = Seq(input)
    val (output, runtime, code) = Utils.execute(f, inputs, 128, inputSize*2, (true, true))

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(0, "for".r.findAllMatchIn(code).length)
    assertEquals(1, "if".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectGroupMoreThanOneIteration(): Unit ={
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapWrg(Barrier() o MapLcl(id)) o Split(128) $ in
    )

    val inputs = Seq(input)
    val (output, runtime, code) = Utils.execute(f, inputs, 128, inputSize/2, (true, true))

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(1, "for".r.findAllMatchIn(code).length)
    assertEquals(0, "if".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectGroupSameExpressionTwice(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapWrg(Barrier() o MapLcl(id)) o Split(128) $ in
    )

    val inputs = Seq(input)
    val (output, runtime, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(0, "for".r.findAllMatchIn(code).length)
    assertEquals(0, "if".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)

    val (output2, runtime2, code2) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    println("output.size = " + output2.length)
    println("output(0) = " + output2(0))
    println("runtime = " + runtime2)

    assertEquals(0, "for".r.findAllMatchIn(code2).length)
    assertEquals(0, "if".r.findAllMatchIn(code2).length)
    assertArrayEquals(input, output2, 0.0f)
  }
}
