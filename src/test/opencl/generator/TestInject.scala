package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Executor, Utils}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestInject {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    Executor.shutdown()
  }
}

class TestInject {

  @Test def injectExactlyOneIterationVariable(): Unit = {

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => MapWrg( MapLcl(id)) o Split(128) $ in
    )

    val code = Compile(f, 128,1,1, N, 1, 1, collection.immutable.Map())

    assertEquals(0, "for\\s*\\(".r.findAllMatchIn(code).length)
    assertEquals(0, "if\\s*\\(".r.findAllMatchIn(code).length)
  }

  @Test def injectExactlyOneIteration(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapWrg( MapLcl(id)) o Split(128) $ in
    )

    val inputs = Seq(input)
    val (output, runtime, code) = Utils.execute(f, inputs, 128, inputSize, (true, false))


    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(1, "for\\s*\\(".r.findAllMatchIn(code).length)
    assertEquals(0, "if\\s*\\(".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectLessThanOneIteration(): Unit ={
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapWrg( MapLcl(id)) o Split(64) $ in
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, false))

    assertEquals(1, "for\\s*\\(".r.findAllMatchIn(code).length)
    assertEquals(1, "if\\s*\\(".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectMoreThanOneIteration(): Unit ={
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapWrg( MapLcl(id)) o Split(256) $ in
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, false))

    assertEquals(2, "for\\s*\\(".r.findAllMatchIn(code).length)
    assertEquals(0, "if\\s*\\(".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectGroupExactlyOneIteration(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapWrg( MapLcl(id)) o Split(128) $ in
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertEquals(0, "for\\s*\\(".r.findAllMatchIn(code).length)
    assertEquals(0, "if\\s*\\(".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectGroupLessThanOneIteration(): Unit ={
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapWrg( MapLcl(id)) o Split(128) $ in
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize*2, (true, true))

    assertEquals(0, "for\\s*\\(".r.findAllMatchIn(code).length)
    assertEquals(1, "if\\s*\\(".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectGroupMoreThanOneIteration(): Unit ={
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapWrg( MapLcl(id)) o Split(128) $ in
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize/2, (true, true))

    assertEquals(1, "for\\s*\\(".r.findAllMatchIn(code).length)
    assertEquals(0, "if\\s*\\(".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)
  }

  @Test def injectGroupSameExpressionTwice(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapWrg( MapLcl(id)) o Split(128) $ in
    )

    val inputs = Seq(input)
    val (output, _, code) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertEquals(0, "for\\s*\\(".r.findAllMatchIn(code).length)
    assertEquals(0, "if\\s*\\(".r.findAllMatchIn(code).length)
    assertArrayEquals(input, output, 0.0f)

    val (output2, _, code2) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertEquals(0, "for\\s*\\(".r.findAllMatchIn(code2).length)
    assertEquals(0, "if\\s*\\(".r.findAllMatchIn(code2).length)
    assertArrayEquals(input, output2, 0.0f)
  }
}
