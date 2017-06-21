package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Execute, Executor, Utils}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

object TestBarrier {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
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
      ArrayTypeWSWC(Float, SizeVar("N")),
      input => Join() o MapWrg(MapLcl(id)) o Split(128) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input)

    assertArrayEquals(input, output, 0.0f)
    assertFalse(kernel.containsSlice("barrier"))
  }

  @Test def reorderGlobalLast(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // Last barrier should be removed
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          MapLcl(id) o Gather(reverse) o MapLcl(id)
        ) o Split(128) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def reorderGlobalFirst(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // All barriers should be removed
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          MapLcl(id) o MapLcl(id) o Gather(reverse)
        ) o Split(128) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(0, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def reorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // First barrier should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def reorderLastLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // No barriers should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o Gather(reverse) o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def reorderWriteLastLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // First barrier should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(Scatter(reverse) o MapLcl(id)) o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }


  @Test def noReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // First barrier should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def noLoopNoReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // All barriers should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join()(MapWrg(
          fun(x => toGlobal(MapLcl(id))(toLocal(MapLcl(id))(x)))
        )(Split(128)(input)))
    )

    val inputs = Seq(input)
    val (output, _, kernel) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(0, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def noLoopReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // All barriers should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    val (output, _, kernel) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(0, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def noLoopReorderLastLocal(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // Last barrier should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o Gather(reverse) o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    val (output, _, kernel) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def noLoopReorder2Local(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // Last and middle barriers should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o MapLcl(id) o Gather(reverse) o
          toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    val (output, _, kernel) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def noLoopReorder3Local(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.reverse).flatten.toArray

    // Last barrier should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o Gather(reverse) o
          MapLcl(id) o Gather(reverse) o toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    val (output, _, kernel) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def loopReorder2Local(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // Middle barrier should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o MapLcl(id) o Gather(reverse) o
          toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(128, inputSize)(f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def copyToLocalInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.map(_+_))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => {
        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
          Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0),
            toLocal(MapLcl(id)) $ Get(pairArrays, 1))
        })) $ Zip(a, b)
      }
    )

    val kernel = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def copyToLocalInZipAndReorder(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.toArray.reverse.map(x => x._1 + x._2))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => {
        MapWrg(toGlobal(MapLcl(add)) o Gather(reverse) o fun(pairArrays => {
          Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0),
            toLocal(MapLcl(id)) $ Get(pairArrays, 1))
        })) $ Zip(a, b)
      }
    )

    val kernel = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def copyToLocalAndReorderInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x.reverse,y).zipped.map(_ + _))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => {
        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
          Zip(Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 0),
            toLocal(MapLcl(id)) $ Get(pairArrays, 1))
        })) $ Zip(a, b)
      }
    )

    val kernel = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def copyToLocalAndReorderInZip2(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y.reverse).zipped.map(_ + _))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => {
        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
          Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0),
            Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 1))
        })) $ Zip(a, b)
      }
    )

    val kernel = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Ignore
  @Test def copyToLocalAndReorderBothInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x.reverse,y.reverse).zipped.map(_ + _))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => {
        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
          Zip(Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 0),
            Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 1))
        })) $ Zip(a, b)
      }
    )

    val kernel = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def copyOneToLocalInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.map(_+_))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays =>
        Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0), Get(pairArrays, 1))
      )) $ Zip(a, b)
    )

    val kernel = Compile(f)

    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def doubleNestedMapLcl(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        toGlobal(MapLcl(0)(MapLcl(1)(id))) o
        toLocal(MapLcl(0)(MapLcl(1)(id)))
      )) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(16, 16, inputSize, inputSize,
      (false, false))(kernel, f, input)

    assertArrayEquals(input.flatten.flatten.flatten, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def tripleNestedMapLcl(): Unit = {

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(MapWrg(2)(
        toGlobal(MapLcl(0)(MapLcl(1)(MapLcl(2)(id)))) o
          toLocal(MapLcl(0)(MapLcl(1)(MapLcl(2)(id))))
      ))) $ input
    )

    val kernel = Compile(f)

    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def tripleNestedMapLclWithScatter(): Unit = {

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(MapWrg(2)(
        toGlobal(MapLcl(0)(MapLcl(1)(MapLcl(2)(id)))) o
          toLocal(MapLcl(0)(MapLcl(1)(Scatter(reverse) o MapLcl(2)(id))))
      ))) $ input
    )

    val kernel = Compile(f)

    println(kernel)

    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def doubleNestedMapLclWithReorder(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.map(_.reverse)))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        toGlobal(MapLcl(0)(MapLcl(1)(id))) o
          Map(Gather(reverse)) o
          toLocal(MapLcl(0)(MapLcl(1)(id)))
      )) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(16, 16, inputSize, inputSize,
      (false, false))(kernel, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def doubleNestedMapLclWithReorder2(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.map(_.reverse)))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        toGlobal(MapLcl(0)(MapLcl(1)(id))) o
          toLocal(MapLcl(0)(Gather(reverse) o MapLcl(1)(id)))
      )) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(16, 16, inputSize, inputSize,
      (false, false))(kernel, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def testIsGather(): Unit = {
    val call = (toGlobal(MapLcl(0)(MapLcl(1)(id) o Gather(reverse))) $ Param()).asInstanceOf[FunCall]

    assertTrue(BarrierElimination.isPattern(call, classOf[Gather]))
  }

  @Test
  def doubleNestedMapLclWithReorder3(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.map(_.reverse)))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        toGlobal(MapLcl(0)(MapLcl(1)(id) o Gather(reverse))) o
          toLocal(MapLcl(0)(MapLcl(1)(id)))
      )) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(16, 16, inputSize, inputSize,
      (false, false))(kernel, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def tail(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.tail).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapWrg(
          MapLcl(id) o Tail() o MapLcl(id)
        ) o Split(128) $ input
    )

    val kernel = Compile(f)
    val (result: Array[Float], _) = Execute(inputSize)(kernel, f, input)

    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def tailInLocal(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.tail).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o Tail() o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    val kernel = Compile(f)
    val (result: Array[Float], _) = Execute(inputSize)(kernel, f, input)

    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
    assertArrayEquals(gold, result, 0.0f)
  }

  @Test(expected = classOf[IllegalKernel])
  def invalidKernel(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize/2,
      inputSize/2)(util.Random.nextInt(5).toFloat)

    val N = SizeVar("N")

    // Should have a barrier, but not all threads take it
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 16), 16), N), N),
      input => MapWrg(0)(MapWrg(1)(
        MapLcl(0)(MapLcl(1)(id) o Gather(reverse) o MapLcl(1)(id))
      )) $ input
    )

    Execute(32, 8, inputSize, inputSize, (true, true))(f, input)
  }

  @Test
  def doubleNestedMapLclWithReorderGlobalMem(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.map(_.reverse)))

    val N = SizeVar("N")

    // Should have a barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        MapLcl(0)(MapLcl(1)(id)) o
          MapLcl(0)(Gather(reverse) o MapLcl(1)(id))
      )) $ input
    )

    val kernel = Compile(f)

    val (output: Array[Float], _) = Execute(16, 16, inputSize, inputSize,
      (false, false))(kernel, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def doubleNestedMapLclWithReorderGlobalMem3(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.reverse))

    val N = SizeVar("N")

    // Should have a barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        MapLcl(0)(MapLcl(1)(id)) o
          Scatter(reverse) o MapLcl(0)(MapLcl(1)(id))
      )) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(16, 16, inputSize, inputSize,
      (false, false))(kernel, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def doubleNestedMapLclWithReorderGlobalMem2(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.map(_.reverse)))

    val N = SizeVar("N")

    // Should have a barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        MapLcl(0)(MapLcl(1)(id) o Gather(reverse)) o
          MapLcl(0)(MapLcl(1)(id))
      )) $ input
    )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(16, 16, inputSize, inputSize,
      (false, false))(kernel, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Ignore
  @Test def reorderInLocalButSequential(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).map(_.grouped(16).map(_.reverse)).
      flatten.flatten.toArray

     // First barrier should be eliminated
     val f = fun(
       ArrayTypeWSWC(Float, SizeVar("N")),
       input =>
         Join() o MapWrg(
           Join() o toGlobal(MapLcl(MapSeq(id))) o Map(Gather(reverse))
           o toLocal(MapLcl(MapSeq(id))) o Split(16)
         ) o Split(128) $ input
     )

    val kernel = Compile(f)
    val (output: Array[Float], _) = Execute(inputSize)(kernel, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def tupleInside2MapLcl(): Unit = {
    val innerSize = 16

    val N = SizeVar("N")

    // Should have 1 barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, innerSize), innerSize), N), N),
      input => MapWrg(0)(MapWrg(1)(
        fun(x => toGlobal(MapLcl(0)(MapLcl(1)(id))) $ Get(x, 0)) o
        fun(x =>
          Unzip() o toLocal(MapLcl(1)(fun(pair =>
            Unzip() o MapLcl(0)(fun( pair =>
              Tuple(id $ Get(pair, 0), id $ Get(pair, 1))
            )) $ Zip(Get(pair, 0), Get(pair, 1))
          ))) $ Zip(x, x)
        )
      )) $ input
    )

    val kernel = Compile(f, innerSize, innerSize, 1)

    println(kernel)

    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def tupleInsideMapLcl(): Unit = {
    val innerSize = 16

    val N = SizeVar("N")

    // Should have 1 barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, innerSize), innerSize), N), N),
      input => MapWrg(0)(MapWrg(1)(
        fun(x => toGlobal(MapLcl(1)(MapLcl(0)(id))) $ Get(x, 0)) o
        fun(x =>
        Unzip() o toLocal(MapLcl(1)(fun(pair =>
          Tuple(MapLcl(0)(id) $ Get(pair, 0), MapLcl(0)(id) $ Get(pair, 1)))
        )) $ Zip(x, x))
      )) $ input)

    val kernel = Compile(f, innerSize, innerSize, 1)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def tupleInsideMapLcl2(): Unit = {
    val innerSize = 16

    val N = SizeVar("N")

    // Should have 1 barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, innerSize), innerSize), N), N),
      input => MapWrg(0)(MapWrg(1)(
        toGlobal(MapLcl(1)(MapLcl(0)(id))) o Get(0) o
        fun(x =>
        Unzip() o toLocal(MapLcl(1)(fun(pair =>
          Tuple(MapLcl(0)(id) $ Get(pair, 0), MapLcl(0)(id) $ Get(pair, 1)))
        )) $ Zip(x, x))
      )) $ input)

    val kernel= Compile(f, innerSize, innerSize, 1)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def tupleWithAsVectorInsideMapLcl(): Unit = {
    val innerSize = 16

    val N = SizeVar("N")

    // Should have 2 barriers
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, innerSize), innerSize), N), N),
      input => MapWrg(0)(MapWrg(1)(fun(x =>
        toGlobal(MapLcl(1)(asScalar() o MapLcl(0)(id.vectorize(4)) o asVector(4))) $ Get(x, 0)) o
        fun(x =>
        Unzip() o toLocal(MapLcl(1)(fun(pair =>
          Tuple(
            asScalar() o MapLcl(0)(id.vectorize(4)) o asVector(4) $ Get(pair, 0),
            asScalar() o MapLcl(0)(id.vectorize(4)) o asVector(4) $ Get(pair, 1))
          )
        )) $ Zip(x, x))
      )) $ input)

    val kernel = Compile(f, innerSize, innerSize, 1)
    assertEquals(2, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test
  def tupleBarrierJustLocal(): Unit = {
    val N = SizeVar("N")

    val f = \(
      ArrayType(Float, N),
      input => MapWrg(\(x =>
        toGlobal(MapLcl(add)) o
          Gather(reverse) o
          MapLcl(\(y => Tuple(toLocal(id) $ y._0, toLocal(id) $ y._1))) $
          Zip(x, x)
      )) o Split(32) $ input
    )

    val kernel = Compile(f, NDRange(32), NDRange(N))
    println(kernel)
    assertEquals(0, "CLK_GLOBAL_MEM_FENCE".r.findAllMatchIn(kernel).length)
    assertEquals(1, "CLK_LOCAL_MEM_FENCE".r.findAllMatchIn(kernel).length)
  }

  @Test
  def tupleBarrierJustGlobal(): Unit = {
    val N = SizeVar("N")

    val f = \(
      ArrayType(Float, N),
      input => MapWrg(\(x =>
        toGlobal(MapLcl(add)) o
          Gather(reverse) o
          MapLcl(\(y => Tuple(id $ y._0, id $ y._1))) $
          Zip(x, x)
      )) o Split(32) $ input
    )

    val kernel = Compile(f, NDRange(32), NDRange(N))
    println(kernel)
    assertEquals(1, "CLK_GLOBAL_MEM_FENCE".r.findAllMatchIn(kernel).length)
    assertEquals(0, "CLK_LOCAL_MEM_FENCE".r.findAllMatchIn(kernel).length)
  }

    @Test
  def tupleBarrierBoth(): Unit = {
    val N = SizeVar("N")

    val f = \(
      ArrayType(Float, N),
      input => MapWrg(\(x =>
        toGlobal(MapLcl(add)) o
          Gather(reverse) o
          MapLcl(\(y => Tuple(toLocal(id) $ y._0, id $ y._1))) $
          Zip(x, x)
      )) o Split(32) $ input
    )

    val kernel = Compile(f, NDRange(32), NDRange(N))
      println(kernel)
    assertEquals(1, "CLK_GLOBAL_MEM_FENCE".r.findAllMatchIn(kernel).length)
    assertEquals(1, "CLK_LOCAL_MEM_FENCE".r.findAllMatchIn(kernel).length)
  }

}
