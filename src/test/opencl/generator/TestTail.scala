package opencl.generator

import lift.arithmetic.SizeVar
import ir.ArrayType
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

object TestTail {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    Executor.shutdown()
  }
}

class TestTail {
  @Test
  def tailBetweenMaps(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.tail).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Tail() o MapSeq(id)
        ) o Split(128) $ input
    )

    val (result: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def tailBetweenMapsScatterBefore(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.reverse.tail).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Tail() o Scatter(reverse) o MapSeq(id)
        ) o Split(128) $ input
    )

    val (result: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Ignore
  @Test
  def tailBetweenMapsScatterAfter(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.tail.reverse).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Scatter(reverse) o Tail() o MapSeq(id)
        ) o Split(128) $ input
    )

    val (result: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Ignore
  @Test
  def tailBetweenMapsScatterBeforeAndAfter(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.reverse.tail.reverse).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Scatter(reverse) o Tail() o Scatter(reverse) o MapSeq(id)
        ) o Split(128) $ input
    )

    val (result: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def scatterAfterIdWithTail(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.tail.reverse).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      input =>
        Join() o MapGlb(
          Scatter(reverse) o MapSeq(id) o Tail()
        ) o Split(128) $ input
    )

    val (result: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def tail(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.tail).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Tail()
        ) o Split(128) $ input
    )

    val (result: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def gatherBeforeTail(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.reverse.tail).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Tail() o Gather(reverse)
        ) o Split(128) $ input
    )

    val (result: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def gatherAfterTail(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.tail.reverse).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Gather(reverse) o Tail()
        ) o Split(128) $ input
    )

    val (result: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def gatherBeforeAndAfter(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.reverse.tail.reverse).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Gather(reverse) o Tail() o Gather(reverse)
        ) o Split(128) $ input
    )

    val (result: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def scatterBeforeGatherAfter(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.reverse.tail.reverse).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Gather(reverse) o Tail() o Scatter(reverse) o MapSeq(id)
        ) o Split(128) $ input
    )

    val (result: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result, 0.0f)
  }
}
