package opencl.generator

import ir.ArrayTypeWSWC
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq}
import org.junit.Assert._
import org.junit.{Ignore, Test}

object TestTail extends TestWithExecutor

class TestTail {
  @Test
  def tailBetweenMaps(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.tail).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Tail() o MapSeq(id)
        ) o Split(128) $ input
    )

    val (result, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def tailBetweenMapsScatterBefore(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.reverse.tail).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Tail() o Scatter(reverse) o MapSeq(id)
        ) o Split(128) $ input
    )

    val (result, _) = Execute(inputSize)[Array[Float]](f, input)

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
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Scatter(reverse) o Tail() o MapSeq(id)
        ) o Split(128) $ input
    )

    val (result, _) = Execute(inputSize)[Array[Float]](f, input)

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
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Scatter(reverse) o Tail() o Scatter(reverse) o MapSeq(id)
        ) o Split(128) $ input
    )

    val (result, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def scatterAfterIdWithTail(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.tail.reverse).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapGlb(
          Scatter(reverse) o MapSeq(id) o Tail()
        ) o Split(128) $ input
    )

    val (result, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, result, 0.0f)
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
        Join() o MapGlb(
          MapSeq(id) o Tail()
        ) o Split(128) $ input
    )

    val (result, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def gatherBeforeTail(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.reverse.tail).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Tail() o Gather(reverse)
        ) o Split(128) $ input
    )

    val (result, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def gatherAfterTail(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.tail.reverse).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Gather(reverse) o Tail()
        ) o Split(128) $ input
    )

    val (result, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def gatherBeforeAndAfter(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.reverse.tail.reverse).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Gather(reverse) o Tail() o Gather(reverse)
        ) o Split(128) $ input
    )

    val (result, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def scatterBeforeGatherAfter(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).map(_.reverse.tail.reverse).flatten.toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapGlb(
          MapSeq(id) o Gather(reverse) o Tail() o Scatter(reverse) o MapSeq(id)
        ) o Split(128) $ input
    )

    val (result, _) = Execute(inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, result, 0.0f)
  }
}
