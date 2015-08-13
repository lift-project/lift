package ir.ast

import apart.arithmetic.Var
import ir.ArrayType
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{toGlobal, ReduceSeq, MapSeq, MapGlb}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestUnpack {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }
}

class TestUnpack {
  @Test
  def splitUnpackId(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i)

    val gold = input

    val N = Var("N")

    val f = fun(
      ArrayType(Int, N),
      input =>
        MapGlb(
          idI o Unpack()
        ) o Split(1) $ input
    )

    val (result: Array[Int], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result)
  }

  @Test
  def unpackHead(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i)

    val gold = input.grouped(16).toArray.map(_.head)

    val N = Var("N")

    val f = fun(
      ArrayType(Int, N),
      input =>
        MapGlb(
          idI o Unpack() o Head()
        ) o Split(16) $ input
    )

    val (result: Array[Int], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result)
  }

  @Test
  def reverseUnpack(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(16).toArray.map(_.sum).reverse

    val N = Var("N")

    val f = fun(
      ArrayType(Float, N),
      input =>
        Scatter(reverse) o MapGlb(
          Unpack() o toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)
        ) o Split(16) $ input
    )

    val (result: Array[Float], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def unpack2D(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i)

    val gold = input

    val N = Var("N")

    val f = fun(
      ArrayType(Int, N),
      input =>
        Join() o MapGlb(
          MapSeq(idI) o Unpack() o Split(128)
        ) o Split(128) $ input
    )

    val (result: Array[Int], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result)
  }

  @Test
  def unpack2DwithWrite(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i)

    val gold = input

    val N = Var("N")

    val f = fun(
      ArrayType(Int, N),
      input =>
        Join() o MapGlb(
          MapSeq(idI) o Unpack() o Split(128) o MapSeq(idI)
        ) o Split(128) $ input
    )

    val (result: Array[Int], _) = Execute(inputSize)(f, input)

    assertArrayEquals(gold, result)
  }
}
