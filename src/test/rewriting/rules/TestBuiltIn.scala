package rewriting.rules

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.ast._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.Rewrite

object TestBuiltIn extends TestWithExecutor

class TestBuiltIn {

  private val N = SizeVar("N")

  private val input = Array.fill(4, 4)(util.Random.nextFloat())
  private val gold = (input.flatten, input.flatten).zipped.map(_*_).sum

  @Test
  def testDot0(): Unit = {

    val input = Array.fill(1, 4)(util.Random.nextFloat())
    val gold = (input.head, input.head).zipped.map(_*_).sum

    val f = fun(
      ArrayTypeWSWC(Float4, 1),
      ArrayTypeWSWC(Float4, 1),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          asScalar() o
          MapSeq(VectorizeUserFun(4, mult)) $ Zip(x, y)
    )

    val g = fun(
      ArrayTypeWSWC(Float4, 1),
      ArrayTypeWSWC(Float4, 1),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          MapSeq(dot) $ Zip(x, y)
    )

    val (outputF: Array[Float], _) = Execute(1, 1)(f, input, input)
    val (outputG: Array[Float], _) = Execute(1, 1)(g, input, input)

    assertEquals(gold, outputF.head, 0.001f)
    assertEquals(gold, outputG.head, 0.001f)
  }

  @Test
  def testDot1(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(Float4, N),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          asScalar() o
          MapSeq(VectorizeUserFun(4, mult)) $ Zip(x, y)
    )

    val g = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(Float4, N),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          MapSeq(dot) $ Zip(x, y)
    )

    val (outputF: Array[Float], _) = Execute(1, 1)(f, input, input)
    val (outputG: Array[Float], _) = Execute(1, 1)(g, input, input)

    assertEquals(gold, outputF.head, 0.001f)
    assertEquals(gold, outputG.head, 0.001f)
  }

  @Test
  def testDotRule(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(Float4, N),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          asScalar() o
          MapSeq(VectorizeUserFun(4, mult)) $ Zip(x, y)
    )

    val g = Rewrite.applyRuleAtId(f, 1, OpenCLRules.dotBuiltinSeq)

    val (outputF: Array[Float], _) = Execute(1, 1)(f, input, input)
    val (outputG: Array[Float], _) = Execute(1, 1)(g, input, input)

    assertEquals(gold, outputF.head, 0.001f)
    assertEquals(gold, outputG.head, 0.001f)
  }
}
