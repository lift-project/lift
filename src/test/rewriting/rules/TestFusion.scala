package rewriting.rules

import ir._
import ir.ast._
import ir.ast.debug.PrintType
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting._

object TestFusion extends TestWithExecutor

class TestFusion {

  import SimplifyAndFuse.getNumberOfTerms

  private val N = SizeVar("N")
  private val A = Array.fill[Float](128)(0.5f)

  private def checkFusedIsSmaller(original: Lambda, fused: Lambda) =
    assertTrue(getNumberOfTerms(fused) < getNumberOfTerms(original))

  @Test
  def ReduceSeqMapSeqArray(): Unit = {

    val A = Array.fill[Float](128, 4)(0.5f)

    val goldF = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 4), N),
      input => toGlobal(MapSeq(MapSeq(id))) o
        ReduceSeq(fun((acc, elem) => MapSeq(add) o fun(elem => Zip(acc, MapSeq(plusOne) $ elem)) $ elem),
          Value(0.0f, ArrayTypeWSWC(Float, 4))) $ input
    )

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 4), N),
      input => toGlobal(MapSeq(MapSeq(id))) o
        ReduceSeq(fun((acc, elem) => MapSeq(add) $ Zip(acc, elem)),
          Value(0.0f, ArrayTypeWSWC(Float, 4))) o MapSeq(MapSeq(plusOne)) $ input
    )

    val (gold,_) = Execute(1, 1)[Array[Float]](goldF, A)

    val lambda = Rewrite.applyRuleUntilCannot(f, FusionRules.reduceSeqMapSeqFusion)

    val (result, _) = Execute(1, 1)[Array[Float]](lambda, A)
    assertArrayEquals(gold, result, 0.0f)
    checkFusedIsSmaller(f, lambda)
  }

  @Test
  def moreComplexReduceSeqMapSeq(): Unit = {
    val goldF = fun(
      ArrayTypeWSWC(Float, N),
      Float,
      (input, a) => PrintType() o toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, add(newValue, a))), 0.0f) $ input
    )

    val f = fun(
      ArrayTypeWSWC(Float, N),
      Float,
      (input, a) => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(fun(x => add(x, a))) $ input
    )

    val a = 2.0f

    val (gold, _) = Execute(1, 1)[Array[Float]](goldF, A, a)

    val lambda = Rewrite.applyRuleUntilCannot(f, FusionRules.reduceSeqMapSeqFusion)

    val (result, _) = Execute(1, 1)[Array[Float]](lambda, A, a)
    assertArrayEquals(gold, result, 0.0f)
    checkFusedIsSmaller(f, lambda)
  }

  @Test
  def ReduceSeqMapSeq(): Unit = {
    val goldF = fun(
      ArrayTypeWSWC(Float, N),
      input =>  toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, plusOne(newValue))), 0.0f) $ input
    )

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(plusOne) $ input
    )

    val lambda = Rewrite.applyRuleUntilCannot(f, FusionRules.reduceSeqMapSeqFusion)

    val (gold, _) = Execute(1, 1)[Array[Float]](goldF, A)

    val (result, _) = Execute(1, 1)[Array[Float]](lambda, A)
    assertArrayEquals(gold, result, 0.0f)
    checkFusedIsSmaller(f, lambda)
  }

  @Test
  def ReduceSeqMapSeqChangesType(): Unit = {

    val userFun = UserFun("idIntToFloat", "x", "{ return x; }", Int, Float)

    val goldF = fun(
      ArrayTypeWSWC(Int, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, userFun(newValue))), 0.0f) $ input
    )

    val f = fun(
      ArrayTypeWSWC(Int, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(userFun) $ input
    )

    val A = Array.tabulate(128)(i => i)

    val lambda = Rewrite.applyRuleUntilCannot(f, FusionRules.reduceSeqMapSeqFusion)


    val (gold, _) = Execute(1, 1)[Array[Float]](goldF, A)

    val (result, _) = Execute(1, 1)[Array[Float]](lambda, A)
    assertArrayEquals(gold, result, 0.0f)
    checkFusedIsSmaller(f, lambda)
  }
}
