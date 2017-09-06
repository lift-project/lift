package rewriting.rules

import ir._
import ir.ast._
import opencl.ir._
import lift.arithmetic.SizeVar
import opencl.executor.TestWithExecutor
import opencl.ir.pattern.ReduceSeq
import org.junit.Test
import org.junit.Assert._
import rewriting.{Rewrite, Rules}

object TestInterchange extends TestWithExecutor

class TestInterchange {

  private val N = SizeVar("N")
  private val M = SizeVar("M")

  @Test
  def transposeBothSides(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(Map(plusOne)) $ input
    )

    TypeChecker.check(f.body)

    assertTrue(Rules.transposeBothSides.rewrite.isDefinedAt(f.body))

    val g = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(Map(Map(plusOne)) o Split(2)) $ input
    )

    TypeChecker.check(g.body)
    assertFalse(Rules.transposeBothSides.rewrite.isDefinedAt(g.body))
  }

  @Test
  def mapMapTransposeWithZipInside(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, M),
      (in1, in2) => Map(fun(x => Map(fun(x => add(Get(x, 0), Get(x, 1)))) $ Zip(in2, x))) $ in1
    )

    assertTrue(Rules.mapMapTransposeZipInside.rewrite.isDefinedAt(f.body))
    val f0 = Rules.mapMapTransposeZipInside.rewrite(f.body)
    TypeChecker(f0)
  }

  @Test
  def mapMapTransposeWithZipOutside(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      ArrayTypeWSWC(Float, M),
      (in1, in2) =>
        Map(fun(x =>
          Map(fun(y => add(y, Get(x, 1)))) $ Get(x, 0)
        )) $ Zip(in1, in2)
    )

    assertTrue(Rules.mapMapTransposeZipOutside.rewrite.isDefinedAt(f.body))
    val f0 = Rules.mapMapTransposeZipOutside.rewrite(f.body)
    TypeChecker(f0)
  }

  @Test
  def mapReduceReduceNesting0(): Unit = {
    // Different f and init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(PartRed(mult, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(Rules.mapReducePartialReduce.isDefinedAt(f.body))
  }

  @Test
  def mapReduceReduceNesting1(): Unit = {
    // Different f and init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(Reduce(mult, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(Rules.mapReducePartialReduce.isDefinedAt(f.body))
  }

  @Test
  def mapReduceReduceNesting2(): Unit = {
    // Different init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(Reduce(add, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(Rules.mapReducePartialReduce.isDefinedAt(f.body))
  }

  @Test
  def mapReduceReduceNesting3(): Unit = {
    // Different init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(PartRed(add, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(Rules.mapReducePartialReduce.isDefinedAt(f.body))
  }

  @Test
  def nestPartialReduceInReduce(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 16), 16), 16),
      a => Map( ReduceSeq(add, 0.0f) o Join() o Map(PartRed(fun((x, y) => add(x, y)), 0.0f)) ) $ a)

    val fResult = Rewrite.applyRuleAtId(f, 0, Rules.mapReducePartialReduce)
    TypeChecker(fResult)
  }

  @Test
  def mapReduceInterchange0(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(Reduce(add, 0.0f)) $ input
    )

    assertTrue(Rules.mapReduceInterchange.rewrite.isDefinedAt(f.body))
    val f0 = Rewrite.applyRuleAtId(f, 0, Rules.mapReduceInterchange)
    TypeChecker(f0)
    assertTrue(f0.body.asInstanceOf[FunCall].args.head.asInstanceOf[FunCall].f.isInstanceOf[Reduce])
  }

  @Test
  def mapReduceInterchange1(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(ReduceSeq(add, 0.0f)) $ input
    )

    assertTrue(Rules.mapReduceInterchange.rewrite.isDefinedAt(f.body))
    val f0 = Rewrite.applyRuleAtId(f, 0, Rules.mapReduceInterchange)
    TypeChecker(f0)
    assertTrue(f0.body.asInstanceOf[FunCall].args.head.asInstanceOf[FunCall].f.isInstanceOf[ReduceSeq])
  }

  @Test
  def mapReduceInterchangeWithZipOutside0(): Unit = {
    // TODO: Reduce

  }

  @Test
  def mapReduceInterchangeWithZipOutside1(): Unit = {
    // TODO: ReduceSeq

  }
}
