package rewriting.rules

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.Assert._
import org.junit.Test
import rewriting.{Lower, Rewrite}

object TestInterchange extends TestWithExecutor

class TestInterchange {

  private val N = SizeVar("N")
  private val M = SizeVar("M")

  private val inputSize = 256
  private val inputMatrix = Array.tabulate(inputSize, inputSize)((_, _) => util.Random.nextFloat())
  private val inputArray = Array.tabulate(inputSize)(_ => util.Random.nextFloat())
  private val input3DMatrix = Array.tabulate(16,16,16)((_, _, _) => util.Random.nextFloat())

  private def test(f: Lambda, g: Lambda, inputs: Any*): Unit = {
    TypeChecker(f)
    TypeChecker(g)
    assertEquals(f.body.t, g.body.t)

    val loweredF = Lower.sequential(f)
    val loweredG = Lower.sequential(g)

    val (outputF: Array[Float], _) = Execute()(loweredF, inputs:_*)
    val (outputG: Array[Float], _) = Execute()(loweredG, inputs:_*)

    assertArrayEquals(outputF, outputG, 0.001f)
  }

  @Test
  def transposeBothSidesNotApplicable(): Unit = {
    val g = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(Map(Map(plusOne)) o Split(2)) $ input
    )

    TypeChecker(g)
    assertFalse(InterchangeRules.transposeBothSides.rewrite.isDefinedAt(g.body))
  }

  @Test
  def transposeBothSides(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(Map(plusOne)) $ input
    )

    val g = Rewrite.applyRuleAtId(f, 0, InterchangeRules.transposeBothSides)

    test(f, g, inputMatrix)
  }

  @Test
  def mapMapTransposeWithZipInside(): Unit = {
    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, M),
      (in1, in2) => Map(fun(x => Map(fun(x => add(Get(x, 0), Get(x, 1)))) $ Zip(in2, x))) $ in1
    )

    val g = Rewrite.applyRuleAt(f, f.body, InterchangeRules.mapMapTransposeZipInside)

    test(f, g, inputMatrix, inputArray)
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

    val g = Rewrite.applyRuleAtId(f, 0, InterchangeRules.mapMapTransposeZipOutside)

    test(f, g, inputMatrix, inputArray)
  }

  @Test
  def mapReduceReduceNesting0(): Unit = {
    // Different f and init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(PartRed(mult, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(InterchangeRules.mapReducePartialReduce.isDefinedAt(f.body))
  }

  @Test
  def mapReduceReduceNesting1(): Unit = {
    // Different f and init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(Reduce(mult, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(InterchangeRules.mapReducePartialReduce.isDefinedAt(f.body))
  }

  @Test
  def mapReduceReduceNesting2(): Unit = {
    // Different init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(Reduce(add, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(InterchangeRules.mapReducePartialReduce.isDefinedAt(f.body))
  }

  @Test
  def mapReduceReduceNesting3(): Unit = {
    // Different init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(PartRed(add, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(InterchangeRules.mapReducePartialReduce.isDefinedAt(f.body))
  }

  @Test
  def nestPartialReduceInReduce(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 16), 16), 16),
      a => Map( ReduceSeq(add, 0.0f) o Join() o Map(PartRed(fun((x, y) => add(x, y)), 0.0f)) ) $ a)

    val g = Rewrite.applyRuleAtId(f, 0, InterchangeRules.mapReducePartialReduce)

    test(f, g, input3DMatrix)
  }

  @Test
  def mapReduceInterchange0(): Unit = {
    val small2DMatrix = Array.tabulate(16, 8)((_, _) => util.Random.nextFloat())

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 16),
      input => Map(Reduce(add, 0.0f)) $ input
    )

    val g = Rewrite.applyRuleAtId(f, 0, InterchangeRules.mapReduceInterchange)
    assertTrue(g.body.asInstanceOf[FunCall].args.head.asInstanceOf[FunCall].f.isInstanceOf[Reduce])

    test(f, g, small2DMatrix)
  }

  @Test
  def mapReduceInterchange1(): Unit = {
    val small2DMatrix = Array.tabulate(16, 8)((_, _) => util.Random.nextFloat())

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 16),
      input => Map(ReduceSeq(add, 0.0f)) $ input
    )

    val g = Rewrite.applyRuleAtId(f, 0, InterchangeRules.mapReduceInterchange)

    assertTrue(g.body.asInstanceOf[FunCall].args.head.asInstanceOf[FunCall].f.isInstanceOf[ReduceSeq])

    test(f, g, small2DMatrix)
  }

  @Test
  def mapReduceInterchangeWithZipOutside0(): Unit = {
    // TODO: Reduce
  }

  @Test
  def mapReduceInterchangeWithZipOutside1(): Unit = {
    // TODO: ReduceSeq
  }

  @Test
  def mapMapZipInsideUsedTwice(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, N), M),
      in1 =>
        Map(fun(y =>
          Map(fun(x =>
            Map(fun(x =>
              add(Get(x, 0), Get(x, 1))
            )) $ Zip(Get(x, 0), Get(x, 1))
          )) $ y
        )) o Split(256) $ Zip(in1, in1)
    )

    val g = fun(
      ArrayType(ArrayType(Float, N), M),
      in1 =>
        Map(fun(y =>
          TransposeW() o
          Map(fun(x =>
            Map(fun(x =>
              add(Get(x, 0), Get(x, 1))
            )) $ Zip(Get(x, 0), Get(x, 1))
          )) $ Zip(Transpose() o Map(Get(0)) $ y, Transpose() o Map(Get(1)) $ y)
        )) o Split(256) $ Zip(in1, in1)
    )

    test(f, g, inputMatrix)
  }

  @Test
  def mapMapZipInsideUsedTwiceUsingRule(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, N), M),
      in1 =>
        Map(fun(y =>
          Map(fun(x =>
            Map(fun(x =>
              add(Get(x, 0), Get(x, 1))
            )) $ Zip(Get(x, 0), Get(x, 1))
          )) $ y
        )) o Split(256) $ Zip(in1, in1)
    )

    val g = Rewrite.applyRuleAtId(f, 5, InterchangeRules.mapMapTransposeZipInside)

    test(f, g, inputMatrix)
  }

  @Test
  def mapMapZipInsideUsedTwicePlusExtra(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, N), M),
      ArrayType(Float, N),
      (matrix, array) =>
        Map(fun(y =>
          Map(fun(x =>
            Map(fun(x =>
              add(Get(x, 0), mult(Get(x, 1), Get(x, 2)))
            )) $ Zip(Get(x, 0), Get(x, 1), array)
          )) $ y
        )) o Split(256) $ Zip(matrix, matrix)
    )

    val g = fun(
      ArrayType(ArrayType(Float, N), M),
      ArrayType(Float, N),
      (matrix, array) =>
        Map(fun(y =>
          TransposeW() o
          Map(fun(outX =>
            Map(fun(x =>
              add(Get(x, 0), mult(Get(x, 1), Get(outX, 2)))
            )) $ Zip(Get(outX, 0), Get(outX, 1))
          )) $ Zip(Transpose() o Map(Get(0)) $ y, Transpose() o Map(Get(1)) $ y, array)
        )) o Split(256) $ Zip(matrix, matrix)
    )

    test(f, g, inputMatrix, inputArray)
  }

  @Test
  def mapMapZipInsideUsedTwicePlusExtraUsingRule(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, N), M),
      ArrayType(Float, N),
      (matrix, array) =>
        Map(fun(y =>
          Map(fun(x =>
            Map(fun(x =>
              add(Get(x, 0), mult(Get(x, 1), Get(x, 2)))
            )) $ Zip(Get(x, 0), Get(x, 1), array)
          )) $ y
        )) o Split(256) $ Zip(matrix, matrix)
    )

    val g = Rewrite.applyRuleAtId(f, 5, InterchangeRules.mapMapTransposeZipInside)

    test(f, g, inputMatrix, inputArray)
  }

  @Test
  def mapMapZipInsideUsedTwiceWithReorder(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, N), M),
      ArrayType(Float, N),
      (matrix, array) =>
        Map(fun(y =>
          Map(fun(x =>
            Map(fun(x =>
              add(Get(x, 0), mult(Get(x, 1), Get(x, 2)))
            )) $ Zip(x, Gather(reverse) $ x, array)
          )) $ y
        )) o Split(256) $ matrix
    )

    val g = Rewrite.applyRuleAtId(f, 3, InterchangeRules.mapMapTransposeZipInside)

    test(f, g, inputMatrix, inputArray)
  }

}
