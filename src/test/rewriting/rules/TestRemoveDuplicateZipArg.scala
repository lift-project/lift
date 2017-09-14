package rewriting.rules

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import org.junit.Assert._
import org.junit.Test
import rewriting.{Lower, Rewrite}

import scala.util.Random

object TestRemoveDuplicateZipArg extends TestWithExecutor

class TestRemoveDuplicateZipArg {

  private val N = SizeVar("N")
  private val arrayType = ArrayType(Float, N)

  private val inputSize = 128

  private val x = Array.tabulate(inputSize)(_ => Random.nextFloat())
  private val y = Array.tabulate(inputSize)(_ => Random.nextFloat())

  private def lowerExecuteAndCompare(original: Lambda, result: Lambda, args: Any*): Unit = {
    val originalType = TypeChecker(original)
    val resultType =TypeChecker(result)

    assertEquals(originalType, resultType)

    val loweredOriginal = Lower.sequential(original)
    val loweredResult = Lower.sequential(result)

    val (outputOriginal: Array[Float], _) = Execute()(loweredOriginal, args:_*)
    val (outputResult: Array[Float], _) = Execute()(loweredResult, args:_*)

    assertArrayEquals(outputOriginal, outputResult, 0.001f)
  }

  @Test
  def zipResultEscapes0(): Unit = {
    val f = \(arrayType,
      in => Map(Id()) $ Zip(in, in)
    )

    TypeChecker(f)
    assertFalse(Rules.removeDuplicateZipArg.isDefinedAt(f.body))
  }

  @Test
  def zipResultEscapes1(): Unit = {
    val f = \(arrayType,
      in => Map(\(x => x)) $ Zip(in, in)
    )

    TypeChecker(f)
    assertFalse(Rules.removeDuplicateZipArg.isDefinedAt(f.body))
  }

  @Test
  def nothingShared(): Unit = {
    val f = \(arrayType, arrayType,
      (a, b) => Map(Id()) $ Zip(a, b)
    )

    TypeChecker(f)
    assertFalse(Rules.removeDuplicateZipArg.isDefinedAt(f.body))
  }

  @Test
  def noZipAfterRemovingDuplicate(): Unit = {
    val f = \(arrayType,
      in => Map(\(x => add(x._0, x._1))) $ Zip(in, in)
    )

    val result = Rewrite.applyRuleAtId(f, 0, Rules.removeDuplicateZipArg)

    assertFalse(result.body.contains({ case FunCall(Zip(_), _*) => }))

    lowerExecuteAndCompare(f, result, x)
  }

  @Test
  def smallerZipAfterRemovingDuplicate(): Unit = {
    val f = \(arrayType, arrayType,
      (a, b) => Map(\(x => add(x._0, mult(x._1, x._2)))) $ Zip(a, b, a)
    )

    val result = Rewrite.applyRuleAtId(f, 0, Rules.removeDuplicateZipArg)

    assertFalse(result.body.contains({ case FunCall(Zip(3), _*) => }))
    assertTrue(result.body.contains({ case FunCall(Zip(2), _*) => }))

    lowerExecuteAndCompare(f, result, x, y)
  }

  @Test
  def moreComplicatedDuplicationAllowed(): Unit = {
    val f = \(arrayType,
      in => Map(\(x =>
        Map(\(x =>
          add(x._0, x._1)
        )) $ Zip(x._0, x._1)
      )) $ Zip(Split(64) $ in, Split(64) $ in)
    )

    val result = Rewrite.applyRuleUntilCannot(f, Rules.removeDuplicateZipArg)

    assertFalse(result.body.contains({ case FunCall(Zip(_), _*) => }))

    lowerExecuteAndCompare(f, result, x)
  }

  @Test
  def moreComplicatedDuplicationCantTell(): Unit = {
    // TODO: No equality checking for arbitrary Lambdas/Expr.
    // TODO: Need to check usage of Params and whatnot
    val f = \(arrayType,
      in => Map(\(x => add(x._0, x._1))) $ Zip(Map(plusOne) $ in, Map(plusOne) $ in)
    )

    TypeChecker(f)
    assertFalse(Rules.removeDuplicateZipArg.isDefinedAt(f.body))
  }

  @Test
  def moreComplicatedNoDuplication(): Unit = {
    val f = \(arrayType, Float,
      (a, b) => Map(\(x =>
        add(x._0, x._1)
      )) $ Zip(Map(\(_ => plusOne(b))) $ a, Map(plusOne) $ a)
    )

    TypeChecker(f)
    assertFalse(Rules.removeDuplicateZipArg.isDefinedAt(f.body))
  }

}
