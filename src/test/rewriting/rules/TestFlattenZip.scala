package rewriting.rules

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import org.junit.Assert._
import org.junit.Test
import rewriting.Rewrite

import scala.util.Random

object TestFlattenZip extends TestWithExecutor

class TestFlattenZip {

  private val N = SizeVar("N")
  private val t = ArrayType(Float, N)

  private val inputSize = 128

  private val x = Array.tabulate(inputSize)(_ => Random.nextFloat())
  private val y = Array.tabulate(inputSize)(_ => Random.nextFloat())
  private val z = Array.tabulate(inputSize)(_ => Random.nextFloat())
  private val w = Array.tabulate(inputSize)(_ => Random.nextFloat())

  @Test
  def noZipsToFlatten(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p => p)) $ Zip(x, y, z)
    )

    TypeChecker(f)
    assertFalse(SimplificationRules.flattenZips.isDefinedAt(f.body))
  }

  @Test
  def zipResultEscapes(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p => p)) $ Zip(x, Zip(y, z))
    )

    TypeChecker(f)
    assertFalse(SimplificationRules.flattenZips.isDefinedAt(f.body))
  }

  @Test
  def zipResultEscapes2(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p => Id() $ p)) $ Zip(x, Zip(y, z))
    )

    TypeChecker(f)
    assertFalse(SimplificationRules.flattenZips.isDefinedAt(f.body))
  }

  @Test
  def subZipResultEscapes(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p => p._1)) $ Zip(x, Zip(y, z))
    )

    TypeChecker(f)
    assertFalse(SimplificationRules.flattenZips.isDefinedAt(f.body))
  }

  @Test
  def subZipUsed(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p => Get(Get(p, 1), 0))) $ Zip(x, Zip(y, z))
    )

    val origType = TypeChecker(f)
    assertTrue(SimplificationRules.flattenZips.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAtId(f, 0, SimplificationRules.flattenZips)
    val resultType = TypeChecker(result)

    assertEquals(origType, resultType)
  }

  @Test
  def someUsed(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p => p._0)) $ Zip(x, Zip(y, z))
    )

    val origType = TypeChecker(f)
    assertTrue(SimplificationRules.flattenZips.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAtId(f, 0, SimplificationRules.flattenZips)
    val resultType = TypeChecker(result)

    assertEquals(origType, resultType)
  }

  @Test
  def twoSubZips(): Unit = {
    val f = \(
      t, t, t, t,
      (x,y,z,w) => Map(\(p => Get(Get(p, 0), 0))) $ Zip(Zip(x, y), Zip(z, w))
    )

    val origType = TypeChecker(f)
    assertTrue(SimplificationRules.flattenZips.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAtId(f, 0, SimplificationRules.flattenZips)
    val resultType = TypeChecker(result)

    assertEquals(origType, resultType)
  }

  @Test
  def secondSubZipAllUsed(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p =>
        mult(
          add(
            Get(Get(p, 1), 0),
            Get(Get(p, 1), 1)
          ),
          Get(p, 0))
      )) $ Zip(x, Zip(y, z))
    )

    val origType = TypeChecker(f)
    assertTrue(SimplificationRules.flattenZips.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAtId(f, 0, SimplificationRules.flattenZips)
    val resultType = TypeChecker(result)

    assertEquals(origType, resultType)

    val lowered = Rewrite.applyRuleUntilCannot(result, OpenCLRules.mapSeq)

    val (output: Array[Float], _) = Execute()(lowered, x,y,z)

    val gold = (x, (y,z).zipped.map(_+_)).zipped.map(_*_)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def firstSubZipAllUsed(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p =>
        mult(
          add(
            Get(Get(p, 0), 0),
            Get(Get(p, 0), 1)
          ),
          Get(p, 1))
      )) $ Zip(Zip(x,y), z)
    )

    val origType = TypeChecker(f)
    assertTrue(SimplificationRules.flattenZips.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAtId(f, 0, SimplificationRules.flattenZips)
    val resultType = TypeChecker(result)

    assertEquals(origType, resultType)

    val lowered = Rewrite.applyRuleUntilCannot(result, OpenCLRules.mapSeq)

    val (output: Array[Float], _) = Execute()(lowered, x,y,z)

    val gold = ((x,y).zipped.map(_+_), z).zipped.map(_*_)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def twoSubZipsAllUsed(): Unit = {
    val f = \(t, t, t, t,
      (x,y,z,w) => Map(\(p =>
        mult(
          add(Get(Get(p, 0), 0), Get(Get(p, 0), 1)),
          add(Get(Get(p, 1), 0), Get(Get(p, 1), 1))
        )
      )) $ Zip(Zip(x, y), Zip(z, w))
    )

    val origType = TypeChecker(f)
    assertTrue(SimplificationRules.flattenZips.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAtId(f, 0, SimplificationRules.flattenZips)
    val resultType = TypeChecker(result)

    assertEquals(origType, resultType)

    val lowered = Rewrite.applyRuleUntilCannot(result, OpenCLRules.mapSeq)

    val (output: Array[Float], _) = Execute()(lowered, x,y,z,w)

    val gold = ((x,y).zipped.map(_+_), (z,w).zipped.map(_+_)).zipped.map(_*_)

    assertArrayEquals(gold, output, 0.001f)
  }
}
