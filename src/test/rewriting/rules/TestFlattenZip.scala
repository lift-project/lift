package rewriting.rules

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.Execute
import opencl.ir._
import org.junit.Assert._
import org.junit.Test
import rewriting.{Rewrite, Rules}

class TestFlattenZip {

  private val N = SizeVar("N")
  private val t = ArrayType(Float, N)

  @Test
  def noZipsToFlatten(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p => p)) $ Zip(x, y, z)
    )

    TypeChecker(f)
    assertFalse(Rules.flattenZips.isDefinedAt(f.body))
  }

  @Test
  def zipResultEscapes(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p => p)) $ Zip(x, Zip(y, z))
    )

    TypeChecker(f)
    assertFalse(Rules.flattenZips.isDefinedAt(f.body))
  }

  @Test
  def zipResultEscapes2(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p => Id() $ p)) $ Zip(x, Zip(y, z))
    )

    TypeChecker(f)
    assertFalse(Rules.flattenZips.isDefinedAt(f.body))
  }

  @Test
  def subZipResultEscapes(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p => p._1)) $ Zip(x, Zip(y, z))
    )

    TypeChecker(f)
    assertFalse(Rules.flattenZips.isDefinedAt(f.body))
  }

  @Test
  def subZipUsed(): Unit = {
    val f = \(
      t, t, t,
      (x,y,z) => Map(\(p => Get(Get(p, 1), 0))) $ Zip(x, Zip(y, z))
    )

    val origType = TypeChecker(f)
    assertTrue(Rules.flattenZips.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAtId(f, 0, Rules.flattenZips)
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
    assertTrue(Rules.flattenZips.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAtId(f, 0, Rules.flattenZips)
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
    assertTrue(Rules.flattenZips.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAtId(f, 0, Rules.flattenZips)
    val resultType = TypeChecker(result)

    assertEquals(origType, resultType)
  }

  @Test
  def oneSubZipAllUsed(): Unit = {
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
    assertTrue(Rules.flattenZips.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAtId(f, 0, Rules.flattenZips)
    val resultType = TypeChecker(result)

    assertEquals(origType, resultType)
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
    assertTrue(Rules.flattenZips.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAtId(f, 0, Rules.flattenZips)
    val resultType = TypeChecker(result)

    assertEquals(origType, resultType)

    val lowered = Rewrite.applyRulesUntilCannot(result, Seq(Rules.mapSeq))

//    val (output: Array[Float], _) = Execute()(lowered)
  }
}
