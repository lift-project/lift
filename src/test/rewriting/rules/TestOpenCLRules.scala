package rewriting.rules

import ir.ast._
import ir._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.{EnabledMappings, Lower, Rewrite}

object TestOpenCLRules extends TestWithExecutor

class TestOpenCLRules {

  private val N = SizeVar("N")
  private val input = Array.tabulate(32)(_ => util.Random.nextFloat())

  @Test
  def testVectorise(): Unit = {

    val orig = \(
      ArrayType(Float, N),
      Map(plusOne) $ _
    )

    val rewritten = Rewrite.applyRuleAt(orig, orig.body, OpenCLRules.vectorize(4))

    val origLowered = Lower.sequential(orig)
    val rewrittenLowered = Lower.sequential(rewritten)

    val (gold, _) = Execute()[Array[Float]](origLowered, input)
    val (output, _) = Execute()[Array[Float]](rewrittenLowered, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def testVectoriseToGlobal(): Unit = {

    val orig = \(
      ArrayType(Float, N),
      Map(toGlobal(plusOne)) $ _
    )

    val rewritten = Rewrite.applyRuleAt(orig, orig.body, OpenCLRules.vectorizeToAddressSpace(4))

    val origLowered = Lower.sequential(orig)
    val rewrittenLowered = Lower.sequential(rewritten)

    val (gold, _) = Execute()[Array[Float]](origLowered, input)
    val (output, _) = Execute()[Array[Float]](rewrittenLowered, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def testVectoriseToLocal(): Unit = {

    val orig = \(
      ArrayType(Float, N),
      Join() o Map(Map(plusOne) o Map(toLocal(id))) o Split(16) $ _
    )

    val rewritten = Rewrite.applyRuleAtId(orig, 5, OpenCLRules.vectorizeToAddressSpace(4))

    val enabledMappings = EnabledMappings(
      global0 = false, global01 = false, global10 = false,
      global012 = false, global210 = false, group0 = true,
      group01 = false, group10 = false)

    val origLowered = Lower.mapCombinations(orig, enabledMappings).head
    val rewrittenLowered = Lower.mapCombinations(rewritten, enabledMappings).head

    val (gold, _) = Execute()[Array[Float]](origLowered, input)
    val (output, _) = Execute()[Array[Float]](rewrittenLowered, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def testVectoriseToPrivate(): Unit = {

    val orig = \(
      ArrayType(Float, 32),
      Map(plusOne) o Map(toPrivate(id)) $ _
    )

    val rewritten = Rewrite.applyRuleAtId(orig, 1, OpenCLRules.vectorizeToAddressSpace(4))

    val origLowered = Lower.sequential(orig)
    val rewrittenLowered = Lower.sequential(rewritten)

    val (gold, _) = Execute()[Array[Float]](origLowered, input)
    val (output, _) = Execute()[Array[Float]](rewrittenLowered, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def testVectoriseMapZip(): Unit = {
    val orig = \(
      ArrayType(Float, N),
      input => Map(\(i => add(i._0, i._1))) $ Zip(input, input)
    )

    val rewritten = Rewrite.applyRuleAt(orig, orig.body, OpenCLRules.vectorizeMapZip(4))

    val origLowered = Lower.sequential(orig)
    val rewrittenLowered = Lower.sequential(rewritten)

    val (gold, _) = Execute()[Array[Float]](origLowered, input)
    val (output, _) = Execute()[Array[Float]](rewrittenLowered, input)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def testVectoriseReduce(): Unit = {
    val orig = \(
      ArrayType(Float, N),
      Reduce(add, 0.0f) o PartRed(add, 0.0f) $ _
    )

    val rewritten = Rewrite.applyRuleAtId(orig, 1, OpenCLRules.partialReduceVectorize(4))

    val origLowered = Lower.sequential(orig)
    val rewrittenLowered = Lower.sequential(rewritten)

    val (gold, _) = Execute()[Array[Float]](origLowered, input)
    val (output, _) = Execute()[Array[Float]](rewrittenLowered, input)

    assertArrayEquals(gold, output, 0.01f)
  }

}
