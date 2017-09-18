package rewriting

import benchmarks.NBody
import exploration.{ExpressionFilter, ParameterRewrite}
import ir._
import ir.ast._
import lift.arithmetic.ArithExpr
import opencl.executor.{Execute, TestWithExecutor, Utils}
import opencl.generator.TestNBody._
import opencl.ir._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._
import rewriting.macrorules.{MacroRules, ReuseRules}
import rewriting.rules.{CopyRules, OpenCLRules, Rules}

object TestRewriteNbody extends TestWithExecutor

class TestRewriteNbody {

  private val f = fun(
    ArrayType(Float4, N),
    ArrayType(Float4, N),
    Float,
    Float,
    (pos, vel, espSqr, deltaT) =>
      Map(fun(p1 =>
        Map(fun(acceleration =>
          NBody.update(Get(p1, 0), Get(p1, 1), deltaT, acceleration)
        )) o Reduce(VectorizeUserFun(4, add), Value("0.0f", Float4)
        ) o Map(\(p2 =>
          NBody.calcAccNoAdd(Get(p1,0), p2, deltaT, espSqr)
        )) $ pos
      )) $ Zip(pos, vel)
  )


  private val group0Mapping = EnabledMappings(
    global0 = false, global01 = false, global10 = false,
    global012 = false, global210 = false,
    group0 = true, group01 = false, group10 = false)

  @Test
  def nBodyLocalMem(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(128))

    val f12 = Rewrite.applyRuleAtId(f1, 6, MacroRules.interchange)

    val f4 = Rewrite.applyRuleAtId(f12, 9, ReuseRules.introduceReuseFromMap(128))
    val f11 = Rewrite.applyRuleAtId(f4, 12, ReuseRules.introduceReuseFromMap(128))

    val lowered = Lower.mapCombinations(f11, group0Mapping).head

    val f21 = Rewrite.applyRuleAtId(lowered, 8, CopyRules.addIdForCurrentValueInReduce)
    val f22 = Rewrite.applyRuleAtId(f21, 16, CopyRules.implementIdAsDeepCopy)
    val f27 = Rewrite.applyRuleAtId(f22, 16, OpenCLRules.localMemory)
    val f28 = Lower.lowerNextLevelWithRule(f27, OpenCLRules.mapLcl)

    // TODO: Breaks
    val f29 = Rewrite.applyRuleUntilCannot(f28, MacroRules.userFunCompositionToPrivate)

    val (output: Array[Float], _) =
      Execute()(f29, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.001f)

    val replacementFilter = collection.immutable.Map[ArithExpr, ArithExpr](N -> 16384)
    val x = ParameterRewrite.replaceInputTypes(f27, replacementFilter)
    assertEquals(ExpressionFilter.Status.Success, ExpressionFilter(x, InferNDRange(x)))
 }

  @Test
  def partialReduceWithReorder(): Unit = {

    val f0 = Rewrite.applyRuleAtId(f, 5, MacroRules.partialReduceWithReorder(128))

    val lowered = Lower.mapCombinations(f0, group0Mapping).head

    val l0 = Rewrite.applyRuleAtId(lowered , 11, CopyRules.addIdAfterReduce)
    val l1 = Rewrite.applyRuleAtId(l0, 24, CopyRules.implementIdAsDeepCopy)
    val l2 = Rewrite.applyRuleAtId(l1, 11, OpenCLRules.localMemory)
    // TODO: Could get away with private memory
    val l3 = Rewrite.applyRuleAtId(l2, 5, CopyRules.addIdAfterReduce)
    val l4 = Rewrite.applyRuleAtId(l3, 34, CopyRules.implementIdAsDeepCopy)
    val l5 = Rewrite.applyRuleAtId(l4, 5, OpenCLRules.localMemory)
    val l6 = Rewrite.applyRuleUntilCannot(l5, MacroRules.userFunCompositionToPrivate)

    val (output: Array[Float], _) = Execute()(l6, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.001f)
  }

}
