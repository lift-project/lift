package rewriting

import benchmarks.NBody
import exploration.{ExpressionFilter, SplitSlideRewrite}
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
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(128))

    val f12 = Rewrite.applyRuleAtId(f1, 6, MacroRules.interchange)

    val f4 = Rewrite.applyRuleAtId(f12, 9, ReuseRules.introduceReuseFromMap(128))
    val f11 = Rewrite.applyRuleAtId(f4, 12, ReuseRules.introduceReuseFromMap(128))

    val lowered = Lower.mapCombinations(f11, group0Mapping).head

    val f21 = Rewrite.applyRuleAtId(lowered, 8, CopyRules.addIdForCurrentValueInReduce)
    val f22 = Rewrite.applyRuleAtId(f21, 16, CopyRules.implementIdAsDeepCopy)
    val f27 = Rewrite.applyRuleAtId(f22, 16, OpenCLRules.localMemory)
    val f28 = Lower.lowerNextLevelWithRule(f27, OpenCLRules.mapLcl)
    val f29 = Rewrite.applyRuleUntilCannot(f28, MacroRules.userFunCompositionToPrivate)

    val (output, _) =
      Execute()[Array[Float]](f29, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.001f)

    val replacementFilter = collection.immutable.Map[ArithExpr, ArithExpr](N -> 16384)
    val x = SplitSlideRewrite.replaceInputTypes(f27, replacementFilter)
    assertEquals(ExpressionFilter.Status.Success, ExpressionFilter(x, InferNDRange(x)))
 }

  @Test
  def nBodyIntroduceReuseFromMapping(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val f0 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(128))
    val f1 = Rewrite.applyRuleAtId(f0, 6, MacroRules.interchange)
    val f2 = Rewrite.applyRuleAtId(f1, 9, ReuseRules.introduceReuseFromMap(128))
    val f3 = Rewrite.applyRuleAtId(f2, 12, ReuseRules.introduceReuseFromMap(128))

    val lowered = Lower.mapCombinations(f3, group0Mapping).head

    val l0 = Rewrite.applyRuleUntilCannot(lowered, MacroRules.userFunCompositionToPrivate)
    val l1 = Rewrite.applyRuleAtId(l0, 8, CopyRules.addIdForCurrentValueInReduce)
    val l2 = Rewrite.applyRuleAtId(l1, 16, OpenCLRules.localMemory)
    val l3 = Rewrite.applyRuleAtId(l2, 18, CopyRules.implementIdAsDeepCopy)
    val l4 = Lower.lowerNextLevelWithRule(l3, OpenCLRules.mapLcl)

    val (output, _) =
      Execute()[Array[Float]](l4, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.001f)

    val replacementFilter = collection.immutable.Map[ArithExpr, ArithExpr](N -> 16384)
    val x = SplitSlideRewrite.replaceInputTypes(l4, replacementFilter)
    assertEquals(ExpressionFilter.Status.Success, ExpressionFilter(x, InferNDRange(x)))
 }

  @Test
  def partialReduceWithReorder(): Unit = {

    val f0 = Rewrite.applyRuleAtId(f, 5, MacroRules.partialReduceWithReorder(128))

    val lowered = Lower.mapCombinations(f0, group0Mapping).head

    val l0 = Rewrite.applyRuleAtId(lowered , 11, CopyRules.addIdAfterReduce)
    val l1 = Rewrite.applyRuleAtId(l0, 24, CopyRules.implementIdAsDeepCopy)
    val l2 = Rewrite.applyRuleAtId(l1, 11, OpenCLRules.localMemory)
    val l3 = Rewrite.applyRuleAtId(l2, 5, CopyRules.addIdAfterReduce)
    val l4 = Rewrite.applyRuleAtId(l3, 34, CopyRules.implementIdAsDeepCopy)
    val l5 = Rewrite.applyRuleAtId(l4, 5, OpenCLRules.localMemory)
    val l6 = Rewrite.applyRuleUntilCannot(l5, MacroRules.userFunCompositionToPrivate)

    val (output, _) = Execute()[Array[Float]](l6, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def partialReduceWithReorderNoRace(): Unit = {

    val f0 = Rewrite.applyRuleAtId(f, 5, MacroRules.partialReduceWithReorder(128))

    val f1 = Lower.pushReduceDeeper(f0)
    val lowered = Lower.mapCombinations(f1, group0Mapping).head

    val l0 = Rewrite.applyRuleUntilCannot(lowered, MacroRules.userFunCompositionToPrivate)
    val l1 = Rewrite.applyRuleAtId(l0, 12, CopyRules.addIdAfterReduce) // matches on 13
    val l2 = Rewrite.applyRuleAtId(l1, 30, OpenCLRules.localMemory)
    val l3 = Rewrite.applyRuleAtId(l2, 32, CopyRules.implementIdAsDeepCopy)

    val (output, _) = Execute()[Array[Float]](l3, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.001f)
  }

}
