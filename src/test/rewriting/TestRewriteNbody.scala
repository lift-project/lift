package rewriting

import benchmarks.NBody
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.generator.TestNBody._
import org.junit._
import org.junit.Assert._

object TestRewriteNbody {
   @BeforeClass
  def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass
  def after(): Unit = {
    Executor.shutdown()
  }
}

class TestRewriteNbody {

  @Ignore
  @Test
  def nBodyLocalMem(): Unit = {

    val f = fun(
      ArrayType(Float4, N),
      ArrayType(Float4, N),
      Float,
      Float,
      (pos, vel, espSqr, deltaT) =>
        Map(fun(p1 =>
          Map(fun(acceleration =>
            NBody.update(Get(p1, 0), Get(p1, 1), deltaT, acceleration)
          )) o Reduce(VectorizeUserFun(4, add), Value("(float4) 0.0f", Float4)
          ) o Map(\(p2 =>
            NBody.calcAcc(Get(p1,0), p2, deltaT, espSqr)
          )) $ pos
        )) $ Zip(pos, vel)
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(128))
    val f2 = Rewrite.applyRuleAtId(f1, 6, Rules.mapFission)
    // TODO: Breaks the expression. Need a new Rule for this case + fix the current one
    val f3 = Rewrite.applyRuleAtId(f2, 7, Rules.mapFission)
    val f4 = Rewrite.applyRuleAtId(f3, 18, Rules.partialReduce)
    val f5 = Rewrite.applyRuleAtId(f4, 19, Rules.partialReduceSplitJoin(128))
    val f6 = Rewrite.applyRuleAtId(f5, 7, MacroRules.mapFissionAtPosition(2))
    val f7 = Rewrite.applyRuleAtId(f6, 7, Rules.mapReducePartialReduce)
    val f8 = Rewrite.applyRuleAtId(f7, 13, Rules.splitJoin(128))
    val f9 = Rewrite.applyRuleAtId(f8, 10, Rules.mapFusion)
    val f10 = Rewrite.applyRuleAtId(f9, 12, Rules.splitJoinId)
    val f11 = Rewrite.applyRuleAtId(f10, 10, MacroRules.mapMapInterchange)
    val f12 = Rewrite.applyRuleAtId(f11, 9, Rules.transposeTransposeId)
    val f13 = Rewrite.applyRuleAtId(f12, 8, MacroRules.reduceMapFusion)
    val f14 = Rewrite.applyRuleAtId(f13, 17, Rules.mapFusionWithZip)
    val f15 = Rewrite.applyRuleAtId(f14, 21, Rules.partialReduceToReduce)
    val f16 = Rewrite.applyRuleAtId(f15, 21, MacroRules.reduceMapFusion)
    val f17 = Rewrite.applyRuleAtId(f16, 37, Rules.dropId)
    val f18 = Rewrite.applyRuleAtId(f17, 35, Rules.dropId)
    val f19 = Rewrite.applyRuleAtId(f18, 21, Rules.removeEmptyMap)
    val f20 = Rewrite.applyRuleAtId(f19, 8, Rules.removeEmptyMap)
    val f21 = Rewrite.applyRuleAtId(f20, 8, Rules.addIdForCurrentValueInReduce)
    val f22 = Rewrite.applyRuleAtId(f21, 16, Rules.implementIdAsDeepCopy)
    val f23 = Rewrite.applyRuleAtId(f22, 6, Rules.globalMemory)
    val f24 = Rewrite.applyRuleAtId(f23, 38, Rules.mapSeq)
    val f25 = Lower.lowerNextLevelWithRule(f24, Rules.mapWrg)
    val f26 = Lower.lowerNextLevelWithRule(f25, Rules.mapLcl)

    val (output: Array[Float], _) = Execute(128, inputSize, (true, false))(
      f26, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.001f)
  }

}
