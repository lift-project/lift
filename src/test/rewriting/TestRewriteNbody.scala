package rewriting

import benchmarks.NBody
import ir._
import ir.ast._
import opencl.executor.{Eval, Execute, Executor}
import opencl.generator.TestNBody._
import opencl.ir._
import org.junit.Assert._
import org.junit._
import rewriting.utils.Utils

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
          )) o Reduce(VectorizeUserFun(4, add), Value("0.0f", Float4)
          ) o Map(\(p2 =>
            NBody.calcAcc(Get(p1,0), p2, deltaT, espSqr)
          )) $ pos
        )) $ Zip(pos, vel)
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(128))
    val f2 = Rewrite.applyRuleAtId(f1, 6, Rules.mapFission2)
    val f3 = Rewrite.applyRuleAtId(f2, 8, Rules.mapFission)
    val f4 = Rewrite.applyRuleAtId(f3, 19, Rules.partialReduce)
    val f5 = Rewrite.applyRuleAtId(f4, 20, Rules.partialReduceSplitJoin(128))
    val f6 = Rewrite.applyRuleAtId(f5, 8, MacroRules.mapFissionAtPosition(2))
    val f7 = Rewrite.applyRuleAtId(f6, 8, Rules.mapReducePartialReduce)
    val f8 = Rewrite.applyRuleAtId(f7, 14, Rules.splitJoin(128))
    val f9 = Rewrite.applyRuleAtId(f8, 11, Rules.mapFusion)
    val f10 = Rewrite.applyRuleAtId(f9, 13, Rules.splitJoinId)
    val f11 = Rewrite.applyRuleAtId(f10, 11, MacroRules.mapMapInterchange)
    val f12 = Rewrite.applyRuleAtId(f11, 10, Rules.transposeTransposeId)
    val f13 = Rewrite.applyRuleAtId(f12, 9, MacroRules.reduceMapFusion)
    val f14 = Rewrite.applyRuleAtId(f13, 18, Rules.mapFusionWithZip)
    val f15 = Rewrite.applyRuleAtId(f14, 22, Rules.partialReduceToReduce)
    val f16 = Rewrite.applyRuleAtId(f15, 22, MacroRules.reduceMapFusion)
    val f17 = Rewrite.applyRuleAtId(f16, 38, Rules.dropId)
    val f18 = Rewrite.applyRuleAtId(f17, 36, Rules.dropId)
    val f19 = Rewrite.applyRuleAtId(f18, 22, Rules.removeEmptyMap)
    val f20 = Rewrite.applyRuleAtId(f19, 9, Rules.removeEmptyMap)
    val f21 = Rewrite.applyRuleAtId(f20, 9, Rules.addIdForCurrentValueInReduce)
    val f22 = Rewrite.applyRuleAtId(f21, 17, Rules.implementIdAsDeepCopy)
    val f23 = Rewrite.applyRuleAtId(f22, 6, Rules.globalMemory)
    val f24 = Rewrite.applyRuleAtId(f23, 42, Rules.mapSeq)
    val f25 = Lower.lowerNextLevelWithRule(f24, Rules.mapWrg)
    val f26 = Lower.lowerNextLevelWithRule(f25, Rules.mapLcl)
    val f27 = Rewrite.applyRuleAtId(f26, 17, Rules.localMemory)

    // TODO: f27 fails in OutputView for some mysterious reason
    val f28 = Eval(Utils.dumpLambdaToString(f27))

    val (output: Array[Float], _) =
      Execute(128, inputSize, (true, false))(f28, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.001f)
  }

}
