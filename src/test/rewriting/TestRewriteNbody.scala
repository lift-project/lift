package rewriting

import benchmarks.NBody
import exploration.{ExpressionFilter, ParameterRewrite}
import ir._
import ir.ast._
import lift.arithmetic.ArithExpr
import opencl.executor.{Execute, Executor, Utils}
import opencl.generator.TestNBody._
import opencl.ir._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._

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

  @Test
  def nBodyLocalMem(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(Float4, N),
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

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(128))
    val f2 = Rewrite.applyRuleAtId(f1, 6, Rules.mapFission2)
    val f3 = Rewrite.applyRuleAtId(f2, 8, Rules.mapFission)
    val f4 = Rewrite.applyRuleAtId(f3, 19, Rules.partialReduce)
    val f5 = Rewrite.applyRuleAtId(f4, 20, Rules.partialReduceSplitJoin(128))
    val f6 = Rewrite.applyRuleAtId(f5, 8, MacroRules.mapFissionAtPosition(2))
    val g1 = Rewrite.applyRuleAtId(f6, 22, Rules.reduceSeq)
    val f7 = Rewrite.applyRuleAtId(g1, 8, Rules.mapReducePartialReduce)
    val f8 = Rewrite.applyRuleAtId(f7, 14, Rules.splitJoin(128))
    val f9 = Rewrite.applyRuleAtId(f8, 11, Rules.mapFusion)
    val f10 = Rewrite.applyRuleAtId(f9, 13, Rules.splitJoinId)
    val f11 = Rewrite.applyRuleAtId(f10, 11, MacroRules.mapMapInterchange)
    val f12 = Rewrite.applyRuleAtId(f11, 10, Rules.transposeTransposeId)
    val f13 = Rewrite.applyRuleAtId(f12, 9, MacroRules.reduceMapFusion)
    val f14 = Rewrite.applyRuleAtId(f13, 14, Rules.mapFusionWithZip)
    val f15 = Rewrite.applyRuleAtId(f14, 18, Rules.partialReduceToReduce)
    val f16 = Rewrite.applyRuleAtId(f15, 18, MacroRules.reduceMapFusion)
    val f21 = Rewrite.applyRuleAtId(f16, 9, Rules.addIdForCurrentValueInReduce)
    val f22 = Rewrite.applyRuleAtId(f21, 14, Rules.implementIdAsDeepCopy)
    val f23 = Rewrite.applyRuleAtId(f22, 6, Rules.globalMemory)
    val f24 = Lower.lowerNextLevelWithRule(f23, Rules.mapWrg)
    val f25 = Lower.lowerNextLevelWithRule(f24, Rules.mapLcl)
    val f26 = Lower.lowerNextLevelWithRule(f25, Rules.mapSeq)
    val f27 = Rewrite.applyRuleAtId(f26, 14, Rules.localMemory)


    val replacement = collection.immutable.Map[ArithExpr, ArithExpr](N -> inputSize)
    val replacementFilter = collection.immutable.Map[ArithExpr, ArithExpr](N -> 16384)

    val (local, global) = InferNDRange(f27)

    val replacedGlobal = global.map(ArithExpr.substitute(_, replacement))

    val (output: Array[Float], _) =
      Execute(local, replacedGlobal, (true, false))(f27, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.001f)

    val x = ParameterRewrite.replaceInputTypes(f27, replacementFilter)
    assertEquals(ExpressionFilter.Status.Success, ExpressionFilter(x, InferNDRange(x)))
 }

}
