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
    val f4 = Rewrite.applyRuleAtId(f2, 10, Rules.partialReduce)
    val f5 = Rewrite.applyRuleAtId(f4, 11, Rules.partialReduceSplitJoin(128))
    val f6 = Rewrite.applyRuleAtId(f5, 8, MacroRules.mapFissionAtPosition(2))
    val g1 = Rewrite.applyRuleAtId(f6, 20, Rules.reduceSeq)
    val f7 = Rewrite.applyRuleAtId(g1, 8, Rules.mapReducePartialReduce)
    val f8 = Rewrite.applyRuleAtId(f7, 14, Rules.splitJoin(128))

    val f11 = Rewrite.applyRuleAtId(f8, 11, MacroRules.mapMapInterchange)
    val f12 = Rewrite.applyRuleAtId(f11, 6, MacroRules.mapMapInterchange)

    val f15 = Lower.lowerPartialReduces(f12)
    val f16 = SimplifyAndFuse(f15)

    val f23 = Lower.lastWriteToGlobal(f16)
    val f24 = Lower.lowerNextLevelWithRule(f23, Rules.mapWrg)
    val f25 = Rewrite.applyRuleAtId(f24, 7, Rules.mapSeq)
    val f26 = Lower.lowerNextLevelWithRule(f25, Rules.mapLcl)

    val f21 = Rewrite.applyRuleAtId(f26, 8, Rules.addIdForCurrentValueInReduce)
    val f22 = Rewrite.applyRuleAtId(f21, 13, Rules.implementIdAsDeepCopy)
    val f27 = Rewrite.applyRuleAtId(f22, 13, Rules.localMemory)
    val f28 = Lower.lowerNextLevelWithRule(f27, Rules.mapLcl)

    val (output: Array[Float], _) =
      Execute()(f28, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.001f)

    val replacementFilter = collection.immutable.Map[ArithExpr, ArithExpr](N -> 16384)
    val x = ParameterRewrite.replaceInputTypes(f27, replacementFilter)
    assertEquals(ExpressionFilter.Status.Success, ExpressionFilter(x, InferNDRange(x)))
 }

}
