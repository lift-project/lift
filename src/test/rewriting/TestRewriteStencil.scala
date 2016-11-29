package rewriting

import ir._
import ir.ast._
import opencl.ir._
import opencl.executor.Executor
import org.junit._
import rewriting.utils.NumberExpression

object TestRewriteStencil {
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

class TestRewriteStencil {

  @Test
  def stencil1DTiling(): Unit = {
    val f = fun(
      ArrayType(Float, 32),
      (input) =>
        Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
      )

    val f1 = Rewrite.applyRuleAtId(f, 1, Rules.slideTiling(4))
    val f2 = Rewrite.applyRuleAtId(f1, 0, Rules.mapJoin)
    val f3 = Rewrite.applyRuleAtId(f2, 1, Rules.mapFusion)
    // introduce low-level primitives
    val f4 = Rewrite.applyRuleAtId(f3, 8, Rules.reduceSeq)
    val f5 = Rewrite.applyRuleAtId(f4, 1, Rules.mapWrg)
    val f6 = Rewrite.applyRuleAtId(f5, 5, Rules.mapLcl)
  }

  @Test
  def stencil1DTilingLocalMemory(): Unit = {
    val f = fun(
      ArrayType(Float, 32),
      (input) =>
        Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
      )

    // tiling
    val f1 = Rewrite.applyRuleAtId(f, 1, Rules.slideTiling(4))
    val f2 = Rewrite.applyRuleAtId(f1, 0, Rules.mapJoin)
    val f3 = Rewrite.applyRuleAtId(f2, 1, Rules.mapFusion)
    // local memory
    val f4 = Rewrite.applyRuleAtId(f3, 6, Rules.addId)
    val f5 = Rewrite.applyRuleAtId(f4, 7, Rules.implementIdAsDeepCopy)
    val f6 = Rewrite.applyRuleAtId(f5, 11, Rules.reduceSeq)
    val f7 = Rewrite.applyRuleAtId(f6, 11, Rules.addIdAfterReduce)
    val f8 = Rewrite.applyRuleAtId(f7, 18, Rules.implementIdAsDeepCopy)
    val f9 = Rewrite.applyRuleAtId(f8, 1, Rules.mapWrg)
    val f10 = Rewrite.applyRuleAtId(f9, 5, Rules.mapLcl)
    val f11 = Rewrite.applyRuleAtId(f10, 7, Rules.mapLcl)
    val f12 = Rewrite.applyRuleAtId(f11, 7, Rules.localMemory)
    val f13 = Rewrite.applyRuleAtId(f12, 13, Rules.globalMemory)
    //val test = NumberExpression.breadthFirst(f13.body)
  }
}
