package rewriting

import ir._
import ir.ast._
import opencl.ir._
import opencl.executor.Executor
import org.junit._

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
    val f4 = Rewrite.applyRuleAtId(f3, 8, Rules.reduceSeq)
    val f5 = Rewrite.applyRuleAtId(f4, 1, Rules.mapWrg)
    val f6 = Rewrite.applyRuleAtId(f5, 5, Rules.mapLcl)
    println(f6)
  }
}
