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
  def stencil1DLocalMem(): Unit = {
    val f = fun(
      ArrayType(Float, 4),
      (input) =>
        Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
      )

    //val f1 = Rewrite.applyRuleAt(f, Rule.slideTiling(6))
  }
}
