package exploration.algorithmic

import exploration.HighLevelRewrite
import ir._
import ir.ast._
import opencl.executor.LongTestsEnabled
import opencl.ir._
import org.junit.Test
import rewriting.macrorules.SlideTiling

class TestHighLevelRewrite {

  LongTestsEnabled()

  private val stencil1D = fun(
    ArrayType(Float, N),
    input => Join() o Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
  )

  @Test ( expected = classOf[TypeException] )//see Issue #114
  def rewriteLambdaUsingInt(): Unit = {
    val div9 = UserFun("div9", "x", "{ return x/9; }", Int, Int)
    val stencilInt = fun(
      ArrayType(ArrayType(Int, 6408), 4802),
      input => {
        Map(Map( \(neighborhood => Map(div9) o Reduce(addI, 0.0f) o Join() $ neighborhood)
          //                                                ^ this must be of type Int
			)) o Slide2D(3, 1) $ input
    })

    val rewriter = new HighLevelRewrite(4, 2, 2)
    rewriter(stencilInt)
  }

  @Test
  def stencil1DRewrite(): Unit = {
    val rewriter = new HighLevelRewrite(4, 2, 2)
    val rewrittenLambdas = rewriter(stencil1D)

    val gold = fun(ArrayType(Float, N),(p_0) => FunCall(Join(), FunCall(Join(), FunCall(Map(fun((p_1) => FunCall(Map(fun((p_2) => FunCall(Reduce(fun((p_3, p_4) => FunCall(add, p_3, p_4))), Value("0.0f", Float), p_2))), FunCall(Slide(3,1), p_1)))), FunCall(Slide(2+v__1,v__1), FunCall(Pad(1,1,Pad.Boundary.Clamp), p_0))))))

    val tiledSeq = Seq(SlideTiling.tileStencils)

    checkExists(gold, tiledSeq, rewrittenLambdas)
    checkDistance(gold)
  }
}
