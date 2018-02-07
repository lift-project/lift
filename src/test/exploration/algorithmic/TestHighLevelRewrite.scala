package exploration.algorithmic

import exploration.HighLevelRewrite
import ir._
import ir.ast._
import lift.arithmetic.Cst
import opencl.executor.LongTestsEnabled
import opencl.ir._
import org.junit.Test
import org.junit.Assert.assertEquals

class TestHighLevelRewrite {

  LongTestsEnabled()

  private val stencil1D = fun(
    ArrayType(Float, N),
    input => Join() o Map(Reduce(add, 0.0f)) o Slide(3, 1) o Pad(1, 1, Pad.Boundary.Clamp) $ input
  )

  @Test(expected = classOf[TypeException]) //see Issue #114
  def rewriteLambdaUsingInt(): Unit = {
    val div9 = UserFun("div9", "x", "{ return x/9; }", Int, Int)
    val stencilInt = fun(
      ArrayType(ArrayType(Int, 6408), 4802),
      input => {
        Map(Map(\(neighborhood => Map(div9) o Reduce(addI, 0.0f) o Join() $ neighborhood)
          //                                                ^ this must be of type Int
        )) o Slide2D(3, 1) $ input
      })

    val rewriter = new HighLevelRewrite(4, 2, 2)
    rewriter(stencilInt)
  }

  @Test
  def stencil1DRewrite(): Unit = {
    val rewriter = new HighLevelRewrite(4, 2, 2)
    val rewrittenLambdas = rewrite(rewriter, stencil1D)

    val gold = fun(ArrayType(Float, N), (p_0) => FunCall(Join(), FunCall(Join(), FunCall(Map(fun((p_1) => FunCall(Map(fun((p_2) => FunCall(Reduce(fun((p_3, p_4) => FunCall(add, p_3, p_4))), Value("0.0f", Float), p_2))), FunCall(Slide(3, 1), p_1)))), FunCall(Slide(2 + v__1, v__1), FunCall(Pad(1, 1, Pad.Boundary.Clamp), p_0))))))

    checkExists(gold, rewrittenLambdas)
    checkDistance(gold)
  }

  @Test
  def stencil2DRewrite(): Unit = {
    def f = UserFun("jacobi", Array("NNWW", "NNW", "NN", "NNE", "NNEE",
      "NWW", "NW", "N", "NE", "NEE",
      "WW", "W", "C", "E", "EE",
      "SWW", "SW", "S", "SE", "SEE",
      "SSWW", "SSW", "SS", "SSE", "SSEE"),
      """return (2*NNWW + 4*NNW + 5*NN + 4*NNE + 2*NNEE +
        | 4*NWW + 9*NW + 12*N + 9*NE + 4*NEE +
        | 5*WW + 12*W + 15*C + 12*E + 5*EE +
        | 4*SWW + 9*SW + 12*S + 9*SE + 4*SEE +
        | 2*SSWW + 4*SSW + 5*SS + 4*SSE + 2*SSEE) / 159;""".stripMargin,
      Seq(Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float), Float)

    val M = Cst(8192)
    val N = Cst(8192)

    def gaussian = λ(
      ArrayType(ArrayType(Float, M), N),
      input => {
        Map(Scatter(Shift(2))) o Scatter(Shift(2)) o Pad2D(2, 2, Pad.Boundary.Clamp) o
          Map(Map(λ(nbh => {
            val nnww = nbh.at(0).at(0)
            val nnw = nbh.at(0).at(1)
            val nn = nbh.at(0).at(2)
            val nne = nbh.at(0).at(3)
            val nnee = nbh.at(0).at(4)
            val nww = nbh.at(2).at(0)
            val nw = nbh.at(1).at(1)
            val n = nbh.at(1).at(2)
            val ne = nbh.at(1).at(3)
            val nee = nbh.at(1).at(4)
            val ww = nbh.at(2).at(0)
            val w = nbh.at(2).at(1)
            val c = nbh.at(2).at(2)
            val e = nbh.at(2).at(3)
            val ee = nbh.at(2).at(4)
            val sww = nbh.at(3).at(0)
            val sw = nbh.at(3).at(1)
            val s = nbh.at(3).at(2)
            val se = nbh.at(3).at(3)
            val see = nbh.at(3).at(4)
            val ssww = nbh.at(4).at(0)
            val ssw = nbh.at(4).at(1)
            val ss = nbh.at(4).at(2)
            val sse = nbh.at(4).at(3)
            val ssee = nbh.at(4).at(4)

            λ(x =>
              f(x, nnw, nn, nne, nnee,
                nww, nw, n, ne, nee,
                ww, w, c, e, ee,
                sww, sw, s, se, see,
                ssww, ssw, ss, sse, ssee)) $ nnww

          }))) o Slide2D(5, 1) $ input
      })

    val rewriter = new HighLevelRewrite(
      vectorWidth = 4,
      repetitions= 1,
      explorationDepth = 5,
      ruleCollection =  "convolution2D")
    val rewrittenLambdas = rewrite(rewriter, gaussian)
    // the convolution2D rule set only contains 1 macro rule which is
    // is applicable to this lambda. Since repetitions is set to 1
    // only two expressions should be generated
    assertEquals(2, rewrittenLambdas.size)
  }
}
