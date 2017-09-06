package opencl.generator.stencil

import ir._
import ir.ast._
import opencl.executor._
import opencl.generator.stencil.acoustic.StencilUtilities._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit._

object TestStencilPPCG extends LoadExecutor

class TestStecilPPCG {

  @Test def j2d5pt: Unit = {
    val M = 8192
    val N = 8192

    def f = UserFun("jacobi", Array("top", "bottom", "left", "right", "center"),
      "return (5 * top + 12 * left + 15 * center + 5 * bottom + 12 * right) / 118;",
      Seq(Float, Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(Float, M), N),
      input => {
        MapGlb(1)(MapGlb(0)(λ(nbh => {

          val (top, bottom, left, right, center) = vonNeumann5pt(nbh)

          toGlobal(id) o toPrivate(λ(x =>
            f(x, bottom, left, right, center))) $ top

        }))) o Slide2D(3, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test def j2d9pt: Unit = {
    val M = 8192
    val N = 8192

    def f = UserFun("jacobi", Array("NW","N","NE","W","C","E","SW","S","SE"),
      "return (7 * NW + 5 * N + 9 * NE + 12 * W + 15 * C + 12 * E + 9 * SW + 5 * S + 7 * SE) / 118;",
      Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val lambda1 = λ(
      ArrayType(ArrayType(Float, M), N),
      input => {
        MapGlb(1)(MapGlb(0)(λ(nbh => {

          val (northWest, north, northEast,
            west, center, east,
            southWest, south, southEast) = moore9pt(nbh)

          toGlobal(id) o toPrivate(λ(x =>
            f(x, north, northEast,
              west, center, east,
              southWest, south, southEast))) $ northEast

        }))) o Slide2D(3, 1) $ input
      })

    val lambda2 = λ(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(ArrayType(Float, 3), 3),
      (input, weights) => {
        MapGlb(1)(MapGlb(0)(λ(nbh =>
          MapSeq(toGlobal(id)) o
          ReduceSeqUnroll(λ( (acc, x) =>
            multAndSumUp(acc, x._0, x._1)), 0.0f)
           $ Zip(
            Join() $ nbh,
            Join() $ weights)
        ))) o Slide2D(3,1) $ input
      }
    )

    val kernel = Compile(lambda1)
    println(kernel)
  }
}
