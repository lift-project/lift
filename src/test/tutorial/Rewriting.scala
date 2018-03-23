package tutorial

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern.{MapLcl, MapWrg}
import org.junit.Test
import rewriting.Rewrite
import rewriting.rules.OpenCLRules


class Rewriting{

  @Test
  def usedExpressions: Unit = {
    val N = SizeVar("N")
    val clamp = Pad.Boundary.Clamp
    val u = Var("u")
    val v = Var("v")

    val highLevel = fun(
      ArrayType(Float, N), input =>
        Map(Reduce(add, 0.0f)) o
          Slide(3, 1) o
            Pad(1, 1, clamp) $ input
    )

    val expression1 = fun(
      ArrayType(Float, N), input =>
        Map(Reduce(add, 0.0f)) o
          Slide(3, 1) o
          Pad(1, 1, clamp) $ input
    )

    val f = Reduce(add, 0.0f)
    val expression2 = fun(
      ArrayType(Float, N), input =>
        Map(f) o Slide(3, 1) o
          Pad(1, 1, clamp) $ input
    )

    val expression3 = fun(
      ArrayType(Float, N), input =>
        Join() o
          Map(fun(tile =>
            Map(f) o Slide(3, 1) $ tile)) o
            Slide(u, v) o
              Pad(1, 1, clamp) $ input
    )

    val expression4 = fun(
      ArrayType(Float, N), input =>
        Join() o
          MapWrg(fun(tile =>
            MapLcl(f) o Slide(3, 1) $ tile)) o
            Slide(u, v) o
              Pad(1, 1, clamp) $ input
    )

    val expression41 = Rewrite.applyRuleAtId(
      expression3, 1, OpenCLRules.mapWrg)
  }
}
