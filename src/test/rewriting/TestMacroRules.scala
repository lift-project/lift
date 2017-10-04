package rewriting

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.TestWithExecutor
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.macrorules.{MacroRules, ReuseRules, SlideTiling}
import rewriting.utils.NumberExpression

object TestMacroRules extends TestWithExecutor

class TestMacroRules {

  @Test
  def moveReduceOutOneLevel(): Unit = {

    val N = SizeVar("N")

    val patternSimple: PartialFunction[Expr, Unit] =
    { case FunCall(TransposeW(), FunCall(Reduce(_), _, _)) => }

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      input => Map(Reduce(add, 0.0f)) $ input
    )

    val fResult = Rewrite.applyRuleAtId(f, 0, MacroRules.moveReduceOutOneLevel)
    assertTrue(patternSimple.isDefinedAt(fResult.body))

    TypeChecker(fResult)

    val g = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      input => Map(Reduce(add, 0.0f) o Gather(reverse)) $ input
    )

    val gResult = Rewrite.applyRuleAtId(g, 0, MacroRules.moveReduceOutOneLevel)
    assertTrue(patternSimple.isDefinedAt(gResult.body))

    TypeChecker(gResult)

    val patternWithMap: PartialFunction[Expr, Unit] =
    { case FunCall(Map(_), FunCall(TransposeW(), FunCall(Reduce(_), _, _))) => }

    val h = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      input => Map(Scatter(reverse) o Reduce(add, 0.0f)) $ input
    )

    val hResult = Rewrite.applyRuleAtId(h, 0, MacroRules.moveReduceOutOneLevel)
    assertTrue(patternWithMap.isDefinedAt(hResult.body))

    TypeChecker(hResult)

    val m = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      input => Map(Scatter(reverse) o Scatter(reverse) o Reduce(add, 0.0f) o Gather(reverse)) $ input
    )

    val mResult = Rewrite.applyRuleAtId(m, 0, MacroRules.moveReduceOutOneLevel)
    assertTrue(patternWithMap.isDefinedAt(mResult.body))

    TypeChecker(mResult)
  }

  @Test
  def tileSlide2D(): Unit = {
    val s = Slide2D(3,1)
    assertTrue(SlideTiling.tileSlide2D.isDefinedAt(s.body))
  }

  @Test
  def tile2DStencils(): Unit = {
    val stencilFunction = Reduce(add, 0.0f) o Join()
    val gold = Map(Map(stencilFunction)) o Slide2D(3,1)
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      a => gold $ a)

    assertTrue(SlideTiling.tile2DStencils.isDefinedAt(f.body))
    TypeChecker(f)
    val tiled = Rewrite.applyRuleAt(f, f.body, SlideTiling.tile2DStencils)
    TypeChecker(tiled)
  }

  @Test
  def tile2DStencilsSeparable(): Unit = {
    val stencilFunction = Reduce(add, 0.0f) o Join()
    val gold = Map(Map(stencilFunction)) o Slide2D(3,1, 1,1) o Pad2D(1,1, 0,0, Pad.Boundary.Clamp)
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      a => gold $ a)

    assertTrue(SlideTiling.tile2DStencils.isDefinedAt(f.body))
    TypeChecker(f)
    val tiled = Rewrite.applyRuleAt(f, f.body, SlideTiling.tile2DStencils)
    TypeChecker(tiled)
  }

  @Test
  def tile2DStencilsWithZip(): Unit = {
    val rodiniaUserFun = UserFun("rodiniaUserFun",
      Array("power", "top", "bottom", "left", "right", "center"), "{ float step_div_cap = 1.365333e+00; return center + step_div_cap*(power + 0.1f*(bottom + top - 2*center) + 0.1*(left + right - 2*center) + 4.882813e-05*(80.0f - center)); }",
      Seq(Float, Float, Float, Float, Float, Float), Float)

    val hotspot = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 1024), 1024),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 1024), 1024),
      (heat, power) => {
        Map(Map(\(tuple => {
          val nbh = tuple._0
          val powerValue = tuple._1

          val top = Get(tuple,0).at(0).at(1)
          val bottom = tuple._0.at(2).at(1)
          val left = tuple._0.at(1).at(0)
          val right = tuple._0.at(1).at(2)
          val center = tuple._0.at(1).at(1)

          toGlobal(id) o toPrivate(fun(x => rodiniaUserFun(x, top, bottom, left, right, center))) $ powerValue})
        )) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(
          Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.Clamp) $ heat,
          power)
      }
    )

    TypeChecker(hotspot)
    assertFalse(SlideTiling.tile2DStencils.isDefinedAt(hotspot.body))
    assertTrue(SlideTiling.tile2DStencilsZip.isDefinedAt(hotspot.body))

    val tiled = Rewrite.applyRuleAt(hotspot, hotspot.body, SlideTiling.tile2DStencilsZip)
    TypeChecker(tiled)
  }

  @Test
  def tile2DStencilsWithZip6(): Unit = {
    val m = SizeVar("M")
    val n = SizeVar("N")

    def calculateDiv = UserFun("calculateDiv", Array("dN", "dS", "dW", "dE", "orgDn", "orgDs", "orgDw", "orgDe"),
      "{ return  (dN*orgDn + dS*orgDs + dW*orgDw + dE*orgDe) ; }", Seq(Float, Float, Float, Float, Float, Float, Float, Float), Float)

    def calculateImageUpdate = UserFun("calculateImageUpdate", Array("img", "div"),
      "{ return img + 0.125 * div; }", Seq(Float, Float), Float)

    val srad2 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n),
      (image, coeff, DN, DS, DE, DW) => {
        Map(Map(fun((m) => {
          val imageNBH = Get(m, 0)
          val coeffNBH = Get(m, 1)

          val imageC = imageNBH.at(1).at(1)
          val coeffC = coeffNBH.at(1).at(1)

          val newDW = coeffC
          val newDN = coeffC
          val newDS = coeffNBH.at(2).at(1)
          val newDE = coeffNBH.at(1).at(2)

          val orgDN = Get(m, 2)
          val orgDS = Get(m, 3)
          val orgDE = Get(m, 4)
          val orgDW = Get(m, 5)

          val div = toPrivate(fun(x => calculateDiv(x, newDS, newDW, newDE, orgDN, orgDS, orgDW, orgDE))) $ newDN
          val newImg = toPrivate(fun(x => calculateImageUpdate(x, div))) $ imageC

          toGlobal(id) $ newImg
        }))
        ) $ Zip2D(Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.Clamp) $ image, Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.Clamp) $ coeff, DN, DS, DE, DW)
      }
    )
    TypeChecker(srad2)
    assertTrue(SlideTiling.tile2DStencilsZip6.isDefinedAt(srad2.body))
    val tiled = Rewrite.applyRuleAt(srad2, srad2.body, SlideTiling.tile2DStencilsZip6)
    TypeChecker(tiled)
  }

   @Test
   def tile2DStencilsSeparable2(): Unit = {
      val stencil = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("N")), SizeVar("M")),
      ArrayTypeWSWC(Float, 17),
      (matrix, weights) => {
        Map(
          Map(\(neighbourhood =>
            Reduce(add, 0.0f) o Map( \(tuple =>
              mult.apply(Get(tuple,0),Get(tuple,1))
            )) $ Zip(weights,
              Join() $ neighbourhood)))
        ) o Slide2D(17, 1, 1, 1) o Pad2D(8, 8, 0, 0, Pad.Boundary.Wrap) $ matrix
      })

     TypeChecker(stencil)
     assertTrue(SlideTiling.tile2DStencils.isDefinedAt(stencil.body))
     TypeChecker(stencil)
     val tiled = Rewrite.applyRuleAt(stencil, stencil.body, SlideTiling.tile2DStencils)

     println(tiled)
     TypeChecker(tiled)
   }

  @Test
  def fissionAtPosition(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      a => Map(Reduce(add, 0.0f) o Map(plusOne) o Map(plusOne) o Map(plusOne)) $ a)

    Rewrite.applyRuleAtId(f, 0, MacroRules.mapFissionAtPosition(0))
    Rewrite.applyRuleAtId(f, 0, MacroRules.mapFissionAtPosition(1))
    Rewrite.applyRuleAtId(f, 0, MacroRules.mapFissionAtPosition(2))
  }

  @Test
  def reshapeMapMap(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = \(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      Map(Map(id)) $ _
    )

    val g = Rewrite.applyRuleAt(f, f.body, MacroRules.reshapeMapMap)
    TypeChecker(g)

    assertEquals(f.body.t, g.body.t)
  }

  @Test
  def twoDBlockingNotDefined(): Unit = {
    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1),
      (p_0, p_1) =>
        FunCall(Map(fun((p_2) =>
          FunCall(Map(fun((p_3) =>
            FunCall(Reduce(fun((p_4, p_5) =>
              FunCall(add, p_4, p_5))
            ), Value(0.0f, Float),
              FunCall(Map(fun((p_6) =>
                FunCall(mult, FunCall(Get(0), p_6), FunCall(Get(1), p_6)))
              ), FunCall(Zip(2), p_2, p_3)))
          )), FunCall(Transpose(), p_1))
        )), FunCall(Transpose(), p_0)))

    val idMap = NumberExpression.breadthFirst(f)
    val expr = Rewrite.getExprForId(f.body, 3, idMap)

    assertFalse(ReuseRules.apply2DRegisterBlocking.isDefinedAt(expr))
  }


  @Test
  def twoDBlockingNotDefined2(): Unit = {
    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")
    val v__3 = SizeVar("v3")
    val v__4 = SizeVar("v4")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1),
      (p_0, p_1) =>
        FunCall(Map(fun((p_2) =>
          FunCall(Scatter(ReorderWithStride(v_N_2 / v__3)), p_2)
        )),
          FunCall(Join(),
            FunCall(Map(fun((p_3) =>
              FunCall(TransposeW(),
                FunCall(Join(),
                  FunCall(Map(fun((p_4) =>
                    FunCall(TransposeW(),
                      FunCall(Map(fun((p_5) =>
                        FunCall(TransposeW(), p_5)
                      )),
                        FunCall(TransposeW(),
                          FunCall(Reduce(fun((p_6, p_7) =>
                            FunCall(Map(fun((p_8) =>
                              FunCall(Map(fun((p_9) =>
                                FunCall(add, FunCall(Get(0), p_9), FunCall(Get(1), p_9))
                              )),
                                FunCall(Zip(2), FunCall(Get(0), p_8), FunCall(Get(1), p_8)))
                            )), FunCall(Zip(2), p_6, p_7))
                          )), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, v__3), v__4)),
                            FunCall(Transpose(),
                              FunCall(Map(fun((p_10) =>
                                FunCall(Transpose(),
                                  FunCall(Transpose(), p_10))
                              )), FunCall(Transpose(),
                                FunCall(Map(fun((p_11) =>
                                  FunCall(Map(fun((p_12) =>
                                    FunCall(Map(fun((p_13) =>
                                      FunCall(mult, p_12, p_13)
                                    )), FunCall(Get(1), p_11))
                                  )), FunCall(Get(0), p_11))
                                )),
                                  FunCall(Zip(2), FunCall(Transpose(), p_3), FunCall(Transpose(), p_4))))))))))
                  )),
                    FunCall(Split(v__3),
                      FunCall(Gather(ReorderWithStride(v_N_2 / v__3)),
                        FunCall(Transpose(), p_1))))))
            )),
              FunCall(Split(v__4),
                FunCall(Transpose(), p_0))))))

    val result = Rewrite.applyRuleAtId(f, 2, ReuseRules.apply2DRegisterBlocking)
    TypeChecker(result)
  }

  @Test
  def finishTiling(): Unit = {
    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")

    val f =
      fun(
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M_0), v_K_1),
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N_2), v_K_1),
        (p_0, p_1) =>
          FunCall(Map(fun((p_2) =>
            FunCall(Map(fun((p_3) =>
              FunCall(Reduce(fun((p_4, p_5) =>
                FunCall(add, p_4, p_5)
              )), Value(0.0f, Float),
                FunCall(Map(fun((p_6) =>
                  FunCall(mult, FunCall(Get(0), p_6), FunCall(Get(1), p_6))
                )),
                  FunCall(Zip(2), p_2, p_3)))
            )), FunCall(Transpose(), p_1))
          )), FunCall(Transpose(), p_0)))

    // TODO: Doesn't quite do what you'd expect
    Rewrite.applyRuleAtId(f, 0, ReuseRules.finishTiling)
  }
}
