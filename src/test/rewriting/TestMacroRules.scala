package rewriting

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.Executor
import opencl.ir._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import rewriting.utils.NumberExpression

object TestMacroRules {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    Executor.shutdown()
  }
}

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

    assertFalse(MacroRules.apply2DRegisterBlocking.isDefinedAt(expr))
  }


  @Test
  def twoDBlockingNotDefined2(): Unit = {
    val v_M_0 = SizeVar("M")
    val v_K_1 = SizeVar("K")
    val v_N_2 = SizeVar("N")
    val v__3 = SizeVar("")
    val v__4 = SizeVar("")

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

    val result = Rewrite.applyRuleAtId(f, 2, MacroRules.apply2DRegisterBlocking)
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
    Rewrite.applyRuleAtId(f, 0, MacroRules.finishTiling)
  }
}
