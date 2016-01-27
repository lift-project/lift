package rewriting

import apart.arithmetic.Var
import rewriting.utils.{NumberPrinter, NumberExpression}
import ir._
import ir.ast._
import jdk.nashorn.internal.ir.annotations.Ignore
import opencl.executor.Executor
import opencl.ir._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestMacroRules {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }
}

class TestMacroRules {

  @Test
  def twoDBlockingNotDefined(): Unit = {
    val v_M_0 = Var("M")
    val v_K_1 = Var("K")
    val v_N_2 = Var("N")

    val f = fun(
      ArrayType(ArrayType(Float, v_M_0), v_K_1),
      ArrayType(ArrayType(Float, v_N_2), v_K_1),
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
    val v_M_0 = Var("M")
    val v_K_1 = Var("K")
    val v_N_2 = Var("N")
    val v__3 = Var("")
    val v__4 = Var("")

    val f = fun(
      ArrayType(ArrayType(Float, v_M_0), v_K_1),
      ArrayType(ArrayType(Float, v_N_2), v_K_1),
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
                          )), Value(0.0f, ArrayType(ArrayType(Float, v__3), v__4)),
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
    val v_M_0 = Var("M")
    val v_K_1 = Var("K")
    val v_N_2 = Var("N")

    val f =
      fun(
        ArrayType(ArrayType(Float, v_M_0), v_K_1),
        ArrayType(ArrayType(Float, v_N_2), v_K_1),
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
