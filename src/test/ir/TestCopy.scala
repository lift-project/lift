package ir

import ir.ast._
import lift.arithmetic.{Cst, SizeVar, StartFromRange, Var}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

class TestCopy {

  @Test
  def testCopy(): Unit = {

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        MapWrg(fun( Arow =>
          Join() o  MapLcl(fun( Bcol =>
            toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
          )) $ B
        )) $ A
      })

    val l = Lambda.copyStructure(f)

    assertNotSame(f, l)
    assertEquals(TypeChecker(f), TypeChecker(l))
    assertFalse(Expr.visitWithState(false)(f.body, (x, y) => y || l.body.contains({ case e if e eq x => })))
  }

  @Test
  def valueWithSameStringDifferentTypes(): Unit = {
    val M = Var("M", StartFromRange(1))
    val N = Var("N", StartFromRange(1))

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(Float, M),
      ArrayType(Float, N),
      Float, Float,
      (p_0, p_1, p_2, p_3, p_4) =>
        FunCall(Map(fun((p_5) =>
          FunCall(Map(fun((p_6) =>
            FunCall(add,
              FunCall(mult, p_6, p_3),
              FunCall(mult,
                FunCall(Get(1), p_5), p_4)))),
            FunCall(Reduce(fun((p_7, p_8) =>
              FunCall(add, p_7, p_8))), Value("0.0f", Float),
              FunCall(asScalar(),
                FunCall(ReduceSeq(fun((p_9, p_10) =>
                  FunCall(VectorizeUserFun(Cst(4),add), p_9,
                    FunCall(VectorizeUserFun(Cst(4),mult),
                      FunCall(Get(0), p_10),
                      FunCall(Get(1), p_10))))), Value("0.0f", VectorType(Float, 4)),
                  FunCall(Zip(2),
                    FunCall(asVector(4), p_1),
                    FunCall(asVector(4),
                      FunCall(Get(0), p_5))))))))),
          FunCall(Zip(2), p_0, p_2)))

    val fCopy = Lambda.copyStructure(f)

    assertEquals(TypeChecker(f), TypeChecker(fCopy))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def someUndeclared(): Unit = {
    val N = SizeVar("N")

    val f = \(
      ArrayType(Float, N),
      Float,
      (A, b) => Map(\(a => add(a, b))) $ A
    )

    Lambda.copyStructure(f.body.asInstanceOf[FunCall].f.asInstanceOf[Map].f)
  }

}
