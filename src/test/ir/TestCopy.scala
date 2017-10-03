package ir

import ir.ast._
import lift.arithmetic.SizeVar
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

    val l = Lambda.copy(f)

    assertNotSame(f, l)
    assertEquals(TypeChecker(f), TypeChecker(l))
    assertFalse(Expr.visitWithState(false)(f.body, (x, y) => y || l.body.contains({ case e if e eq x => })))
  }

}
