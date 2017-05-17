package ir.ast

import ir.ArrayTypeWSWC
import lift.arithmetic.SizeVar
import opencl.ir._
import org.junit.Assert._
import org.junit.Test

class TestPartialReduce {

  @Test
  def testUnapply0(): Unit = {

    val lambda = fun((x,y) => add(x, y))

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input => Reduce(lambda, 0.0f) $ input
    )

    val x = f.body match {
      case FunCall(AbstractPartRed(reduceLambda), _, _) => reduceLambda
    }

    assertSame(lambda, x)
  }

  @Test
  def testUnapply1(): Unit = {

    val lambda = fun((x,y) => add(x, y))

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input => PartRed(lambda, 0.0f) $ input
    )

    val x = f.body match {
      case FunCall(AbstractPartRed(reduceLambda), _, _) => reduceLambda
    }

    assertSame(lambda, x)
  }

}
