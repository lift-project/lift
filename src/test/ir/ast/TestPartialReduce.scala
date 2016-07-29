package ir.ast

import apart.arithmetic.SizeVar
import ir.ArrayType
import opencl.ir._
import org.junit.Test
import org.junit.Assert._

class TestPartialReduce {

  @Test
  def testUnapply0(): Unit = {

    val lambda = fun((x,y) => add(x, y))

    val f = fun(
      ArrayType(Float, SizeVar("N")),
      input => Reduce(lambda, 0.0f) $ input
    )

    val x = f.body match {
      case FunCall(AbstractPartRed(reduceLambda), _, _) => reduceLambda
    }

    assertTrue(x.eq(lambda))
  }

  @Test
  def testUnapply1(): Unit = {

    val lambda = fun((x,y) => add(x, y))

    val f = fun(
      ArrayType(Float, SizeVar("N")),
      input => PartRed(lambda, 0.0f) $ input
    )

    val x = f.body match {
      case FunCall(AbstractPartRed(reduceLambda), _, _) => reduceLambda
    }

    assertTrue(x.eq(lambda))
  }

}
