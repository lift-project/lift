package ir.ast

import ir.{ArrayTypeWSWC, TupleType}
import lift.arithmetic.SizeVar
import opencl.ir._
import org.junit.Assert._
import org.junit.Test

class TestLambda {
  @Test
  def lambdaFactoryMethodBug(): Unit = {
    val N = SizeVar("N")

    val f = fun(TupleType(ArrayTypeWSWC(Float, N), ArrayTypeWSWC(Float, N)),
      input => fun(x =>Tuple(asScalar() $ Get(x, 0), asScalar() $ Get(x, 1))) o
        fun(y => Tuple(Map(id.vectorize(4)) $ Get(y, 0), Map(id.vectorize(4)) $ Get(y, 1)))
        $ Tuple(asVector(4) $ Get(input, 0), asVector(4) $ Get(input, 1)))

    val function: PartialFunction[Expr, Unit] = {
      case FunCall(Lambda(_, FunCall(Lambda(_, _, _), FunCall(_, _*)), _), _) =>
    }

    assertTrue(function.isDefinedAt(f.body))
  }
}
