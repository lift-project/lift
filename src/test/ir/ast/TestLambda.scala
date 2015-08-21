package ir.ast

import apart.arithmetic.Var
import ir.{ArrayType, TupleType}
import opencl.ir._
import org.junit.Assert._
import org.junit.Test

class TestLambda {
  @Test
  def lambdaFactoryMethodBug(): Unit = {
    val N = Var("N")

    val f = fun(TupleType(ArrayType(Float, N), ArrayType(Float, N)),
      input => fun(x =>Tuple(asScalar() $ Get(x, 0), asScalar() $ Get(x, 1))) o
        fun(y => Tuple(Map(id.vectorize(4)) $ Get(y, 0), Map(id.vectorize(4)) $ Get(y, 1)))
        $ Tuple(asVector(4) $ Get(input, 0), asVector(4) $ Get(input, 1)))

    val function: PartialFunction[Expr, Unit] = {
      case FunCall(Lambda(_, _), FunCall(Lambda(_, _), FunCall(_, _*))) =>
    }

    assertTrue(function.isDefinedAt(f.body))
  }
}
