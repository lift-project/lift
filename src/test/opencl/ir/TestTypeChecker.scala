package opencl.ir

import apart.arithmetic._
import ir._
import ir.ast._
import opencl.ir.pattern._
import org.junit.Test

class TestTypeChecker {

  @Test(expected = classOf[TypeException])
  def incorrectReduceSeq(): Unit = {
    val lambda = fun(
      ArrayType(Float, Var("K")),
      a => ReduceSeq(fun((acc, x) => MapSeq(fun(a => add(acc, a))) $ x), 0.0f) o Split(1) $ a
    )

    TypeChecker(lambda)
  }

}
