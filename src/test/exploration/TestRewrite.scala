package exploration

import apart.arithmetic.Var
import ir.ast._
import opencl.executor.Executor
import ir._
import opencl.ir._
import org.junit.{AfterClass, BeforeClass, Test}
import org.junit.Assert._

object TestRewrite {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }
}

class TestRewrite {

  @Test
  def lowering(): Unit = {
    val N = Var("N")

    val f = fun(ArrayType(ArrayType(ArrayType(Float, N), N), N),
        input => Map(Map(Map(id))) $ input)

    val newLambdas = Lower.lowerByLevels(f)

    assertTrue(newLambdas.nonEmpty)
    assertTrue(newLambdas.forall(_.isGenerable))
  }

}
