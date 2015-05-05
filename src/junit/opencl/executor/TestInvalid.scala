/**
 * Test cases for diagnostics and error recovery in the Executor.
 */

package opencl.executor

import arithmetic.Var
import ir._
import opencl.ir.{MapGlb, Float}
import org.junit.{Test, AfterClass, BeforeClass}

object TestInvalid {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestInvalid {
  // Test invalid 1D array with default local size
  @Test(expected=classOf[InvalidIndexSpaceException])
  def Indivisible1DRange(): Unit = {
    // global size
    val inputSize = 31

    // dummy input
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // dummy user function
    val fct = UserFunDef("afunc", "array", " return array * 2.0f; ", Seq(Float), Float)

    // Expression
    val f = fun(
      ArrayType(Float, Var("N")),
      (in) => MapGlb(fun(a => fct(a))) $ in )

    // execute
    val (output, runtime) = Execute(inputSize)(f, array, inputSize) // should throw
    println("runtime = " + runtime)

    // explicit failure
    assert(assertion = false)
  }

  // TODO(tlutz): missing test cases:
  // - invalid 2D volume
  // - invalid 3D volume
  // - explicit local size
  // - local size > global size
  // - local size == 0
  // - global size == 0
}
