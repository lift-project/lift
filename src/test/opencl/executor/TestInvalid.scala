/**
 * Test cases for diagnostics and error recovery in the Executor.
 */

package opencl.executor

import arithmetic.Var
import ir._
import opencl.ir.{MapGlb, Float}
import org.junit.Assert._
import org.junit._

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
  // Dummy user function
  val fct = UserFunDef("afunc", "array", " return array * 2.0f; ", Float, Float)
  // Dummy function
  val f = fun(ArrayType(Float, Var("N")), (in) => MapGlb(fun(a => fct(a))) $ in )

  // Test invalid 1D array with default local size
  @Test(expected=classOf[InvalidIndexSpaceException])
  def Indivisible1DRange(): Unit = {
    // Input Array
    val inputSize = 31
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(inputSize)(f, array, inputSize) // should throw

    // explicit failure
    assert(assertion = false)
  }

  // Test invalid 1D array with explicit local size
  @Test(expected=classOf[InvalidIndexSpaceException])
  def Indivisible1DExplicitRange(): Unit = {
    // Input Array
    val inputSize = 499
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(inputSize, 100)(f, array, inputSize) // should throw

    // explicit failure
    assert(assertion = false)
  }

  // global size < local size
  @Test(expected=classOf[InvalidIndexSpaceException])
  def Invalid1DExplicitRange(): Unit = {
    // Input Array
    val inputSize = 64
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(inputSize)(f, array, inputSize) // should throw

    // explicit failure
    assert(assertion = false)
  }

  // global size == 0
  @Test(expected=classOf[InvalidGlobalSizeException])
  def EmptyGlobalSize(): Unit = {
    // Input Array
    val inputSize = 128
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(0)(f, array, inputSize) // should throw

    // explicit failure
    assert(assertion = false)
  }

  // local size == 0
  @Test(expected=classOf[InvalidGlobalSizeException])
  def EmptyLocalSize(): Unit = {
    // Input Array
    val inputSize = 128
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(inputSize,0)(f, array, inputSize) // should throw

    // explicit failure
    assert(assertion = false)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def WrongNumberOfArguments(): Unit = {
    // Input Array
    val inputSize = 128
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(inputSize)(f, array, array, inputSize) // should throw

    // explicit failure
    assert(assertion = false)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def WrongArgumentType(): Unit = {
    // Input Array
    val inputSize = 128
    val array = Array.fill(inputSize)(util.Random.nextInt())

    // execute
    Execute(inputSize)(f, array, inputSize) // should throw

    // explicit failure
    assert(assertion = false)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def NamesAndTypesDontMatchInUserFunDef(): Unit = {
    UserFunDef("inc", Array("x", "y"),
      " return x+1.0; ", Seq(Float), Float)
  }


  @Test(expected=classOf[IllegalArgumentException])
  def NamesAndTypesDontMatchInUserFunDef2(): Unit = {
    UserFunDef("inc", Array("x"),
      " return x+1.0; ", Seq(Float, Float), Float)
  }

  @Ignore
  @Test def InvalidUserFunction(): Unit = {
    // domain size
    val inputSize = 128
    val N = Var("N")

    // Input variables
    val xs = Array.fill(inputSize)(util.Random.nextFloat())

    // Cross validation
    val gold = xs.map(_ + 1)

    // user function
    val fct = UserFunDef("inc", "x",
      " return x+1.0; ", Float, Float)

    // Expression
    val f = fun(
      ArrayType(Float, N),
      (xs) => MapGlb(
        fun(x => fct(x))
      ) $ xs
    )

    // execute
    val (output: Array[Float], runtime) = Execute(inputSize)(f, xs)

    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.001f)
  }

  // TODO(tlutz): missing test cases:
  // - invalid 2D volume
  // - invalid 3D volume
}
