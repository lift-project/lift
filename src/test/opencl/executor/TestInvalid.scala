/**
 * Test cases for diagnostics and error recovery in the Executor.
 */

package opencl.executor

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.generator.IllegalKernel
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assume.assumeFalse
import org.junit.Assert.assertEquals
import org.junit._

object TestInvalid {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestInvalid {
  // Dummy user function
  val fct = UserFun("afunc", "array", " return array * 2.0f; ", Float, Float)
  // Dummy function
  val f = fun(ArrayTypeWSWC(Float, SizeVar("N")), (in) => MapGlb(fun(a => fct(a))) $ in )
  val f2 = fun(ArrayTypeWSWC(Float, SizeVar("N")), ArrayTypeWSWC(Float, SizeVar("M")),
    (in1, in2) => MapGlb(fun(a => fct(a))) $ in1 )
  val f3 = fun(ArrayTypeWSWC(Float, SizeVar("N")), ArrayTypeWSWC(Float, SizeVar("M")), ArrayTypeWSWC(Float, SizeVar("O")),
    (in1, in2, in3) => MapGlb(fun(a => fct(a))) $ in1 )

  // Test invalid 1D array with default local size
  @Test(expected=classOf[InvalidIndexSpaceException])
  def Indivisible1DRange(): Unit = {
    // Input Array
    val inputSize = 31
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(inputSize)(f, array) // should throw

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
    Execute(100, inputSize)(f, array) // should throw

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
    Execute(inputSize)(f, array) // should throw

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
    Execute(0)(f, array) // should throw

    // explicit failure
    assert(assertion = false)
  }

  // global size < 0
  @Test(expected=classOf[InvalidGlobalSizeException])
  def NegativeGlobalSize(): Unit = {
    // Input Array
    val inputSize = 128
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(-inputSize)(f, array) // should throw

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
    Execute(inputSize,0)(f, array) // should throw

    // explicit failure
    assert(assertion = false)
  }

  // local size == 0
  @Test(expected=classOf[InvalidGlobalSizeException])
  def NegativeLocalSize(): Unit = {
    // Input Array
    val inputSize = 128
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(inputSize,-inputSize)(f, array) // should throw

    // explicit failure
    assert(assertion = false)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def WrongNumberOfArguments(): Unit = {
    // Input Array
    val inputSize = 128
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(inputSize)(f, array, array) // should throw

    // explicit failure
    assert(assertion = false)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def WrongArgumentType(): Unit = {
    // Input Array
    val inputSize = 128
    val array = Array.fill(inputSize)(util.Random.nextInt())

    // execute
    Execute(inputSize)(f, array) // should throw

    // explicit failure
    assert(assertion = false)
  }

  @Test(expected=classOf[IllegalArgumentException])
  def NamesAndTypesDontMatchInUserFunDef(): Unit = {
    UserFun("inc", Array("x", "y"),
      " return x+1.0; ", Seq(Float), Float)
  }


  @Test(expected=classOf[IllegalArgumentException])
  def NamesAndTypesDontMatchInUserFunDef2(): Unit = {
    UserFun("inc", Array("x"),
      " return x+1.0; ", Seq(Float, Float), Float)
  }

  // Test invalid 2D space with explicit local size
  @Test(expected=classOf[InvalidIndexSpaceException])
  def Indivisible2DExplicitRange(): Unit = {
    // Input Array
    val inputSize = 499
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(100, 100, inputSize + 1, inputSize, (false, false))(f2, array, array) // should throw

    // explicit failure
    assert(assertion = false)
  }

  // global size < local size
  @Test(expected=classOf[InvalidIndexSpaceException])
  def Invalid2DExplicitRange(): Unit = {
    // Input Array
    val inputSize = 64
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(inputSize, 128*2, inputSize, inputSize, (false, false))(f2, array, array) // should throw

    // explicit failure
    assert(assertion = false)
  }


  // Test invalid 3D space with explicit local size
  @Test(expected=classOf[InvalidIndexSpaceException])
  def Indivisible3DExplicitRange(): Unit = {
    // Input Array
    val inputSize = 499
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(100, 100, 100, inputSize, inputSize, inputSize + 1, (false, false))(f3, array, array, array) // should throw

    // explicit failure
    assert(assertion = false)
  }

  // global size < local size
  @Test(expected=classOf[InvalidIndexSpaceException])
  def Invalid3DExplicitRange(): Unit = {
    // Input Array
    val inputSize = 64
    val array = Array.fill(inputSize)(util.Random.nextFloat())

    // execute
    Execute(inputSize, inputSize, inputSize*2, inputSize, inputSize, inputSize, (false, false)
    )(f3, array, array, array) // should throw

    // explicit failure
    assert(assertion = false)
  }

  @Test(expected = classOf[DeviceCapabilityException])
  def TooMuchLocalMemoryRequired(): Unit = {
    val localMemSize = Executor.getDeviceLocalMemSize

    val inputSize = math.ceil((localMemSize + 4 ) / 4.0).toInt

    val input = Array.ofDim[Float](inputSize)

    val f = fun(
      ArrayTypeWSWC(Float, inputSize),
      in => Join() o MapWrg(toGlobal(MapLcl(id)) o toLocal(MapLcl(id))) o Split(inputSize) $ in
    )

    Execute(1, inputSize)(f, input)
  }

  @Test(expected = classOf[IllegalKernel])
  def notWritingToGlobal(): Unit = {
    val inputSize = 1024
    val input = Array.ofDim[Float](inputSize)

    val f = fun(
      ArrayTypeWSWC(Float, inputSize),
      in => Join() o MapWrg(toLocal(MapLcl(id))) o Split(inputSize) $ in
    )

    Execute(1, inputSize)(f, input)
  }

  @Test(expected = classOf[IllegalKernel])
  def concreteMap(): Unit = {
    val inputSize = 1024
    val input = Array.ofDim[Float](inputSize)

    val f = fun(
      ArrayTypeWSWC(Float, inputSize),
      in => Join() o
        MapGlb(Map(id)) o
        Split(inputSize) $ in
    )

    Execute(1, inputSize)(f, input)
  }

  @Test(expected = classOf[IllegalKernel])
  def localWithoutMapWrg(): Unit = {
    val inputSize = 1024
    val input = Array.ofDim[Float](inputSize)

    val f = fun(
      ArrayTypeWSWC(Float, inputSize),
      in => Join() o
        MapGlb(toGlobal(MapSeq(id)) o toLocal(MapSeq(id))) o
        Split(inputSize) $ in
    )

    Execute(1, inputSize)(f, input)
  }

  @Test(expected = classOf[IllegalKernel])
  def mapLocalWithoutMapWrg(): Unit = {
    val inputSize = 1024
    val input = Array.ofDim[Float](inputSize)

    val f = fun(
      ArrayTypeWSWC(Float, inputSize),
      in => Join() o
        MapGlb(toGlobal(MapSeq(id)) o MapLcl(id)) o
        Split(inputSize) $ in
    )

    Execute(1, inputSize)(f, input)
  }

  @Test(expected = classOf[IllegalKernel])
  def illegalMapNesting(): Unit = {
    val inputSize = 1024
    val input = Array.ofDim[Float](inputSize)

    val f = fun(
      ArrayTypeWSWC(Float, inputSize),
      in => Join() o MapGlb(MapGlb(id)) o Split(inputSize) $ in
    )

    Execute(1, inputSize)(f, input)
  }

  // Trigger an error in the executor in the executor and recover
  @Test
  def ExecutorFailureRecovery(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    try {
      Executor.execute(Build("this is not a valid OpenCL Kernel and should crash the executor"), 1, 1, 1, 1, 1, 1, Array())
    } catch {
      case ea: Executor.ExecutorFailureException =>
        ea.consume()
      case e: Exception =>
        assert(assertion = false)
    }

    // This should work
    try {
      println("Executing a valid kernel")
      Executor.execute(Build("kernel void KERNEL(){}"), 1, 1, 1, 1, 1, 1, Array())
    } catch {
      case _: Throwable => assert(assertion = false)
    }
  }

  // Test allocating too much local memory
  @Test def AllocateTooMuchLocalMemory(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    try {
      // Allocate 4 times the maximum
      val arg = LocalArg.create(Executor.getDeviceMaxMemAllocSize.asInstanceOf[Int])
      Executor.execute(Build("kernel void KERNEL(local float* mem){}"), 1, 1, 1, 1, 1, 1, Array(arg))
    } catch {
      case e: Executor.ExecutorFailureException =>
        e.consume()

      case _: Throwable =>
      // This might be acceptable depending on how we handle insufficient resources
        assert(assertion = false)
    }

    // This should work
    try {
      println("Executing a valid kernel")
      Executor.execute(Build("kernel void KERNEL(){}"), 1, 1, 1, 1, 1, 1, Array())
    } catch {
      case _: Throwable => assert(assertion = false)
    }
  }

  @Test(expected = classOf[DeviceCapabilityException])
  def workgroupTooBig(): Unit = {

    val maxGroupSize = Executor.getDeviceMaxWorkGroupSize.asInstanceOf[Int]

    val f = fun(ArrayTypeWSWC(Float, SizeVar("N")),
      input => MapGlb(id) $ input
    )

    val size = maxGroupSize * 2
    val input = Array.fill(size)(util.Random.nextFloat())

    Execute(size, size)(f, input)
  }

  @Ignore
  @Test(expected = classOf[IllegalKernel])
  def illegalGather(): Unit = {
    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => Gather(reverse) o MapSeq(id) $ input
    )

    Compile(f)
  }

  @Ignore
  @Test(expected = classOf[IllegalKernel])
  def illegalScatter(): Unit = {
    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => MapSeq(id) o Scatter(reverse) $ input
    )

    Compile(f)
  }

  @Ignore
  @Test(expected = classOf[IllegalKernel])
  def conflictingScatter(): Unit = {
    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => fun(x => MapSeq(fun(y => add(y._0, y._1))) $ Zip(x, Scatter(reverse) $ x)) o MapSeq(id) $ input
    )

    Compile(f)
  }

  // Issue #98, snippet 1
  @Test
  def incorrectInference(): Unit = {
    val f = \(
      ArrayType(Float, 3),
      ArrayType(Float, SizeVar("N")),
      (a, b) => MapGlb(plusOne) $ b
    )
  
    val input1 = Array.fill(3)(util.Random.nextFloat()  )
    val input2 = Array.fill(12)(util.Random.nextFloat()  )
  
    val floats = Execute(1, 1)(f, input1, input2)._1.asInstanceOf[Array[Float]]
    assertEquals(input2.length, floats.length)
  }
  
  // Issue #98, snippet 2
  @Test(expected = classOf[IllegalKernelArgument])
  def inconsistentInference(): Unit = {
    val sizeVar = SizeVar("N")
    val f = \(
      ArrayType(Float, sizeVar),
      ArrayType(Float, sizeVar),
      (a, b) => MapGlb(plusOne) $ b
    )
    
    val input1 = Array.fill(4)(util.Random.nextFloat()  )
    val input2 = Array.fill(12)(util.Random.nextFloat()  )
    
    Execute(1, 1)(f, input1, input2)
  }
  
  // Issue #98, snippet 3
  @Test(expected = classOf[IllegalKernelArgument])
  def incorrectUsage(): Unit = {
    val f = \(
      ArrayType(Float, 6),
      MapGlb(plusOne) $ _
    )
    
    val input1 = Array.fill(4)(util.Random.nextFloat()  )
    
    val (floats: Array[Float], _) = Execute(1, 1)(f, input1)
    assertEquals(input1.length, floats.length)
  }
}
