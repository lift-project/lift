package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import org.junit.{AfterClass, Assume, BeforeClass, Test}
import org.junit.Assert.{assertArrayEquals, assertEquals}


object TestArray {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }

  def toByteArray(arr: Array[Boolean]): Array[Byte] = {
    arr.map(b => (if (b) 1 else 0).toByte)
  }
}

class TestArray {
  import TestArray.toByteArray

  /**
    * Size is not statically know but the capacity is.
    * The array is filled with integers so we don't need to store the offsets.
    * Layout: [size, elt_0, elt_1, …, elt_{κ-1}]
    */
  @Test def unknownSizeReduce(): Unit = {
    val capacity = 1024
    val size = 700
    val input = Array.fill(size)(util.Random.nextInt(16))
    
    val f = fun(
      ArrayTypeWC(Int, capacity),
      in =>
        MapGlb(toGlobal(id(Int))) o ReduceSeq(addI, 0) $ in
    )

    val t = TypeChecker(f)
    assertEquals(t, ArrayTypeWSWC(Int, 1, 1))

    val (output: Array[Int], _) = Execute(128)(f, input)
    assertEquals(input.sum, output.head)
  }
  
  /**
    * Same situation but this time the output is an array of the same shape
    * and not a constant.
    */
  @Test def unknownSizeMap(): Unit = {
    val capacity = 128
    val size = 87

    val bInput = Array.fill(size)(util.Random.nextBoolean())
    val iInput = Array.fill(size)(util.Random.nextInt())
    val fInput = Array.fill(size)(util.Random.nextFloat())

    def mkMapId(st: ScalarType): Lambda1 = fun(
      ArrayTypeWC(st, capacity),
      MapGlb(toGlobal(id(st))) $ _
    )

    val (bOutput: Array[Boolean], _) = Execute(128)(mkMapId(Bool), bInput)
    assertArrayEquals(toByteArray(bInput), toByteArray(bOutput.slice(1, size + 1)))
    val (iOutput: Array[Int], _) = Execute(128)(mkMapId(Int), iInput)
    assertArrayEquals(iInput, iOutput.slice(1, size + 1))
    val (fOutput: Array[Float], _) = Execute(128)(mkMapId(Float), fInput)
    assertArrayEquals(fInput, fOutput.slice(1, size + 1), 0f)
  }

  @Test def unknownSizeMapDouble(): Unit = {
    Assume.assumeTrue("Needs double support", Executor.supportsDouble())

    val capacity = 128
    val size = 87
    val input = Array.fill(size)(util.Random.nextDouble())

    val mapId = fun(
      ArrayTypeWC(Double, capacity),
      MapGlb(toGlobal(id(Double))) $ _
    )

    val (dOutput: Array[Double], _) = Execute(128)(mapId, input)
    assertArrayEquals(input, dOutput.slice(1, size + 1), 0d)
  }

  /**
    * This time we don't know the size either but we know the shape of the
    * output.
    * Layout: [κ, size, elt_0, elt_1, elt_2, …]
    */
  @Test def inputZeroKnowledge(): Unit = {
    val size = 128
    val input = Array.fill(size)(util.Random.nextInt(16))
    
    val f = fun(
      ArrayType(Int),
      MapGlb(toGlobal(idI)) o ReduceSeq(addI, 0) $ _
    )
    
    assertEquals(TypeChecker(f), ArrayTypeWSWC(Int, 1, 1))
    
    val (output: Array[Int], _) = Execute(size)(f, input)
    assertEquals(input.sum, output.head)
  }
  
  /**
    * Here, you know nothing (Jon Snow).
    */
  @Test def zeroKnowledge(): Unit = {
    val f = fun(
      ArrayType(Int),
      in =>
        MapGlb(toGlobal(idI)) $ in
    )
    
    assertEquals(TypeChecker(f), ArrayType(Int))
    
    Compile(f)
  }
  
  /**
    * Nested arrays with no size
    */
  @Test def nestedArraysNoSize(): Unit = {
    val capacity = 128
    val size1 = 90
    val size2 = 42
    val input = Array.fill(size1, size2)(util.Random.nextInt())
    val f = fun(
      ArrayTypeWC(ArrayTypeWC(Int, capacity), capacity),
      MapGlb(MapSeq(idI)) $ _
    )
    
    assertEquals(TypeChecker(f), ArrayTypeWC(ArrayTypeWC(Int, capacity), capacity))
    
    val (outputRaw: Array[Int], _) = Execute(capacity)(f, input)
    val output = outputRaw
      .slice(1, 1 + size1 * 129)                // Remove main header and cau at `size`
      .grouped(129).map(_.slice(1, 1 + size2))  // Remove sub-arrays' headers
      .toArray
    
    assertArrayEquals(input.flatten, output.flatten)
  }
  
  /**
   * If some capacity is not known at compile time in the type of an input
   * we can still allocate it at the last minute.
   */
  @Test
  def unknownInnerCapacity(): Unit = {
    val size = 128
    val N = SizeVar("N")
    
    val f = fun(
      ArrayTypeWSWC(ArrayType(Int), N),
      
      Join() o
      MapGlb(MapSeq(toGlobal(idI)) o ReduceSeq(addI, 0)) $ _
    )
    
    val input = Array.fill(size)(
      Array.fill(4 + util.Random.nextInt(8))(util.Random.nextInt(16))
    )
    val (output: Array[Int], _) = Execute(size)(f, input)
    assertArrayEquals(input.map(_.sum), output)
  }
  
  /**
    * Nested arrays.
    */
  @Test def nestedArraysZeroKnowledge(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Int)),
      in =>
        MapGlb(MapSeq(toGlobal(idI))) $ in
    )
  
    assertEquals(TypeChecker(f), ArrayType(ArrayType(Int)))
  
    Compile(f)
  }
}
