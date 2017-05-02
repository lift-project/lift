package opencl.generator

import ir.{ArrayType, ArrayTypeWC, ArrayTypeWSWC, TypeChecker}
import ir.ast.fun
import opencl.executor.{Compile, Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import org.junit.{AfterClass, BeforeClass, Test}
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
}

class TestArray {
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
    val capacity = 1024
    val size = 842
    val input = Array.fill(size)(util.Random.nextInt())
    
    val f = fun(
      ArrayTypeWC(Int, capacity),
      in =>
        MapGlb(toGlobal(idI)) $ in
    )
    
    assertEquals(TypeChecker(f), ArrayTypeWC(Int, capacity))
    
    val (outputRaw: Array[Int], _) = Execute(128)(f, input)
    // Decode: remove header and cut at `size`
    val output = outputRaw.slice(1, size + 1) // Decode
    
    assertArrayEquals(input, output)
  }
  
  /**
    * This time we don't know the size either but we know the shape of the
    * output.
    * Layout: [κ, size, elt_0, elt_1, elt_2, …]
    */
  @Test def inputZeroKnowledge(): Unit = {
    val f = fun(
      ArrayType(Int),
      in =>
        MapGlb(toGlobal(idI)) o ReduceSeq(addI, 0) $ in
    )
    
    assertEquals(TypeChecker(f), ArrayTypeWSWC(Int, 1, 1))
    
    Compile(f)
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
