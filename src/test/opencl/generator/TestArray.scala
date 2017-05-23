package opencl.generator

import ir.ast.{Zip, fun, _}
import ir.{ArrayType, ArrayTypeWC, ArrayTypeWSWC, TypeChecker, _}
import lift.arithmetic.{Cst, SizeVar}
import opencl.executor.{Compile, Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import org.junit.Assert.{assertArrayEquals, assertEquals}
import org.junit.{AfterClass, Assume, BeforeClass, Test}


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

  def assertBoolArrayEquals(expected: Array[Boolean], actual: Array[Boolean]): Unit = {
    def toByte(b: Boolean): Byte = (if (b) 1 else 0).toByte
    assertArrayEquals(expected.map(toByte), actual.map(toByte))
  }
}

class TestArray {
  import TestArray.assertBoolArrayEquals

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

    val exec = Execute(128)
    val (output, _) = exec[Vector[Int]](f, input)
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

    val exec = Execute(128)
    val (bOutput, _) = exec[Vector[Boolean]](mkMapId(Bool), bInput)
    assertBoolArrayEquals(bInput, bOutput.toArray)
    val (iOutput, _) = exec[Vector[Int]](mkMapId(Int), iInput)
    assertArrayEquals(iInput, iOutput.toArray)
    val (fOutput, _) = exec[Vector[Float]](mkMapId(Float), fInput)
    assertArrayEquals(fInput, fOutput.toArray, 0f)
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

    val exec = Execute(128)
    val (dOutput, _) = exec[Vector[Double]](mapId, input)
    assertArrayEquals(input, dOutput.toArray, 0d)
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
    
    val exec = Execute(128)
    val (output, _) = exec[Vector[Int]](f, input)
    assertEquals(Vector(input.sum), output)
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

    val exec = Execute(capacity)
    val (output, _) = exec[Vector[Vector[Int]]](f, input)

    assertArrayEquals(input.flatten, output.flatten.toArray)
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
    val exec = Execute(size)
    val (output, _) = exec[Vector[Int]](f, input)
    assertArrayEquals(input.map(_.sum), output.toArray)
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

  @Test def arrZipMap(): Unit = {
    val f = fun(
      ArrayType(Float), ArrayType(Float), (p1, p2) =>
        ReduceSeq(add) o MapSeq(add) $ Zip(p1, p2)
    )

    assertEquals(TypeChecker(f), ArrayTypeWSWC(Float, Cst(1)))

    Compile(f)
  }
}
