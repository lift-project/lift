package opencl.generator

import ir.{ArrayType, ArrayTypeWC, ArrayTypeWSWC, TypeChecker}
import ir.ast.fun
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Execute}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import org.junit.Test
import org.junit.Assert.assertEquals


class TestArray {
  /**
    * Size is not statically know but the capacity is.
    * The array is filled with integers so we don't need to store the offsets.
    * Layout: [size, elt_0, elt_1, …, elt_{κ-1}]
    */
  @Test def unknownSizeReduce(): Unit = {
    val f = fun(
      ArrayTypeWC(Int, 1024),
      in =>
        MapGlb(toGlobal(id(Int))) o ReduceSeq(addI, 0) $ in
    )

    val t = TypeChecker(f)
    assertEquals(t, ArrayTypeWSWC(Int, 1, 1))

    Compile(f)
  }
  
  /**
    * Same situation but this time the output is an array of the same shape
    * and not a constant.
    */
  @Test def unknownSizeMap(): Unit = {
    val f = fun(
      ArrayTypeWC(Int, 1024),
      in =>
        MapGlb(toGlobal(idI)) $ in
    )
    
    assertEquals(TypeChecker(f), ArrayTypeWC(Int, 1024))
    
    Compile(f)
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
    * Nested arrays.
    * TODO: shape
    */
  @Test def nestedArrays(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Int)),
      in =>
        MapGlb(MapSeq(toGlobal(idI))) $ in
    )
  
    assertEquals(TypeChecker(f), ArrayType(ArrayType(Int)))
  
    Compile(f)
  }
}
