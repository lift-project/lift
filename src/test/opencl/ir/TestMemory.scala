package opencl.ir

import ir._
import ir.ast._
import lift.arithmetic.{ArithExpr, SizeVar}
import opencl.generator.IllegalKernel
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

class TestMemory {
  import TestMemory._

  @Test
  def zipInsideToLocalAllocation(): Unit = {
    val N = SizeVar("N")

    val arrayType = ArrayTypeWSWC(Float, N)
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (A, B) =>
        MapWrg(fun( tuple =>
          toLocal(MapLcl(fun( tuple =>
            Tuple(MapSeq(id) $ Get(tuple, 0), MapSeq(id) $ Get(tuple, 1))
          ))) $ Zip(Split(1) $ Get(tuple, 0), Split(1) $ Get(tuple, 1))
        )) $ Zip(A, B)
    )

    TypeChecker(f)

    try {
      InferOpenCLAddressSpace(f)
    } catch {
      // Don't care that final is in local
      case _: IllegalKernel =>
    }

    OpenCLMemoryAllocator(f)

    assertEquals(classOf[OpenCLMemoryCollection], f.body.mem.getClass)
    val subMemories = f.body.mem.asInstanceOf[OpenCLMemoryCollection].subMemories
    assertEquals(2, subMemories.length)
    assertEquals(LocalMemory, subMemories(0).addressSpace)
    assertEquals(LocalMemory, subMemories(1).addressSpace)
    assertEquals(Type.getAllocatedSize(arrayType), subMemories(0).size)
    assertEquals(Type.getAllocatedSize(arrayType), subMemories(1).size)
  }

  @Test
  def zipInsideToGlobalAllocation(): Unit = {
    val N = SizeVar("N")

    val arrayType = ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N)
    val f = fun(
      arrayType,
      arrayType,
      (A, B) =>
        toGlobal(MapGlb(fun( tuple =>
          MapSeq(fun( tuple =>
            Tuple(MapSeq(id) $ Get(tuple, 0), MapSeq(id) $ Get(tuple, 1))
          )) $ Zip(Split(1) $ Get(tuple, 0), Split(1) $ Get(tuple, 1))
        ))) $ Zip(A, B)
    )

    TypeChecker(f)

    try {
      InferOpenCLAddressSpace(f)
    } catch {
      // Don't care the output is a tuple
      case _: IllegalKernel =>
    }

    OpenCLMemoryAllocator(f)

    assertEquals(classOf[OpenCLMemoryCollection], f.body.mem.getClass)
    val subMemories = f.body.mem.asInstanceOf[OpenCLMemoryCollection].subMemories
    assertEquals(2, subMemories.length)
    assertEquals(GlobalMemory, subMemories(0).addressSpace)
    assertEquals(GlobalMemory, subMemories(1).addressSpace)
    assertEquals(Type.getAllocatedSize(arrayType), subMemories(0).size)
    assertEquals(Type.getAllocatedSize(arrayType), subMemories(1).size)
  }

  @Test
  def zipAllocation(): Unit = {
    val N = SizeVar("N")

    val arrayType = ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N)
    val f = fun(
      arrayType,
      arrayType,
      (A, B) =>
        toGlobal(MapGlb(fun( tuple =>
          MapSeq(fun( tuple =>
            Tuple(MapSeq(id) $ Get(tuple, 0), MapSeq(id) $ Get(tuple, 1))
          )) $ Zip(Split(1) $ Get(tuple, 0), Split(1) $ Get(tuple, 1))
        ))) $ Zip(A, B)
    )

    TypeChecker(f)

    try {
      InferOpenCLAddressSpace(f)
    } catch {
      // Don't care the output is a tuple
      case _: IllegalKernel =>
    }

    OpenCLMemoryAllocator(f)

    assertEquals(classOf[OpenCLMemoryCollection], f.body.mem.getClass)
    val subMemories = f.body.mem.asInstanceOf[OpenCLMemoryCollection].subMemories
    assertEquals(2, subMemories.length)
    assertEquals(GlobalMemory, subMemories(0).addressSpace)
    assertEquals(GlobalMemory, subMemories(1).addressSpace)
    assertEquals(Type.getAllocatedSize(arrayType), subMemories(0).size)
    assertEquals(Type.getAllocatedSize(arrayType), subMemories(1).size)
  }

  @Test
  def allocHeaders(): Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    val O = SizeVar("O")

    testAlloc(ArrayTypeWC(Double, M), 8 + 8 * M)
    testAlloc(ArrayTypeWC(Bool, M), 4 + ((M + 3)/4)*4)
    testAlloc(ArrayTypeWC(ArrayTypeWC(Double, N), M), 8 + M * (8 + 8 * N))
    testAlloc(ArrayTypeWC(ArrayTypeWSWC(ArrayTypeWC(Double, O), N, N), M), 8 + M * N * (8 + O * 8))
  }

  @Test
  def allocTuples(): Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")

    testAlloc(ArrayTypeWSWC(TupleType(Int, Bool), N), 8 * N)
    testAlloc(ArrayTypeWC(TupleType(Float4, Float), N), 20 + N * 5 * 4)
    testAlloc(ArrayTypeWSWC(ArrayTypeWC(TupleType(Double, TupleType(Bool, Bool)), M), N), N * (3 * 8 + M * (3 * 8)))
  }
}

object TestMemory {
  private def idForType(ty: Type): Lambda = ty match {
    case ArrayType(elemT) => MapSeq(idForType(elemT))
    case _ => toGlobal(id(ty))
  }

  def testAlloc(ty: Type, expected: ArithExpr): Unit = {
    val f = fun(ty, idForType(ty) $ _)
    TypeChecker(f)
    InferOpenCLAddressSpace(f)
    OpenCLMemoryAllocator(f)
    assertEquals(expected, Type.getAllocatedSize(ty))
    assertEquals(expected, f.body.mem.size)
  }
}
