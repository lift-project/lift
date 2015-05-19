package opencl.ir

import arithmetic.{ExprSimplifier, Var}
import ir.UserFunDef._
import ir._
import opencl.executor.Compile
import org.junit.Assert._
import org.junit.{Test, Ignore}

class TestMemory {

  @Ignore
  @Test
  def zipInsideToLocalAllocation(): Unit = {
    val N = Var("N")

    val arrayType = ArrayType(ArrayType(Float, N), N)
    val f = fun(
      arrayType,
      arrayType,
      (A, B) =>
        toLocal(MapGlb(fun( tuple =>
          MapSeq(fun( tuple =>
            Tuple(MapSeq(id) $ Get(tuple, 0), MapSeq(id) $ Get(tuple, 1))
          )) $ Zip(Split(1) $ Get(tuple, 0), Split(1) $ Get(tuple, 1))
        ))) $ Zip(A, B)
    )

    Compile(f)

    assertEquals(f.body.mem.getClass, classOf[OpenCLMemoryCollection])
    val subMemories = f.body.mem.asInstanceOf[OpenCLMemoryCollection].subMemories
    assertEquals(subMemories.length, 2)
    assertEquals(subMemories(0).addressSpace, LocalMemory)
    assertEquals(subMemories(1).addressSpace, LocalMemory)
    assertEquals(ExprSimplifier.simplify(subMemories(0).size),
      ExprSimplifier.simplify(OpenCLMemory.getMaxSizeInBytes(arrayType)))
    assertEquals(ExprSimplifier.simplify(subMemories(1).size),
      ExprSimplifier.simplify(OpenCLMemory.getMaxSizeInBytes(arrayType)))
  }
}
