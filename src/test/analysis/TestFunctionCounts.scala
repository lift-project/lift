package analysis

import apart.arithmetic.{Cst, SizeVar}
import ir.ArrayType
import ir.ast.{VectorizeUserFun, \, asVector}
import opencl.ir._
import opencl.generator.get_global_size
import opencl.ir.pattern.MapGlb
import opencl.ir.ast._
import org.junit.Test
import org.junit.Assert._

class TestFunctionCounts {

  val N = SizeVar("N")
  val globalSize0 = get_global_size(0)

  @Test
  def simple(): Unit = {

    val f = \(ArrayType(Float, N),
      MapGlb(plusOne) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(plusOne))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(plusOne))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(plusOne))
  }

  @Test
  def vectorised(): Unit = {

    val f = \(ArrayType(Float, N),
      MapGlb(VectorizeUserFun(4, plusOne)) o asVector(4) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(Cst(0), functionCounts.getFunctionCount(plusOne))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedCount(plusOne))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getTotalCount(plusOne))
  }

  @Test
  def builtin(): Unit = {
    val f = \(ArrayType(Float, N),
      MapGlb(\(a => fma(a, a, a))) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(fma))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(fma))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(fma))
  }
}
