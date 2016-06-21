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

  private val N = SizeVar("N")
  private val globalSize0 = get_global_size(0)

  @Test
  def simple(): Unit = {

    val f = \(ArrayType(Float, N),
      MapGlb(plusOne) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(plusOne))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(plusOne))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(plusOne))
    assertEquals(Cst(0), functionCounts.getAddMultCount())
    assertEquals(Cst(0), functionCounts.getVectorisedAddMultCount())
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
    assertEquals(Cst(0), functionCounts.getAddMultCount())
    assertEquals(Cst(0), functionCounts.getVectorisedAddMultCount())
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
    assertEquals(Cst(0), functionCounts.getAddMultCount())
    assertEquals(Cst(0), functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def addMult(): Unit = {

    val f = \(ArrayType(Float, N),
      MapGlb(\(a => add(mult(a, a), a))) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(add))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(add))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(add))

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(mult))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(mult))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(mult))

    assertEquals(N /^ globalSize0, functionCounts.getAddMultCount())
    assertEquals(Cst(0), functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def addMult2(): Unit = {

    val f = \(ArrayType(Float, N),
      MapGlb(\(a => add(a, mult(a, a)))) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(add))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(add))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(add))

    assertEquals(N /^ globalSize0, functionCounts.getFunctionCount(mult))
    assertEquals(Cst(0), functionCounts.getVectorisedCount(mult))
    assertEquals(N /^ globalSize0, functionCounts.getTotalCount(mult))

    assertEquals(N /^ globalSize0, functionCounts.getAddMultCount())
    assertEquals(Cst(0), functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def addMultVec(): Unit = {

    val f = \(ArrayType(Float, N),
      MapGlb(\(a =>
        VectorizeUserFun(4, add)(VectorizeUserFun(4, mult)(a, a), a)
      )) o asVector(4) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(Cst(0), functionCounts.getFunctionCount(add))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedCount(add))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getTotalCount(add))

    assertEquals(Cst(0), functionCounts.getFunctionCount(mult))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedCount(mult))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getTotalCount(mult))

    assertEquals(Cst(0), functionCounts.getAddMultCount())
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedAddMultCount())
  }

  @Test
  def addMultVec2(): Unit = {

    val f = \(ArrayType(Float, N),
      MapGlb(\(a =>
        VectorizeUserFun(4, add)(a, VectorizeUserFun(4, mult)(a, a))
      )) o asVector(4) $ _
    )

    val functionCounts = FunctionCounts(f)

    assertEquals(Cst(0), functionCounts.getFunctionCount(add))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedCount(add))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getTotalCount(add))

    assertEquals(Cst(0), functionCounts.getFunctionCount(mult))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedCount(mult))
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getTotalCount(mult))

    assertEquals(Cst(0), functionCounts.getAddMultCount())
    assertEquals(N /^ 4 /^ globalSize0, functionCounts.getVectorisedAddMultCount())
  }
}
