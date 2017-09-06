package opencl.ir.ast

import ir.ArrayTypeWSWC
import ir.ast.{\, asVector}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern.MapGlb
import org.junit.Assert._
import org.junit.Test

object TestBuiltIn extends TestWithExecutor

class TestBuiltIn {

  private val N = SizeVar("")
  private val size = 1024
  private val input = Array.fill(size)(util.Random.nextFloat())

  @Test
  def testDot(): Unit = {

    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(\(a => dot(a, a))) o asVector(4) $ _
    )

    val (output: Array[Float], _) = Execute(size)(f, input)

    val gold = input.grouped(4).map(x => (x, x).zipped.map(_*_).sum).toArray

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def testFma(): Unit = {

    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(\(a => fma(a, a, a))) $ _
    )

    val gold = input.map(a => a*a + a)

    val (output: Array[Float], _) = Execute(size)(f, input)

    assertArrayEquals(gold, output, 0.001f)
  }

}
