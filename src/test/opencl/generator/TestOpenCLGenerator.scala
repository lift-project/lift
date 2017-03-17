package opencl.generator

import lift.arithmetic.SizeVar
import ir._
import ir.ast.{\, fun}
import opencl.executor.{Compile, Executor}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapLcl, MapSeq, MapWrg}
import org.junit._
import org.junit.Assert._

class TestOpenCLGenerator {

  private val a = SizeVar("A")
  private val b = SizeVar("B")
  private val c = SizeVar("C")
  private val d = SizeVar("D")

  private val gold = """int v_A_\d+, int v_B_\d+, int v_C_\d+, int v_D_\d+""".r

  @Test
  def one(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, b), a),
      ArrayType(ArrayType(Float, d), c),
      (a, _) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def two(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, a), b),
      ArrayType(ArrayType(Float, c), d),
      (a, _) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def three(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, c), d),
      ArrayType(ArrayType(Float, b), a),
      (a, _) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def four(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, b), d),
      ArrayType(ArrayType(Float, a), c),
      (a, _) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def five(): Unit = {
    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, a), b), d),
      ArrayType(Float, c),
      (a, _) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def incorrectLocalSize(): Unit = {

    val localSize = 32

    val f = \(
      ArrayType(ArrayType(Float, localSize), a),
      MapWrg(MapLcl(plusOne)) $ _
    )

    val code = Compile(f, localSize, 1, 1)

    assertTrue(code.contains("attribute"))
  }

  @Test
  def noAttributeForNoGroups(): Unit = {

    val localSize = 32

    val f = \(
      ArrayType(ArrayType(Float, localSize), a),
      MapGlb(MapSeq(plusOne)) $ _
    )

    val code = Compile(f, localSize, 1, 1)

    assertFalse(code.contains("attribute"))
  }

}
