package opencl.generator

import apart.arithmetic.SizeVar
import ir._
import ir.ast.fun
import opencl.executor.Compile
import opencl.ir._
import org.junit.Test
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
      (a, b) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def two(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, a), b),
      ArrayType(ArrayType(Float, c), d),
      (a, b) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def three(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, c), d),
      ArrayType(ArrayType(Float, b), a),
      (a, b) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def four(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, b), d),
      ArrayType(ArrayType(Float, a), c),
      (a, b) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def five(): Unit = {
    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, a), b), d),
      ArrayType(Float, c),
      (a, b) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

}
