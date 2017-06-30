package opencl.generator

import ir._
import ir.ast.{\, fun}
import lift.arithmetic.SizeVar
import opencl.executor.Compile
import opencl.generator.OpenCLAST.{ArithExpression, StructConstructor}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapLcl, MapSeq, MapWrg}
import org.junit.Assert._
import org.junit._

class TestOpenCLGenerator {

  private val a = SizeVar("A")
  private val b = SizeVar("B")
  private val c = SizeVar("C")
  private val d = SizeVar("D")

  private val gold = """int v_A_\d+, int v_B_\d+, int v_C_\d+, int v_D_\d+""".r

  @Test
  def one(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, b), a),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, d), c),
      (a, _) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def two(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, a), b),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, c), d),
      (a, _) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def three(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, c), d),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, b), a),
      (a, _) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def four(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, b), d),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, a), c),
      (a, _) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def five(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, a), b), d),
      ArrayTypeWSWC(Float, c),
      (a, _) => a
    )

    val code = Compile(f)
    assertTrue(gold.findFirstIn(code).isDefined)
  }

  @Test
  def incorrectLocalSize(): Unit = {

    val localSize = 32

    val f = \(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, localSize), a),
      MapWrg(MapLcl(plusOne)) $ _
    )

    val code = Compile(f, localSize, 1, 1)

    assertTrue(code.contains("attribute"))
  }

  @Test
  def noAttributeForNoGroups(): Unit = {

    val localSize = 32

    val f = \(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, localSize), a),
      MapGlb(MapSeq(plusOne)) $ _
    )

    val code = Compile(f, localSize, 1, 1)

    assertFalse(code.contains("attribute"))
  }
  
  /**
   * The printer used to omit the comma in some situations.
   * see commit: 649c3b88a
   */
  @Test
  def structConstructor(): Unit = {
    val n = ArithExpression(42)
    val ty = TupleType(Int, Int)
    val node = StructConstructor(ty, Vector(n, n))
    val code = OpenCLPrinter().apply(node)
    
    // Before 649c3b88a1f26133: "(Tuple2_int_int){4242}"
    assertEquals(s"(${Type.name(ty)}){42, 42}", code)
  }
  
  @Test
  def printFraction(): Unit = {
    val num = a pow 2
    val den = b + c
    
    assertEquals(
      s"((${OpenCLPrinter.toString(num)})/(${OpenCLPrinter.toString(den)}))",
      OpenCLPrinter.toString(num /^ den)
    )
  }
}
