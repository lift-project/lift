package opencl.executor

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._

object TestExecute {
  @BeforeClass def before(): Unit =
    Executor.loadAndInit()

  @AfterClass def after(): Unit =
    Executor.shutdown()
}

class TestExecute {

  private val N = SizeVar("N")
  private val M = SizeVar("M")

  @Test
  def testInferSeq(): Unit = {

    val size = 1024
    val input = Vector.fill(size)(util.Random.nextFloat() * 10)

    val f = \(ArrayTypeWSWC(Float, N),
      MapSeq(plusOne) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Vector[Float]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertEquals(Cst(1), local(0))
    assertEquals(Cst(1), local(1))
    assertEquals(Cst(1), local(2))
    assertEquals(Cst(1), global(0))
    assertEquals(Cst(1), global(1))
    assertEquals(Cst(1), global(2))
    assertArrayEquals(input.map(_+1).toArray, output.toArray, 0.001f)
  }

  @Test
  def testInferNoLocalOneDim(): Unit = {

    val size = 1024
    val input = Vector.fill(size)(util.Random.nextFloat() * 10)

    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(plusOne) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Vector[Float]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertEquals(Cst(128), local(0))
    assertEquals(Cst(1), local(1))
    assertEquals(Cst(1), local(2))
    assertEquals(Cst(size), global(0))
    assertEquals(Cst(1), global(1))
    assertEquals(Cst(1), global(2))
    assertArrayEquals(input.map(_+1).toArray, output.toArray, 0.001f)
  }

  @Test
  def testInferNoLocalTwoDim(): Unit = {

    val size1 = 1024
    val size2 = 512
    val input = Vector.fill(size1, size2)(util.Random.nextFloat() * 10)

    val f = \(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      MapGlb(1)(MapGlb(0)(plusOne)) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Vector[Vector[Float]]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertEquals(Cst(16), local(0))
    assertEquals(Cst(16), local(1))
    assertEquals(Cst(1), local(2))
    assertEquals(Cst(size2), global(0))
    assertEquals(Cst(size1), global(1))
    assertEquals(Cst(1), global(2))
    assertArrayEquals(input.flatten.map(_+1).toArray, output.flatten.toArray, 0.001f)
  }

  @Test
  def testInferOneDim(): Unit = {
    val size = 1024
    val split = 32
    val input = Vector.fill(size)(util.Random.nextFloat() * 10)

    val f = \(ArrayTypeWSWC(Float, N),
      MapWrg(MapLcl(plusOne)) o Split(split) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Vector[Vector[Float]]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertEquals(Cst(split), local(0))
    assertEquals(Cst(1), local(1))
    assertEquals(Cst(1), local(2))
    assertEquals(Cst(size), global(0))
    assertEquals(Cst(1), global(1))
    assertEquals(Cst(1), global(2))
    assertArrayEquals(input.map(_+1).toArray, output.flatten.toArray, 0.001f)
  }

  @Test
  def testInferTwoDim(): Unit = {

    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val size1 = 1024
    val size2 = 512
    val split1 = 32
    val split2 = 8
    val input = Vector.fill(size1, size2)(util.Random.nextFloat() * 10)

    val f = \(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      Untile2D() o
        MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(plusOne)))) o
        Tile(split1, split2) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Vector[Float]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertEquals(Cst(split2), local(0))
    assertEquals(Cst(split1), local(1))
    assertEquals(Cst(1), local(2))
    assertEquals(Cst(size2), global(0))
    assertEquals(Cst(size1), global(1))
    assertEquals(Cst(1), global(2))
    assertArrayEquals(input.flatten.map(_+1).toArray, output.toArray, 0.001f)
  }

  @Test
  def testInferComplexArgSize() : Unit = {
    val A = SizeVar("A")
    val B = SizeVar("B")

    val cst1 = 8

    val sizeA = 128
    val sizeB = 16*cst1

    val input = Vector.fill(sizeA, sizeB)(util.Random.nextFloat() * 10)

    val f = λ(ArrayTypeWSWC(ArrayTypeWSWC(Float, B*Cst(cst1)), A),
      MapWrg(
        MapLcl(MapSeq(id)) o Split(cst1)
      ) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Vector[Vector[Vector[Float]]]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertArrayEquals(input.flatten.toArray, output.flatten.flatten.toArray, 0.001f)
  }

  @Test
  def testInferConstantFirstArgSizes() : Unit = {
    val A = SizeVar("A")

    val cst1 = 8

    val inputA = Vector.fill(cst1)(util.Random.nextFloat() * 10)
    val inputB = Vector.fill(cst1)(util.Random.nextFloat() * 10)

    val gold = inputA.sum + inputB.sum

    val f = λ(ArrayTypeWSWC(Float, cst1), ArrayTypeWSWC(Float, A),
      (cstArr, varArr) =>
      MapSeq(toGlobal(id)) o Join() o MapSeq(λ(Float, (r_res) =>
        ReduceSeq(add, r_res) $ cstArr
      )) o ReduceSeq(add, 0.0f) $ varArr
    )

    val execute = Execute()
    val (output, _) = execute[Vector[Float]](f, inputA, inputB)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, inputA, inputB))

    assertEquals(1, output.length)
    assertEquals(gold, output.head, 0.001f)
  }
}
