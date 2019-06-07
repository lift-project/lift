package opencl.executor

import ir._
import ir.ast._
import lift.arithmetic._
import core.generator.GenericAST.ArithExpression
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._

object TestExecute extends TestWithExecutor

class TestExecute {

  private val N = SizeVar("N")
  private val M = SizeVar("M")

  @Test
  def allocateMoreThanMaxIntForInput(): Unit = {
    val f = fun(
      ArrayType(Int, N),
      toGlobal(MapSeq(idI)) o ReduceSeq(addI, 0) $ _
    )

    try {
      val inputSize = scala.Int.MaxValue / 4 + 1
      val input = Array.fill(inputSize)(1)

      val (output, _) = Execute(1, 1)[Array[Int]](f, input)
      assertTrue(inputSize * 4 < 0)
      assertEquals(inputSize, output.head)
    } catch {
      case x: DeviceCapabilityException =>
        Assume.assumeNoException("Device can't allocate that much memory", x)
      case x: java.lang.OutOfMemoryError =>
        Assume.assumeNoException("Not enough heap space or RAM", x)
    }
  }

  // TODO: SEGFAULT in JNIEnv_::SetIntArrayRegion
  @Ignore
  @Test
  def allocateMoreThanMaxIntForOutput(): Unit = {
    val f = fun(
      ArrayType(Int, N),
      MapGlb(idI) $ _
    )

    val input = Array.tabulate(scala.Int.MaxValue / 4 + 1 )(i => i)

    val (output, _) = Execute(input.length)[Array[Int]](f, input)

    assertArrayEquals(input, output)
  }

  @Test
  def testInferSeq(): Unit = {

    val size = 1024
    val input = Array.fill(size)(util.Random.nextFloat() * 10)

    val f = \(ArrayTypeWSWC(Float, N),
      MapSeq(plusOne) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Array[Float]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertEquals(Cst(1), local(0))
    assertEquals(Cst(1), local(1))
    assertEquals(Cst(1), local(2))
    assertEquals(Cst(1), global(0))
    assertEquals(Cst(1), global(1))
    assertEquals(Cst(1), global(2))
    assertArrayEquals(input.map(_+1), output, 0.001f)
  }

  @Test
  def testInferNoLocalOneDim(): Unit = {

    val size = 1024
    val input = Array.fill(size)(util.Random.nextFloat() * 10)

    val f = \(ArrayTypeWSWC(Float, N),
      MapGlb(plusOne) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Array[Float]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertEquals(Cst(128), local(0))
    assertEquals(Cst(1), local(1))
    assertEquals(Cst(1), local(2))
    assertEquals(Cst(size), global(0))
    assertEquals(Cst(1), global(1))
    assertEquals(Cst(1), global(2))
    assertArrayEquals(input.map(_+1), output, 0.001f)
  }

  @Test
  def testInferNoLocalTwoDim(): Unit = {

    val size1 = 1024
    val size2 = 512
    val input = Array.fill(size1, size2)(util.Random.nextFloat() * 10)

    val f = \(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      MapGlb(1)(MapGlb(0)(plusOne)) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Array[Float]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertEquals(Cst(16), local(0))
    assertEquals(Cst(16), local(1))
    assertEquals(Cst(1), local(2))
    assertEquals(Cst(size2), global(0))
    assertEquals(Cst(size1), global(1))
    assertEquals(Cst(1), global(2))
    assertArrayEquals(input.flatten.map(_+1), output, 0.001f)
  }

  @Test
  def testInferOneDim(): Unit = {
    val size = 1024
    val split = 32
    val input = Array.fill(size)(util.Random.nextFloat() * 10)

    val f = \(ArrayTypeWSWC(Float, N),
      MapWrg(MapLcl(plusOne)) o Split(split) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Array[Float]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertEquals(Cst(split), local(0))
    assertEquals(Cst(1), local(1))
    assertEquals(Cst(1), local(2))
    assertEquals(Cst(size), global(0))
    assertEquals(Cst(1), global(1))
    assertEquals(Cst(1), global(2))
    assertArrayEquals(input.map(_+1), output, 0.001f)
  }

  @Test
  def testInferTwoDim(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val size1 = 1024
    val size2 = 512
    val split1 = 32
    val split2 = 8
    val input = Array.fill(size1, size2)(util.Random.nextFloat() * 10)

    val f = \(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      Untile2D() o
        MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(plusOne)))) o
        Tile(split1, split2) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Array[Float]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertEquals(Cst(split2), local(0))
    assertEquals(Cst(split1), local(1))
    assertEquals(Cst(1), local(2))
    assertEquals(Cst(size2), global(0))
    assertEquals(Cst(size1), global(1))
    assertEquals(Cst(1), global(2))
    assertArrayEquals(input.flatten.map(_+1), output, 0.001f)
  }

  @Test
  def testInferComplexArgSize() : Unit = {
    val A = SizeVar("A")
    val B = SizeVar("B")

    val cst1 = 8

    val sizeA = 128
    val sizeB = 16*cst1

    val input = Array.fill(sizeA, sizeB)(util.Random.nextFloat() * 10)

    val f = λ(ArrayTypeWSWC(ArrayTypeWSWC(Float, B*Cst(cst1)), A),
      MapWrg(
        MapLcl(MapSeq(id)) o Split(cst1)
      ) $ _
    )

    val execute = Execute()
    val (output, _) = execute[Array[Float]](f, input)

    val (local, global) = execute.getAndValidateSizesForExecution(f,
      Execute.createValueMap(f, input))

    assertArrayEquals(input.flatten, output, 0.001f)
  }

  @Test
  def testInferConstantFirstArgSizes() : Unit = {
    val A = SizeVar("A")

    val cst1 = 8

    val inputA = Array.fill(cst1)(util.Random.nextFloat() * 10)
    val inputB = Array.fill(cst1)(util.Random.nextFloat() * 10)

    val gold = inputA.sum + inputB.sum

    val f = λ(ArrayTypeWSWC(Float, cst1), ArrayTypeWSWC(Float, A),
      (cstArr, varArr) =>
      MapSeq(toGlobal(id)) o Join() o MapSeq(λ(Float, (r_res) =>
        ReduceSeq(add, r_res) $ cstArr
      )) o ReduceSeq(add, 0.0f) $ varArr
    )

    val execute = Execute()
    val (output, _) = execute[Array[Float]](f, inputA, inputB)

    execute.getAndValidateSizesForExecution(f, Execute.createValueMap(f, inputA, inputB))

    assertEquals(1, output.length)
    assertEquals(gold, output.head, 0.001f)
  }

  @Test
  def tupleArgument(): Unit = {
    val size = 32
    val inputA = Array.fill(size)(util.Random.nextFloat() * 10)
    val inputB = (13, 42.24f)

    val addTuple = UserFun(
      "addTuple", Array("x", "y"), "Tuple z = {x._0 + (int)y, x._1 + y}; return z;",
      Seq(TupleType(Int, Float), Float), TupleType(Int, Float)
    )

    val f = fun(
      ArrayType(Float, size),
      TupleType(Int, Float),
      (arr, t) =>
        MapSeq(id(TupleType(Int, Float))) o ReduceSeq(addTuple, t) $ arr
    )

    val (Vector((intSum, floatSum)), _) = Execute(1, 1)[Vector[(Int, Float)]](f, inputA, inputB)

    assertEquals(inputA.map(_.toInt).sum + inputB._1, intSum)
    assertEquals(inputA.sum + inputB._2, floatSum, 0.0001f)
  }

  @Test
  def allocateMoreThan2GB(): Unit = {
    val size = 268435456 // 2^28
    val chunkSize = 1048576 // 2^20
    val lclSize = 32768 // 2^15

    assumeFalse(
      "Cannot allocate memory for this kernel",
      Executor.getDeviceGlobalMemSize < 2147483648L + lclSize * 4 ||
        Executor.getDeviceMaxMemAllocSize < size * 4
    )

    val uf = UserFun(
      "count", Array("nb", "x"), "return nb + (int)(x % 10 == 0);", Seq(Int, Int), Int
    )
    val f = Lambda(
      Array(),
      MapWrg(
        MapLcl(
          MapSeq(toGlobal(id(Int))) o ReduceSeq(uf, 0) // allocates lclSize * 4 bytes = ε
            o MapSeq(toGlobal(id(Int))) // allocates 2^30
            o MapSeq(toGlobal(id(Int))) // allocates 2^30
        ) o Split(lclSize)
      ) o Split(chunkSize)
        $ ArrayFromGenerator((i, _) => ArithExpression(i), ArrayType(Int, size))
    ) // Allocates 2GB + ε in total

    val (output, _) = Execute(128, 512)[Array[Int]](f)
    val gold = Array.tabulate(size / lclSize)(i =>
      Math.ceil(((i+1) * lclSize - 1).toDouble / 10d).toInt + 1
        - Math.ceil((i * lclSize).toDouble / 10d).toInt - 1
    )
    assertArrayEquals(gold, output)
  }
}
