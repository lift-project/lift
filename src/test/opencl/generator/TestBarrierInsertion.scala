package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.executor.{Compile, Execute, Executor, Utils}
import opencl.generator.TestBarrierInsertion.cleanCode
import opencl.ir._
import opencl.ir.pattern._
import org.junit.{AfterClass, BeforeClass, Test}
import org.junit.Assert._
import org.junit.Assume.assumeFalse

import scala.util.matching.Regex

object TestBarrierInsertion {
  private var barrierInsertionFlagOrig: Boolean = false
  private var barrierEliminationFlagOrig: Boolean = false
  private var ignoreBarrierFlagsOrig: Boolean = false

  @BeforeClass def before(): Unit = {
    Executor.loadAndInit()
    barrierInsertionFlagOrig = PerformBarrierInsertion()
    barrierEliminationFlagOrig = PerformBarrierElimination()
    ignoreBarrierFlagsOrig = IgnoreBarrierFlags()
  }

  @AfterClass def after(): Unit = {
    Executor.shutdown()
    PerformBarrierInsertion.set(barrierInsertionFlagOrig)
    PerformBarrierElimination.set(barrierEliminationFlagOrig)
    IgnoreBarrierFlags.set(ignoreBarrierFlagsOrig)
  }

  /**
   * Cleans an OpenCL program by standardising variable names.
   * This is achieved by reodering variable IDs in order of appearance,
   * thus replacing the compiler-assigned numbering, which
   * is affected by the internal mechanics of the compiler. Example:
   * { val v_someName_37 = 1
   *   v_someName_37 + v__23
   * } is replaced with
   * { val v_someName_r0 = 1
   *   v_someName_r0 + v__r1
   * }
   * TODO: clean duplicate function names (e.g. id_0, id_1)
   */
  def cleanCode(code: String): String = {
//    val regexVarNames = new Regex("""(v_(.*?)_[0-9]+)""", "varFullName", "varCoreName")
    val regexVarNames = new Regex("""((?:v_)?v_(.*?)_[0-9]+(?:_[0-9]+_[0-9]+)?)""", "varFullName", "varCoreName")
    val varSubstitutions = {
      val varNames = regexVarNames.
        findAllMatchIn(code).
        map(m => (m.group("varFullName"), m.group("varCoreName"))).
        toList.distinct

      val replacements = varNames.zipWithIndex.map {
        case ((varFullName: String, varCoreName: String), orderId: Int) =>
          if (!varCoreName.isEmpty) (varFullName, "v_" + varCoreName + "_r" + orderId)
          else (varFullName, "v__r" + orderId)
      }

      replacements
    }
    val updatedCode = varSubstitutions.foldLeft(code) {
      case (partUpdatedCode, varSubst) => partUpdatedCode.replaceAllLiterally(varSubst._1, varSubst._2)
    }
    updatedCode
  }
}

class TestBarrierInsertion {

  def switchToBarrierElimination(): Unit = {
    PerformBarrierElimination.set(true)
    IgnoreBarrierFlags.set(false)
    PerformBarrierInsertion.set(false)
  }

  def switchToBarrierInsertion(): Unit = {
    PerformBarrierElimination.set(false)
    IgnoreBarrierFlags.set(true)
    PerformBarrierInsertion.set(true)
  }

  @Test
  def mapLcls(): Unit = {
    val M = 8//SizeVar("M")
    val N = 12//inputSize
    val K = 14//SizeVar("K")

    val f_p = Param()
    val f_call = FunCall(id, f_p)

    val h_p = Param()
    val h_call = FunCall(id, h_p)

    val lambda = fun(
      ArrayType(ArrayType(ArrayType(Float, K), N), M), a =>
        MapWrg(0)(
          MapSeq(MapLcl(0)(toGlobal(Lambda(Array(f_p), f_call)))) o
            MapSeq(MapLcl(0)(toLocal(Lambda(Array(h_p), h_call))))
        ) $ a)

    switchToBarrierElimination()
    val (localSize, globalSize) = (NDRange(N - 2, 1, 1), NDRange(N - 2, 1, 1))
    val code = Compile(lambda, localSize, globalSize)
    print(lambda)
    println(code)
    println("COMMENT: Note the unnecessary global mem fence left by BarrierElimination")
    switchToBarrierInsertion()
    val lambdaWithBarriers = BarrierInsertion(lambda, localSize, globalSize)
    println(lambdaWithBarriers)

    val patternWithoutBarrier: PartialFunction[Expr, Unit] = {
      case FunCall(MapSeq(Lambda1(_, FunCall(MapLcl(0, _), _))), FunCall(MapSeq(_), _)) =>
    }

    assertTrue(rewriting.utils.Utils.findExpressionForPattern(lambdaWithBarriers, patternWithoutBarrier).isDefined)
  }

  @Test
  def mapLclsDiffDims(): Unit = {
    val M = 8//SizeVar("M")
    val N = 12//inputSize
    val K = 14//SizeVar("K")

    val f_p = Param()
    val f_call = FunCall(id, f_p)

    val h_p = Param()
    val h_call = FunCall(id, h_p)

    val lambda = fun(
      ArrayType(ArrayType(ArrayType(Float, K), N), M), a =>
        MapWrg(1)(fun(_ =>
          MapWrg(0)(
            MapLcl(0)(MapLcl(1)(toGlobal(Lambda(Array(f_p), f_call)))) o
              MapLcl(1)(MapLcl(0)(toLocal(Lambda(Array(h_p), h_call))))
          ) $ a)
        ) $ Value("0.0f", ArrayType(Float, 1)))

    switchToBarrierElimination()
    val (localSize, globalSize) = (NDRange(N - 2, K, 1), NDRange(N - 2, K, 1))
    val code = Compile(lambda, localSize, globalSize)
    print(lambda)
    println(code)
    println("COMMENT: Note the unnecessary global mem fence left by BarrierElimination")
    switchToBarrierInsertion()
    val lambdaWithBarriers = BarrierInsertion(lambda, localSize, globalSize)
    println(lambdaWithBarriers)

    val patternWithBarriers: PartialFunction[Expr, Unit] = {
      case FunCall(Barrier(true, false), FunCall(MapLcl(0, Lambda1(_, FunCall(MapLcl(1, _), _))),
        FunCall(Barrier(true, false), FunCall(MapLcl(1, _), _)))) =>
    }

    assertTrue(rewriting.utils.Utils.findExpressionForPattern(lambdaWithBarriers, patternWithBarriers).isDefined)

    switchToBarrierInsertion()

    print(Compile(lambdaWithBarriers, NDRange(N - 2, K, 1), NDRange(N - 2, K, 1)))
  }

  @Test
  def mapLclsTransposed(): Unit = {
    val M = 8//SizeVar("M")
    val N = 12//inputSize
    val K = 14//SizeVar("K")

    val f_p = Param()
    val f_call = FunCall(id, f_p)

    val h_p = Param()
    val h_call = FunCall(id, h_p)

    val lambda = fun(
      ArrayType(ArrayType(ArrayType(Float, K), N), M), a =>
        MapWrg(0)(
          MapSeq(MapLcl(0)(toGlobal(Lambda(Array(f_p), f_call)))) o
            MapLcl(0)(MapSeq(toLocal(Lambda(Array(h_p), h_call))))
        ) $ a)

    switchToBarrierElimination()
    val (localSize, globalSize) = (NDRange(N - 2, 1, 1), NDRange(N - 2, 1, 1))
    val code = Compile(lambda, localSize, globalSize)
    print(lambda)
    println(code)
    println("COMMENT: Note the unnecessary global mem fence left by BarrierElimination")
    switchToBarrierInsertion()
    val lambdaWithBarriers = BarrierInsertion(lambda, localSize, globalSize)
    println(lambdaWithBarriers)

    val patternWithInsertedBarrier: PartialFunction[Expr, Unit] = {
      case FunCall(Barrier(true, false), FunCall(MapSeq(Lambda1(_, FunCall(MapLcl(0, _), _))),
        FunCall(Barrier(true, false), FunCall(MapLcl(0, _), _)))) =>
    }

    assertTrue(rewriting.utils.Utils.findExpressionForPattern(lambdaWithBarriers, patternWithInsertedBarrier).isDefined)
  }

  @Test
  def mapLclsWithTranspose(): Unit = {
    val M = 8//SizeVar("M")
    val N = 12//inputSize
    val K = 14//SizeVar("K")

    val f_p = Param()
    val f_call = FunCall(id, f_p)

    val h_p = Param()
    val h_call = FunCall(id, h_p)

    val lambda = fun(
      ArrayType(ArrayType(ArrayType(Float, K), N), M), a =>
        MapWrg(0)(
          MapSeq(MapLcl(0)(toGlobal(Lambda(Array(f_p), f_call)))) o
            Transpose() o
            MapSeq(MapLcl(0)(toLocal(Lambda(Array(h_p), h_call))))
        ) $ a)

    switchToBarrierElimination()
    val (localSize, globalSize) = (NDRange(N - 2, 1, 1), NDRange(N - 2, 1, 1))
    val code = Compile(lambda, localSize, globalSize)
    print(lambda)
    println(code)
    println("COMMENT: Note the unnecessary global mem fence left by BarrierElimination")
    switchToBarrierInsertion()
    val lambdaWithBarriers = BarrierInsertion(lambda, localSize, globalSize)
    println(lambdaWithBarriers)

    val patternWithInsertedBarrier: PartialFunction[Expr, Unit] = {
      case FunCall(Barrier(true, false), FunCall(MapSeq(_),
      FunCall(Barrier(true, false), FunCall(Transpose(), FunCall(MapSeq(_), _))))) =>
    }

    assertTrue(rewriting.utils.Utils.findExpressionForPattern(lambdaWithBarriers, patternWithInsertedBarrier).isDefined)
  }

  @Test
  def mapLclsWith2Transposes(): Unit = {
    val M = 8//SizeVar("M")
    val N = 12//inputSize
    val K = 14//SizeVar("K")

    val f_p = Param()
    val f_call = FunCall(id, f_p)

    val h_p = Param()
    val h_call = FunCall(id, h_p)

    val lambda = fun(
      ArrayType(ArrayType(ArrayType(Float, K), N), M), a =>
        MapWrg(0)(
          MapSeq(MapLcl(0)(toGlobal(Lambda(Array(f_p), f_call)))) o
            Transpose() o Transpose() o
            MapSeq(MapLcl(0)(toLocal(Lambda(Array(h_p), h_call))))
        ) $ a)

    val (localSize, globalSize) = (NDRange(N - 2, 1, 1), NDRange(N - 2, 1, 1))
    val code = Compile(lambda, localSize, globalSize)
    print(lambda)
    println(code)
    println("COMMENT: Note the unnecessary global mem fence left by BarrierElimination")
    val lambdaWithBarriers = BarrierInsertion(lambda, localSize, globalSize)
    println(lambdaWithBarriers)

    val patternWithInsertedBarrier: PartialFunction[Expr, Unit] = {
      case FunCall(MapSeq(Lambda1(_, FunCall(MapLcl(_, _), _))),
      FunCall(Transpose(), FunCall(Transpose(), FunCall(MapSeq(_), _)))) =>
    }

    assertTrue(rewriting.utils.Utils.findExpressionForPattern(lambdaWithBarriers, patternWithInsertedBarrier).isDefined)
  }

  @Test
  def vgg_layer0(): Unit = {
    val f = barrier_insertion_resources.VGGLayer0LambdaCompilable.lambda
    val fWithBarriersExpected = barrier_insertion_resources.VGGLayer0LambdaExpectedAfterBarrierInsertion.lambda

    switchToBarrierInsertion()
    val kernelWithBarriersInserted = Compile(f)

    // Compile with barrier elimination, but ignoring barrier flags, effectively disabling barrier inference
    switchToBarrierElimination()
    IgnoreBarrierFlags.set(true)
    val kernelWithBarriersExpected = Compile(fWithBarriersExpected)

    assertEquals(cleanCode(kernelWithBarriersExpected), cleanCode(kernelWithBarriersInserted))
  }

  @Test
  def vgg_layer10(): Unit = {
    val f = barrier_insertion_resources.VGGLayer10LambdaCompilable.lambda
    val fWithBarriersExpected = barrier_insertion_resources.VGGLayer10LambdaExpectedAfterBarrierInsertion.lambda

    switchToBarrierInsertion()
    val kernelWithBarriersInserted = Compile(f, Cst(51), Cst(2), Cst(1))

    // Compile with barrier elimination, but ignoring barrier flags, effectively disabling barrier inference
    switchToBarrierElimination()
    IgnoreBarrierFlags.set(true)
    val kernelWithBarriersExpected = Compile(fWithBarriersExpected, Cst(51), Cst(2), Cst(1))

    assertEquals(cleanCode(kernelWithBarriersExpected), cleanCode(kernelWithBarriersInserted))
  }

  /**
   * All tests below are adapted from TestBarrier.scala
   */
  @Test
  def sequentialMapFollowsParallel(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input => Join() o MapWrg(MapSeq(id) o MapLcl(id)) o Split(128) $ input
    )

//    switchToBarrierElimination()
//    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

//    assertEquals(cleanCode(kernel), cleanCode(kernel2))
    val (output, _) = Execute(128)[Array[Float]](kernel2, f, input)

    assertArrayEquals(input, output, 0.0f)
    assertEquals(2, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def sequentialReduceFollowsParallel(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input => Join() o MapWrg(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapLcl(id)) o Split(128) $ input
    )

//    switchToBarrierElimination()
//    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(128)[Array[Float]](kernel2, f, input)

    val gold = input.grouped(128).map(_.sum).toArray

    assertArrayEquals(gold, output, 0.001f)
//    assertEquals(cleanCode(kernel), cleanCode(kernel2))
    assertEquals(2, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test def basicBarrier(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    // Barrier should be removed
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input => Join() o MapWrg(MapLcl(id)) o Split(128) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input)

    assertArrayEquals(input, output, 0.0f)
    assertEquals(cleanCode(kernel), cleanCode(kernel2))
    assertFalse(kernel2.containsSlice("barrier"))
  }

  @Test def reorderGlobalLast(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).flatMap(_.reverse).toArray

    // Last barrier should be removed
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          MapLcl(id) o Gather(reverse) o MapLcl(id)
        ) o Split(128) $ input
    )

//    switchToBarrierElimination()
//    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test def reorderGlobalFirst(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).flatMap(_.reverse).toArray

    // All barriers should be removed
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          MapLcl(id) o MapLcl(id) o Gather(reverse)
        ) o Split(128) $ input
    )

//    switchToBarrierElimination()
//    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(0, "barrier".r.findAllMatchIn(kernel2).length)
  }

  @Test def reorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).flatMap(_.reverse).toArray

    // First barrier should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals("barrier(CLK_GLOBAL_MEM_FENCE);", cleanCode(kernel).diff(cleanCode(kernel2)).trim)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def reorderLastLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).flatMap(_.reverse).toArray

    // No barriers should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o Gather(reverse) o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

//    switchToBarrierElimination()
//    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test def reorderWriteLastLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).flatMap(_.reverse).toArray

    // First barrier should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(Scatter(reverse) o MapLcl(id)) o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals("barrier(CLK_GLOBAL_MEM_FENCE);", cleanCode(kernel).diff(cleanCode(kernel2)).trim)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }


  @Test def noReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // First barrier should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals("barrier(CLK_GLOBAL_MEM_FENCE);", cleanCode(kernel).diff(cleanCode(kernel2)).trim)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def noLoopNoReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // All barriers should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join()(MapWrg(
          fun(x => toGlobal(MapLcl(id))(toLocal(MapLcl(id))(x)))
        )(Split(128)(input)))
    )

    val inputs = Seq(input)

    switchToBarrierElimination()
    val (_, _, kernel) = Utils.execute(f, inputs, 128, inputSize, (true, true))
    switchToBarrierInsertion()
    val (output, _, kernel2) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(cleanCode(kernel), cleanCode(kernel2))
    assertEquals(0, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def noLoopReorderLocal(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).flatMap(_.reverse).toArray

    // All barriers should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    switchToBarrierElimination()
    val (_, _, kernel) = Utils.execute(f, inputs, 128, inputSize, (true, true))
    switchToBarrierInsertion()
    val (output, _, kernel2) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(cleanCode(kernel), cleanCode(kernel2))
    assertEquals(0, "barrier".r.findAllMatchIn(kernel).length)
  }

  @Test def noLoopReorderLastLocal(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).flatMap(_.reverse).toArray

    // Last barrier should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o Gather(reverse) o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    switchToBarrierElimination()
    val (_, _, kernel) = Utils.execute(f, inputs, 128, inputSize, (true, true))
    switchToBarrierInsertion()
    val (output, _, kernel2) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals("barrier(CLK_LOCAL_MEM_FENCE);", cleanCode(kernel2).diff(cleanCode(kernel)).trim)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test def noLoopReorder2Local(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // Last and middle barriers should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o MapLcl(id) o Gather(reverse) o
          toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    switchToBarrierElimination()
    val (_, _, kernel) = Utils.execute(f, inputs, 128, inputSize, (true, true))
    switchToBarrierInsertion()
    val (output, _, kernel2) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test def noLoopReorder3Local(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.grouped(128).flatMap(_.reverse).toArray

    // Last barrier should be eliminated
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o Gather(reverse) o
          MapLcl(id) o Gather(reverse) o toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    val inputs = Seq(input)
    switchToBarrierElimination()
    val (_, _, kernel) = Utils.execute(f, inputs, 128, inputSize, (true, true))
    switchToBarrierInsertion()
    val (output, _, kernel2) = Utils.execute(f, inputs, 128, inputSize, (true, true))

    assertArrayEquals(gold, output, 0.0f)
//    assertEquals(cleanCode(kernel), cleanCode(kernel2))
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test def loopReorder2Local(): Unit = {
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input

    // (Middle barrier should be eliminated) No, it shouldn't!
    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o MapLcl(id) o Gather(reverse) o
          toLocal(MapLcl(id)) o Gather(reverse)
        ) o Split(128) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f, NDRange(128), NDRange(inputSize))

    switchToBarrierInsertion()
    val kernel2 = Compile(f, NDRange(128), NDRange(inputSize))
    val (output, _) = Execute(128, inputSize)[Array[Float]](f, input)

    assertArrayEquals(gold, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test def copyToLocalInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.map(_+_))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => {
        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
          Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0),
            toLocal(MapLcl(id)) $ Get(pairArrays, 1))
        })) $ Zip(a, b)
      }
    )

    switchToBarrierElimination()
    val kernel = Compile(f, NDRange(128), NDRange(inputSize))
    switchToBarrierInsertion()
    val kernel2 = Compile(f, NDRange(128), NDRange(inputSize))

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(1, "barrier".r.findAllMatchIn(kernel).length)
    assertEquals(0, "barrier".r.findAllMatchIn(kernel2).length)
  }

  @Test def copyToLocalInZipAndReorder(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.toArray.reverse.map(x => x._1 + x._2))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => {
        MapWrg(toGlobal(MapLcl(add)) o Gather(reverse) o fun(pairArrays => {
          Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0),
            toLocal(MapLcl(id)) $ Get(pairArrays, 1))
        })) $ Zip(a, b)
      }
    )

    switchToBarrierElimination()
    //val kernel = Compile(f, NDRange(128), NDRange(inputSize))
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test def copyToLocalReorderedInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.toArray.reverse.map(x => x._1 + x._2))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => {
        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
          Zip(Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 0),
            Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 1))
        })) $ Zip(a, b)
      }
    )

    switchToBarrierElimination()
    //val kernel = Compile(f, NDRange(128), NDRange(inputSize))
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test def copyToLocalAndReorderInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x.reverse,y).zipped.map(_ + _))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => {
        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
          Zip(Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 0),
            toLocal(MapLcl(id)) $ Get(pairArrays, 1))
        })) $ Zip(a, b)
      }
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test def copyToLocalAndReorderInZip2(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y.reverse).zipped.map(_ + _))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => {
        MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays => {
          Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0),
            Gather(reverse) o toLocal(MapLcl(id)) $ Get(pairArrays, 1))
        })) $ Zip(a, b)
      }
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test def copyOneToLocalInZip(): Unit = {
    val inputSize = 512
    val input = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, input).zipped.map((x, y) => (x,y).zipped.map(_+_))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (a, b) => MapWrg(toGlobal(MapLcl(add)) o fun(pairArrays =>
        Zip(toLocal(MapLcl(id)) $ Get(pairArrays, 0), Get(pairArrays, 1))
      )) $ Zip(a, b)
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(inputSize)[Array[Float]](kernel2, f, input, input)

    assertArrayEquals(gold.flatten, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(0, "barrier".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def doubleNestedMapLcl(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        toGlobal(MapLcl(0)(MapLcl(1)(id))) o
        toLocal(MapLcl(0)(MapLcl(1)(id)))
      )) $ input
    )

    switchToBarrierElimination()
    //val kernel = Compile(f, NDRange(16,16), NDRange(inputSize,inputSize))
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(16, 16, inputSize, inputSize, (false, false))[Array[Float]](kernel2, f, input)

    assertArrayEquals(input.flatten.flatten.flatten, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(0, "barrier".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def tripleNestedMapLcl(): Unit = {

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(MapWrg(2)(
        toGlobal(MapLcl(0)(MapLcl(1)(MapLcl(2)(id)))) o
          toLocal(MapLcl(0)(MapLcl(1)(MapLcl(2)(id))))
      ))) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(0, "barrier".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def tripleNestedMapLclWithScatter(): Unit = {

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(MapWrg(2)(
        toGlobal(MapLcl(0)(MapLcl(1)(MapLcl(2)(id)))) o
          toLocal(MapLcl(0)(MapLcl(1)(Scatter(reverse) o MapLcl(2)(id))))
      ))) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def doubleNestedMapLclWithReorder(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.map(_.reverse)))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        toGlobal(MapLcl(0)(MapLcl(1)(id))) o
          Map(Gather(reverse)) o
          toLocal(MapLcl(0)(MapLcl(1)(id)))
      )) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)
    val (output, _) = Execute(16, 16, inputSize, inputSize, (false, false))[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def doubleNestedMapLclWithReorder2(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.map(_.reverse)))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        toGlobal(MapLcl(0)(MapLcl(1)(id))) o
          toLocal(MapLcl(0)(Gather(reverse) o MapLcl(1)(id)))
      )) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(16, 16, inputSize, inputSize, (false, false))[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def doubleNestedMapLclWithReorder3(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.map(_.reverse)))

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        toGlobal(MapLcl(0)(MapLcl(1)(id) o Gather(reverse))) o
          toLocal(MapLcl(0)(MapLcl(1)(id)))
      )) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(16, 16, inputSize, inputSize, (false, false))[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def tail(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).flatMap(_.tail).toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapWrg(
          MapLcl(id) o Tail() o MapLcl(id)
        ) o Split(128) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (result, _) = Execute(inputSize)[Array[Float]](kernel2, f, input)
    assertArrayEquals(gold, result, 0.0f)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def tailInLocal(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(i => i.toFloat)

    val gold = input.grouped(128).flatMap(_.tail).toArray

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Join() o MapWrg(
          toGlobal(MapLcl(id)) o Tail() o toLocal(MapLcl(id))
        ) o Split(128) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)
    val (result, _) = Execute(inputSize)[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold, result, 0.0f)
    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test(expected = classOf[IllegalKernel])
  def invalidKernel(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize/2,
      inputSize/2)(util.Random.nextInt(5).toFloat)

    val N = SizeVar("N")

    // Should have a barrier, but not all threads take it
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 16), 16), N), N),
      input => MapWrg(0)(MapWrg(1)(
        MapLcl(0)(MapLcl(1)(id) o Gather(reverse) o MapLcl(1)(id))
      )) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f, NDRange(32,8), NDRange(inputSize,inputSize))
    switchToBarrierInsertion()
    val kernel2 = Compile(f, NDRange(32,8), NDRange(inputSize,inputSize))

    assertEquals(cleanCode(kernel), cleanCode(kernel2))
    Execute(32, 8, inputSize, inputSize, (true, true))[Array[Float]](f, input)
  }

  @Test
  def doubleNestedMapLclWithReorderGlobalMem(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.map(_.reverse)))

    val N = SizeVar("N")

    // Should have a barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        MapLcl(0)(MapLcl(1)(id)) o
          MapLcl(0)(Gather(reverse) o MapLcl(1)(id))
      )) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(16, 16, inputSize, inputSize, (false, false))[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def doubleNestedMapLclWithReorderGlobalMem3(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.reverse))

    val N = SizeVar("N")

    // Should have a barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        MapLcl(0)(MapLcl(1)(id)) o
          Scatter(reverse) o MapLcl(0)(MapLcl(1)(id))
      )) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(16, 16, inputSize, inputSize, (false, false))[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def doubleNestedMapLclWithReorderGlobalMem2(): Unit = {
    val inputSize = 32
    val input = Array.fill(inputSize, inputSize, inputSize,
      inputSize)(util.Random.nextInt(5).toFloat)
    val gold = input.map(_.map(_.map(_.reverse)))

    val N = SizeVar("N")

    // Should have a barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N), N),
      input => MapWrg(0)(MapWrg(1)(
        MapLcl(0)(MapLcl(1)(id) o Gather(reverse)) o
          MapLcl(0)(MapLcl(1)(id))
      )) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f)
    switchToBarrierInsertion()
    val kernel2 = Compile(f)

    val (output, _) = Execute(16, 16, inputSize, inputSize, (false, false))[Array[Float]](kernel2, f, input)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def tupleInside2MapLcl(): Unit = {
    val innerSize = 16

    val N = SizeVar("N")

    // Should have 1 barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, innerSize), innerSize), N), N),
      input => MapWrg(0)(MapWrg(1)(
        fun(x => toGlobal(MapLcl(0)(MapLcl(1)(id))) $ Get(x, 0)) o
        fun(x =>
          Unzip() o toLocal(MapLcl(1)(fun(pair => // Swapped MapLcl(1) and (0) -- need barrier!
            Unzip() o MapLcl(0)(fun( pair =>
              Tuple(id $ Get(pair, 0), id $ Get(pair, 1))
            )) $ Zip(Get(pair, 0), Get(pair, 1))
          ))) $ Zip(x, x)
        )
      )) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f, innerSize, innerSize, 1)
    switchToBarrierInsertion()
    val kernel2 = Compile(f, innerSize, innerSize, 1)

    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def tupleInsideMapLcl(): Unit = {
    val innerSize = 16

    val N = SizeVar("N")

    // Should have 1 barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, innerSize), innerSize), N), N),
      input => MapWrg(0)(MapWrg(1)(
        fun(x => toGlobal(MapLcl(1)(MapLcl(0)(id))) $ Get(x, 0)) o
        fun(x =>
        Unzip() o toLocal(MapLcl(1)(fun(pair =>
          Tuple(MapLcl(0)(id) $ Get(pair, 0), MapLcl(0)(id) $ Get(pair, 1)))
        )) $ Zip(x, x))
      )) $ input)

    switchToBarrierElimination()
    val kernel = Compile(f, innerSize, innerSize, 1)
    switchToBarrierInsertion()
    val kernel2 = Compile(f, innerSize, innerSize, 1)

    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(0, "barrier".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def tupleInsideMapLcl2(): Unit = {
    val innerSize = 16

    val N = SizeVar("N")

    // Should have 1 barrier
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, innerSize), innerSize), N), N),
      input => MapWrg(0)(MapWrg(1)(
        toGlobal(MapLcl(1)(MapLcl(0)(id))) o Get(0) o
        fun(x =>
        Unzip() o toLocal(MapLcl(1)(fun(pair =>
          Tuple(MapLcl(0)(id) $ Get(pair, 0), MapLcl(0)(id) $ Get(pair, 1)))
        )) $ Zip(x, x))
      )) $ input)

    switchToBarrierElimination()
    val kernel = Compile(f, innerSize, innerSize, 1)
    switchToBarrierInsertion()
    val kernel2 = Compile(f, innerSize, innerSize, 1)

    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(0, "barrier".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def tupleWithAsVectorInsideMapLcl(): Unit = {
    val innerSize = 16

    val N = SizeVar("N")

    // Should have 2 barriers (0 actually -- vectorizing doesn't change the index expressions!)
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, innerSize), innerSize), N), N),
      input => MapWrg(0)(MapWrg(1)(fun(x =>
        toGlobal(MapLcl(1)(asScalar() o MapLcl(0)(id.vectorize(4)) o asVector(4))) $ Get(x, 0)) o
        fun(x =>
        Unzip() o toLocal(MapLcl(1)(fun(pair =>
          Tuple(
            asScalar() o MapLcl(0)(id.vectorize(4)) o asVector(4) $ Get(pair, 0),
            asScalar() o MapLcl(0)(id.vectorize(4)) o asVector(4) $ Get(pair, 1))
          )
        )) $ Zip(x, x))
      )) $ input)

    switchToBarrierElimination()
    val kernel = Compile(f, innerSize,innerSize, 1)
    switchToBarrierInsertion()
    val kernel2 = Compile(f, innerSize,innerSize, 1)

    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(0, "barrier".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def tupleBarrierJustLocal(): Unit = {
    val N = SizeVar("N")

    val f = \(
      ArrayType(Float, N),
      input => MapWrg(\(x =>
        toGlobal(MapLcl(add)) o
          Gather(reverse) o
          MapLcl(\(y => Tuple(toLocal(id) $ y._0, toLocal(id) $ y._1))) $
          Zip(x, x)
      )) o Split(32) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f, NDRange(32), NDRange(N))
    switchToBarrierInsertion()
    val kernel2 = Compile(f, NDRange(32), NDRange(N))

    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def tupleBarrierJustGlobal(): Unit = {
    val N = SizeVar("N")

    val f = \(
      ArrayType(Float, N),
      input => MapWrg(\(x =>
        toGlobal(MapLcl(add)) o
          Gather(reverse) o
          MapLcl(\(y => Tuple(id $ y._0, id $ y._1))) $
          Zip(x, x)
      )) o Split(32) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f, NDRange(32), NDRange(N))
    switchToBarrierInsertion()
    val kernel2 = Compile(f, NDRange(32), NDRange(N))

    assertEquals(1, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  @Test
  def tupleBarrierBoth(): Unit = {
    val N = SizeVar("N")

    val f = \(
      ArrayType(Float, N),
      input => MapWrg(\(x =>
        toGlobal(MapLcl(add)) o
          Gather(reverse) o
          MapLcl(\(y => Tuple(toLocal(id) $ y._0, id $ y._1))) $
          Zip(x, x)
      )) o Split(32) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f, NDRange(32), NDRange(N))
    switchToBarrierInsertion()
    val kernel2 = Compile(f, NDRange(32), NDRange(N))

    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE \\| CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE \\| CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

  /**
   * The barrier between the reads of mapLcl3 and writes of mapLcl2 can be placed in either of two locations,
   * both correct: after maplcl3 and after maplcl1
   */
  @Test
  def twoCorrectBarrierLocations(): Unit = {
    val N = SizeVar("N")

    val f = \(
      ArrayType(Float, N),
      input => MapWrg(\(x =>
        /* first correct barrier location */
        toGlobal(/*mapLcl3*/MapLcl(add)) o
          Gather(reverse) o
          /* another barrier (unrelated to the issue) */
          /*mapLcl2*/MapLcl(\(y => Tuple(toLocal(id) $ y._0, id $ y._1))) o
          /* second correct barrier location */
          /*mapLcl1*/MapLcl(\(y => Tuple(toLocal(id) $ y._0, id $ y._1))) $
          Zip(x, x)
      )) o Split(32) $ input
    )

    switchToBarrierElimination()
    val kernel = Compile(f, NDRange(32), NDRange(N/2))
    switchToBarrierInsertion()
    val kernel2 = Compile(f, NDRange(32), NDRange(N/2))

    assertEquals(1, "barrier\\(CLK_LOCAL_MEM_FENCE \\| CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel).length)
    assertEquals(2, "barrier\\(CLK_LOCAL_MEM_FENCE \\| CLK_GLOBAL_MEM_FENCE\\);".r.findAllMatchIn(kernel2).length)
  }

}
