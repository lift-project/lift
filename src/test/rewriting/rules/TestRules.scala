package rewriting.rules

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.Rewrite
import rewriting.macrorules.EnablingRules

object TestRules extends TestWithExecutor

class TestRules {

  private val N = SizeVar("N")
  private val M = SizeVar("M")
  private val A = Array.fill[Float](128)(0.5f)

  @Test
  def extract0(): Unit = {
    val f = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (in1, in2) => Map(fun(x => Map(fun(y => add(x,y))) o Map(id) $ in2)) $ in1
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, FissionRules.extractFromMap)
    TypeChecker(f1)

    assertTrue(f1.body.asInstanceOf[FunCall].f.isInstanceOf[Lambda])
  }

  @Test
  def extract1(): Unit = {
    val f = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (in1, in2) => Map(fun(x =>
        ReduceSeq(fun((acc, y) => add(acc, mult(x,y))), 0.0f) o Map(id) $ in2
      )) $ in1
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, FissionRules.extractFromMap)
    TypeChecker(f1)

    assertTrue(f1.body.asInstanceOf[FunCall].f.isInstanceOf[Lambda])
  }

  @Test
  def mapFusionAfterExtract(): Unit = {
    val f0 = fun(
      ArrayTypeWSWC(Float, N),
      Map(plusOne) o Let(Map(id) $ _) $ _
    )

    val f1 = Rewrite.applyRuleAtId(f0, 0, FusionRules.mapFusion)
    TypeChecker(f1)
    assertTrue(f1.body.asInstanceOf[FunCall].f.isInstanceOf[Lambda])
  }

  @Test
  def mapTransposePromotion(): Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    val K = SizeVar("K")
    val O = SizeVar("O")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), K), O),
      input => Map(Transpose()) o Join() $ input
    )

    assertTrue(EnablingRules.movingJoin.rewrite.isDefinedAt(f.body))
    val result = EnablingRules.movingJoin.rewrite(f.body)
    TypeChecker.check(result)
  }

  @Test
  def slidePromotion(): Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    val K = SizeVar("K")

    val u = SizeVar("u")
    val v = SizeVar("v")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), K),
      input => Slide(u,v) o Map(Join()) $ input
    )

    assertTrue(Rules.slidePromotion.rewrite.isDefinedAt(f.body))
    val result = Rules.slidePromotion.rewrite(f.body)
    TypeChecker.check(result)
  }

  @Test
  def slideSwap(): Unit = {
    val K = SizeVar("K")
    val N = SizeVar("N")
    val M = SizeVar("M")

    val n = SizeVar("n")
    val s = SizeVar("s")
    val u = SizeVar("u")
    val v = SizeVar("v")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), K),
      input => Slide(u, v) o Map(Map(Slide(n,s))) $ input
    )

    assertTrue(Rules.slideSwap.rewrite.isDefinedAt(f.body))
    val result = Rules.slideSwap.rewrite(f.body)
    TypeChecker.check(result)
  }

  @Test
  def joinSwap(): Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    val K = SizeVar("K")
    val O = SizeVar("O")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), K), O),
      input => Join() o Map(Map(Join())) $ input
    )

    assertTrue(Rules.joinSwap.rewrite.isDefinedAt(f.body))
    val result = Rules.joinSwap.rewrite(f.body)
    TypeChecker.check(result)
  }

  @Test
  def transposeSwap(): Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    val K = SizeVar("K")
    val O = SizeVar("O")
    val P = SizeVar("P")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), K), O), P),
      input => Map(Map(Map(Transpose()))) o Map(Transpose()) $ input
    )

    assertTrue(Rules.transposeSwap.rewrite.isDefinedAt(f.body))
    val result = Rules.transposeSwap.rewrite(f.body)
    TypeChecker.check(result)
  }

  @Test
  def slideTransposeSwap(): Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    val K = SizeVar("K")
    val O = SizeVar("O")

    val u = SizeVar("u")
    val v = SizeVar("v")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), K), O),
      input => Map(Map(Map(Slide(u,v)))) o Map(Transpose()) $ input
    )

    assertTrue(Rules.slideTransposeSwap.rewrite.isDefinedAt(f.body))
    val result = Rules.slideTransposeSwap.rewrite(f.body)
    TypeChecker.check(result)
  }

  @Test
  def slideTransposeReordering(): Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    val K = SizeVar("K")

    val u = SizeVar("u")
    val v = SizeVar("v")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), K),
      input => Map(Slide(u,v)) o Map(Transpose()) $ input
    )

    assertTrue(Rules.slideTransposeReordering.rewrite.isDefinedAt(f.body))
    val result = Rules.slideTransposeReordering.rewrite(f.body)
    TypeChecker.check(result)
  }

  @Test
  def transposeMapJoinReordering(): Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), K),
      input => Transpose() o Map(Join()) $ input
    )

    assertTrue(Rules.transposeMapJoinReordering.rewrite.isDefinedAt(f.body))
    val result = Rules.transposeMapJoinReordering.rewrite(f.body)
    TypeChecker.check(result)
  }

  @Test
  def slideTiling(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    // n/s need to be positive
    val n = SizeVar("n")
    val s = SizeVar("s")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Slide(n, s) $ input
    )

    assertTrue(Rules.slideTiling(s).rewrite.isDefinedAt(f.body))
    assertTrue(Rules.slideTiling(s+1).rewrite.isDefinedAt(f.body))
    assertFalse(Rules.slideTiling(s-2).rewrite.isDefinedAt(f.body))

    val result = Rules.slideTiling(s+1).rewrite(f.body)
    TypeChecker.check(result)
  }

  @Test
  def mapJoin(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(id) o Join() $ input
    )

    assertTrue(EnablingRules.movingJoin.rewrite.isDefinedAt(f.body))
    val result = EnablingRules.movingJoin.rewrite(f.body)
    TypeChecker.check(result)
  }

  @Test
  def simpleMapTest(): Unit = {

    def f = fun(
      ArrayTypeWSWC(Float, N),
      input => Map(id) $ input
      // input => Join() o MapWrg(Join() o Map(MapLane(id)) o Split(2) ) o Split(2) $ input
    )

    def goldF = fun(
      ArrayTypeWSWC(Float, N),
      input => MapGlb(id) $ input
    )

    val options = Rewrite.rewriteJustGenerable(f)
    val (gold, _) = Execute(128)[Array[Float]](goldF, A)

    assertTrue(options.nonEmpty)

    options.foreach(l => {
      val (result, _) = Execute(128)[Array[Float]](l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def slightlyMoreComplexMap(): Unit = {
    val goldF = fun(
      ArrayTypeWSWC(Float, N),
      Float,
      (input, a) => MapGlb(fun(x => add(x, a))) $ input
    )

    def f = fun(
      ArrayTypeWSWC(Float, N),
      Float,
      (input, a) => Map(fun(x => add(a, x))) $ input
    )

    val a = 1.0f
    val (gold, _) = Execute(128)[Array[Float]](goldF, A, a)
    val lambdaOptions = Rewrite.rewriteJustGenerable(f)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.zipWithIndex.foreach(l => {
      val (result, _) = Execute(128)[Array[Float]](l._1, A, a)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def joinSplit(): Unit = {
    val M = SizeVar("M")
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(Map(id)) $ input
    )

    TypeChecker(f)

    val g = Rewrite.applyRuleAt(f, f.body, Rules.joinSplit)
    val h = Rewrite.applyRuleUntilCannot(g, OpenCLRules.mapGlb)

    val input = Array.fill[Float](128, 128)(util.Random.nextFloat())

    val (result, _) = Execute(128)[Array[Float]](h, input)

    assertArrayEquals(input.flatten, result, 0.0f)
    assertEquals(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), h.body.t)
  }

  @Test
  def simpleReduceTest(): Unit = {
    val goldF = fun(
      ArrayTypeWSWC(Float, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) $ input
    )

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => Reduce(add, 0.0f) $ input
    )

    val lambdaOptions = Rewrite.rewriteJustGenerable(f,
      Seq(OpenCLRules.reduceSeq, CopyRules.addIdAfterReduce, CopyRules.implementIdAsDeepCopy, OpenCLRules.globalMemory), 4)

    val (gold, _) = Execute(1, 1)[Array[Float]](goldF, A)

    assertTrue(lambdaOptions.nonEmpty)

    val (result, _) = Execute(1, 1)[Array[Float]](lambdaOptions(0), A)
    assertArrayEquals(lambdaOptions(1) + " failed", gold, result, 0.0f)
  }

  @Test
  def joinFromZip0(): Unit = {
    val t0 = ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M)
    val t1 = ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N)

    val f = fun(
      t0, t1,
      (a,b) => Zip(Join() $ a, Join() $ b)
    )

    TypeChecker(f)
    assertFalse(Rules.joinFromZip.isDefinedAt(f.body))
  }

  @Test
  def joinFromZip1(): Unit = {
    val t0 = ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M)

    val f = fun(
      t0, t0,
      (a,b) => Zip(Join() $ a, Join() $ b)
    )

    TypeChecker(f)
    assertTrue(Rules.joinFromZip.isDefinedAt(f.body))

    val result = Rewrite.applyRuleAt(f, f.body, Rules.joinFromZip)
    TypeChecker(result)

    assertEquals(f.body.t, result.body.t)
  }
}
