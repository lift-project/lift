package rewriting.rules

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting._

object TestSimplification extends TestWithExecutor

class TestSimplification {

  import SimplifyAndFuse.getNumberOfTerms

  private val N = SizeVar("N")
  private val M = SizeVar("M")
  private val A = Array.fill[Float](128)(0.5f)

  private def checkSimplifiedIsSmaller(original: Lambda, simplified: Lambda): Unit =
    assertTrue(getNumberOfTerms(simplified) < getNumberOfTerms(original))

  @Test
  def scatterGatherWithRangesDivided(): Unit = {
    val var1 = Var(RangeMul(1, N, 2))
    val var2 = Var(RangeMul(1, N, 2))
    val var3 = Var(RangeMul(1, 16, 2))
    val var4 = Var(RangeMul(1, 16, 2))

    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => {
        Gather(ReorderWithStride(var3/var1)) o Scatter(ReorderWithStride(var4/var2)) $ in
      })

    TypeChecker(f)
    assertTrue(SimplificationRules.gatherScatterId.isDefinedAt(f.body))
  }

  @Test
  def joinSplitIdWithRanges(): Unit = {
    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => Join() o Split(Var(RangeMul(1, N, 2))) $ in
    )

    TypeChecker(f)

    assertTrue(SimplificationRules.joinSplitId.isDefinedAt(f.body))
  }

  @Test
  def splitJoinIdWithRanges(): Unit = {
    val var1 = Var(RangeMul(1, N, 2))
    val var2 = Var(RangeMul(1, N, 2))

    val f = fun(
    ArrayTypeWSWC(Float, N),
    in => {
        Map(Map(id)) o Split(var1) o Join() o Split(var2) $ in
      })

    TypeChecker(f)

    val applyAt = Rewrite.listAllPossibleRewrites(f, SimplificationRules.splitJoinId).head.expr

    val rewritten = Rewrite.applyRuleAt(f, applyAt, SimplificationRules.splitJoinId)
    TypeChecker(rewritten)

    assertTrue(rewritten.body.t.isInstanceOf[ArrayType])
    val len = Type.getLength(rewritten.body.t.asInstanceOf[ArrayType].elemT)

    assertEquals(var2, len)
  }

  @Test
  def scatterGatherId(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float, 16),
      in => Gather(ReorderWithStride(16)) o Scatter(ReorderWithStride(16)) $ in
    )

    val g = fun(
      ArrayTypeWSWC(Float, 16),
      in => Scatter(ReorderWithStride(16)) o Gather(ReorderWithStride(16)) $ in
    )

    assertTrue(SimplificationRules.gatherScatterId.rewrite.isDefinedAt(f.body))
    assertTrue(SimplificationRules.scatterGatherId.rewrite.isDefinedAt(g.body))
  }

  @Test
  def scatterGatherWithRanges(): Unit = {
    val var1 = Var(RangeMul(1, N, 2))
    val var2 = Var(RangeMul(1, N, 2))

    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => {
        Gather(ReorderWithStride(var1)) o Scatter(ReorderWithStride(var2)) $ in
      })

    TypeChecker(f)
    assertTrue(SimplificationRules.gatherScatterId.isDefinedAt(f.body))
  }

  @Test
  def transposeTransposeId(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Transpose() o Transpose() $ input
    )

    assertTrue(SimplificationRules.transposeTransposeId.rewrite.isDefinedAt(f.body))
    assertSame(f.params.head, SimplificationRules.transposeTransposeId.rewrite(f.body))
  }

  @Test
  def idTransposeTranspose(): Unit = {
    val M = SizeVar("M")
    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Id() $ input
    )

    assertTrue(Rules.idTransposeTranspose.rewrite.isDefinedAt(f.body))
    val result = Rules.idTransposeTranspose.rewrite(f.body)
    TypeChecker.check(result)
  }


  @Test
  def joinSplitId(): Unit = {
    val goldF = fun(
      ArrayTypeWSWC(Float, N),
      input => MapGlb(id) $ input
    )

    val (gold, _) = Execute(128)[Array[Float]](goldF, A)

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => MapGlb(id) o Join() o Split(8) $ input
    )

    val lambda = Rewrite.applyRuleUntilCannot(f, SimplificationRules.joinSplitId)

    val (result, _) = Execute(128)[Array[Float]](lambda, A)
    assertArrayEquals(gold, result, 0.0f)
    checkSimplifiedIsSmaller(f, lambda)
  }

  @Test
  def splitJoinId(): Unit = {
    val A = Array.fill[Float](128, 4)(0.5f)

    val goldF = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 4), N),
      input => MapGlb(MapSeq(id)) $ input
    )

    val (gold, _) = Execute(128)[Array[Float]](goldF, A)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 4), N),
      input => MapGlb(MapSeq(id)) o Split(4) o Join() $ input
    )

    val lambda = Rewrite.applyRuleUntilCannot(f, SimplificationRules.splitJoinId)

    val (result, _) = Execute(128)[Array[Float]](lambda, A)
    assertArrayEquals(gold, result, 0.0f)
    checkSimplifiedIsSmaller(f, lambda)
  }

  @Test
  def asScalarAsVector(): Unit = {
    val goldF = fun(
      ArrayTypeWSWC(Float, N),
      input => MapGlb(id) $ input
    )

    val (gold, _) = Execute(128)[Array[Float]](goldF, A)

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => MapGlb(id) o asScalar() o asVector(8) $ input
    )

    TypeChecker.check(f.body)

    val lambda = Rewrite.applyRuleUntilCannot(f, SimplificationRules.asScalarAsVectorId)

    val (result, _) = Execute(128)[Array[Float]](lambda, A)
    assertArrayEquals(gold, result, 0.0f)
    checkSimplifiedIsSmaller(f, lambda)
  }

  @Test
  def asVectorAsScalar(): Unit = {
    val A = Array.fill[Float](128, 4)(0.5f)

    val goldF = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 4), N),
      input => MapGlb(MapSeq(id)) $ input
    )

    val (gold, _) = Execute(128)[Array[Float]](goldF, A)

    val f = fun(
      ArrayTypeWSWC(VectorType(Float, 4), N),
      input => MapGlb(id.vectorize(4)) o asVector(4) o asScalar() $ input
    )

    val lambda = Rewrite.applyRuleUntilCannot(f, SimplificationRules.asVectorAsScalarId)

    val (result, _) = Execute(128)[Array[Float]](lambda, A)
    assertArrayEquals(gold, result, 0.0f)
    checkSimplifiedIsSmaller(f, lambda)
  }
}
