package rewriting

import ir.ast._
import ir._
import lift.arithmetic.{RangeMul, SizeVar, Var}
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.ast._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import rewriting.utils.NumberExpression

object TestRules {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    Executor.shutdown()
  }
}

class TestRules {

  def applyRule(lambda: Lambda, expr: Expr, rule: Rule) = {
    TypeChecker.check(lambda.body)
    FunDecl.replace(lambda, expr, rule.rewrite(expr))
  }

  val N = SizeVar("N")
  val A = Array.fill[Float](128)(0.5f)

  @Test
  def ruleTest(): Unit = {
    val size = 128
    val a = Array.fill(size)(util.Random.nextInt(5))
    val b = Array.fill(size)(util.Random.nextInt(5))

    // Split $ Zip( ... ) => Zip( Split $ ... )
    val gold = (a, b).zipped.map(_ * _)
    val test = (a, b).zipped.toArray.grouped(16).map(_.map(x => x._1 * x._2)).flatten.toArray
    val test2 = (a.grouped(16).toArray, b.grouped(16).toArray).zipped.map((x, y) => (x, y).zipped.map(_*_)).flatten

    assertArrayEquals(gold, test)
    assertArrayEquals(gold, test2)

    val A = Array.tabulate(size, size)((x, y) => x*size + y)

    // Reduce => Reduce() o Reduce( ... $ Zip( ... ) )
    val gold2 = a.sum
    val test4 = a.grouped(16).toArray.reduce((x, y) => (x, y).zipped.map(_+_)).sum

    assertEquals(gold2, test4, 0.0f)

    // Reorder $ Zip( ... ) => Zip( Reorder $ ... )
    val goldReorderZip = (a, b).zipped.toArray.reverse
    val testReorderZip = (a.reverse, b.reverse).zipped.toArray

    assertArrayEquals(goldReorderZip.map(_._1), testReorderZip.map(_._1))
    assertArrayEquals(goldReorderZip.map(_._2), testReorderZip.map(_._2))

    // Map-Reduce interchange
    val goldSwapMapReduce = A.map(row => Array(row.sum))
    val testSwapMapReduce = Array(A.transpose.reduce((x, y) => (x, y).zipped.map(_+_))).transpose

    assertArrayEquals(goldSwapMapReduce.flatten, testSwapMapReduce.flatten)

    // Map-Map transpose, pulling zip out
    val goldMapMapPullZip = A.map(a => (a, b).zipped.map(_*_))
    val testMapMapPullZip = (A.transpose, b).zipped.map((a, bElem) => a.map(_ * bElem)).transpose

    assertArrayEquals(goldMapMapPullZip.flatten, testMapMapPullZip.flatten)

    // Map-Map transpose, pushing zip in
    val goldMapMapPushZip = (A, b).zipped.map((a, bElem) => a.map(_ * bElem))
    val testMapMapPushZip = A.transpose.map(a => (a, b).zipped.map(_ * _)).transpose

    assertArrayEquals(goldMapMapPushZip.flatten, testMapMapPushZip.flatten)

    // map(split) o transpose => transpose o map(transpose) o split
    val goldMapSplitTranspose = A.transpose.map(_.grouped(16).toArray)
    val testMapSplitTranspose = A.grouped(16).toArray.map(_.transpose).transpose

    assertArrayEquals(goldMapSplitTranspose.flatten.flatten, testMapSplitTranspose.flatten.flatten)

    // map(transpose) o split =>  transpose o map(split) o transpose
    val miscGold = A.grouped(16).toArray.map(_.transpose)
    val miscTest = A.transpose.map(_.grouped(16).toArray).transpose

    assertArrayEquals(miscGold.flatten.flatten, miscTest.flatten.flatten)

    // transpose o map(split) => map(transpose) o split o transpose
    val miscGold2 = A.map(_.grouped(16).toArray).transpose
    val miscTest2 = A.transpose.grouped(16).toArray.map(_.transpose)

    assertArrayEquals(miscGold2.flatten.flatten, miscTest2.flatten.flatten)

    // split o transpose => map(transpose) o transpose o map(split)
    val miscGold3 = A.transpose.grouped(16).toArray
    val miscTest3 = A.map(_.grouped(16).toArray).transpose.map(_.transpose)

    assertArrayEquals(miscGold3.flatten.flatten, miscTest3.flatten.flatten)

    // macro rule join-split and split-join-id
    // split o map(split()) => map(map(split)) o split()
    val miscGold4 = A.map(_.grouped(16).toArray).grouped(16).toArray
    val miscTest4 = A.grouped(16).toArray.map(_.map(_.grouped(16).toArray))

    assertArrayEquals(miscGold4.flatten.flatten.flatten, miscTest4.flatten.flatten.flatten)

    val B = Array.fill(size, size, size)(util.Random.nextInt(size))

    val gold5 = B.map(_.map(_.grouped(16).toArray.map(_.sum).sum))
    val test5 = B.transpose.map(_.map(_.grouped(16).toArray.map(_.sum).sum)).transpose

    assertArrayEquals(gold5.flatten, test5.flatten)

    // map(transpose) o transpose o map(transpose) == transpose o map(transpose) o transpose
    val gold6 = B.transpose.map(_.transpose).transpose
    val test6 = B.map(_.transpose).transpose.map(_.transpose)

    assertArrayEquals(gold6.flatten.flatten, test6.flatten.flatten)

    // map(reduce(f, init) o join o map(reduce(f, init2)) =>
    // reduce(acc, a => map(acc, a => reduce(f, acc) $ a ) o zip(acc, a) , array(init)) o transpose
    val gold7 = B.map(_.map(_.sum).sum)
    val misc7 = B.transpose.foldLeft(Array.fill(size)(0))((acc, a) => (acc, a).zipped.map((acc, a) => a.foldLeft(acc)((a, b) => a+b)))
    assertArrayEquals(gold7, misc7)

    val gold8 = A.reverse.transpose
    val test8 = A.transpose.map(_.reverse)

    assertArrayEquals(gold8.flatten, test8.flatten)

    val gold9 = B.transpose.map(_.transpose.reverse)
    val test9 = B.map(_.map(_.reverse)).transpose.map(_.transpose)

    assertArrayEquals(gold9.flatten.flatten, test9.flatten.flatten)

    // slide(n, s) => join() o map(slide(n, s)) o slide(u, v)
    val slideGold = a.sliding(3,1).toArray
    val slideTest = a.sliding(5,3).toArray.map(_.sliding(3,1).toArray).flatten

    assertArrayEquals(slideGold.flatten, slideTest.flatten)

    // map(id) o join => join o map(map(id))
    val gold10 = A.flatten.map(x => x)
    val test10 = A.map(_.map(x => x)).flatten

    assertArrayEquals(gold10, test10)

    // split o map(transpose) =>

    // transpose o split =>
  }

  @Test
  def extract0(): Unit = {
    val f = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (in1, in2) => Map(fun(x => Map(fun(y => add(x,y))) o Map(id) $ in2)) $ in1
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.extractFromMap)
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

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.extractFromMap)
    TypeChecker(f1)

    assertTrue(f1.body.asInstanceOf[FunCall].f.isInstanceOf[Lambda])
  }

  @Test
  def mapFusionAfterExtract(): Unit = {
    val f0 = fun(
      ArrayTypeWSWC(Float, N),
      Map(plusOne) o Let(Map(id) $ _) $ _
    )

    val f1 = Rewrite.applyRuleAtId(f0, 0, Rules.mapFusion)
    TypeChecker(f1)
    assertTrue(f1.body.asInstanceOf[FunCall].f.isInstanceOf[Lambda])
  }

  @Test
  def testDot0(): Unit = {

    val input = Array.fill(1, 4)(util.Random.nextFloat())
    val gold = (input.head, input.head).zipped.map(_*_).sum

    val f = fun(
      ArrayTypeWSWC(Float4, 1),
      ArrayTypeWSWC(Float4, 1),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          asScalar() o
          MapSeq(VectorizeUserFun(4, mult)) $ Zip(x, y)
    )

    val g = fun(
      ArrayTypeWSWC(Float4, 1),
      ArrayTypeWSWC(Float4, 1),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          MapSeq(dot) $ Zip(x, y)
    )

    val (outputF: Array[Float], _) = Execute(1, 1)(f, input, input)
    val (outputG: Array[Float], _) = Execute(1, 1)(g, input, input)

    assertEquals(gold, outputF.head, 0.001f)
    assertEquals(gold, outputG.head, 0.001f)
  }

  @Test
  def testDot1(): Unit = {

    val input = Array.fill(4, 4)(util.Random.nextFloat())
    val gold = (input.flatten, input.flatten).zipped.map(_*_).sum

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(Float4, N),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          asScalar() o
          MapSeq(VectorizeUserFun(4, mult)) $ Zip(x, y)
    )

    val g = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(Float4, N),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          MapSeq(dot) $ Zip(x, y)
    )

    val (outputF: Array[Float], _) = Execute(1, 1)(f, input, input)
    val (outputG: Array[Float], _) = Execute(1, 1)(g, input, input)

    assertEquals(gold, outputF.head, 0.001f)
    assertEquals(gold, outputG.head, 0.001f)
  }

  @Test
  def testDotRule(): Unit = {

    val input = Array.fill(4, 4)(util.Random.nextFloat())
    val gold = (input.flatten, input.flatten).zipped.map(_*_).sum

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(Float4, N),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          asScalar() o
          MapSeq(VectorizeUserFun(4, mult)) $ Zip(x, y)
    )

    val g = Rewrite.applyRuleAtId(f, 1, Rules.dotBuiltinSeq)

    val (outputF: Array[Float], _) = Execute(1, 1)(f, input, input)
    val (outputG: Array[Float], _) = Execute(1, 1)(g, input, input)

    assertEquals(gold, outputF.head, 0.001f)
    assertEquals(gold, outputG.head, 0.001f)
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
    assertTrue(Rules.gatherScatterId.isDefinedAt(f.body))
  }

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
    assertTrue(Rules.gatherScatterId.isDefinedAt(f.body))
  }

  @Test
  def joinSplitIdWithRanges(): Unit = {
    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => Join() o Split(Var(RangeMul(1, N, 2))) $ in
    )

    TypeChecker(f)

    assertTrue(Rules.joinSplitId.isDefinedAt(f.body))
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

    val applyAt = Rewrite.listAllPossibleRewrites(f, Rules.splitJoinId).head._2

    val rewritten = Rewrite.applyRuleAt(f, applyAt, Rules.splitJoinId)
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

    assertTrue(Rules.gatherScatterId.rewrite.isDefinedAt(f.body))
    assertTrue(Rules.scatterGatherId.rewrite.isDefinedAt(g.body))
  }

  @Test
  def nestPartialReduceInReduce(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 16), 16), 16),
      a => Map( ReduceSeq(add, 0.0f) o Join() o Map(PartRed(fun((x, y) => add(x, y)), 0.0f)) ) $ a)

    val fResult = Rewrite.applyRuleAtId(f, 0, Rules.mapReducePartialReduce)
    TypeChecker(fResult)
  }

  @Test
  def pullingExpressionsOutOfZip(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (in1, in2) => MapGlb(fun(x =>
        MapSeq(fun(y =>
          MapSeq(fun(a => add(Get(a, 0), Get(a,1)))) $ Zip(Gather(reverse) $ x, Gather(reverse) $ y)
        )) $ in2
      )) $ in1
    )

    val fP = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (in1, in2) => Map(fun(x =>
        Map(fun(y =>
          MapSeq(fun(a => add(Get(a, 0), Get(a,1)))) $ Zip(Gather(reverse) $ x, Gather(reverse) $ y)
        )) $ in2
      )) $ in1
    )

    val size = 128

    val A = Array.tabulate(size, size)((x, y) => x*size + y.toFloat)

    val (result: Array[Float], _) = Execute(size)(f, A, A)

    val g0 = Rewrite.applyRuleAtId(fP, 2, Rules.mapFissionWithZipInside)
    val g1 = Rewrite.applyRuleAtId(g0, 0, Rules.mapGlb)
    val g2 = Rewrite.applyRuleAtId(g1, 2, Rules.mapSeq)

    val (resultG: Array[Float], _) = Execute(size)(g2, A, A)

    val h0 = Rewrite.applyRuleAtId(fP, 0, Rules.mapFissionWithZipInside)
    val h1 = Rewrite.applyRuleAtId(h0, 0, Rules.mapGlb)
    val h2 = Rewrite.applyRuleAtId(h1, 5, Rules.mapSeq)

    val (resultH: Array[Float], _) = Execute(size)(h2, A, A)

    assertArrayEquals(result, resultG, 0.0f)
    assertArrayEquals(result, resultH, 0.0f)
  }

  @Test
  def mapFission(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => Map(id o id) $ input
    )

    assertTrue(Rules.mapFission.rewrite.isDefinedAt(f.body))
    val f0 = Lambda(f.params, Rules.mapFission.rewrite(f.body))
    TypeChecker(f0)

    val M = SizeVar("M")

    val g = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(fun(x => Reduce(add, 0.0f) o Map(id) $ x)) $ input
    )

    assertTrue(Rules.mapFission.rewrite.isDefinedAt(g.body))
    val g0 = Lambda(g.params, Rules.mapFission.rewrite(g.body))
    TypeChecker(g0)
  }

  @Test
  def addIdMapWrg(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => MapWrg(MapLcl(id)) o Slide(3,1) $ input
    )
    TypeChecker(f)

    val g = fun(
      ArrayTypeWSWC(Float, N),
      input => MapWrg(MapLcl(Reduce(add, 0.0f)) o Slide(3,1)) o Slide(4,2) $ input
    )
    TypeChecker(g)

    val h = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Join() o MapWrg(1)(TransposeW() o MapWrg(0)(
        MapLcl(1)(MapLcl(0)(Reduce(add, 0.0f) o Join())) o Slide2D(3,1)
      )) o Slide2D(4,2) $ input
    )
    TypeChecker(h)

    // checks that the rule is only applicable once,
    // otherwise this will fail with a StackOverflowError
    val rewrittenG = Rewrite.applyRulesUntilCannot(g, Seq(Rules.addIdMapWrg))
    val rewrittenH = Rewrite.applyRulesUntilCannot(h, Seq(Rules.addIdMapWrg))

    assertTrue(Rules.addIdMapWrg.rewrite.isDefinedAt(f.body))
    assertTrue(Rules.addIdMapWrg.rewrite.isDefinedAt(g.body))
    assertTrue(rewrittenH.body.contains({case FunCall(Id(), a) =>}))
  }

  @Test
  def transposeTransposeId(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Transpose() o Transpose() $ input
    )

    assertTrue(Rules.transposeTransposeId.rewrite.isDefinedAt(f.body))
    assertSame(f.params.head, Rules.transposeTransposeId.rewrite(f.body))
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

    assertTrue(MacroRules.movingJoin.rewrite.isDefinedAt(f.body))
    val result = MacroRules.movingJoin.rewrite(f.body)
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

    assertTrue(MacroRules.movingJoin.rewrite.isDefinedAt(f.body))
    val result = MacroRules.movingJoin.rewrite(f.body)
    TypeChecker.check(result)
  }

  @Test
  def mapReduceInterchange0(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(Reduce(add, 0.0f)) $ input
    )

    assertTrue(Rules.mapReduceInterchange.rewrite.isDefinedAt(f.body))
    val f0 = Rewrite.applyRuleAtId(f, 0, Rules.mapReduceInterchange)
    TypeChecker(f0)
    assertTrue(f0.body.asInstanceOf[FunCall].args.head.asInstanceOf[FunCall].f.isInstanceOf[Reduce])
  }

  @Test
  def mapReduceInterchange1(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(ReduceSeq(add, 0.0f)) $ input
    )

    assertTrue(Rules.mapReduceInterchange.rewrite.isDefinedAt(f.body))
    val f0 = Rewrite.applyRuleAtId(f, 0, Rules.mapReduceInterchange)
    TypeChecker(f0)
    assertTrue(f0.body.asInstanceOf[FunCall].args.head.asInstanceOf[FunCall].f.isInstanceOf[ReduceSeq])
  }

  @Test
  def mapReduceInterchangeWithZipOutside0(): Unit = {
    // TODO: Reduce

  }

  @Test
  def mapReduceInterchangeWithZipOutside1(): Unit = {
    // TODO: ReduceSeq

  }

  @Test
  def transposeBothSides(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(Map(plusOne)) $ input
    )

    TypeChecker.check(f.body)

    assertTrue(Rules.transposeBothSides.rewrite.isDefinedAt(f.body))

    val g = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(Map(Map(plusOne)) o Split(2)) $ input
    )

    TypeChecker.check(g.body)
    assertFalse(Rules.transposeBothSides.rewrite.isDefinedAt(g.body))
  }

  @Test
  def mapMapTransposeWithZipInside(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, M),
      (in1, in2) => Map(fun(x => Map(fun(x => add(Get(x, 0), Get(x, 1)))) $ Zip(in2, x))) $ in1
    )

    assertTrue(Rules.mapMapTransposeZipInside.rewrite.isDefinedAt(f.body))
    val f0 = Rules.mapMapTransposeZipInside.rewrite(f.body)
    TypeChecker(f0)
  }

  @Test
  def mapMapTransposeWithZipOutside(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      ArrayTypeWSWC(Float, M),
      (in1, in2) =>
        Map(fun(x =>
          Map(fun(y => add(y, Get(x, 1)))) $ Get(x, 0)
        )) $ Zip(in1, in2)
    )

    assertTrue(Rules.mapMapTransposeZipOutside.rewrite.isDefinedAt(f.body))
    val f0 = Rules.mapMapTransposeZipOutside.rewrite(f.body)
    TypeChecker(f0)
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

    val options = Rewrite.rewriteJustGenerable(f, levels = 1)
    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    assertTrue(options.nonEmpty)

    options.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l, A)
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
    val (gold: Array[Float], _) = Execute(128)(goldF, A, a)
    val lambdaOptions = Rewrite.rewriteJustGenerable(f)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A, a)
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
    val h = Rewrite.applyRulesUntilCannot(g, Seq(Rules.mapGlb))

    val input = Array.fill[Float](128, 128)(util.Random.nextFloat())

    val (result:Array[Float], _) = Execute(128)(h, input)

    assertArrayEquals(input.flatten, result, 0.0f)
    assertEquals(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), h.body.t)
  }

  @Test
  def joinSplitId(): Unit = {
    val goldF = fun(
      ArrayTypeWSWC(Float, N),
      input => MapGlb(id) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => MapGlb(id) o Join() o Split(8) $ input
    )

    TypeChecker.check(f.body)

    val lambdaOptions = Rewrite.rewriteJustGenerable(f, simplificationRules, 1)

    assertTrue(lambdaOptions.nonEmpty)
    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def splitJoinId(): Unit = {
    val A = Array.fill[Float](128, 4)(0.5f)

    val goldF = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 4), N),
      input => MapGlb(MapSeq(id)) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 4), N),
      input => MapGlb(MapSeq(id)) o Split(4) o Join() $ input
    )

    TypeChecker.check(f.body)

    val lambdaOptions = Rewrite.rewriteJustGenerable(f, simplificationRules, 1)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def asScalarAsVector(): Unit = {
    val goldF = fun(
      ArrayTypeWSWC(Float, N),
      input => MapGlb(id) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => MapGlb(id) o asScalar() o asVector(8) $ input
    )

    TypeChecker.check(f.body)

    val lambdaOptions = Rewrite.rewriteJustGenerable(f, simplificationRules, 1)

    assertTrue(lambdaOptions.nonEmpty)
    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def asVectorAsScalar(): Unit = {
    val A = Array.fill[Float](128, 4)(0.5f)

    val goldF = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 4), N),
      input => MapGlb(MapSeq(id)) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayTypeWSWC(VectorType(Float, 4), N),
      input => MapGlb(id.vectorize(4)) o asVector(4) o asScalar() $ input
    )

    TypeChecker.check(f.body)

    val lambdaOptions = Rewrite.rewriteJustGenerable(f, simplificationRules, 1)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
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
      Seq(Rules.reduceSeq, Rules.addIdAfterReduce, Rules.implementIdAsDeepCopy, Rules.globalMemory), 4)

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)

    assertTrue(lambdaOptions.nonEmpty)

    val (result: Array[Float], _) = Execute(1, 1)(lambdaOptions(0), A)
    assertArrayEquals(lambdaOptions(1) + " failed", gold, result, 0.0f)
  }

  @Test
  def ReduceSeqMapSeq(): Unit = {
    val goldF = fun(
      ArrayTypeWSWC(Float, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, plusOne(newValue))), 0.0f) $ input
    )

    val f = fun(
      ArrayTypeWSWC(Float, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(plusOne) $ input
    )

    val lambdaOptions = Rewrite.rewriteJustGenerable(f, fusionRules, 1)

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }


  @Test
  def ReduceSeqMapSeqChangesType(): Unit = {

    val userFun = UserFun("idIntToFloat", "x", "{ return x; }", Int, Float)

    val goldF = fun(
      ArrayTypeWSWC(Int, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, userFun(newValue))), 0.0f) $ input
    )

    val f = fun(
      ArrayTypeWSWC(Int, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(userFun) $ input
    )

    val A = Array.tabulate(128)(i => i)

    val lambdaOptions = Rewrite.rewriteJustGenerable(f, fusionRules, 1)

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def ReduceSeqMapSeqArray(): Unit = {

    val A = Array.fill[Float](128, 4)(0.5f)

    val goldF = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 4), N),
      input => toGlobal(MapSeq(MapSeq(id))) o
        ReduceSeq(fun((acc, elem) => MapSeq(add) o fun(elem => Zip(acc, MapSeq(plusOne) $ elem)) $ elem),
          Value(0.0f, ArrayTypeWSWC(Float, 4))) $ input
    )

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 4), N),
      input => toGlobal(MapSeq(MapSeq(id))) o
        ReduceSeq(fun((acc, elem) => MapSeq(add) $ Zip(acc, elem)),
          Value(0.0f, ArrayTypeWSWC(Float, 4))) o MapSeq(MapSeq(plusOne)) $ input
    )

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)

    val lambdaOptions = Rewrite.rewriteJustGenerable(f, fusionRules, 1)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def moreComplexReduceSeqMapSeq(): Unit = {
    val goldF = fun(
      ArrayTypeWSWC(Float, N),
      Float,
      (input, a) => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, add(newValue, a))), 0.0f) $ input
    )

    val f = fun(
      ArrayTypeWSWC(Float, N),
      Float,
      (input, a) => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(fun(x => add(x, a))) $ input
    )

    val a = 2.0f

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A, a)

    val lambdaOptions = Rewrite.rewriteJustGenerable(f, fusionRules, 1)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A, a)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def simpleId(): Unit = {
    val f: Lambda = fun(
      ArrayTypeWSWC(Float, N),
      input => Map(plusOne) $ input
    )

    val e = f match {
      case Lambda(_, FunCall(Map(Lambda(_, c)), _)) => c
    }

    println(applyRule(f, e, Rules.addCopy))
  }

  @Test
  def arrayId(): Unit = {
    val f: Lambda = fun(
      ArrayTypeWSWC(Float, N),
      input => Map(id) $ input
    )

    val e = f match {
      case Lambda(_, c@FunCall(_, _)) => c
    }

    println(applyRule(f, e, Rules.addCopy))
  }

  @Test
  def zipId(): Unit = {
    val f: Lambda = fun(
      ArrayTypeWSWC(Float, N),
      input => Map(idFF) $ Zip(input, input)
    )

    val e = f match {
      case Lambda(_, c@FunCall(_, _)) => c
    }

    println(applyRule(f, e, Rules.addCopy))
  }

  @Test
  def mapFissionNotAllowed0(): Unit = {
    // Map o Map
    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Map(fun(x => Map(fun(y => add(x, y))) o Map(plusOne) $ input)) $ input
    )

    assertFalse(Rules.mapFission.rewrite.isDefinedAt(f.body))
  }

  @Test
  def mapFissionNotAllowed1(): Unit = {
    // Map o Reduce
    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Map(fun(x => Map(fun(y => add(x, y))) o Reduce(add, 0.0f) $ input)) $ input
    )

    assertFalse(Rules.mapFission.rewrite.isDefinedAt(f.body))
  }

  @Test
  def mapFissionNotAllowed2(): Unit = {

    // Reduce o Map
    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Map(fun(x =>
          ReduceSeq(fun((acc, y) => add(acc, mult(x,y))), 0.0f) o
            Map(plusOne) $ input
        )) $ input
    )

    assertFalse(Rules.mapFission.rewrite.isDefinedAt(f.body))
  }

  @Test
  def mapFissionNotAllowed3(): Unit = {
    // Reduce o Reduce
    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Map(fun(x =>
          ReduceSeq(fun((acc, y) => add(acc, mult(x,y))), 0.0f) o
            Reduce(add, 0.0f) $ input
        )) $ input
    )

    assertFalse(Rules.mapFission.rewrite.isDefinedAt(f.body))
  }

  @Test
  def mapFissionWhenArgUsedInBoth0(): Unit = {
    // Map o Map
    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Map(fun(x => Map(fun(y => add(x, y))) o Map(plusOne) $ input)) $ input
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.mapFission2)
    TypeChecker(f1)
  }

  @Test
  def mapFissionWhenArgUsedInBoth1(): Unit = {
    // Map o Reduce
    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Map(fun(x => Map(fun(y => add(x, y))) o Reduce(add, 0.0f) $ input)) $ input
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.mapFission2)
    TypeChecker(f1)
  }

  @Test
  def mapFissionWhenArgUsedInBoth2(): Unit = {
    // Reduce o Map
    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Map(fun(x =>
          ReduceSeq(fun((acc, y) => add(acc, mult(x,y))), 0.0f) o
            Map(plusOne) $ input
        )) $ input
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.mapFission2)
    TypeChecker(f1)
    assertTrue(f1.body.asInstanceOf[FunCall].f.asInstanceOf[Map].f.body.asInstanceOf[FunCall].f.isInstanceOf[ReduceSeq])
  }

  @Test
  def mapFissionWhenArgUsedInBoth3(): Unit = {
    // Reduce o Reduce
    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Map(fun(x =>
          ReduceSeq(fun((acc, y) => add(acc, mult(x,y))), 0.0f) o
            Reduce(add, 0.0f) $ input
        )) $ input
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.mapFission2)
    TypeChecker(f1)
    assertTrue(f1.body.asInstanceOf[FunCall].f.asInstanceOf[Map].f.body.asInstanceOf[FunCall].f.isInstanceOf[ReduceSeq])
  }

  @Test
  def mapReduceReduceNesting0(): Unit = {
    // Different f and init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(PartRed(mult, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(Rules.mapReducePartialReduce.isDefinedAt(f.body))
  }

  @Test
  def mapReduceReduceNesting1(): Unit = {
    // Different f and init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(Reduce(mult, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(Rules.mapReducePartialReduce.isDefinedAt(f.body))
  }

  @Test
  def mapReduceReduceNesting2(): Unit = {
    // Different init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(Reduce(add, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(Rules.mapReducePartialReduce.isDefinedAt(f.body))
  }

  @Test
  def mapReduceReduceNesting3(): Unit = {
    // Different init
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(ReduceSeq(add, 0.0f) o Join() o Map(PartRed(add, 1.0f))) $ input
    )

    TypeChecker(f)
    assertFalse(Rules.mapReducePartialReduce.isDefinedAt(f.body))
  }

}
