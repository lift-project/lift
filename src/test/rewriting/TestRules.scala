package rewriting

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

object TestRules extends TestWithExecutor

class TestRules {

  private val N = SizeVar("N")
  private val M = SizeVar("M")
  private val A = Array.fill[Float](128)(0.5f)

  //noinspection MapFlatten
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
