package rewriting

import apart.arithmetic.{RangeMul, SizeVar, Var}
import ir.ast._
import ir.{ArrayType, Type, TypeChecker, VectorType}
import opencl.executor.{Eval, Execute, Executor}
import opencl.ir._
import opencl.ir.ast._
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestRules {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }
}

class TestRules {

  def applyRule(lambda: Lambda, expr: Expr, rule: Rule) = {
    TypeChecker.check(lambda.body)
    val newLambda = FunDecl.replace(lambda, expr, rule.rewrite(expr))
    newLambda
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

    // split o map(transpose) =>

    // transpose o split =>
  }

  @Test
  def testDot(): Unit = {

    val input = Array.fill(4)(util.Random.nextFloat())
    val gold = (input, input).zipped.map(_*_).sum

    val f = fun(
      ArrayType(Float4, 1),
      ArrayType(Float4, 1),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          asScalar() o
          MapSeq(VectorizeUserFun(4, mult)) $ Zip(x, y)
    )

    val g = fun(
      ArrayType(Float4, 1),
      ArrayType(Float4, 1),
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
  def testDot2(): Unit = {

    val input = Array.fill(16)(util.Random.nextFloat())
    val gold = (input, input).zipped.map(_*_).sum

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float4, N),
      ArrayType(Float4, N),
      (x, y) =>
        toGlobal(MapSeq(id)) o
          ReduceSeq(add, 0.0f) o
          asScalar() o
          MapSeq(VectorizeUserFun(4, mult)) $ Zip(x, y)
    )

    val g = fun(
      ArrayType(Float4, N),
      ArrayType(Float4, N),
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

    val input = Array.fill(16)(util.Random.nextFloat())
    val gold = (input, input).zipped.map(_*_).sum

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float4, N),
      ArrayType(Float4, N),
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
      ArrayType(Float, N),
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
      ArrayType(Float, N),
      in => {
        Gather(ReorderWithStride(var3/var1)) o Scatter(ReorderWithStride(var4/var2)) $ in
      })

    TypeChecker(f)
    assertTrue(Rules.gatherScatterId.isDefinedAt(f.body))
  }

  @Test
  def joinSplitIdWithRanges(): Unit = {
    val f = fun(
      ArrayType(Float, N),
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
    ArrayType(Float, N),
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
      ArrayType(Float, 16),
      in => Gather(ReorderWithStride(16)) o Scatter(ReorderWithStride(16)) $ in
    )

    val g = fun(
      ArrayType(Float, 16),
      in => Scatter(ReorderWithStride(16)) o Gather(ReorderWithStride(16)) $ in
    )

    assertTrue(Rules.gatherScatterId.rewrite.isDefinedAt(f.body))
    assertTrue(Rules.scatterGatherId.rewrite.isDefinedAt(g.body))
  }

  @Test
  def nestPartialReduceInReduce(): Unit = {
    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, 16), 16), 16),
      a => Map( Reduce(add, 0.0f) o Join() o Map(PartRed(fun((x, y) => add(x, y)), 0.0f)) ) $ a)

    println(Rewrite.applyRuleAtId(f, 0, Rules.mapReducePartialReduce))
  }

  @Test
  def fissionAtPosition(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      a => Map(Reduce(add, 0.0f) o Map(plusOne) o Map(plusOne) o Map(plusOne)) $ a)

    Rewrite.applyRuleAtId(f, 0, MacroRules.mapFissionAtPosition(0))
    Rewrite.applyRuleAtId(f, 0, MacroRules.mapFissionAtPosition(1))
    Rewrite.applyRuleAtId(f, 0, MacroRules.mapFissionAtPosition(2))
  }

  @Test
  def pullingExpressionsOutOfZip(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), N),
      (in1, in2) => MapGlb(fun(x =>
        MapSeq(fun(y =>
          MapSeq(fun(a => add(Get(a, 0), Get(a,1)))) $ Zip(Gather(reverse) $ x, Gather(reverse) $ y)
        )) $ in2
      )) $ in1
    )

    val fP = fun(
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), N),
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
      ArrayType(Float, N),
      input => Map(id o id) $ input
    )

    assertTrue(Rules.mapFission.rewrite.isDefinedAt(f.body))
    println(Lambda(f.params, Rules.mapFission.rewrite(f.body)))

    val M = SizeVar("M")

    val g = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Map(fun(x => Reduce(add, 0.0f) o Map(id) $ Zip(x, x))) $ input
    )

    assertTrue(Rules.mapFission.rewrite.isDefinedAt(g.body))
    println(Lambda(g.params, Rules.mapFission.rewrite(g.body)))
  }

  @Test
  def transposeTransposeId(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Transpose() o Transpose() $ input
    )

    assertTrue(Rules.transposeTransposeId.rewrite.isDefinedAt(f.body))
    assertSame(f.params.head, Rules.transposeTransposeId.rewrite(f.body))
  }

  @Test
  def mapReduceInterchange(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(ArrayType(ArrayType(Float, M), N),
      input => Map(Reduce(add, 0.0f)) $ input
    )

    assertTrue(Rules.mapReduceInterchange.rewrite.isDefinedAt(f.body))
  }

  @Test
  def transposeBothSides(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(ArrayType(ArrayType(Float, M), N),
      input => Map(Map(plusOne)) $ input
    )

    TypeChecker.check(f.body)

    assertTrue(Rules.transposeBothSides.rewrite.isDefinedAt(f.body))

    val g = fun(ArrayType(ArrayType(Float, M), N),
      input => Map(Map(Map(plusOne)) o Split(2)) $ input
    )

    TypeChecker.check(g.body)
    assertFalse(Rules.transposeBothSides.rewrite.isDefinedAt(g.body))
  }

  @Test
  def mapMapTransposeWithZipInside(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(ArrayType(ArrayType(Float, M), N),
      ArrayType(Float, M),
      (in1, in2) => Map(fun(x => Map(fun(x => add(Get(x, 0), Get(x, 1)))) $ Zip(in2, x))) $ in1
    )

    assertTrue(Rules.mapMapTransposeZipInside.rewrite.isDefinedAt(f.body))
    println(Rules.mapMapTransposeZipInside.rewrite(f.body))
  }

  @Test
  def mapMapTransposeWithZipOutside(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(ArrayType(ArrayType(Float, M), N),
      ArrayType(Float, M),
      (in1, in2) => Map(fun(x => Map(fun(y => add(y, Get(x, 1)))) $ Get(x, 0))) $ Zip(in1, in2)
    )

    assertTrue(Rules.mapMapTransposeZipOutside.rewrite.isDefinedAt(f.body))
    println(Rules.mapMapTransposeZipOutside.rewrite(f.body))
  }

  @Test
  def simpleMapTest(): Unit = {

    def f = fun(
      ArrayType(Float, N),
      input => Map(id) $ input
      // input => Join() o MapWrg(Join() o Map(MapLane(id)) o Split(2) ) o Split(2) $ input
    )

    def goldF = fun(
      ArrayType(Float, N),
      input => MapGlb(id) $ input
    )

    val options = Rewrite.rewriteJustGenerable(f, levels = 1)
    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    assertTrue(options.nonEmpty)

    options.foreach(l => {
      println("execute: " + l)
      val (result: Array[Float], _) = Execute(128)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def slightlyMoreComplexMap(): Unit = {
    val goldF = fun(
      ArrayType(Float, N),
      Float,
      (input, a) => MapGlb(fun(x => add(x, a))) $ input
    )

    def f = fun(
      ArrayType(Float, N),
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
    val goldF = fun(
      ArrayType(Float, N),
      input => MapGlb(id) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayType(Float, N),
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
  def splitJoin(): Unit = {
    val A = Array.fill[Float](128, 4)(0.5f)

    val goldF = fun(
      ArrayType(ArrayType(Float, 4), N),
      input => MapGlb(MapSeq(id)) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayType(ArrayType(Float, 4), N),
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
      ArrayType(Float, N),
      input => MapGlb(id) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayType(Float, N),
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
      ArrayType(ArrayType(Float, 4), N),
      input => MapGlb(MapSeq(id)) $ input
    )

    val (gold: Array[Float], _) = Execute(128)(goldF, A)

    val f = fun(
      ArrayType(VectorType(Float, 4), N),
      input => MapGlb(id.vectorize(4)) o asVector(4) o asScalar() $ input
    )

    TypeChecker.check(f.body)

    val lambdaOptions = Rewrite.rewriteJustGenerable(f, simplificationRules, 1)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.zipWithIndex.foreach(l => {
      val (result: Array[Float], _) = Execute(128)(l._1, A.flatten)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def simpleReduceTest(): Unit = {
    val goldF = fun(
      ArrayType(Float, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) $ input
    )

    val f = fun(
      ArrayType(Float, N),
      input => Reduce(add, 0.0f) $ input
    )

    val lambdaOptions = Rewrite.rewriteJustGenerable(f,
      Seq(Rules.reduceSeq, Rules.implementIdAsDeepCopy, Rules.globalMemory), 3)

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)

    assertTrue(lambdaOptions.nonEmpty)

      val (result: Array[Float], _) = Execute(1, 1)(lambdaOptions(1), A)
      assertArrayEquals(lambdaOptions(1) + " failed", gold, result, 0.0f)
  }

  @Test
  def ReduceSeqMapSeq(): Unit = {
    val goldF = fun(
      ArrayType(Float, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, plusOne(newValue))), 0.0f) $ input
    )

    val f = fun(
      ArrayType(Float, N),
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
      ArrayType(Int, N),
      input => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, userFun(newValue))), 0.0f) $ input
    )

    val f = fun(
      ArrayType(Int, N),
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
      ArrayType(ArrayType(Float, 4), N),
      input => toGlobal(MapSeq(MapSeq(id))) o
        ReduceSeq(fun((acc, elem) => MapSeq(add) o fun(elem => Zip(acc, MapSeq(plusOne) $ elem)) $ elem),
          Value(0.0f, ArrayType(Float, 4))) $ input
    )

    val f = fun(
      ArrayType(ArrayType(Float, 4), N),
      input => toGlobal(MapSeq(MapSeq(id))) o
        ReduceSeq(fun((acc, elem) => MapSeq(add) $ Zip(acc, elem)),
          Value(0.0f, ArrayType(Float, 4))) o MapSeq(MapSeq(plusOne)) $ input
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
      ArrayType(Float, N),
      Float,
      (input, a) => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, newValue) => add(acc, add(newValue, a))), 0.0f) $ input
    )

    val f = fun(
      ArrayType(Float, N),
      Float,
      (input, a) => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(fun(x => add(x, a))) $ input
    )

    val a = 2.0f

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A, a)

    val lambdaOptions = Rewrite.rewriteJustGenerable(f, fusionRules, 1)

    println(lambdaOptions)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A, a)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }

  @Test
  def simpleId(): Unit = {
    val f: Lambda = fun(
      ArrayType(Float, N),
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
      ArrayType(Float, N),
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
      ArrayType(Float, N),
      input => Map(idFF) $ Zip(input, input)
    )

    val e = f match {
      case Lambda(_, c@FunCall(_, _)) => c
    }

    println(applyRule(f, e, Rules.addCopy))
  }

  @Test
  def moveReduceOutOneLevel(): Unit = {

    val patternSimple: PartialFunction[Expr, Unit] =
      { case FunCall(TransposeW(), FunCall(Reduce(_), _, _)) => }

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      input => Map(Reduce(add, 0.0f)) $ input
    )

    val fResult = Rewrite.applyRuleAtId(f, 0, MacroRules.moveReduceOutOneLevel)
    assertTrue(patternSimple.isDefinedAt(fResult.body))

    val g = fun(
      ArrayType(ArrayType(Float, N), N),
      input => Map(Reduce(add, 0.0f) o Gather(reverse)) $ input
    )

    val gResult = Rewrite.applyRuleAtId(g, 0, MacroRules.moveReduceOutOneLevel)
    assertTrue(patternSimple.isDefinedAt(gResult.body))

    val patternWithMap: PartialFunction[Expr, Unit] =
      { case FunCall(Map(_), FunCall(TransposeW(), FunCall(Reduce(_), _, _))) => }

    val h = fun(
      ArrayType(ArrayType(Float, N), N),
      input => Map(Scatter(reverse) o Reduce(add, 0.0f)) $ input
    )

    val hResult = Rewrite.applyRuleAtId(h, 0, MacroRules.moveReduceOutOneLevel)
    assertTrue(patternWithMap.isDefinedAt(hResult.body))

    val m = fun(
      ArrayType(ArrayType(Float, N), N),
      input => Map(Scatter(reverse) o Scatter(reverse) o Reduce(add, 0.0f) o Gather(reverse)) $ input
    )

    val mResult = Rewrite.applyRuleAtId(m, 0, MacroRules.moveReduceOutOneLevel)
    assertTrue(patternWithMap.isDefinedAt(mResult.body))
  }
}
