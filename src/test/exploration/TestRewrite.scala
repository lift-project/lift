package exploration

import apart.arithmetic.Var
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestRewrite {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }
}

class TestRewrite {
  val N = Var("N")
  val A = Array.fill[Float](128)(0.5f)

  @Test
  def mmReuseA(): Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    TypeChecker.check(f.body)

    val splitJoin = f.body match {
      case FunCall(Map(Lambda(_, call @ FunCall(_, _))), _) => call
    }

    val splitJoinRewrite = Rules.splitJoin.rewrite
    assertTrue(splitJoinRewrite.isDefinedAt(splitJoin))

    val f1 = FunDecl.replace(f, splitJoin, splitJoinRewrite(splitJoin))
    TypeChecker.check(f1.body)

    val mapFission = f1.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, call)), _)))), _) => call
    }

    val mapFissionRewrite = Rules.mapFission.rewrite
    assertTrue(mapFissionRewrite.isDefinedAt(mapFission))

    val f2 = FunDecl.replace(f1, mapFission, mapFissionRewrite(mapFission))
    TypeChecker.check(f2.body)

    val mapReduceInterchange = f2.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, call)), _)))), _) => call
    }

    val mapReduceInterchangeRewrite = Rules.mapReduceInterchange.rewrite
    assertTrue(mapReduceInterchangeRewrite.isDefinedAt(mapReduceInterchange))

    val f3 = FunDecl.replace(f2, mapReduceInterchange, mapReduceInterchangeRewrite(mapReduceInterchange))
    TypeChecker.check(f3.body)

    val mapMapTranspose = f3.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(_, _, FunCall(_, call)))), _)))), _) => call
    }

    val mapMapTransposeRewrite = Rules.transposeBothSides.rewrite
    assertTrue(mapMapTransposeRewrite.isDefinedAt(mapMapTranspose))

    val f4 = FunDecl.replace(f3, mapMapTranspose, mapMapTransposeRewrite(mapMapTranspose))
    TypeChecker.check(f4.body)

    val transposeTranspose = f4.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(_, _, call))), _)))), _) => call
    }

    val transposeTransposeRewrite = Rules.transposeTransposeId.rewrite
    assertTrue(transposeTransposeRewrite.isDefinedAt(transposeTranspose))

    val f5 = FunDecl.replace(f4, transposeTranspose, transposeTransposeRewrite(transposeTranspose))
    TypeChecker.check(f5.body)


    val mapToMapSeq = f5.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(_, _, FunCall(Map(Lambda(_, call)), _)))), _)))), _) => call
    }

    val mapToMapSeqRewrite = Rules.mapSeq.rewrite
    assertTrue(mapToMapSeqRewrite.isDefinedAt(mapToMapSeq))

    val f6 = FunDecl.replace(f5, mapToMapSeq, mapToMapSeqRewrite(mapToMapSeq))
    TypeChecker.check(f6.body)

    val mapToMapSeq2 = f6.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, call)), _, _))), _)))), _) => call
    }

    assertTrue(mapToMapSeqRewrite.isDefinedAt(mapToMapSeq2))

    val f7 = FunDecl.replace(f6, mapToMapSeq2, mapToMapSeqRewrite(mapToMapSeq2))
    TypeChecker.check(f7.body)

    val mapToMapSeq3 = f7.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(_, _, call))), _)))), _) => call
    }

    assertTrue(mapToMapSeqRewrite.isDefinedAt(mapToMapSeq3))

    val f8 = FunDecl.replace(f7, mapToMapSeq3, mapToMapSeqRewrite(mapToMapSeq3))
    TypeChecker.check(f8.body)

    val reduceToReduceSeq = f8.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, call)), _)))), _) => call
    }

    val reduceToReduceSeqRewrite = Rules.reduceSeq.rewrite
    assertTrue(reduceToReduceSeqRewrite.isDefinedAt(reduceToReduceSeq))

    val f9 = FunDecl.replace(f8, reduceToReduceSeq, reduceToReduceSeqRewrite(reduceToReduceSeq))
    TypeChecker.check(f9.body)

    val fusion = f9.body match {
      case FunCall(Map(Lambda(_, FunCall(_, FunCall(Map(Lambda(_, FunCall(_, call))), _)))), _) => call
    }

    val fusionRewrite = Rules.mapReduceFusion.rewrite
    assertTrue(fusionRewrite.isDefinedAt(fusion))

    val f10 = FunDecl.replace(f9, fusion, fusionRewrite(fusion))
    TypeChecker.check(f10.body)

    println(f10)
  }

  @Test
  def mapFission(): Unit = {
    val N = Var("N")

    val f = fun(
      ArrayType(Float, N),
      input => Map(id o id) $ input
    )

    assertTrue(Rules.mapFission.rewrite.isDefinedAt(f.body))
    println(Lambda(f.params, Rules.mapFission.rewrite(f.body)))

    val M = Var("M")

    val g = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Map(fun(x => Reduce(add, 0.0f) o Map(id) $ Zip(x, x))) $ input
    )

    assertTrue(Rules.mapFission.rewrite.isDefinedAt(g.body))
    println(Lambda(g.params, Rules.mapFission.rewrite(g.body)))
  }

  @Test
  def transposeTransposeId(): Unit = {
    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Transpose() o Transpose() $ input
    )

    assertTrue(Rules.transposeTransposeId.rewrite.isDefinedAt(f.body))
    assertSame(f.params.head, Rules.transposeTransposeId.rewrite(f.body))
  }

  @Test
  def mapReduceInterchange(): Unit = {
    val N = Var("N")
    val M = Var("M")

    val f = fun(ArrayType(ArrayType(Float, M), N),
      input => Map(Reduce(add, 0.0f)) $ input
    )

    assertTrue(Rules.mapReduceInterchange.rewrite.isDefinedAt(f.body))
  }

  @Test
  def transposeBothSides(): Unit = {
    val N = Var("N")
    val M = Var("M")

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

    val h = fun(ArrayType(ArrayType(ArrayType(Float, 2), M), N),
      input => Map(Join() o Map(Map(plusOne))) $ input
    )

    TypeChecker.check(h.body)
    assertTrue(Rules.transposeBothSides.rewrite.isDefinedAt(h.body))
  }

  @Test
  def mapMapTransposeWithZipInside(): Unit = {
    val N = Var("N")
    val M = Var("M")

    val f = fun(ArrayType(ArrayType(Float, M), N),
                ArrayType(Float, M),
      (in1, in2) => Map(fun(x => Map(fun(x => add(Get(x, 0), Get(x, 1)))) $ Zip(in2, x))) $ in1
    )

    assertTrue(Rules.mapMapTransposeZipInside.rewrite.isDefinedAt(f.body))
    println(Rules.mapMapTransposeZipInside.rewrite(f.body))
  }

  @Test
  def mapMapTransposeWithZipOutside(): Unit = {
    val N = Var("N")
    val M = Var("M")

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

    val options = Rewrite.rewrite(f, levels = 1)
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
    val lambdaOptions = Rewrite.rewrite(f)

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

    val lambdaOptions = Rewrite.rewrite(f)

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

    val lambdaOptions = Rewrite.rewrite(f)

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

    val lambdaOptions = Rewrite.rewrite(f)

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

    val lambdaOptions = Rewrite.rewrite(f)

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

    val lambdaOptions = Rewrite.rewrite(f)

    val (gold: Array[Float] ,_) = Execute(1, 1)(goldF, A)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
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

    val lambdaOptions = Rewrite.rewrite(f)

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

    val lambdaOptions = Rewrite.rewrite(f)

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

    val lambdaOptions = Rewrite.rewrite(f)

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

    val lambdaOptions = Rewrite.rewrite(f)

    assertTrue(lambdaOptions.nonEmpty)

    lambdaOptions.foreach(l => {
      val (result: Array[Float], _) = Execute(1, 1)(l, A, a)
      assertArrayEquals(l + " failed", gold, result, 0.0f)
    })
  }


}
