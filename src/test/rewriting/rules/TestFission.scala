package rewriting.rules

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{Ignore, Test}
import rewriting.{Rewrite, Rules}

object TestFission extends TestWithExecutor

class TestFission {

  private val N = SizeVar("N")

  @Test
  def mapFissionZip(): Unit = {
    val t1 = ArrayType(TupleType(ArrayType(Float, N), ArrayType(Float, N)), N)
    val t2 = ArrayType(Float, N)

    val f = fun(
      t1, t2, (a,b) =>
        Map(fun(x => Split(64) $ Zip(b, Get(x, 0)))) $ a
    )
    TypeChecker(f)

    assertTrue(Rules.mapFission.isDefinedAt(f.body))
    val result = Rewrite.applyRuleAt(f, f.body, Rules.mapFission)
    TypeChecker(result)
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

  @Ignore
  @Test
  def mapFissionWhenArgUsedInBoth0(): Unit = {
    // Map o Map
    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Map(fun(x => Map(fun(y => add(x, y))) o Map(plusOne) $ input)) $ input
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.mapFissionCreateZip)
    TypeChecker(f1)
  }

  @Ignore
  @Test
  def mapFissionWhenArgUsedInBoth1(): Unit = {
    // Map o Reduce
    val f = fun(
      ArrayTypeWSWC(Float, N),
      input =>
        Map(fun(x => Map(fun(y => add(x, y))) o Reduce(add, 0.0f) $ input)) $ input
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.mapFissionCreateZip)
    TypeChecker(f1)
  }

  @Ignore
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

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.mapFissionCreateZip)
    TypeChecker(f1)
    assertTrue(f1.body.asInstanceOf[FunCall].f.asInstanceOf[Map].f.body.asInstanceOf[FunCall].f.isInstanceOf[ReduceSeq])
  }

  @Ignore
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

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.mapFissionCreateZip)
    TypeChecker(f1)
    assertTrue(f1.body.asInstanceOf[FunCall].f.asInstanceOf[Map].f.body.asInstanceOf[FunCall].f.isInstanceOf[ReduceSeq])
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

    val (result, _) = Execute(size)[Array[Float]](f, A, A)

    val g0 = Rewrite.applyRuleAtId(fP, 2, Rules.mapFissionWithZipInside)
    val g1 = Rewrite.applyRuleAtId(g0, 0, Rules.mapGlb)
    val g2 = Rewrite.applyRuleAtId(g1, 2, Rules.mapSeq)

    val (resultG, _) = Execute(size)[Array[Float]](g2, A, A)

    val h0 = Rewrite.applyRuleAtId(fP, 0, Rules.mapFissionWithZipInside)
    val h1 = Rewrite.applyRuleAtId(h0, 0, Rules.mapGlb)
    val h2 = Rewrite.applyRuleAtId(h1, 5, Rules.mapSeq)

    val (resultH, _) = Execute(size)[Array[Float]](h2, A, A)

    assertArrayEquals(result, resultG, 0.0f)
    assertArrayEquals(result, resultH, 0.0f)
  }
}
