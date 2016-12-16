package rewriting

import ir._
import ir.ast._
import opencl.ir._
import opencl.executor.{Compile, Execute, Executor}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import rewriting.utils.NumberExpression

object TestRewriteStencil {
  @BeforeClass
  def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass
  def after(): Unit = {
    Executor.shutdown()
  }
}

class TestRewriteStencil {

  @Test
  def stencil1DTiling(): Unit = {
    val n = 128
    val f = fun(
      ArrayType(Float, n),
      (input) =>
        Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
      )

    val A = Array.fill(n)(util.Random.nextInt(500).toFloat)
    val gold = (A.slice(0,1) ++ A ++ A.slice(n-1,n)).sliding(3,1).map(x =>
      x.sum).toArray

    val f1 = Rewrite.applyRuleAtId(f, 1, Rules.slideTiling(4))
    val f2 = Rewrite.applyRuleAtId(f1, 0, Rules.mapJoin)
    val f3 = Rewrite.applyRuleAtId(f2, 1, Rules.mapFusion)
    // introduce low-level primitives
    val f4 = Rewrite.applyRuleAtId(f3, 8, Rules.reduceSeq)
    val f5 = Rewrite.applyRuleAtId(f4, 1, Rules.mapWrg)
    val f6 = Rewrite.applyRuleAtId(f5, 5, Rules.mapLcl)
    // copy result back to global memory
    val f7 = Rewrite.applyRuleAtId(f6, 8, Rules.addIdAfterReduce)
    val f8 = Rewrite.applyRuleAtId(f7, 15, Rules.implementIdAsDeepCopy)
    val f9 = Rewrite.applyRuleAtId(f8, 8, Rules.globalMemory)

    val (result: Array[Float], _) = Execute(n)(f9, A)
    assertArrayEquals(gold, result, 0.001f)
  }

  @Test
  def stencil1DTilingLocalMemory(): Unit = {
    val n = 128
    val f = fun(
      ArrayType(Float, n),
      (input) =>
        Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
      )

    val A = Array.fill(n)(util.Random.nextInt(500).toFloat)
    val gold = (A.slice(0,1) ++ A ++ A.slice(n-1,n)).sliding(3,1).map(x =>
      x.sum).toArray

    // tiling
    val f1 = Rewrite.applyRuleAtId(f, 1, Rules.slideTiling(4))
    val f2 = Rewrite.applyRuleAtId(f1, 0, Rules.mapJoin)
    val f3 = Rewrite.applyRuleAtId(f2, 1, Rules.mapFusion)
    // local memory
    val f4 = Rewrite.applyRuleAtId(f3, 6, Rules.addId)
    val f5 = Rewrite.applyRuleAtId(f4, 7, Rules.implementIdAsDeepCopy)
    val f6 = Rewrite.applyRuleAtId(f5, 11, Rules.reduceSeq)
    val f7 = Rewrite.applyRuleAtId(f6, 11, Rules.addIdAfterReduce)
    val f8 = Rewrite.applyRuleAtId(f7, 18, Rules.implementIdAsDeepCopy)
    val f9 = Rewrite.applyRuleAtId(f8, 1, Rules.mapWrg)
    val f10 = Rewrite.applyRuleAtId(f9, 5, Rules.mapLcl)
    val f11 = Rewrite.applyRuleAtId(f10, 7, Rules.mapLcl)
    val f12 = Rewrite.applyRuleAtId(f11, 7, Rules.localMemory)
    val f13 = Rewrite.applyRuleAtId(f12, 13, Rules.globalMemory)
    //val test = NumberExpression.breadthFirst(f13.body)

    val (result: Array[Float], _) = Execute(n)(f13, A)
    assertArrayEquals(gold, result, 0.001f)
  }

  @Test
  def stencil1DMacroRule(): Unit = {
    val n = 128
    val f = fun(
      ArrayType(Float, n),
      (input) =>
        Join() o Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
      )

    val f1 = Rewrite.applyRuleAtId(f, 1, MacroRules.tileStencils)
    val f2 = Rewrite.applyRuleAtId(f1, 9, Rules.reduceSeq)
    val f3 = Rewrite.applyRuleAtId(f2, 9, Rules.addIdAfterReduce)
    val f4 = Rewrite.applyRuleAtId(f3, 16, Rules.implementIdAsDeepCopy)
    val f5 = Rewrite.applyRuleAtId(f4, 9, Rules.globalMemory)
    val f6 = Rewrite.applyRuleAtId(f5, 2, Rules.mapWrg)
    val f7 = Rewrite.applyRuleAtId(f6, 6, Rules.mapLcl)
    val test = NumberExpression.breadthFirst(f5.body)
    println(test.mkString("\n\n"))

    val A = Array.fill(n)(util.Random.nextInt(500).toFloat)
    val gold = (A.slice(0,1) ++ A ++ A.slice(n-1,n)).sliding(3,1).map(x =>
      x.sum).toArray

    TypeChecker(f7)
    // kernel contains generic tile size as additional kernel argument
    val kernel = Compile(f7)
    //val (result: Array[Float], _) = Execute(n)(kernel, f7, A, 4)
    //assertArrayEquals(gold, result, 0.001f)
  }
}
