package rewriting.rules

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.TestWithExecutor
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.Rewrite

object TestCopy extends TestWithExecutor

class TestCopy {

  private val N = SizeVar("N")
  private val M = SizeVar("M")

  @Test
  def simpleId(): Unit = {
    val f: Lambda = fun(
      ArrayTypeWSWC(Float, N),
      input => Map(plusOne) $ input
    )

    val e = f match {
      case Lambda(_, FunCall(Map(Lambda(_, c, _)), _), _) => c
    }

    val value = Rewrite.applyRuleAt(f, e, CopyRules.addCopy)
    println(value)
  }

  @Test
  def arrayId(): Unit = {
    val f: Lambda = fun(
      ArrayTypeWSWC(Float, N),
      input => Map(id) $ input
    )

    val e = f match {
      case Lambda(_, c@FunCall(_, _), _) => c
    }

    println(Rewrite.applyRuleAt(f, e, CopyRules.addCopy))
  }

  @Test
  def zipId(): Unit = {
    val f: Lambda = fun(
      ArrayTypeWSWC(Float, N),
      input => Map(idFF) $ Zip(input, input)
    )

    val e = f match {
      case Lambda(_, c@FunCall(_, _), _) => c
    }

    println(Rewrite.applyRuleAt(f, e, CopyRules.addCopy))
  }

    @Test
  def addIdMapWrg(): Unit = {


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
    val rewrittenG = Rewrite.applyRuleUntilCannot(g, CopyRules.addIdForMapWrgParam)
    val rewrittenH = Rewrite.applyRuleUntilCannot(h, CopyRules.addIdForMapWrgParam)

    assertTrue(CopyRules.addIdForMapWrgParam.rewrite.isDefinedAt(f.body))
    assertTrue(CopyRules.addIdForMapWrgParam.rewrite.isDefinedAt(g.body))
    assertTrue(rewrittenH.body.contains({case FunCall(Id(), _) =>}))
  }
}
