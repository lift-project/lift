package ir.ast

import opencl.ir.pattern._
import org.junit._
import org.junit.Assert._
import opencl.ir._

class TestExpr {

  @Test
  def testReplaceMap(): Unit = {
    val lambda = fun(x => Map(id) $ x)
    val result = Expr.replace(lambda.body, lambda.body, lambda.body)
    assertSame(lambda.body, result)
  }

  @Test
  def testReplaceFunCall(): Unit = {
    val lambda: Lambda = fun(x => Map(id) $ x)

    val userFunCall = lambda match {
      case Lambda(_, FunCall(Map(l), _*)) => l.body
    }

    val replacementFunCall = FunCall(plusOne, Param())

    val result = Expr.replace(lambda.body, userFunCall, replacementFunCall)

    val newFunCall = result match {
      case FunCall(Map(l), _*) => l.body
    }

    assertNotSame(lambda.body, result)
    assertSame(replacementFunCall, newFunCall)
  }

  @Test
  def testReplaceFunCallInMapGlb(): Unit = {
    val lambda: Lambda = fun(x => MapGlb(1)(id) $ x)

    val userFunCall = lambda match {
      case Lambda(_, FunCall(MapGlb(_, l), _*)) => l.body
    }

    val replacementFunCall = FunCall(plusOne, Param())

    val result = Expr.replace(lambda.body, userFunCall, replacementFunCall)

    val (newDim, newFunCall) = result match {
      case FunCall(MapGlb(dim, l), _*) =>
        (dim, l.body)
    }

    assertNotSame(lambda.body, result)
    assertSame(replacementFunCall, newFunCall)
    assertEquals(1, newDim)
  }

  @Test
  def testReplaceFunCallInLambda(): Unit = {
    val lambda: Lambda = fun(x => Reduce(add, 0.0f) $ x)

    val userFunCall = lambda match {
      case Lambda(_, FunCall(Lambda(_, FunCall(Reduce(l), _*)), _*)) => l.body
    }

    val replacementFunCall = FunCall(mult, Param(), Param())

    val result = Expr.replace(lambda.body, userFunCall, replacementFunCall)

    val newFunCall = result match {
      case FunCall(Lambda(_, FunCall(Reduce(l), _*)), _*) => l.body
    }

    assertNotSame(lambda.body, result)
    assertSame(replacementFunCall, newFunCall)
  }

  @Test
  def replaceArg(): Unit = {
    val lambda: Lambda = fun(x => MapGlb(id) $ x)

    val arg = lambda match {
      case Lambda(_, FunCall(_, a)) => a
    }

    val replacementArg = Param()

    val result = Expr.replace(lambda.body, arg, replacementArg)

    val newArg = result match {
      case FunCall(_, a) => a
    }

    assertNotSame(lambda.body, result)
    assertSame(replacementArg, newArg)
  }

  @Test
  def replaceInCompFun(): Unit = {
    val lambda: Lambda = fun(x => MapSeq(id) o MapSeq(id) $ x)

    val (nestedLambda, call) = lambda match {
      case Lambda(_, FunCall(CompFun(_, other, Lambda(_, c)), _)) => (other, c)
    }

    val replacementFunCall = FunCall(Map(plusOne), Param())

    val result = Expr.replace(lambda.body, call, replacementFunCall)

    val (newNestedLambda, newCall) = result match {
      case FunCall(CompFun(_, other, Lambda(_, c)), _) => (other, c)
    }

    assertNotSame(lambda.body, result)
    assertSame(replacementFunCall, newCall)
    assertSame(nestedLambda, newNestedLambda)
  }

  @Test
  def replaceSameInCompFun(): Unit = {
    val lambda: Lambda = fun(x => MapSeq(id) o MapSeq(id) $ x)

    val call = lambda match {
      case Lambda(_, FunCall(CompFun(_, _, Lambda(_, c)), _)) => c
    }

    val result = Expr.replace(lambda.body, call, call)

    assertSame(lambda.body, result)
  }

  @Test
  def replaceArgWithSame(): Unit = {
    val lambda: Lambda = fun(x => MapGlb(id) $ x)

    val arg = lambda match {
      case Lambda(_, FunCall(_, a)) => a
    }

    val result = Expr.replace(lambda.body, arg, arg)
    assertSame(lambda.body, result)
  }

  @Test
  def testReplaceMapGlb(): Unit = {
    val lambda = fun(x => MapGlb(id) $ x)
    val result = Expr.replace(lambda.body, lambda.body, lambda.body)
    assertSame(lambda.body, result)
  }

  @Test
  def testReplaceMapSeq(): Unit = {
    val lambda = fun(x => MapSeq(id) $ x)
    val result = Expr.replace(lambda.body, lambda.body, lambda.body)
    assertSame(lambda.body, result)
  }

  @Test
  def testReplaceReduce(): Unit = {
    val lambda = fun(x => Reduce(add, 0.0f) $ x)
    val result = Expr.replace(lambda.body, lambda.body, lambda.body)
    assertSame(lambda.body, result)
    val lambdaSeq = fun(x => ReduceSeq(add, 0.0f) $ x)
    val resultSeq = Expr.replace(lambdaSeq.body, lambdaSeq.body, lambdaSeq.body)
    assertSame(lambdaSeq.body, resultSeq)
  }

}
