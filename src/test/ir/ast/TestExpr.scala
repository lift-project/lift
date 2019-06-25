package ir.ast

import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._

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
      case Lambda(_, FunCall(Map(l), _*), _) => l.body
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
      case Lambda(_, FunCall(MapGlb(_, l), _*), _) => l.body
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
      case Lambda(_, FunCall(Reduce(l), _*), _) => l.body
    }

    val replacementFunCall = FunCall(mult, Param(), Param())

    val result = Expr.replace(lambda.body, userFunCall, replacementFunCall)

    val newFunCall = result match {
      case FunCall(Reduce(l), _*) => l.body
    }

    assertNotSame(lambda.body, result)
    assertSame(replacementFunCall, newFunCall)
  }

  @Test
  def replaceArg(): Unit = {
    val lambda: Lambda = fun(x => MapGlb(id) $ x)

    val arg = lambda match {
      case Lambda(_, FunCall(_, a), _) => a
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

    val (firstFun, call) = lambda match {
      case Lambda(_, FunCall(f, c), _) => (f, c)
    }

    val replacementFunCall = FunCall(Map(plusOne), Param())

    val result = Expr.replace(lambda.body, call, replacementFunCall)

    val (newFirstFun, newCall) = result match {
      case FunCall(f, c) => (f, c)
    }

    assertNotSame(lambda.body, result)
    assertSame(replacementFunCall, newCall)
    assertSame(firstFun, newFirstFun)
  }

  @Test
  def replaceSameInCompFun(): Unit = {
    val lambda: Lambda = fun(x => MapSeq(id) o MapSeq(id) $ x)

    val call = lambda match {
      case Lambda(_, FunCall(_, c), _) => c
    }

    val result = Expr.replace(lambda.body, call, call)

    assertSame(lambda.body, result)
  }

  @Test
  def replaceArgWithSame(): Unit = {
    val lambda: Lambda = fun(x => MapGlb(id) $ x)

    val arg = lambda match {
      case Lambda(_, FunCall(_, a), _) => a
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
