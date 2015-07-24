package ir.ast

import opencl.ir._
import opencl.ir.pattern.MapSeq
import org.junit.Assert._
import org.junit.Test

class TestFunDecl {

  @Test
  def testReplaceMapWithSame(): Unit = {
    val lambda: Lambda = fun(x => Map(id) $ x)
    val result = FunDecl.replace(lambda, Seq(lambda), Seq(lambda))
    assertSame(lambda, result)
  }

  @Test
  def testReplaceMap(): Unit = {
    val lambda: Lambda = fun(x => Map(id) $ x)
    val replacementLambda = fun(x => Map(plusOne) $ x)
    val result = FunDecl.replace(lambda, Seq(lambda), Seq(replacementLambda))
    assertSame(replacementLambda, result)
  }

  @Test
  def testReplaceMapWithEmpty(): Unit = {
    val lambda: Lambda = fun(x => Map(id) $ x)
    val result = FunDecl.replace(lambda, Seq(lambda), Seq())

    assertNotSame(lambda, result)
    assertTrue(result.body.asInstanceOf[FunCall].f.isInstanceOf[Epsilon])
  }

  @Test
  def testReplaceMapWithSplitJoin(): Unit = {
    val lambda: Lambda = fun(x => Map(id) $ x)

    val join = Lambda.FunDefToLambda(Join())
    val split = Lambda.FunDefToLambda(Split(2))

    val result = FunDecl.replace(lambda, Seq(lambda), Seq(join, lambda, split))

    assertNotSame(lambda, result)
  }

  @Test
  def testReplaceInFunCall(): Unit = {
    val lambda: Lambda = fun(x => Map(id) $ x)

    val userFunCall = lambda match {
      case Lambda(_, FunCall(Map(l), _*)) => l
    }

    val replacementFunCall = Lambda(Array(Param()), FunCall(plusOne, Param()))

    val result = FunDecl.replace(lambda, Seq(userFunCall), Seq(replacementFunCall))

    val newFunCall = result match {
      case Lambda(_, FunCall(Map(l), _*)) => l
    }

    assertNotSame(lambda, result)
    assertSame(replacementFunCall, newFunCall)
  }

  @Test
  def testReplaceInFunCallWithEmpty(): Unit = {
    val lambda: Lambda = fun(x => Map(id) $ x)

    val userFunCall = lambda match {
      case Lambda(_, FunCall(Map(l), _*)) => l
    }

    val result = FunDecl.replace(lambda, Seq(userFunCall), Seq())

    result match {
      case Lambda(_, FunCall(Map(Lambda(_, FunCall(Epsilon(), _*))), _*)) =>
    }

    assertNotSame(lambda, result)
  }

  @Test
  def testReplaceNestedInFunCall(): Unit = {
    val lambda: Lambda = fun(x => Map(Map(id)) $ x)

    val userFunCall = lambda match {
      case Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(l), _))), _*)) => l
    }

    val replacementFunCall = Lambda(Array(Param()), FunCall(plusOne, Param()))

    val result = FunDecl.replace(lambda, Seq(userFunCall), Seq(replacementFunCall))

    val newFunCall = result match {
      case Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(l), _))), _*)) => l
    }

    assertNotSame(lambda, result)
    assertSame(replacementFunCall, newFunCall)
  }

  @Test
  def testNoReplaceNestedInFunCall(): Unit = {
    val lambda: Lambda = fun(x => Map(Map(id)) $ x)

    val userFunCall = lambda match {
      case Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(l), _))), _*)) => l
    }

    val result = FunDecl.replace(lambda, Seq(userFunCall), Seq(userFunCall))

    assertSame(lambda, result)
  }

  @Test
  def testNoReplaceInFunCall(): Unit = {
    val lambda: Lambda = fun(x => Map(id) $ x)

    val userFunCall = lambda match {
      case Lambda(_, FunCall(Map(l), _*)) => l
    }

    val result = FunDecl.replace(lambda, Seq(userFunCall), Seq(userFunCall))

    assertSame(lambda, result)
  }

  @Test
  def replaceInCompFun(): Unit = {
    val lambda: Lambda = fun(x => MapSeq(id) o MapSeq(id) $ x)

    val (nestedLambda, call) = lambda match {
      case Lambda(_, FunCall(CompFun(other, c), _)) => (other, c)
    }

    val replacementFunCall = Lambda(Array(Param()), FunCall(Map(plusOne), Param()))

    val result = FunDecl.replace(lambda, Seq(call), Seq(replacementFunCall))

    val (newNestedLambda, newCall) = result match {
      case Lambda(_, FunCall(CompFun(other,c), _)) => (other, c)
    }

    assertNotSame(lambda, result)
    assertSame(replacementFunCall, newCall)
    assertSame(nestedLambda, newNestedLambda)
  }

  @Test
  def replaceSameInCompFun(): Unit = {
    val lambda: Lambda = fun(x => MapSeq(id) o MapSeq(id) $ x)

    val call = lambda match {
      case Lambda(_, FunCall(CompFun(_, c), _)) => c
    }

    val result = FunDecl.replace(lambda, Seq(call), Seq(call))

    assertSame(lambda, result)
  }

  @Test
  def replaceBothInCompFun(): Unit = {
    val lambda: Lambda = fun(x => MapSeq(id) o MapSeq(id) $ x)

    val (nestedLambda, call) = lambda match {
      case Lambda(_, FunCall(CompFun(other, c), _)) => (other, c)
    }

    val replacementFunCall = Lambda(Array(Param()), FunCall(Map(plusOne), Param()))

    val result = FunDecl.replace(lambda, Seq(nestedLambda, call), Seq(replacementFunCall))

    assertNotSame(lambda, result)
    assertSame(replacementFunCall, result)
  }

  @Test
  def replaceBothWithNoneInCompFun(): Unit = {
    val lambda: Lambda = fun(x => MapSeq(id) o MapSeq(id) $ x)

    val (nestedLambda, call) = lambda match {
      case Lambda(_, FunCall(CompFun(other, c), _)) => (other, c)
    }

    val result = FunDecl.replace(lambda, Seq(nestedLambda, call), Seq())

    assertNotSame(lambda, result)
    assertTrue(result.body.asInstanceOf[FunCall].f.isInstanceOf[Epsilon])
  }

  @Test
  def replaceBothWithSameInCompFun(): Unit = {
    val lambda: Lambda = fun(x => MapSeq(id) o MapSeq(id) $ x)

    val (nestedLambda, call) = lambda match {
      case Lambda(_, FunCall(CompFun(other, c), _)) => (other, c)
    }

    val result = FunDecl.replace(lambda, Seq(nestedLambda, call), Seq(nestedLambda, call))

    assertSame(lambda, result)
  }

  @Test
  def replaceNestedInCompFun(): Unit = {
    val lambda: Lambda = fun(x => Map(id) o Map(Map(id) o Map(id)) $ x)

    val (notToBeReplaced1, notToBeReplaced2, toBeReplaced) = lambda match {
      case Lambda(_, FunCall(CompFun(other, Lambda(_, FunCall(Map(Lambda(_, FunCall(CompFun(one, two), _))), _))), _)) => (other, one, two)
    }

    val toReplace = Lambda(Array(Param()), FunCall(Map(plusOne), Param()))

    val result = FunDecl.replace(lambda, Seq(toBeReplaced), Seq(toReplace))

    val (newNotToBeReplaced1, newNotToBeReplaced2, newToBeReplaced) = lambda match {
      case Lambda(_, FunCall(CompFun(other, Lambda(_, FunCall(Map(Lambda(_, FunCall(CompFun(one, two), _))), _))), _)) => (other, one, two)
    }

    assertNotSame(lambda, result)
    assertSame(toBeReplaced, newToBeReplaced)
    assertSame(notToBeReplaced1, newNotToBeReplaced1)
    assertSame(notToBeReplaced2, newNotToBeReplaced2)
  }

  @Test
  def replaceNestedInCompFunWithSame(): Unit = {
    val lambda: Lambda = fun(x => Map(id) o Map(Map(id) o Map(id)) $ x)

    val notToBeReplaced = lambda match {
      case Lambda(_, FunCall(CompFun(other, Lambda(_, FunCall(Map(Lambda(_, FunCall(CompFun(one, two), _))), _))), _)) => two
    }

    val result = FunDecl.replace(lambda, Seq(notToBeReplaced), Seq(notToBeReplaced))

    assertSame(lambda, result)
  }
}
