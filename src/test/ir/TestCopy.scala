package ir

import ir.ast._
import opencl.ir.pattern._
import opencl.ir._
import lift.arithmetic.SizeVar
import opencl.executor.Eval
import org.junit.Test
import org.junit.Assert._
import rewriting.utils.DumpToFile

class TestCopy {

  @Test
  def testCopy(): Unit = {

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        MapWrg(fun( Arow =>
          Join() o  MapLcl(fun( Bcol =>
            toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
          )) $ B
        )) $ A
      })

    val lambdaParams = f.params

    val bodyParams = Expr.visitWithState(Set[Param]())(f.body, {
      case (p: Param, set) => set + p
      case (_, set) => set
    }).filter(!lambdaParams.contains(_))

    val bodyParamsMap = bodyParams.map({
      case oldValue: Value => (oldValue, oldValue.copy)
      case param => (param, Param())
    }).toMap

    val lambdaParamsMap = lambdaParams.map(oldParam => (oldParam: Expr, Param(oldParam.t))).toMap

    val allParamsMap: collection.Map[Expr, Expr] = lambdaParamsMap ++ bodyParamsMap

    val replaceFun: Expr => Expr = expr => {allParamsMap.getOrElse(expr, expr)}

    val l = FunDecl.replace(f, replaceFun)

    assertNotSame(f, l)
    assertEquals(TypeChecker(f), TypeChecker(l))
    assertFalse(Expr.visitWithState(false)(f.body, (x, y) => y || l.body.contains({ case e if e eq x => })))
  }

  @Test
  def testEval(): Unit = {

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        MapWrg(fun( Arow =>
          Join() o  MapLcl(fun( Bcol =>
            toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
          )) $ B
        )) $ A
      })

    val l = Eval(DumpToFile.dumpLambdaToString(f))

    assertNotSame(f, l)
//    assertEquals(TypeChecker(f), TypeChecker(l))
    assertFalse(Expr.visitWithState(false)(f.body, (x, y) => y || l.body.contains({ case e if e eq x => })))
  }

}
