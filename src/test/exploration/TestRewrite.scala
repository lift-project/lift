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

  def applyRule(lambda: Lambda, expr: Expr, rule: Rule) = {
    TypeChecker.check(lambda.body)
    val newLambda = FunDecl.replace(lambda, expr, rule.rewrite(expr))
    newLambda
  }

  val N = Var("N")
  val A = Array.fill[Float](128)(0.5f)

/*
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

    val mapMapTransposeRewrite = Rules.mapMapTransposeZipInside.rewrite
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
  def mmReuseBoth(): Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f: Lambda = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val expr = f match { case Lambda(_, FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), _))), _)) => call }
    val f1 = applyRule(f, expr, Rules.reorderBothSides)

    val expr1 = f1 match { case Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Scatter(_), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), FunCall(Gather(_), _))))), _)) => call }
    val f2 = applyRule(f1, expr1, Rules.mapFission)

    val expr2 = f2 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), FunCall(Gather(_), _)))), _))) => call }
    val f3 = applyRule(f2, expr2, Rules.splitJoin)

    val expr3 = f3 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), FunCall(Gather(_), _)))), _))), FunCall(Split(_), _))))) => call }
    val f4 = applyRule(f3, expr3, Rules.mapMapInterchange)

    val expr4 = f4 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), _))), FunCall(Gather(_), _))))), FunCall(Split(_), _))))) => call }
    val f5 = applyRule(f4, expr4, Rules.splitJoin)

    val expr5 = f5 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), _))), _))), FunCall(Split(_), FunCall(Gather(_), _))))))), FunCall(Split(_), _))))) => call }
    val f6 = applyRule(f5, expr5, Rules.mapMapInterchange)

    val expr6 = f6 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), call@FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), _))), _)))), FunCall(Split(_), FunCall(Gather(_), _))))))), FunCall(Split(_), _))))) => call }
    val f7 = applyRule(f6, expr6, Rules.mapFission)

    val expr7 = f7 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), _))), _))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f8 = applyRule(f7, expr7, Rules.mapFission)

    val expr8 = f8 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), _)))), _))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f9 = applyRule(f8, expr8, Rules.mapReduceInterchange)

    val expr9 = f9 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), _))))), _))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f10 = applyRule(f9, expr9, Rules.mapMapTransposeZipInside)

    val expr10 = f10 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), _, call@FunCall(Transpose(), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), _))), FunCall(Get(1), _)))), FunCall(Zip(2), _, FunCall(Transpose(), _)))))))), _))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f11 = applyRule(f10, expr10, Rules.transposeTransposeId)

    val expr11 = f11 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), _))), FunCall(Get(1), _)))), FunCall(Zip(2), _, FunCall(Transpose(), _)))))), _))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f12 = applyRule(f11, expr11, Rules.mapFission)

    val expr12 = f12 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), _, _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), _))), FunCall(Get(1), _)))), FunCall(Zip(2), _, FunCall(Transpose(), _))))), _)))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f13 = applyRule(f12, expr12, Rules.mapReduceInterchange)

    val expr13 = f13 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), _))), FunCall(Get(1), _)))), FunCall(Zip(2), _, FunCall(Transpose(), _))))), _))))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f14 = applyRule(f13, expr13, Rules.mapMapTransposeZipInside)

    val expr14 = f14 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, call@FunCall(Transpose(), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, _))), FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), FunCall(Transpose(), _)))))))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f15 = applyRule(f14, expr14, Rules.transposeTransposeId)

    val expr15 = f15 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, _))), FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), FunCall(Transpose(), _)))))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f16 = applyRule(f15, expr15, Rules.mapSeq)

    val expr16 = f16 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, call@FunCall(Map(_), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, _))), FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), FunCall(Transpose(), _)))))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f17 = applyRule(f16, expr16, Rules.mapSeq)

    val expr17 = f17 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, call @ FunCall(Reduce(_), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, _))), FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), FunCall(Transpose(), _)))))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f18 = applyRule(f17, expr17, Rules.reduceSeq)

    val expr18 = f18 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(toGlobal(_), FunCall(ReduceSeq(_), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _))), _))), _), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(_, _, _))), FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), FunCall(Transpose(), _))))))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f19 = applyRule(f18, expr18, Rules.mapSeq)

    val expr19 = f19 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(toGlobal(_), FunCall(ReduceSeq(_), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _))), _))), _), FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(MapSeq(_), FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), FunCall(Transpose(), _))))))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f20 = applyRule(f19, expr19, Rules.mapSeq)

    val expr20 = f20 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(toGlobal(_), FunCall(ReduceSeq(_), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _))), _))), _), call@FunCall(Map(Lambda(_, FunCall(MapSeq(_), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), FunCall(Transpose(), _))))))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f21 = applyRule(f20, expr20, Rules.mapSeq)

    val expr21 = f21 match { case Lambda(_, FunCall(Map(Lambda(_, FunCall(Scatter(_), _))), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), _))), FunCall(Map(Lambda(_, FunCall(toGlobal(_), call@FunCall(ReduceSeq(_), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _))), _))), _), FunCall(MapSeq(_), FunCall(Zip(2), FunCall(Transpose(), _), FunCall(Transpose(), _))))))), FunCall(Split(_), FunCall(Gather(_), _)))))))), FunCall(Split(_), _))))) => call }
    val f22 = applyRule(f21, expr21, Rules.mapReduceFusion)

    println(f22)
  }

  @Test
  def mmSquareTiles() = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f0: Lambda = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val expr0 = f0 match { case Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), _))), _)) => call }
    val f1 = applyRule(f0, expr0, Rules.splitJoin)

    val expr1 = f1 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), _))), _))), FunCall(Split(_), _)))) => call }
    val f2 = applyRule(f1, expr1, Rules.mapMapInterchange)

    val expr2 = f2 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), _))), _)))), FunCall(Split(_), _)))) => call }
    val f3 = applyRule(f2, expr2, Rules.splitJoin)

    val expr3 = f3 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, call@FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), _))), _))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f4 = applyRule(f3, expr3, Rules.partialReduce)

    val expr4 = f4 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, call@FunCall(PartRed(Lambda(_, FunCall(_, _, _))), _, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))))), _))), _))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f5 = applyRule(f4, expr4, Rules.partialReduceSplitJoin)

    val expr5 = f5 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Join(), FunCall(Map(Lambda(_, call@FunCall(PartRed(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))))))), _))), _))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f6 = applyRule(f5, expr5, Rules.partialReduceToReduce)

    val expr6 = f6 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))))))), _))), _))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f7 = applyRule(f6, expr6, Rules.mapFission)

    val expr7 = f7 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))))), _)))), _))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f8 = applyRule(f7, expr7, Rules.mapReduceInterchange)

//    val f8: Lambda = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p317986356, p331510866) => FunCall(Join(), FunCall(Map(fun((p640363654) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p924477420) => FunCall(Map(fun((p99451533) => FunCall(Reduce(fun((p84739718, p2050835901) => FunCall(Map(fun((p511473681) => FunCall(add, FunCall(Get(0), p511473681), FunCall(Get(1), p511473681)))), FunCall(Zip(2), p84739718, p2050835901)))), Value(0.0f, ArrayType(Float, 4)), FunCall(Transpose(), FunCall(Map(fun((p2011986105) => FunCall(Join(), FunCall(Map(fun((p439904756) => FunCall(Reduce(fun((p171497379, p2012846597) => FunCall(add, p171497379, p2012846597))), Value(0.0f, Float), p439904756))), FunCall(Split(4), FunCall(Map(fun((p1665404403) => FunCall(mult, FunCall(Get(0), p1665404403), FunCall(Get(1), p1665404403)))), FunCall(Zip(2), p2011986105, p99451533))))))), p640363654))))), p924477420))), FunCall(Split(4), p331510866)))))), FunCall(Split(4), p317986356))))

    val expr8 = f8 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))))), _))))), _))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f9 = applyRule(f8, expr8, Rules.mapFission)

    val expr9 = f9 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), _, _))), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))))), _)))), _)))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f10 = applyRule(f9, expr9, Rules.mapReduceInterchange)

    // Next 5 Could be done in 1 step if could specify split position, or using a macro rule
    val expr10 = f10 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))))), _)))), _))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f11 = applyRule(f10, expr10, Rules.mapFission)

    val expr11 = f11 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), _))), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))))), _))))), _))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f12 = applyRule(f11, expr11, Rules.mapFission)

    val expr12 = f12 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _))), call@FunCall(Map(Lambda(_, FunCall(Split(_), FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), _)))))), _))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f13 = applyRule(f12, expr12, Rules.mapFission)

    val expr13 = f13 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Join(), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _))), FunCall(Map(Lambda(_, FunCall(Split(_), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), _))))))), _))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f14 = applyRule(f13, expr13, Rules.mapFusion)

    val expr14 = f14 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), FunCall(Map(Lambda(_, FunCall(Split(_), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), _)))))), _))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f15 = applyRule(f14, expr14, Rules.mapFusion)

//    // end of what could be done in 1

    val expr15 = f15 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), _))))), _))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f16 = applyRule(f15, expr15, Rules.mapMapTransposeZipInside)

//    val f16:Lambda = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p209833425, p532854629) => FunCall(Join(), FunCall(Map(fun((p1971851377) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p712025048) => FunCall(Reduce(fun((p681384962, p586084331) => FunCall(Map(fun((p399534175) => FunCall(Map(fun((p949057310) => FunCall(add, FunCall(Get(0), p949057310), FunCall(Get(1), p949057310)))), FunCall(Zip(2), FunCall(Get(0), p399534175), FunCall(Get(1), p399534175))))), FunCall(Zip(2), p681384962, p586084331)))), Value(0.0f, ArrayType(ArrayType(Float, 4), 4)), FunCall(Transpose(), FunCall(Map(fun((p2024542466) => FunCall(Transpose(), FunCall(Map(fun((p770189387) => FunCall(Join(), FunCall(Map(fun((p963522361) => FunCall(Reduce(fun((p175408781, p315138752) => FunCall(add, p175408781, p315138752))), Value(0.0f, Float), p963522361))), FunCall(Split(4), p770189387))))), FunCall(Transpose(), FunCall(Map(fun((p2114874018) => FunCall(Map(fun((p911312317) => FunCall(mult, p911312317, FunCall(Get(1), p2114874018)))), FunCall(Get(0), p2114874018)))), FunCall(Zip(2), FunCall(Transpose(), p1971851377), p2024542466))))))), p712025048))))), FunCall(Split(4), p532854629)))))), FunCall(Split(4), p209833425))))

    // again, could be done in 1

    val expr16 = f16 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), _))))))), _))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f17 = applyRule(f16, expr16, Rules.mapFission)

    val expr17 = f17 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), _)))))), _)))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f18 = applyRule(f17, expr17, Rules.mapFission)

    val expr18 = f18 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), _))), call@FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), _))))), _))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f19 = applyRule(f18, expr18, Rules.mapFission)

    val expr19 = f19 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), _))), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), _)))), _)))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f20 = applyRule(f19, expr19, Rules.mapFusion)

    val expr20 = f20 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), _)))), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), _)))), _))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f21 = applyRule(f20, expr20, Rules.mapFusion)

    // end could be done in 1
    val expr21 = f21 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), FunCall(Transpose(), _))))), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Transpose(), _), _)))), _)))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f22 = applyRule(f21, expr21, Rules.mapMapTransposeZipInside)

//    val f22 : Lambda = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p209833425, p532854629) => FunCall(Join(), FunCall(Map(fun((p1971851377) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p712025048) => FunCall(Reduce(fun((p681384962, p586084331) => FunCall(Map(fun((p399534175) => FunCall(Map(fun((p949057310) => FunCall(add, FunCall(Get(0), p949057310), FunCall(Get(1), p949057310)))), FunCall(Zip(2), FunCall(Get(0), p399534175), FunCall(Get(1), p399534175))))), FunCall(Zip(2), p681384962, p586084331)))), Value(0.0f, ArrayType(ArrayType(Float, 4), 4)), FunCall(Transpose(), FunCall(Map(fun((p2024542466) => FunCall(Transpose(), FunCall(Map(fun((p770189387) => FunCall(Join(), FunCall(Map(fun((p963522361) => FunCall(Reduce(fun((p175408781, p315138752) => FunCall(add, p175408781, p315138752))), Value(0.0f, Float), p963522361))), FunCall(Split(4), p770189387))))), FunCall(Transpose(), p2024542466))))), FunCall(Transpose(), FunCall(Map(fun((p2114874018) => FunCall(Map(fun((p911312317) => FunCall(Map(fun((p415186196) => FunCall(mult, p415186196, p911312317))), FunCall(Get(0), p2114874018)))), FunCall(Get(1), p2114874018)))), FunCall(Zip(2), FunCall(Transpose(), p1971851377), FunCall(Transpose(), p712025048))))))))), FunCall(Split(4), p532854629)))))), FunCall(Split(4), p209833425))))

    val expr22 = f22 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), FunCall(Transpose(), _))))), FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, _))), FunCall(Get(0), _)))), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Transpose(), _), FunCall(Transpose(), _))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f23 = applyRule(f22, expr22, Rules.splitJoin)

    val expr23 = f23 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), FunCall(Transpose(), _))))), FunCall(Transpose(), FunCall(Join(), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, _))), FunCall(Get(0), _)))), FunCall(Get(1), _)))), _))), FunCall(Split(_), FunCall(Zip(2), FunCall(Transpose(), _), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f24 = applyRule(f23, expr23, Rules.splitZip)

    val expr24 = f24 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), FunCall(Transpose(), _))))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, _))), FunCall(Get(0), _)))), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f25 = applyRule(f24, expr24, Rules.mapMapTransposeZipOutside)

    val expr25 = f25 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), FunCall(Transpose(), _))))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _, FunCall(Get(1), _)))), FunCall(Get(0), _)))), FunCall(Zip(2), FunCall(Get(0), _), _)))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f26 = applyRule(f25, expr25, Rules.mapMapTransposeZipOutside)

    // again could be done in 1

    val expr26 = f26 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _))))), FunCall(Transpose(), _))))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f27 = applyRule(f26, expr26, Rules.mapFission)

    val expr27 = f27 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), _))), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Split(_), _)))), FunCall(Transpose(), _)))))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f28 = applyRule(f27, expr27, Rules.mapFission)

    val expr28 = f28 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Join(), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _))), FunCall(Map(Lambda(_, FunCall(Split(_), _))), FunCall(Transpose(), _))))))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f29 = applyRule(f28, expr28, Rules.mapFusion)

    // end could be done in 1

//    val f29 : Lambda = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p334203599, p1372082959) => FunCall(Join(), FunCall(Map(fun((p1946403944) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1131645570) => FunCall(Reduce(fun((p209833425, p532854629) => FunCall(Map(fun((p1971851377) => FunCall(Map(fun((p712025048) => FunCall(add, FunCall(Get(0), p712025048), FunCall(Get(1), p712025048)))), FunCall(Zip(2), FunCall(Get(0), p1971851377), FunCall(Get(1), p1971851377))))), FunCall(Zip(2), p209833425, p532854629)))), Value(0.0f, ArrayType(ArrayType(Float, 4), 4)), FunCall(Transpose(), FunCall(Map(fun((p681384962) => FunCall(Transpose(), FunCall(Map(fun((p586084331) => FunCall(Join(), FunCall(Map(fun((p399534175) => FunCall(Reduce(fun((p949057310, p2024542466) => FunCall(add, p949057310, p2024542466))), Value(0.0f, Float), p399534175))), p586084331)))), FunCall(Map(fun((p770189387) => FunCall(Split(4), p770189387))), FunCall(Transpose(), p681384962)))))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(fun((p963522361) => FunCall(Transpose(), FunCall(Map(fun((p175408781) => FunCall(Transpose(), FunCall(Map(fun((p315138752) => FunCall(Map(fun((p2114874018) => FunCall(mult, FunCall(Get(0), p2114874018), FunCall(Get(1), p2114874018)))), FunCall(Zip(2), p315138752, p175408781)))), FunCall(Transpose(), FunCall(Get(0), p963522361)))))), FunCall(Transpose(), FunCall(Get(1), p963522361)))))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p1946403944)), FunCall(Split(4), FunCall(Transpose(), p1131645570))))))))))), FunCall(Split(4), p1372082959)))))), FunCall(Split(4), p334203599))))

    val expr29 = f29 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), call@FunCall(Map(Lambda(_, FunCall(Split(_), _))), FunCall(Transpose(), _)))))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f30 = applyRule(f29, expr29, Rules.mapSplitTranspose)

    val expr30 = f30 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Split(_), _))))))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f31 = applyRule(f30, expr30, Rules.transposeBothSides)

    val expr31 = f31 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, call@FunCall(Transpose(), FunCall(TransposeW(), FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), FunCall(Transpose(), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Split(_), _))))))))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f32 = applyRule(f31, expr31, Rules.transposeTransposeId)

    val expr32 = f32 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), call@FunCall(Transpose(), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Split(_), _))))))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f33 = applyRule(f32, expr32, Rules.transposeTransposeId)

    val expr33 = f33 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Split(_), _))))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f34 = applyRule(f33, expr33, Rules.mapFission)

//    val f34 : Lambda = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p99451533, p84739718) => FunCall(Join(), FunCall(Map(fun((p2050835901) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p511473681) => FunCall(Reduce(fun((p2011986105, p439904756) => FunCall(Map(fun((p171497379) => FunCall(Map(fun((p2012846597) => FunCall(add, FunCall(Get(0), p2012846597), FunCall(Get(1), p2012846597)))), FunCall(Zip(2), FunCall(Get(0), p171497379), FunCall(Get(1), p171497379))))), FunCall(Zip(2), p2011986105, p439904756)))), Value(0.0f, ArrayType(ArrayType(Float, 4), 4)), FunCall(Transpose(), FunCall(Map(fun((p1665404403) => FunCall(Map(fun((p988458918) => FunCall(Join(), FunCall(Map(fun((p1990451863) => FunCall(Reduce(fun((p1295083508, p249155636) => FunCall(add, p1295083508, p249155636))), Value(0.0f, Float), p1990451863))), p988458918)))), p1665404403))), FunCall(Map(fun((p1629604310) => FunCall(Map(fun((p142555199) => FunCall(Transpose(), p142555199))), FunCall(Split(4), p1629604310)))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(fun((p1320677379) => FunCall(Transpose(), FunCall(Map(fun((p246399377) => FunCall(Transpose(), FunCall(Map(fun((p1630521067) => FunCall(Map(fun((p274773041) => FunCall(mult, FunCall(Get(0), p274773041), FunCall(Get(1), p274773041)))), FunCall(Zip(2), p1630521067, p246399377)))), FunCall(Transpose(), FunCall(Get(0), p1320677379)))))), FunCall(Transpose(), FunCall(Get(1), p1320677379)))))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p2050835901)), FunCall(Split(4), FunCall(Transpose(), p511473681)))))))))))), FunCall(Split(4), p84739718)))))), FunCall(Split(4), p99451533))))

    val expr34 = f34 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), _))), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Split(_), _)))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _)))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f35 = applyRule(f34, expr34, Rules.mapFission)

    val expr35 = f35 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _))), call@FunCall(Map(Lambda(_, FunCall(Split(_), _))), FunCall(Transpose(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f36 = applyRule(f35, expr35, Rules.mapSplitTranspose)

    val expr36 = f36 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _))), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), call@FunCall(Split(_), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _)))))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f37 = applyRule(f36, expr36, Rules.splitJoinId)

    val expr37 = f37 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _))), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _)))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f38 = applyRule(f37, expr37, Rules.mapFusion)

//    val f38 : Lambda = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p99451533, p84739718) => FunCall(Join(), FunCall(Map(fun((p2050835901) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p511473681) => FunCall(Reduce(fun((p2011986105, p439904756) => FunCall(Map(fun((p171497379) => FunCall(Map(fun((p2012846597) => FunCall(add, FunCall(Get(0), p2012846597), FunCall(Get(1), p2012846597)))), FunCall(Zip(2), FunCall(Get(0), p171497379), FunCall(Get(1), p171497379))))), FunCall(Zip(2), p2011986105, p439904756)))), Value(0.0f, ArrayType(ArrayType(Float, 4), 4)), FunCall(Transpose(), FunCall(Map(fun((p1665404403) => FunCall(Map(fun((p988458918) => FunCall(Join(), FunCall(Map(fun((p1990451863) => FunCall(Reduce(fun((p1295083508, p249155636) => FunCall(add, p1295083508, p249155636))), Value(0.0f, Float), p1990451863))), p988458918)))), FunCall(Map(fun((p1629604310) => FunCall(Transpose(), p1629604310))), p1665404403)))), FunCall(Transpose(), FunCall(Map(fun((p142555199) => FunCall(Transpose(), p142555199))), FunCall(Map(fun((p1320677379) => FunCall(Transpose(), FunCall(Map(fun((p246399377) => FunCall(Transpose(), FunCall(Map(fun((p1630521067) => FunCall(Map(fun((p274773041) => FunCall(mult, FunCall(Get(0), p274773041), FunCall(Get(1), p274773041)))), FunCall(Zip(2), p1630521067, p246399377)))), FunCall(Transpose(), FunCall(Get(0), p1320677379)))))), FunCall(Transpose(), FunCall(Get(1), p1320677379)))))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p2050835901)), FunCall(Split(4), FunCall(Transpose(), p511473681))))))))))), FunCall(Split(4), p84739718)))))), FunCall(Split(4), p99451533))))

    val expr38 = f38 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Transpose(), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _)))), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f39 = applyRule(f38, expr38, Rules.transposeBothSides)

    val expr39 = f39 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, call@FunCall(Transpose(), FunCall(TransposeW(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _)))), FunCall(Transpose(), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f40 = applyRule(f39, expr39, Rules.transposeTransposeId)

    val expr40 = f40 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _)))), call@FunCall(Transpose(), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f41 = applyRule(f40, expr40, Rules.transposeTransposeId)

    val expr41 = f41 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _)))), FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f42 = applyRule(f41, expr41, Rules.mapFission)

//    val f42 : Lambda = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p99451533, p84739718) => FunCall(Join(), FunCall(Map(fun((p2050835901) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p511473681) => FunCall(Reduce(fun((p2011986105, p439904756) => FunCall(Map(fun((p171497379) => FunCall(Map(fun((p2012846597) => FunCall(add, FunCall(Get(0), p2012846597), FunCall(Get(1), p2012846597)))), FunCall(Zip(2), FunCall(Get(0), p171497379), FunCall(Get(1), p171497379))))), FunCall(Zip(2), p2011986105, p439904756)))), Value(0.0f, ArrayType(ArrayType(Float, 4), 4)), FunCall(Map(fun((p1665404403) => FunCall(Map(fun((p988458918) => FunCall(Join(), FunCall(Map(fun((p1990451863) => FunCall(Reduce(fun((p1295083508, p249155636) => FunCall(add, p1295083508, p249155636))), Value(0.0f, Float), p1990451863))), p988458918)))), p1665404403))), FunCall(Map(fun((p1629604310) => FunCall(Map(fun((p142555199) => FunCall(Transpose(), p142555199))), p1629604310))), FunCall(Map(fun((p1320677379) => FunCall(Transpose(), p1320677379))), FunCall(Map(fun((p246399377) => FunCall(Transpose(), FunCall(Map(fun((p1630521067) => FunCall(Transpose(), FunCall(Map(fun((p274773041) => FunCall(Map(fun((p1629911510) => FunCall(mult, FunCall(Get(0), p1629911510), FunCall(Get(1), p1629911510)))), FunCall(Zip(2), p274773041, p1630521067)))), FunCall(Transpose(), FunCall(Get(0), p246399377)))))), FunCall(Transpose(), FunCall(Get(1), p246399377)))))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p2050835901)), FunCall(Split(4), FunCall(Transpose(), p511473681)))))))))), FunCall(Split(4), p84739718)))))), FunCall(Split(4), p99451533))))

    val expr42 = f42 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _))), call@FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _)))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f43 = applyRule(f42, expr42, Rules.mapFusion)

    val expr43 = f43 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _))), FunCall(Map(Lambda(_, call@FunCall(Transpose(), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _))))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f44 = applyRule(f43, expr43, Rules.transposeTransposeId)

    val expr45 = f44 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), _))), call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), _))), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f45 = applyRule(f44, expr45, Rules.mapFusion)

    val expr46 = f45 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), _))), FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Transpose(), _))), FunCall(Map(Lambda(_, FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _)))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f46 = applyRule(f45, expr46, Rules.mapFusion)

//    val f46 : Lambda = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p1682463303, p633075331) => FunCall(Join(), FunCall(Map(fun((p1858609436) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1920387277) => FunCall(Reduce(fun((p1414147750, p775931202) => FunCall(Map(fun((p22069592) => FunCall(Map(fun((p1160003871) => FunCall(add, FunCall(Get(0), p1160003871), FunCall(Get(1), p1160003871)))), FunCall(Zip(2), FunCall(Get(0), p22069592), FunCall(Get(1), p22069592))))), FunCall(Zip(2), p1414147750, p775931202)))), Value(0.0f, ArrayType(ArrayType(Float, 4), 4)), FunCall(Map(fun((p1075738627) => FunCall(Map(fun((p282828951) => FunCall(Join(), FunCall(Map(fun((p394721749) => FunCall(Reduce(fun((p1884122755, p1134612201) => FunCall(add, p1884122755, p1134612201))), Value(0.0f, Float), p394721749))), p282828951)))), p1075738627))), FunCall(Map(fun((p246550802) => FunCall(Map(fun((p786041152) => FunCall(Transpose(), FunCall(Transpose(), FunCall(Map(fun((p897074030) => FunCall(Map(fun((p1885996206) => FunCall(mult, FunCall(Get(0), p1885996206), FunCall(Get(1), p1885996206)))), FunCall(Zip(2), p897074030, p786041152)))), FunCall(Transpose(), FunCall(Get(0), p246550802))))))), FunCall(Transpose(), FunCall(Get(1), p246550802))))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p1858609436)), FunCall(Split(4), FunCall(Transpose(), p1920387277)))))))), FunCall(Split(4), p633075331)))))), FunCall(Split(4), p1682463303))))

    val expr47 = f46 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, call@FunCall(Transpose(), FunCall(Transpose(), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _))))))), FunCall(Transpose(), FunCall(Get(1), _))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _)))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f47 = applyRule(f46, expr47, Rules.transposeTransposeId)

    val expr48 = f47 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _))))), FunCall(Transpose(), FunCall(Get(1), _))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _)))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f48 = applyRule(f47, expr48, Rules.mapFusion)

    val expr49 = f48 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, call@FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), _)))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _))))), FunCall(Transpose(), FunCall(Get(1), _)))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f49 = applyRule(f48, expr49, Rules.mapFusion)

    val expr50 = f49 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), call@FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, _))), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _)))), FunCall(Transpose(), FunCall(Get(0), _))))))), FunCall(Transpose(), FunCall(Get(1), _))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f50 = applyRule(f49, expr50, Rules.mapFusion)

    val expr51 = f50 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, call@FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), _, _))))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f51 = applyRule(f50, expr51, Rules.mapSeq)

//    val f51 : Lambda = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p633075331, p1858609436) => FunCall(Join(), FunCall(Map(fun((p1920387277) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1414147750) => FunCall(Reduce(fun((p775931202, p22069592) => FunCall(Map(fun((p1160003871) => FunCall(Map(fun((p1075738627) => FunCall(add, FunCall(Get(0), p1075738627), FunCall(Get(1), p1075738627)))), FunCall(Zip(2), FunCall(Get(0), p1160003871), FunCall(Get(1), p1160003871))))), FunCall(Zip(2), p775931202, p22069592)))), Value(0.0f, ArrayType(ArrayType(Float, 4), 4)), FunCall(Map(fun((p282828951) => FunCall(Map(fun((p394721749) => FunCall(Join(), FunCall(Map(fun((p1884122755) => FunCall(Reduce(fun((p1134612201, p246550802) => FunCall(add, p1134612201, p246550802))), Value(0.0f, Float), FunCall(MapSeq(fun((p399534175) => FunCall(mult, FunCall(Get(0), p399534175), FunCall(Get(1), p399534175)))), FunCall(Zip(2), p1884122755, p394721749))))), FunCall(Transpose(), FunCall(Get(0), p282828951)))))), FunCall(Transpose(), FunCall(Get(1), p282828951))))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p1920387277)), FunCall(Split(4), FunCall(Transpose(), p1414147750))))))), FunCall(Split(4), p1858609436)))))), FunCall(Split(4), p633075331))))

    val expr52 = f51 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, call@FunCall(Reduce(Lambda(_, FunCall(_, _, _))), _, FunCall(MapSeq(_), FunCall(Zip(2), _, _))))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f52 = applyRule(f51, expr52, Rules.reduceSeq)

    val expr53 = f52 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(toGlobal(_), call@FunCall(ReduceSeq(_), FunCall(_, _), FunCall(MapSeq(_), FunCall(Zip(2), _, _)))))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f53 = applyRule(f52, expr53, Rules.mapReduceFusion)

    val expr54 = f53 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, call@FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(toGlobal(_), FunCall(ReduceSeq(_), FunCall(_, _), FunCall(Zip(2), _, _))))), FunCall(Transpose(), FunCall(Get(0), _)))))), FunCall(Transpose(), FunCall(Get(1), _))))), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f54 = applyRule(f53, expr54, Rules.mapSeq)

    val expr55 = f54 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, call@FunCall(Reduce(Lambda(_, FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, FunCall(Get(0), _), FunCall(Get(1), _)))), FunCall(Zip(2), FunCall(Get(0), _), FunCall(Get(1), _))))), FunCall(Zip(2), _, _)))), _, FunCall(MapSeq(_), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f55 = applyRule(f54, expr55, Rules.reduceSeq)

    val expr56 = f55 match { case Lambda(_, FunCall(Join(), FunCall(Map(Lambda(_, FunCall(TransposeW(), FunCall(Join(), FunCall(Map(Lambda(_, FunCall(toGlobal(_), call@FunCall(ReduceSeq(_), FunCall(Map(Lambda(_, FunCall(Map(Lambda(_, FunCall(_, _))), _))), _), FunCall(MapSeq(_), FunCall(Zip(2), FunCall(Split(_), FunCall(Transpose(), _)), FunCall(Split(_), FunCall(Transpose(), _)))))))), FunCall(Split(_), _)))))), FunCall(Split(_), _)))) => call }
    val f56 = applyRule(f55, expr56, Rules.mapReduceFusion)

    println(f56)
}
*/

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
