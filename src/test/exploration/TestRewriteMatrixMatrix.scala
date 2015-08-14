package exploration

import apart.arithmetic.Var
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestRewriteMatrixMatrix {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }
}

class TestRewriteMatrixMatrix {

  def applyRule(lambda: Lambda, expr: Expr, rule: Rule) = {
    TypeChecker.check(lambda.body)
    val newLambda = FunDecl.replace(lambda, expr, rule.rewrite(expr))
    newLambda
  }

  @Test
  def reuseWithTiling(): Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f0: Lambda = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, N), K),
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Unpack() o Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) o Transpose() $ B
        )) $ A
      })

    val tileSizeMN = 16
    val tileSizeK = 8
    val workPerThreadN = 4

    val f4 = Rewrite.applyRuleAtId(f0, 0, Rules.tileOutput(tileSizeMN))
  }

  @Test
  def tiledTransposeWithLocalMemory(): Unit = {
    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Map(Map(id)) o Transpose() $ input
    )

    val x = 8
    val y = 4

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.tileMapMap(x, y))
    val f2 = Rewrite.applyRuleAtId(f1, 1, Rules.mapFissionAtPosition(2))
    val f3 = Rewrite.applyRuleAtId(f2, 2, Rules.moveTransposeInsideTiling)

    // Add a copy
    val f4 = Rewrite.applyRuleAtId(f3, 17, Rules.addId)

    // Lower to OpenCL execution model
    val fw0 = Rewrite.applyRuleAtId(f4, 1, Rules.mapWrg(0))
    val fw1 = Rewrite.applyRuleAtId(fw0, 13, Rules.mapWrg(1))
    val fl1 = Rewrite.applyRuleAtId(fw1, 16, Rules.mapLcl(1))
    val fl0 = Rewrite.applyRuleAtId(fl1, 24, Rules.mapLcl(0))
    val fl01 = Rewrite.applyRuleAtId(fl0, 18, Rules.mapLcl(1))
    val fl00 = Rewrite.applyRuleAtId(fl01, 20, Rules.mapLcl(0))

    // Lower to OpenCL memory model
    val f11 = Rewrite.applyRuleAtId(fl00, 16, Rules.globalMemory)
    val f12 = Rewrite.applyRuleAtId(f11, 18, Rules.localMemory)

    val nSize = 12
    val mSize = 8
    val matrix = Array.tabulate(nSize, mSize)((r, c) => c * 1.0f + r * 8.0f)
    val gold = matrix.transpose

    val (output: Array[Float], _) =
      Execute(y, x, nSize, mSize, (false, false))(f12, matrix)
    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test
  def transposeBothSidesWithSplit(): Unit = {

    val tileSize = 4
    val N = Var("N")

    val f0 = fun( ArrayType(ArrayType(ArrayType(Float, tileSize), tileSize), N),
      input =>
        Transpose() o
          Map(
            Transpose() o
              Map(
                Join() o
                Map(ReduceSeq(add, 0.0f)) o
                Split(tileSize)
              ) o
              Transpose()
          ) o
          Transpose() $ input)

    val f1 = Rewrite.applyRuleAtId(f0, 4, Rules.transposeBothSidesWithSplit)
    val f2 = Rewrite.applyRuleAtId(f1, 0, Rules.transposeBothSidesWithSplit)

    println(f2)

    val h0 = fun( ArrayType(ArrayType(ArrayType(Float, tileSize), tileSize), N),
      input =>
        Transpose() o
          Map(
            Map(Join() o Map(ReduceSeq(add, 0.0f))) o
              Map(Transpose()) o
              Split(tileSize)) o
          Transpose() $ input)

    val h1 = Rewrite.applyRuleAtId(h0, 0, Rules.transposeBothSidesWithSplit)

    println(h1)
  }

  @Test
  def mmSquareTilesLocalMemory(): Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f0 = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p951880373, p1752203484) => FunCall(Join(), FunCall(Map(fun((p243745864) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p699780352) => FunCall(Unpack() o toGlobal(fun((p1613255205) => FunCall(MapSeq(fun((p1897115967) => FunCall(Map(fun((p1166151249) => FunCall(Map(fun((p1121453612) => FunCall(id, p1121453612))), p1166151249))), p1897115967))), p1613255205))), FunCall(ReduceSeq(fun((p2619171, p1728790703) => FunCall(Map(fun((p1227074340) => FunCall(Map(fun((p1154002927) => FunCall(add, FunCall(Get(0), p1154002927), FunCall(Get(1), p1154002927)))), FunCall(Zip(2), FunCall(Get(0), p1227074340), FunCall(Get(1), p1227074340))))), FunCall(Zip(2), p2619171, FunCall(fun((p2070529722) => FunCall(Map(fun((p1188753216) => FunCall(Join(), FunCall(Map(fun((p317986356) => FunCall(toPrivate(fun((p331510866) => FunCall(MapSeq(fun((p640363654) => FunCall(id, p640363654))), p331510866))), FunCall(ReduceSeq(fun((p924477420, p99451533) => FunCall(add, p924477420, FunCall(fun((p84739718) => FunCall(mult, FunCall(Get(0), p84739718), FunCall(Get(1), p84739718))), p99451533)))), FunCall(toPrivate(id), Value(0.0f, Float)), FunCall(Zip(2), p317986356, p1188753216))))), FunCall(Transpose(), FunCall(Get(0), p2070529722)))))), FunCall(Transpose(), FunCall(Get(1), p2070529722)))), p1728790703))))), FunCall(toPrivate(Map(fun((p2050835901) => FunCall(Map(fun((p511473681) => FunCall(id, p511473681))), p2050835901)))), Value(0.0f, ArrayType(ArrayType(Float, 4), 4))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p243745864)), FunCall(Split(4), FunCall(Transpose(), p699780352))))))), FunCall(Split(4), p1752203484)))))), FunCall(Split(4), p951880373))))

    // Add a copy
    val f1 = Rewrite.applyRuleAtId(f0, 28, Rules.addId)

    // Lower to OpenCL execution model
    val f2 = Rewrite.applyRuleAtId(f1, 1, Rules.mapWrg(0))
    val f3 = Rewrite.applyRuleAtId(f2, 6, Rules.mapWrg(1))
    val f4 = Rewrite.applyRuleAtId(f3, 94, Rules.mapLcl(1))
    val f5 = Rewrite.applyRuleAtId(f4, 96, Rules.mapLcl(0))
    val f6 = Rewrite.applyRuleAtId(f5, 26, Rules.mapLcl(1))
    val f7 = Rewrite.applyRuleAtId(f6, 78, Rules.mapLcl(0))
    val f8 = Rewrite.applyRuleAtId(f7, 46, Rules.mapLcl(1))
    val f9 = Rewrite.applyRuleAtId(f8, 51, Rules.mapLcl(0))
    val f10 = Rewrite.applyRuleAtId(f9, 39, Rules.mapLcl(1))
    val f11 = Rewrite.applyRuleAtId(f10, 42, Rules.mapLcl(0))
    val f12 = Rewrite.applyRuleAtId(f11, 32, Rules.mapLcl(1))
    val f13 = Rewrite.applyRuleAtId(f12, 35, Rules.mapLcl(0))
    val f14 = Rewrite.applyRuleAtId(f13, 20, Rules.mapLcl(1))
    val f15 = Rewrite.applyRuleAtId(f14, 22, Rules.mapLcl(0))

    // Lower to OpenCL memory model
    val f16 = Rewrite.applyRuleAtId(f15, 39, Rules.localMemory)
    val f17 = Rewrite.applyRuleAtId(f16, 32, Rules.localMemory)

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val (output: Array[Float], _) = Execute(4, 4, mSize, kSize, (true, true))(f17, matrixA, matrixB.transpose)

    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB)

    assertArrayEquals(gold.flatten, output, 0.0001f)
  }

  @Test
  def rectangularTiles(): Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    // Derivation the same, except split on the zip is different than the others
    val h0 = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, N), K), (p951880373, p1752203484) => FunCall(Join(), FunCall(Map(fun((p243745864) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p699780352) => FunCall(Unpack() o toGlobal(fun((p1613255205) => FunCall(MapSeq(fun((p1897115967) => FunCall(Map(fun((p1166151249) => FunCall(Map(fun((p1121453612) => FunCall(id, p1121453612))), p1166151249))), p1897115967))), p1613255205))), FunCall(ReduceSeq(fun((p2619171, p1728790703) => FunCall(Map(fun((p1227074340) => FunCall(Map(fun((p1154002927) => FunCall(add, FunCall(Get(0), p1154002927), FunCall(Get(1), p1154002927)))), FunCall(Zip(2), FunCall(Get(0), p1227074340), FunCall(Get(1), p1227074340))))), FunCall(Zip(2), p2619171, FunCall(fun((p2070529722) => FunCall(Map(fun((p1188753216) => FunCall(Join(), FunCall(Map(fun((p317986356) => FunCall(toPrivate(fun((p331510866) => FunCall(MapSeq(fun((p640363654) => FunCall(id, p640363654))), p331510866))), FunCall(ReduceSeq(fun((p924477420, p99451533) => FunCall(add, p924477420, FunCall(fun((p84739718) => FunCall(mult, FunCall(Get(0), p84739718), FunCall(Get(1), p84739718))), p99451533)))), FunCall(toPrivate(id), Value(0.0f, Float)), FunCall(Zip(2), p317986356, p1188753216))))), FunCall(Transpose(), FunCall(Get(0), p2070529722)))))), FunCall(Transpose(), FunCall(Get(1), p2070529722)))), p1728790703))))), FunCall(toPrivate(Map(fun((p2050835901) => FunCall(Map(fun((p511473681) => FunCall(id, p511473681))), p2050835901)))), Value(0.0f, ArrayType(ArrayType(Float, 8), 8))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p243745864)), FunCall(Split(4), FunCall(Transpose(), p699780352))))))), FunCall(Split(8), FunCall(Transpose(), p1752203484))))))), FunCall(Split(8), p951880373))))

    val h1 = Rewrite.applyRuleAtId(h0, 6, Rules.mapFissionWithZip)

    val h4 = Rewrite.applyRuleAtId(h1, 7, Rules.moveTransposeInsideTiling)

    val h5 = Rewrite.applyRuleAtId(h4, 1, Rules.mapFissionWithZip)
    val h6 = Rewrite.applyRuleAtId(h5, 2, Rules.mapFission)
    val h7 = Rewrite.applyRuleAtId(h6, 3, Rules.mapTransposeSplit)
    val h8 = Rewrite.applyRuleAtId(h7, 2, Rules.mapSplitTranspose)

    println(h8)

    // The rest same as before
  }

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

  */

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
          Map(fun( bCol => // Missing the Unpack, crucial for correctness...
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val tileSize = 8

    val f3 = Rewrite.applyRuleAtId(f0, 0, Rules.tileOutput(tileSize))

    // Split Reduce in 2
    val f4 = Rewrite.applyRuleAtId(f3, 14, Rules.partialReduce)
    val f5 = Rewrite.applyRuleAtId(f4, 15, Rules.partialReduceSplitJoin)

    // Pull out the last one
    val f6 = Rewrite.applyRuleAtId(f5, 12, Rules.mapFission)
    val f7 = Rewrite.applyRuleAtId(f6, 12, Rules.mapReduceInterchange)
    val f8 = Rewrite.applyRuleAtId(f7, 10, Rules.mapFission)
    val f9 = Rewrite.applyRuleAtId(f8, 10, Rules.mapReduceInterchange)

    // Pull Zip outside
    val f10 = Rewrite.applyRuleAtId(f9, 15, Rules.mapFissionAtPosition(2))
    val f11 = Rewrite.applyRuleAtId(f10, 16, Rules.mapMapTransposeZipInside)
    val f12 = Rewrite.applyRuleAtId(f11, 12, Rules.mapFissionAtPosition(2))
    val f13 = Rewrite.applyRuleAtId(f12, 13, Rules.mapMapTransposeZipInside)

    // Split the Zip
    val f14 = Rewrite.applyRuleAtId(f13, 14, Rules.splitJoin)
    val f15 = Rewrite.applyRuleAtId(f14, 15, Rules.splitZip)

    // And push back in to tile the input
    val f16 = Rewrite.applyRuleAtId(f15, 23, Rules.mapMapTransposeZipOutside)
    val f17 = Rewrite.applyRuleAtId(f16, 28, Rules.mapMapTransposeZipOutside)

    // Move split from partial reduce out and eliminate
    val f18 = Rewrite.applyRuleAtId(f17, 42, Rules.transposeBothSidesWithSplit)
    val f19 = Rewrite.applyRuleAtId(f18, 11, Rules.transposeBothSidesWithSplit)
    val f20 = Rewrite.applyRuleAtId(f19, 13, Rules.splitJoinId)

    // Fuse maps and get rid of transposes in between Reduce(add) and Map(mult)
    val f21 = Rewrite.applyRuleAtId(f20, 13, Rules.mapFission)
    val f22 = Rewrite.applyRuleAtId(f21, 12, Rules.mapFusion)
    val f23 = Rewrite.applyRuleAtId(f22, 39, Rules.transposeTransposeId)
    val f24 = Rewrite.applyRuleAtId(f23, 12, Rules.removeEmptyMap)
    val f25 = Rewrite.applyRuleAtId(f24, 20, Rules.mapFission)
    val f26 = Rewrite.applyRuleAtId(f25, 12, Rules.mapFission)
    val f27 = Rewrite.applyRuleAtId(f26, 11, Rules.mapFusion)
    val f28 = Rewrite.applyRuleAtId(f27, 38, Rules.mapFusion)
    val f29 = Rewrite.applyRuleAtId(f28, 11, Rules.mapFusion)
    val f30 = Rewrite.applyRuleAtId(f29, 19, Rules.mapFusion)
    val f31 = Rewrite.applyRuleAtId(f30, 39, Rules.transposeTransposeId)
    val f32 = Rewrite.applyRuleAtId(f31, 19, Rules.mapFusion)
    val f33 = Rewrite.applyRuleAtId(f32, 24, Rules.mapFusion)

    // ReduceSeq o MapSeq fusion x2
    val f34 = Rewrite.applyRuleAtId(f33, 28, Rules.partialReduceToReduce)
    val f35 = Rewrite.applyRuleAtId(f34, 28, Rules.reduceSeq)
    val f36 = Rewrite.applyRuleAtId(f35, 30, Rules.mapSeq)
    val f37 = Rewrite.applyRuleAtId(f36, 29, Rules.mapReduceFusion)

    val f38 = Rewrite.applyRuleAtId(f37, 10, Rules.reduceSeq)
    val f39 = Rewrite.applyRuleAtId(f38, 12, Rules.mapSeq)
    val f40 = Rewrite.applyRuleAtId(f39, 11, Rules.mapReduceFusion)
  }

}
