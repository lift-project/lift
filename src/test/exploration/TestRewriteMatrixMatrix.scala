package exploration

import apart.arithmetic.Var
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.generator.OpenCLGenerator
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
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) o Transpose() $ B
        )) $ A
      })

    val tileSizeMN = 16
    val tileSizeK = 8
    val workPerThreadN = 2

    val f1 = Rewrite.applyRuleAtId(f0, 0, Rules.tileOutput(tileSizeMN))

    val f2 = Rewrite.applyRuleAtId(f1, 13, Rules.mapFission)
    val f3 = Rewrite.applyRuleAtId(f2, 14, Rules.mapMapTransposeZipInside)
    val f4 = Rewrite.applyRuleAtId(f3, 11, Rules.mapFissionAtPosition(1))
    val f5 = Rewrite.applyRuleAtId(f4, 12, Rules.mapMapTransposeZipInside)

    val f6 = Rewrite.applyRuleAtId(f5, 13, Rules.finishTilingInput(tileSizeK))

    // Experimenting from here on

    val f7 = Rewrite.applyRuleAtId(f6, 23, Rules.splitJoin(workPerThreadN))
    val f8 = Rewrite.applyRuleAtId(f7, 29, Rules.mapFission)
    val f9 = Rewrite.applyRuleAtId(f8, 30, Rules.mapMapInterchange)
    val f10 = Rewrite.applyRuleAtId(f9, 35, Rules.mapMapTransposeZipInside)

    // Input's good
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

    // Add the copy
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

    // The copy for transpose + rest same as before
  }

  @Test
  def mmReuseA(): Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f0 = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, K), N), // Already transposed
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val f1 = Rewrite.applyRuleAtId(f0, 2, Rules.splitJoin)
    val f2 = Rewrite.applyRuleAtId(f1, 6, Rules.mapFission)
    val f3 = Rewrite.applyRuleAtId(f2, 6, Rules.mapReduceInterchange)
    val f4 = Rewrite.applyRuleAtId(f3, 8, Rules.mapMapTransposeZipInside)
    val f5 = Rewrite.applyRuleAtId(f4, 7, Rules.transposeTransposeId)
    val f6 = Rewrite.applyRuleAtId(f5, 6, Rules.reduceMapFusion)

    println(f6)
  }
    @Test
    def mmReuseBoth(): Unit = {
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

      val f1 = Rewrite.applyRuleAtId(f0, 2, Rules.reorderBothSides)
      val f2 = Rewrite.applyRuleAtId(f1, 0, Rules.mapFission)
      val f3 = Rewrite.applyRuleAtId(f2, 1, Rules.splitJoin)
      val f4 = Rewrite.applyRuleAtId(f3, 5, Rules.mapMapInterchange)
      val f5 = Rewrite.applyRuleAtId(f4, 6, Rules.splitJoin)
      val f6 = Rewrite.applyRuleAtId(f5, 11, Rules.mapMapInterchange)
      val f7 = Rewrite.applyRuleAtId(f6, 14, Rules.mapFission)
      val f8 = Rewrite.applyRuleAtId(f7, 14, Rules.mapReduceInterchange)
      val f9 = Rewrite.applyRuleAtId(f8, 16, Rules.mapMapTransposeZipInside)
      val f10 = Rewrite.applyRuleAtId(f9, 15, Rules.transposeTransposeId)
      val f11 = Rewrite.applyRuleAtId(f10, 12, Rules.mapFission)
      val f12 = Rewrite.applyRuleAtId(f11, 12, Rules.mapReduceInterchange)
      val f13 = Rewrite.applyRuleAtId(f12, 14, Rules.mapMapTransposeZipInside)
      val f14 = Rewrite.applyRuleAtId(f13, 13, Rules.transposeTransposeId)
      val f15 = Rewrite.applyRuleAtId(f14, 12, Rules.reduceMapFusion)

      println(f15)
    }


  @Test
  def mmTiled() = {
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

    val tileSizeMN = 8
    val tileSizeK = 4

    val f1 = Rewrite.applyRuleAtId(f0, 0, Rules.tileOutput(tileSizeMN))

    // Split Reduce in 2
    val f2 = Rewrite.applyRuleAtId(f1, 14, Rules.partialReduce)
    val f3 = Rewrite.applyRuleAtId(f2, 15, Rules.partialReduceSplitJoin)

    // Pull out the last one
    val f4 = Rewrite.applyRuleAtId(f3, 12, Rules.mapFission)
    val f5 = Rewrite.applyRuleAtId(f4, 12, Rules.mapReduceInterchange)
    val f6 = Rewrite.applyRuleAtId(f5, 10, Rules.mapFission)
    val f7 = Rewrite.applyRuleAtId(f6, 10, Rules.mapReduceInterchange)

    // Pull Zip outside
    val f8 = Rewrite.applyRuleAtId(f7, 15, Rules.mapFissionAtPosition(2))
    val f9 = Rewrite.applyRuleAtId(f8, 16, Rules.mapMapTransposeZipInside)
    val f10 = Rewrite.applyRuleAtId(f9, 12, Rules.mapFissionAtPosition(2))
    val f12 = Rewrite.applyRuleAtId(f10, 13, Rules.mapMapTransposeZipInside)

    val f13 = Rewrite.applyRuleAtId(f12, 14, Rules.finishTilingInput(tileSizeK))

    // Move split from partial reduce out and eliminate transposes on the left hand side
    val f14 = Rewrite.applyRuleAtId(f13, 42, Rules.transposeBothSidesWithSplit)
    val f15 = Rewrite.applyRuleAtId(f14, 11, Rules.transposeBothSidesWithSplit)
    val f16 = Rewrite.applyRuleAtId(f15, 13, Rules.splitJoinId)

    // Fuse maps and get rid of transposes in between Reduce(add) and Map(mult)
    val f17 = Rewrite.applyRuleAtId(f16, 12, Rules.mapFusion)
    val f18 = Rewrite.applyRuleAtId(f17, 20, Rules.transposeTransposeId)
    val f19 = Rewrite.applyRuleAtId(f18, 11, Rules.mapFusion)
    val f20 = Rewrite.applyRuleAtId(f19, 20, Rules.mapFusion)
    val f21 = Rewrite.applyRuleAtId(f20, 24, Rules.transposeTransposeId)
    val f22 = Rewrite.applyRuleAtId(f21, 19, Rules.mapFusion)
    val f23 = Rewrite.applyRuleAtId(f22, 24, Rules.mapFusion)

    // ReduceSeq o MapSeq fusion x2
    val f24 = Rewrite.applyRuleAtId(f23, 28, Rules.partialReduceToReduce)
    val f25 = Rewrite.applyRuleAtId(f24, 28, Rules.reduceMapFusion)
    val f26 = Rewrite.applyRuleAtId(f25, 10, Rules.reduceMapFusion)

    println(f26)
  }

}
