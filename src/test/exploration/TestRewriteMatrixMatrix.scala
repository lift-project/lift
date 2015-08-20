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

  @Test
  def reuseBothWithTiling(): Unit = {
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
    val workPerThreadM = 4

    val f1 = Rewrite.applyRuleAtId(f0, 0, Rules.tileOutput(tileSizeMN))

    val f2 = Rewrite.applyRuleAtId(f1, 13, Rules.mapFission)
    val f3 = Rewrite.applyRuleAtId(f2, 14, Rules.mapMapTransposeZipInside)
    val f4 = Rewrite.applyRuleAtId(f3, 11, Rules.mapFissionAtPosition(1))
    val f5 = Rewrite.applyRuleAtId(f4, 12, Rules.mapMapTransposeZipInside)

    val f6 = Rewrite.applyRuleAtId(f5, 13, Rules.finishTilingInput(tileSizeK))

    // Experimenting

    val f7 = Rewrite.applyRuleAtId(f6, 28, Rules.reorderBothSides) // TODO: check the stride
    val f8 = Rewrite.applyRuleAtId(f7, 23, Rules.mapFissionAtPosition(1))
    val f9 = Rewrite.applyRuleAtId(f8, 24, Rules.splitJoin(workPerThreadN))
    val f10 = Rewrite.applyRuleAtId(f9, 30, Rules.mapMapInterchange)
    val f11 = Rewrite.applyRuleAtId(f10, 31, Rules.splitJoin(workPerThreadM))
    val f12 = Rewrite.applyRuleAtId(f11, 38, Rules.mapMapInterchange)
    val f13 = Rewrite.applyRuleAtId(f12, 41, Rules.mapMapTransposeZipInside)
    val f14 = Rewrite.applyRuleAtId(f13, 39, Rules.mapFission)
    val f15 = Rewrite.applyRuleAtId(f14, 40, Rules.mapMapTransposeZipInside)

    // Input's good

    val f16 = Rewrite.applyRuleAtId(f15, 61, Rules.reorderBothSides) // TODO: check the stride
    val f17 = Rewrite.applyRuleAtId(f16, 11, Rules.mapFission)
    val f18 = Rewrite.applyRuleAtId(f17, 12, Rules.splitJoin(workPerThreadN))
    val f19 = Rewrite.applyRuleAtId(f18, 64, Rules.mapFission)
    val f20 = Rewrite.applyRuleAtId(f19, 64, Rules.transposeBothSides)
    val f21 = Rewrite.applyRuleAtId(f20, 65, Rules.splitJoin(workPerThreadM))
    val f22 = Rewrite.applyRuleAtId(f21, 74, Rules.transposeBothSides)

    val f23 = Rewrite.applyRuleAtId(f22, 66, Rules.mapFission)
    val f24 = Rewrite.applyRuleAtId(f23, 13, Rules.mapFissionAtPosition(2))

    val f25 = Rewrite.applyRuleAtId(f24, 78, Rules.partialReduce)
    val f26 = Rewrite.applyRuleAtId(f25, 79, Rules.partialReduceSplitJoin(tileSizeK))

    val f27 = Rewrite.applyRuleAtId(f26, 76, Rules.mapFissionAtPosition(2))
    val f28 = Rewrite.applyRuleAtId(f27, 76, Rules.mapReducePartialReduce)
    val f29 = Rewrite.applyRuleAtId(f28, 73, Rules.mapFissionAtPosition(1))
    val f30 = Rewrite.applyRuleAtId(f29, 73, Rules.mapFission)
    val f31 = Rewrite.applyRuleAtId(f30, 74, Rules.mapReduceInterchange)
    val f32 = Rewrite.applyRuleAtId(f31, 65, Rules.mapFissionAtPosition(2))
    val f33 = Rewrite.applyRuleAtId(f32, 65, Rules.mapFissionAtPosition(1))
    val f34 = Rewrite.applyRuleAtId(f33, 66, Rules.mapReduceInterchange)
    val f35 = Rewrite.applyRuleAtId(f34, 14, Rules.mapFissionAtPosition(2))
    val f36 = Rewrite.applyRuleAtId(f35, 14, Rules.mapFissionAtPosition(1))
    val f37 = Rewrite.applyRuleAtId(f36, 15, Rules.mapReduceInterchange)

    // Output's good, nested reduces use same memory
    // Can be brought into a form, where a large chunk (splits, joins, transposes and reorders)
    // can be commented out and runs fine.

    println(NumberPrinter(f37))
    println(f37)
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

    val f11 = Rewrite.applyRuleAtId(f10, 53, Rules.partialReduce)
    val f12 = Rewrite.applyRuleAtId(f11, 54, Rules.partialReduceSplitJoin(tileSizeK))
    val f13 = Rewrite.applyRuleAtId(f12, 11, Rules.splitJoin(workPerThreadN))
    val f14 = Rewrite.applyRuleAtId(f13, 52, Rules.mapFission)
    val f15 = Rewrite.applyRuleAtId(f14, 52, Rules.transposeBothSides)
    val f16 = Rewrite.applyRuleAtId(f15, 12, Rules.mapFission)

    val f17 = Rewrite.applyRuleAtId(f16, 59, Rules.mapFission)
    val f18 = Rewrite.applyRuleAtId(f17, 59, Rules.mapReduceInterchangeBroken)
    val f19 = Rewrite.applyRuleAtId(f18, 53, Rules.mapFission)
    val f20 = Rewrite.applyRuleAtId(f19, 53, Rules.mapReduceInterchangeBroken)
    val f21 = Rewrite.applyRuleAtId(f20, 13, Rules.mapFission)
    val f22 = Rewrite.applyRuleAtId(f21, 13, Rules.mapReduceInterchangeBroken)

    // Output seems good

    val f23 = Rewrite.applyRuleAtId(f22, 63, Rules.mapFissionAtPosition(1))
    val f24 = Rewrite.applyRuleAtId(f23, 63, Rules.transposeBothSides)
    val f25 = Rewrite.applyRuleAtId(f24, 62, Rules.transposeTransposeId)
    val f26 = Rewrite.applyRuleAtId(f25, 63, Rules.transposeMapSplit)
    val f27 = Rewrite.applyRuleAtId(f26, 62, Rules.mapFusion)
    val f28 = Rewrite.applyRuleAtId(f27, 56, Rules.mapFission)
    val f29 = Rewrite.applyRuleAtId(f28, 57, Rules.mapFission)
    val f30 = Rewrite.applyRuleAtId(f29, 58, Rules.mapTransposeTransposeMapTranspose)
    val f31 = Rewrite.applyRuleAtId(f30, 57, Rules.mapSplitTranspose)
    val f32 = Rewrite.applyRuleAtId(f31, 56, Rules.transposeBothSides)
    val f33 = Rewrite.applyRuleAtId(f32, 55, Rules.transposeTransposeId)
    val f34 = Rewrite.applyRuleAtId(f33, 56, Rules.transposeTransposeId)

    val f35 = Rewrite.applyRuleAtId(f34, 15, Rules.mapFission)
    val f36 = Rewrite.applyRuleAtId(f35, 15, Rules.transposeBothSides)
    val f37 = Rewrite.applyRuleAtId(f36, 14, Rules.transposeTransposeId)

    // Now there's a massive bunch of transposes, splits and joins, which are id.
    // Can comment out and runs fine. Otherwise good.

    val f38 = Rewrite.applyRuleAtId(f37, 16, Rules.mapFission)
    val f39 = Rewrite.applyRuleAtId(f38, 15, Rules.transposeMapMapTranspose)
    val f40 = Rewrite.applyRuleAtId(f39, 17, Rules.mapFission)
    val f41 = Rewrite.applyRuleAtId(f40, 18, Rules.mapFission)
    val f42 = Rewrite.applyRuleAtId(f41, 19, Rules.mapTransposeSplit)
    val f43 = Rewrite.applyRuleAtId(f42, 21, Rules.transposeTransposeId)
    val f44 = Rewrite.applyRuleAtId(f43, 16, Rules.transposeMapSplit)
    val f45 = Rewrite.applyRuleAtId(f44, 18, Rules.transposeMapMapTranspose)
    val f46 = Rewrite.applyRuleAtId(f45, 19, Rules.transposeTransposeId)
    val f47 = Rewrite.applyRuleAtId(f46, 18, Rules.splitJoin(8))
    val f48 = Rewrite.applyRuleAtId(f47, 17, Rules.splitJoinId)
    val f49 = Rewrite.applyRuleAtId(f48, 19, Rules.splitJoin(8))
    val f50 = Rewrite.applyRuleAtId(f49, 18, Rules.splitJoinId)
    val f51 = Rewrite.applyRuleAtId(f50, 19, Rules.splitJoinId)

    // Splits & joins eliminated, just transposes left

    val f52 = Rewrite.applyRuleAtId(f51, 18, Rules.mapFusion)
    val f53 = Rewrite.applyRuleAtId(f52, 26, Rules.mapSplitTranspose)
    val f54 = Rewrite.applyRuleAtId(f53, 28, Rules.splitJoinId)
    val f55 = Rewrite.applyRuleAtId(f54, 15, Rules.mapFusion)
    val f56 = Rewrite.applyRuleAtId(f55, 15, Rules.mapFusion)
    val f57 = Rewrite.applyRuleAtId(f56, 15, Rules.mapFusion)
    val f58 = Rewrite.applyRuleAtId(f57, 24, Rules.transposeMapMapTranspose)
    val f59 = Rewrite.applyRuleAtId(f58, 25, Rules.transposeTransposeId)
    val f60 = Rewrite.applyRuleAtId(f59, 23, Rules.mapFusion)
    val f61 = Rewrite.applyRuleAtId(f60, 23, Rules.mapFusion)
    val f62 = Rewrite.applyRuleAtId(f61, 31, Rules.mapFission)
    val f63 = Rewrite.applyRuleAtId(f62, 29, Rules.mapTransposeTransposeMapTranspose)
    val f64 = Rewrite.applyRuleAtId(f63, 23, Rules.mapFusion)
    val f65 = Rewrite.applyRuleAtId(f64, 30, Rules.transposeTransposeId)
    val f66 = Rewrite.applyRuleAtId(f65, 29, Rules.mapFusion)
    val f67 = Rewrite.applyRuleAtId(f66, 47, Rules.transposeTransposeId)
    val f68 = Rewrite.applyRuleAtId(f67, 29, Rules.removeEmptyMap)
    val f69 = Rewrite.applyRuleAtId(f68, 28, Rules.transposeTransposeId)

    // Most transposes eliminated, just one left

    val f70 = Rewrite.applyRuleAtId(f69, 52, Rules.partialReduceToReduce)
    val f71 = Rewrite.applyRuleAtId(f70, 49, Rules.mapReduceInterchangeBroken)
    val f72 = Rewrite.applyRuleAtId(f71, 50, Rules.transposeTransposeId)

    println(f72)

    // All unwanted transposes eliminated, start fusing

    val f73 = Rewrite.applyRuleAtId(f72, 14, Rules.mapFusion)
    val f74 = Rewrite.applyRuleAtId(f73, 22, Rules.mapFusion)
    val f75 = Rewrite.applyRuleAtId(f74, 27, Rules.mapFusion)
    val f76 = Rewrite.applyRuleAtId(f75, 32, Rules.reduceMapFusion)
    val f77 = Rewrite.applyRuleAtId(f76, 13, Rules.reduceMapFusion)

    println(f77)
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

    val f0 =  fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p662736689, p1897115967) => FunCall(Join(), FunCall(Map(fun((p140799417) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p926370398) => FunCall(TransposeW(), FunCall(Map(fun((p1391942103) => FunCall(TransposeW(), p1391942103))), FunCall(TransposeW(), FunCall(toGlobal(fun((p17037394) => FunCall(MapSeq(fun((p1484531981) => FunCall(Map(fun((p1159114532) => FunCall(Map(fun((p1256728724) => FunCall(id, p1256728724))), p1159114532))), p1484531981))), p17037394))), FunCall(ReduceSeq(fun((p183284570, p1607305514) => FunCall(Map(fun((p1453128758) => FunCall(Map(fun((p1122805102) => FunCall(add, FunCall(Get(0), p1122805102), FunCall(Get(1), p1122805102)))), FunCall(Zip(2), FunCall(Get(0), p1453128758), FunCall(Get(1), p1453128758))))), FunCall(Zip(2), p183284570, FunCall(fun((p668210649) => FunCall(Map(fun((p188576144) => FunCall(Join(), FunCall(Map(fun((p282432134) => FunCall(toPrivate(fun((p999609945) => FunCall(MapSeq(fun((p615634843) => FunCall(id, p615634843))), p999609945))), FunCall(ReduceSeq(fun((p1758386724, p673068808) => FunCall(add, p1758386724, FunCall(fun((p266437232) => FunCall(mult, FunCall(Get(0), p266437232), FunCall(Get(1), p266437232))), p673068808)))), FunCall(id, Value(0.0f, Float)), FunCall(Zip(2), p188576144, p282432134))))), FunCall(Transpose(), FunCall(Get(1), p668210649)))))), FunCall(Transpose(), FunCall(Get(0), p668210649)))), p1607305514))))), FunCall(Map(fun((p900008524) => FunCall(Map(fun((p520232556) => FunCall(id, p520232556))), p900008524))), Value(0.0f, ArrayType(ArrayType(Float, 4), 4))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p140799417)), FunCall(Split(4), FunCall(Transpose(), p926370398)))))))))), FunCall(Split(4), p1897115967)))))), FunCall(Split(4), p662736689))))

    // Add a copy
    val f1 = Rewrite.applyRuleAtId(f0, 29, Rules.addId)

    // Lower to OpenCL execution model
    val f2 = Rewrite.applyRuleAtId(f1, 1, Rules.mapWrg(0))
    val f3 = Rewrite.applyRuleAtId(f2, 6, Rules.mapWrg(1))

    val f4 = Rewrite.applyRuleAtId(f3, 90, Rules.mapLcl(1))
    val f5 = Rewrite.applyRuleAtId(f4, 92, Rules.mapLcl(0))
    val f6 = Rewrite.applyRuleAtId(f5, 27, Rules.mapLcl(1))
    val f7 = Rewrite.applyRuleAtId(f6, 77, Rules.mapLcl(0))
    val f8 = Rewrite.applyRuleAtId(f7, 47, Rules.mapLcl(1))
    val f9 = Rewrite.applyRuleAtId(f8, 52, Rules.mapLcl(0))
    val f10 = Rewrite.applyRuleAtId(f9, 40, Rules.mapLcl(1))
    val f11 = Rewrite.applyRuleAtId(f10, 43, Rules.mapLcl(0))
    val f12 = Rewrite.applyRuleAtId(f11, 33, Rules.mapLcl(1))
    val f13 = Rewrite.applyRuleAtId(f12, 36, Rules.mapLcl(0))
    val f14 = Rewrite.applyRuleAtId(f13, 21, Rules.mapLcl(1))
    val f15 = Rewrite.applyRuleAtId(f14, 23, Rules.mapLcl(0))

    // Lower to OpenCL memory model
    val f16 = Rewrite.applyRuleAtId(f15, 40, Rules.localMemory)
    val f17 = Rewrite.applyRuleAtId(f16, 33, Rules.localMemory)

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
    val f4 = Rewrite.applyRuleAtId(f3, 9, Rules.mapMapTransposeZipInside)
    val f5 = Rewrite.applyRuleAtId(f4, 8, Rules.transposeTransposeId)
    val f6 = Rewrite.applyRuleAtId(f5, 7, Rules.reduceMapFusion)

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
      val f9 = Rewrite.applyRuleAtId(f8, 17, Rules.mapMapTransposeZipInside)
      val f10 = Rewrite.applyRuleAtId(f9, 16, Rules.transposeTransposeId)
      val f11 = Rewrite.applyRuleAtId(f10, 12, Rules.mapFission)
      val f12 = Rewrite.applyRuleAtId(f11, 13, Rules.mapFission)
      val f13 = Rewrite.applyRuleAtId(f12, 13, Rules.mapReduceInterchange)
      val f14 = Rewrite.applyRuleAtId(f13, 16, Rules.mapMapTransposeZipInside)
      val f15 = Rewrite.applyRuleAtId(f14, 15, Rules.transposeTransposeId)
      val f16 = Rewrite.applyRuleAtId(f15, 14, Rules.reduceMapFusion)

      println(f16)
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
    val f7 = Rewrite.applyRuleAtId(f6, 11, Rules.mapFission)
    val f8 = Rewrite.applyRuleAtId(f7, 11, Rules.mapReduceInterchange)


    // Pull Zip outside
    val f9 = Rewrite.applyRuleAtId(f8, 17, Rules.mapFissionAtPosition(2))
    val f10 = Rewrite.applyRuleAtId(f9, 18, Rules.mapMapTransposeZipInside)
    val f11 = Rewrite.applyRuleAtId(f10, 14, Rules.mapFissionAtPosition(2))
    val f12 = Rewrite.applyRuleAtId(f11, 15, Rules.mapMapTransposeZipInside)

    val f13 = Rewrite.applyRuleAtId(f12, 16, Rules.finishTilingInput(tileSizeK))

    // Move split from partial reduce out and eliminate transposes on the left hand side
    val f14 = Rewrite.applyRuleAtId(f13, 44, Rules.transposeBothSidesWithSplit)
    val f15 = Rewrite.applyRuleAtId(f14, 13, Rules.transposeBothSidesWithSplit)
    val f16 = Rewrite.applyRuleAtId(f15, 15, Rules.splitJoinId)

    // Fuse maps and get rid of transposes in between Reduce(add) and Map(mult)
    val f17 = Rewrite.applyRuleAtId(f16, 14, Rules.mapFusion)
    val f18 = Rewrite.applyRuleAtId(f17, 22, Rules.transposeTransposeId)
    val f19 = Rewrite.applyRuleAtId(f18, 13, Rules.mapFusion)
    val f20 = Rewrite.applyRuleAtId(f19, 22, Rules.mapFusion)
    val f21 = Rewrite.applyRuleAtId(f20, 26, Rules.transposeTransposeId)
    val f22 = Rewrite.applyRuleAtId(f21, 21, Rules.mapFusion)
    val f23 = Rewrite.applyRuleAtId(f22, 26, Rules.mapFusion)

    // ReduceSeq o MapSeq fusion x2
    val f24 = Rewrite.applyRuleAtId(f23, 30, Rules.partialReduceToReduce)
    val f25 = Rewrite.applyRuleAtId(f24, 30, Rules.reduceMapFusion)
    val f26 = Rewrite.applyRuleAtId(f25, 12, Rules.reduceMapFusion)

    println(f26)
  }

}
