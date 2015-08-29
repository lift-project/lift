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

    val tileSizeMN = 128
    val tileSizeK = 16
    val workPerThreadN = 8
    val workPerThreadM = 8

    val tileTranspositionM = 32
    val tileTranspositionK = 16

    val stride = tileSizeMN/workPerThreadM

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.tileInputAndOutput(tileSizeMN, tileSizeK))

    val g0 = Rewrite.applyRuleAtId(f1, 44, Rules.partialReduce)
    val g1 = Rewrite.applyRuleAtId(g0, 45, Rules.partialReduceSplitJoin(tileSizeK))


    // Experimenting

    val f7 = Rewrite.applyRuleAtId(g1, 28, Rules.reorderBothSidesWithStride(stride))
    val f8 = Rewrite.applyRuleAtId(f7, 23, MacroRules.mapFissionAtPosition(1))
    val f9 = Rewrite.applyRuleAtId(f8, 24, Rules.splitJoin(workPerThreadN))
    val f10 = Rewrite.applyRuleAtId(f9, 30, Rules.mapMapInterchange)
    val f11 = Rewrite.applyRuleAtId(f10, 31, Rules.splitJoin(workPerThreadM))
    val f12 = Rewrite.applyRuleAtId(f11, 38, Rules.mapMapInterchange)
    val f13 = Rewrite.applyRuleAtId(f12, 41, Rules.mapMapTransposeZipInside)
    val f14 = Rewrite.applyRuleAtId(f13, 39, Rules.mapFission)
    val f15 = Rewrite.applyRuleAtId(f14, 40, Rules.mapMapTransposeZipInside)

    // Input's good

    val f16 = Rewrite.applyRuleAtId(f15, 61, Rules.reorderBothSidesWithStride(stride))
    val f17 = Rewrite.applyRuleAtId(f16, 11, Rules.mapFission)
    val f18 = Rewrite.applyRuleAtId(f17, 12, Rules.splitJoin(workPerThreadN))
    val f19 = Rewrite.applyRuleAtId(f18, 64, Rules.mapFission)
    val f20 = Rewrite.applyRuleAtId(f19, 64, Rules.transposeBothSides)
    val f21 = Rewrite.applyRuleAtId(f20, 65, Rules.splitJoin(workPerThreadM))
    val f22 = Rewrite.applyRuleAtId(f21, 74, Rules.transposeBothSides)

    val f23 = Rewrite.applyRuleAtId(f22, 66, Rules.mapFission)
    val f24 = Rewrite.applyRuleAtId(f23, 13, MacroRules.mapFissionAtPosition(2))


    val f27 = Rewrite.applyRuleAtId(f24, 76, MacroRules.mapFissionAtPosition(2))
    val f28 = Rewrite.applyRuleAtId(f27, 76, Rules.mapReducePartialReduce)
    val f31 = Rewrite.applyRuleAtId(f28, 73, MacroRules.moveReduceOutOneLevel)
    val f34 = Rewrite.applyRuleAtId(f31, 65, MacroRules.moveReduceOutOneLevel)
    val f37 = Rewrite.applyRuleAtId(f34, 14, MacroRules.moveReduceOutOneLevel)

    // Output's good, nested reduces use same memory, performance drops by a third for NVIDIA and
    // half for AMD if not.
    // Can be brought into a form, where a large chunk (splits, joins, transposes and reorders)
    // can be commented out and runs fine.

    val f38 = Rewrite.applyRuleAtId(f37, 111, Rules.partialReduceToReduce)
    val f39 = Rewrite.applyRuleAtId(f38, 105, Rules.mapReduceInterchangeWithZip)
    val f40 = Rewrite.applyRuleAtId(f39, 98, MacroRules.mapFissionAtPosition(1))
    val f41 = Rewrite.applyRuleAtId(f40, 99, Rules.mapFissionWithZipOutside)
    val f42 = Rewrite.applyRuleAtId(f41, 99, Rules.mapReduceInterchangeWithZip)

    println(f42)

    // TODO: Find missing derivation
    // Continuing from where the splits, joins, transposes and reorders have been eliminated.
    // Obviously possible, don't yet know how to.
    val h0 = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, N), K),(p1418385211, p1282811396) => FunCall(Join(), FunCall(Map(fun((p789219251) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p832279283) => FunCall(TransposeW(), FunCall(Map(fun((p265119009) => FunCall(Scatter(ReorderWithStride(tileSizeMN/workPerThreadM)), p265119009))), FunCall(Join(), FunCall(Map(fun((p2050404090) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p388043093) => FunCall(TransposeW(), p388043093))), p2050404090))))), FunCall(Map(fun((p188576144) => FunCall(Map(fun((p1608230649) => FunCall(Map(fun((p282432134) => FunCall(TransposeW(), p282432134))), FunCall(TransposeW(), p1608230649)))), FunCall(TransposeW(), p188576144)))), FunCall(TransposeW(), FunCall(Reduce(fun((p266437232, p1873859565) => FunCall(Map(fun((p1843289228) => FunCall(Map(fun((p1361289747) => FunCall(Map(fun((p1381128261) => FunCall(Join(), FunCall(Transpose(), p1381128261)))), FunCall(Transpose(), FunCall(Reduce(fun((p999609945, p615634843) => FunCall(Map(fun((p1758386724) => FunCall(Map(fun((p673068808) => FunCall(add, FunCall(Get(0), p673068808), FunCall(Get(1), p673068808)))), FunCall(Zip(2), FunCall(Get(0), p1758386724), FunCall(Get(1), p1758386724))))), FunCall(Zip(2), p999609945, p615634843)))), FunCall(Get(0), p1361289747), FunCall(Transpose(), FunCall(Map(fun((p900008524) => FunCall(Transpose(), p900008524))), FunCall(Get(1), p1361289747)))))))), FunCall(Zip(2), FunCall(Get(0), p1843289228), FunCall(Get(1), p1843289228))))), FunCall(Zip(2), p266437232, p1873859565)))), FunCall(Map(fun((p520232556) => FunCall(Map(fun((p17037394) => FunCall(Map(fun((p1484531981) => FunCall(Map(fun((p1159114532) => FunCall(id, p1159114532))), p1484531981))), p17037394))), p520232556))), Value(0.0f, ArrayType(ArrayType(ArrayType(ArrayType(Float, workPerThreadM), workPerThreadN), tileSizeMN/workPerThreadM), tileSizeMN/workPerThreadN))), FunCall(Map(fun((p1256728724) => FunCall(Map(fun((p1412925683) => FunCall(Map(fun((p1832580921) => FunCall(Map(fun((p497359413) => FunCall(TransposeW(), p497359413))), FunCall(TransposeW(), p1832580921)))), FunCall(Map(fun((p369241501) => FunCall(Map(fun((p2124046270) => FunCall(Map(fun((p1151593579) => FunCall(Map(fun((p1902260856) => FunCall(mult, p1151593579, p1902260856))), FunCall(Get(1), p2124046270)))), FunCall(Get(0), p2124046270)))), FunCall(Zip(2), FunCall(Transpose(), p1412925683), FunCall(Transpose(), p369241501))))), FunCall(Split(workPerThreadM), FunCall(Gather(ReorderWithStride(tileSizeMN/workPerThreadM)), FunCall(Transpose(), FunCall(Get(1), p1256728724)))))))), FunCall(Split(workPerThreadN), FunCall(Transpose(), FunCall(Get(0), p1256728724)))))), FunCall(Zip(2), FunCall(Split(tileSizeK), FunCall(Transpose(), p789219251)), FunCall(Split(tileSizeK), FunCall(Transpose(), p832279283))))))))))))), FunCall(Split(tileSizeMN), FunCall(Transpose(), p1282811396))))))), FunCall(Split(tileSizeMN), p1418385211))))

    val h9 = Lower.simplifyAndFuse(h0)

    // Final steps, move transpose inside tiling + tiling (kernel) for A

    val h15 = Rewrite.applyRuleAtId(h9, 1, MacroRules.finishRectangularTiles)

    // Tile the transposition

    val h16 = Rewrite.applyRuleAtId(h15, 5, Rules.addCopy)
    val h17 = Rewrite.applyRuleAtId(h16, 6, MacroRules.tileTranspose(tileTranspositionM, tileTranspositionK))

    println(h17)
  }

  @Test
  def vectorLoads(): Unit = {
    val tileSizeMN = 16
    val tileSizeK = 8
    val workPerThreadN = 2
    val workPerThreadM = 4

    val tileTranspositionM = 32
    val tileTranspositionK = 32

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f0 = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, N), K), (p450003680, p2134991632) => FunCall(Join(), FunCall(Map(fun((p756185697) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1237598030) => FunCall(TransposeW(), FunCall(Map(fun((p660879561) => FunCall(Scatter(ReorderWithStride(tileSizeMN/workPerThreadM)), p660879561))), FunCall(Join(), FunCall(Map(fun((p376416077) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1089504328) => FunCall(TransposeW(), p1089504328))), p376416077))))), FunCall(Map(fun((p294184992) => FunCall(Map(fun((p793315160) => FunCall(Map(fun((p270397815) => FunCall(TransposeW(), p270397815))), FunCall(TransposeW(), p793315160)))), FunCall(TransposeW(), p294184992)))), FunCall(TransposeW(), FunCall(toGlobal(fun((p1485697819) => FunCall(MapSeq(fun((p867398280) => FunCall(Map(fun((p2007331442) => FunCall(Map(fun((p1904324159) => FunCall(Map(fun((p1176735295) => FunCall(Map(fun((p1848415041) => FunCall(id, p1848415041))), p1176735295))), p1904324159))), p2007331442))), p867398280))), p1485697819))), FunCall(ReduceSeq(fun((p1032000752, p843467284) => FunCall(Map(fun((p124407148) => FunCall(Map(fun((p1825027294) => FunCall(Map(fun((p19986569) => FunCall(Join(), FunCall(Transpose(), p19986569)))), FunCall(Transpose(), FunCall(toGlobal(fun((p852445367) => FunCall(MapSeq(fun((p1738236591) => FunCall(Map(fun((p1558021762) => FunCall(Map(fun((p225290371) => FunCall(id, p225290371))), p1558021762))), p1738236591))), p852445367))), FunCall(ReduceSeq(fun((p1004095028, p1169146729) => FunCall(Map(fun((p1948863195) => FunCall(Map(fun((p1890187342) => FunCall(add, FunCall(Get(0), p1890187342), FunCall(Get(1), p1890187342)))), FunCall(Zip(2), FunCall(Get(0), p1948863195), FunCall(Get(1), p1948863195))))), FunCall(Zip(2), p1004095028, FunCall(Map(fun((p876213901) => FunCall(Map(fun((p230528013) => FunCall(mult, p876213901, p230528013))), FunCall(Get(1), p1169146729)))), FunCall(Get(0), p1169146729)))))), FunCall(Get(0), p1825027294), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p124407148)), FunCall(Transpose(), FunCall(Get(1), p1825027294))))))))), FunCall(Zip(2), FunCall(Get(0), p124407148), FunCall(Split(workPerThreadM), FunCall(Gather(ReorderWithStride(tileSizeMN/workPerThreadM)), FunCall(Transpose(), FunCall(Get(1), p843467284)))))))), FunCall(Zip(2), p1032000752, FunCall(Split(workPerThreadN), FunCall(Transpose(), FunCall(Get(0), p843467284))))))), FunCall(Map(fun((p1822383117) => FunCall(Map(fun((p233021551) => FunCall(Map(fun((p1991313236) => FunCall(Map(fun((p736778932) => FunCall(id, p736778932))), p1991313236))), p233021551))), p1822383117))), Value(0.0f, ArrayType(ArrayType(ArrayType(ArrayType(Float, workPerThreadM), workPerThreadN), tileSizeMN/workPerThreadM), tileSizeMN/workPerThreadN))), FunCall(Zip(2), p756185697, p1237598030))))))))))), FunCall(Transpose(), FunCall(Map(fun((p24606376) => FunCall(Transpose(), p24606376))), FunCall(Split(tileSizeK), FunCall(Map(fun((p302155142) => FunCall(Split(tileSizeMN), p302155142))), p2134991632))))))))), FunCall(Transpose(), FunCall(Map(fun((p1891546521) => FunCall(Transpose(), p1891546521))), FunCall(Split(tileSizeK), FunCall(Map(fun((p297927961) => FunCall(Split(tileSizeMN), p297927961))), FunCall(Join(), FunCall(Map(fun((p1881129850) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p912011468) => FunCall(TransposeW(), FunCall(Map(fun((p1312884893) => FunCall(Map(fun((p849373393) => FunCall(id, p849373393))), p1312884893))), FunCall(Transpose(), p912011468))))), p1881129850))))), FunCall(Transpose(), FunCall(Map(fun((p2142080121) => FunCall(Transpose(), p2142080121))), FunCall(Split(tileTranspositionK), FunCall(Map(fun((p673186785) => FunCall(Split(tileTranspositionM), p673186785))), p450003680)))))))))))))

    val f1 = Rewrite.applyRuleAtId(f0, 52, Rules.addIdForCurrentValueInReduce)
    val f2 = Rewrite.applyRuleAtId(f1, 67, Rules.implementIdAsDeepCopy)
    val f3 = Rewrite.applyRuleAtId(f2, 69, Rules.tupleMap)
    val f4 = Rewrite.applyRuleAtId(f3, 82, Rules.vectorize)
    val f5 = Rewrite.applyRuleAtId(f4, 77, Rules.vectorize)
    val f6 = Rewrite.applyRuleAtId(f5, 76, Rules.tupleFission)
    val f7 = Rewrite.applyRuleAtId(f6, 77, Rules.tupleFission)
    val f8 = Rewrite.applyRuleAtId(f7, 85, Rules.tupleMap)

    println(f8)
  }

  @Test
  def loweringTiledAndBlockedBInnermost(): Unit = {
    val tileSizeMN = 16
    val tileSizeK = 8
    val workPerThreadN = 2
    val workPerThreadM = 4

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f0 = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, N), K),(p450003680, p2134991632) => FunCall(Join(), FunCall(Map(fun((p756185697) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1237598030) => FunCall(TransposeW(), FunCall(Map(fun((p660879561) => FunCall(Scatter(ReorderWithStride(tileSizeMN/workPerThreadM)), p660879561))), FunCall(Join(), FunCall(Map(fun((p376416077) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1089504328) => FunCall(TransposeW(), p1089504328))), p376416077))))), FunCall(Map(fun((p294184992) => FunCall(Map(fun((p793315160) => FunCall(Map(fun((p270397815) => FunCall(TransposeW(), p270397815))), FunCall(TransposeW(), p793315160)))), FunCall(TransposeW(), p294184992)))), FunCall(TransposeW(), FunCall(toGlobal(fun((p1485697819) => FunCall(MapSeq(fun((p867398280) => FunCall(Map(fun((p2007331442) => FunCall(Map(fun((p1904324159) => FunCall(Map(fun((p1176735295) => FunCall(Map(fun((p1848415041) => FunCall(id, p1848415041))), p1176735295))), p1904324159))), p2007331442))), p867398280))), p1485697819))), FunCall(ReduceSeq(fun((p1032000752, p843467284) => FunCall(Map(fun((p124407148) => FunCall(Map(fun((p1825027294) => FunCall(Map(fun((p19986569) => FunCall(Join(), FunCall(Transpose(), p19986569)))), FunCall(Transpose(), FunCall(toPrivate(fun((p852445367) => FunCall(MapSeq(fun((p1738236591) => FunCall(Map(fun((p1558021762) => FunCall(Map(fun((p225290371) => FunCall(id, p225290371))), p1558021762))), p1738236591))), p852445367))), FunCall(ReduceSeq(fun((p1004095028, p1169146729) => FunCall(Map(fun((p1948863195) => FunCall(Map(fun((p1890187342) => FunCall(add, FunCall(Get(0), p1890187342), FunCall(Get(1), p1890187342)))), FunCall(Zip(2), FunCall(Get(0), p1948863195), FunCall(Get(1), p1948863195))))), FunCall(Zip(2), p1004095028, FunCall(Map(fun((p876213901) => FunCall(Map(fun((p230528013) => FunCall(mult, p876213901, p230528013))), FunCall(Get(1), p1169146729)))), FunCall(Get(0), p1169146729)))))), FunCall(Get(0), p1825027294), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p124407148)), FunCall(Transpose(), FunCall(Get(1), p1825027294))))))))), FunCall(Zip(2), FunCall(Get(0), p124407148), FunCall(Split(workPerThreadM), FunCall(Gather(ReorderWithStride(tileSizeMN/workPerThreadM)), FunCall(Transpose(), FunCall(Get(1), p843467284)))))))), FunCall(Zip(2), p1032000752, FunCall(Split(workPerThreadN), FunCall(Transpose(), FunCall(Get(0), p843467284))))))), FunCall(Map(fun((p1822383117) => FunCall(Map(fun((p233021551) => FunCall(Map(fun((p1991313236) => FunCall(Map(fun((p736778932) => FunCall(id, p736778932))), p1991313236))), p233021551))), p1822383117))), Value(0.0f, ArrayType(ArrayType(ArrayType(ArrayType(Float, workPerThreadM), workPerThreadN), tileSizeMN/workPerThreadM), tileSizeMN/workPerThreadN))), FunCall(Zip(2), p756185697, p1237598030))))))))))), FunCall(Transpose(), FunCall(Map(fun((p24606376) => FunCall(Transpose(), p24606376))), FunCall(Split(tileSizeK), FunCall(Map(fun((p302155142) => FunCall(Split(tileSizeMN), p302155142))), p2134991632))))))))), FunCall(Transpose(), FunCall(Map(fun((p1891546521) => FunCall(Transpose(), p1891546521))), FunCall(Split(tileSizeK), FunCall(Map(fun((p297927961) => FunCall(Split(tileSizeMN), p297927961))), FunCall(Transpose(), p450003680))))))))

    // Add and simplify copies
    val f1 = Rewrite.applyRuleAtId(f0, 64, Rules.addIdForCurrentValueInReduce)
    val f2 = Rewrite.applyRuleAtId(f1, 31, Rules.addIdForCurrentValueInReduce)
    val f3 = Rewrite.applyRuleAtId(f2, 46, Rules.implementIdAsDeepCopy)
    val f4 = Rewrite.applyRuleAtId(f3, 93, Rules.implementOneLevelOfId)
    val f5 = Rewrite.applyRuleAtId(f4, 99, Rules.dropId)
    val f6 = Rewrite.applyRuleAtId(f5, 96, Rules.implementIdAsDeepCopy)
    val f7 = Rewrite.applyRuleAtId(f6, 105, Rules.addCopy)
    val f8 = Rewrite.applyRuleAtId(f7, 48, Rules.tupleMap)
    val f9 = Rewrite.applyRuleAtId(f8, 55, Rules.tupleMap)

    // Lower to OpenCL execution model
    val f10 = Lower.lowerNextLevelWithRule(f9, Rules.mapWrg(0))
    val f11 = Lower.lowerNextLevelWithRule(f10, Rules.mapWrg(1))
    val f12 = Lower.lowerNextLevelWithRule(f11, Rules.mapLcl(1))
    val f13 = Lower.lowerNextLevelWithRule(f12, Rules.mapLcl(0))
    val f14 = Lower.lowerNextLevelWithRule(f13, Rules.mapSeq)
    val f15 = Lower.lowerNextLevelWithRule(f14, Rules.mapSeq)

    // Lower to OpenCL memory model
    val f16 = Rewrite.applyRuleAtId(f15, 49, Rules.localMemory)
    val f17 = Rewrite.applyRuleAtId(f16, 114, Rules.privateMemory)
    val f18 = Rewrite.applyRuleAtId(f17, 104, Rules.privateMemory)

    val mSize = 256
    val kSize = 256
    val nSize = 256
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val values = Seq(matrixA, matrixB)

    val (localRange, globalRange) = InferNDRange(f18, values:_*)

    val (output: Array[Float], _) = Execute(localRange(0).eval, localRange(1).eval,
      globalRange(0).eval, globalRange(1).eval, (true, true))(f18, values:_*)

    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB)

    assertArrayEquals(gold.flatten, output, 0.0001f)
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

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.tileInputAndOutput(tileSizeMN, tileSizeK))

    // Experimenting from here on

    val f7 = Rewrite.applyRuleAtId(f1, 23, Rules.splitJoin(workPerThreadN))
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
    val f18 = Rewrite.applyRuleAtId(f17, 59, Rules.mapReduceInterchange)
    val f19 = Rewrite.applyRuleAtId(f18, 53, Rules.mapFission)
    val f20 = Rewrite.applyRuleAtId(f19, 54, Rules.mapFission)
    val f21 = Rewrite.applyRuleAtId(f20, 54, Rules.mapReduceInterchange)
    val f22 = Rewrite.applyRuleAtId(f21, 13, MacroRules.mapFissionAtPosition(1))
    val f23 = Rewrite.applyRuleAtId(f22, 14, Rules.mapFission)
    val f24 = Rewrite.applyRuleAtId(f23, 14, Rules.mapReduceInterchange)

    // Output seems good

    val f25 = Rewrite.applyRuleAtId(f24, 65, MacroRules.mapFissionAtPosition(1))
    val f26 = Rewrite.applyRuleAtId(f25, 65, Rules.transposeBothSides)
    val f27 = Rewrite.applyRuleAtId(f26, 64, Rules.transposeTransposeId)
    val f28 = Rewrite.applyRuleAtId(f27, 65, Rules.transposeMapSplit)
    val f29 = Rewrite.applyRuleAtId(f28, 64, Rules.mapFusion)
    val f30 = Rewrite.applyRuleAtId(f29, 58, Rules.mapFission)
    val f31 = Rewrite.applyRuleAtId(f30, 59, Rules.mapFission)
    val f32 = Rewrite.applyRuleAtId(f31, 60, Rules.mapTransposeTransposeMapTranspose)
    val f33 = Rewrite.applyRuleAtId(f32, 59, Rules.mapSplitTranspose)
    val f34 = Rewrite.applyRuleAtId(f33, 58, Rules.transposeBothSides)
    val f35 = Rewrite.applyRuleAtId(f34, 57, Rules.transposeTransposeId)
    val f36 = Rewrite.applyRuleAtId(f35, 58, Rules.transposeTransposeId)

    val f37 = Rewrite.applyRuleAtId(f36, 17, Rules.mapFission)
    val f38 = Rewrite.applyRuleAtId(f37, 17, Rules.transposeBothSides)
    val f39 = Rewrite.applyRuleAtId(f38, 16, Rules.transposeTransposeId)

    // Now there's a massive bunch of transposes, splits and joins, which are id.
    // Can comment out and runs fine. Otherwise good.

    val f40 = Rewrite.applyRuleAtId(f39, 18, Rules.mapFission)
    val f41 = Rewrite.applyRuleAtId(f40, 17, MacroRules.transposeMapMapTranspose)
    val f42 = Rewrite.applyRuleAtId(f41, 19, Rules.mapFission)
    val f43 = Rewrite.applyRuleAtId(f42, 20, Rules.mapFission)
    val f44 = Rewrite.applyRuleAtId(f43, 21, Rules.mapTransposeSplit)
    val f45 = Rewrite.applyRuleAtId(f44, 23, Rules.transposeTransposeId)
    val f46 = Rewrite.applyRuleAtId(f45, 18, Rules.transposeMapSplit)
    val f47 = Rewrite.applyRuleAtId(f46, 20, MacroRules.transposeMapMapTranspose)
    val f48 = Rewrite.applyRuleAtId(f47, 21, Rules.transposeTransposeId)
    val f49 = Rewrite.applyRuleAtId(f48, 20, Rules.splitJoin(tileSizeK))
    val f50 = Rewrite.applyRuleAtId(f49, 19, Rules.splitJoinId)
    val f51 = Rewrite.applyRuleAtId(f50, 21, Rules.splitJoin(tileSizeK))
    val f52 = Rewrite.applyRuleAtId(f51, 20, Rules.splitJoinId)
    val f53 = Rewrite.applyRuleAtId(f52, 21, Rules.splitJoinId)

    // Splits & joins eliminated, just transposes left

    val f54 = Rewrite.applyRuleAtId(f53, 20, Rules.mapFusion)
    val f55 = Rewrite.applyRuleAtId(f54, 28, Rules.mapSplitTranspose)
    val f56 = Rewrite.applyRuleAtId(f55, 30, Rules.splitJoinId)
    val f57 = Rewrite.applyRuleAtId(f56, 17, Rules.mapFusion)
    val f58 = Rewrite.applyRuleAtId(f57, 17, Rules.mapFusion)
    val f59 = Rewrite.applyRuleAtId(f58, 17, Rules.mapFusion)
    val f60 = Rewrite.applyRuleAtId(f59, 26, MacroRules.transposeMapMapTranspose)
    val f61 = Rewrite.applyRuleAtId(f60, 27, Rules.transposeTransposeId)
    val f62 = Rewrite.applyRuleAtId(f61, 25, Rules.mapFusion)
    val f63 = Rewrite.applyRuleAtId(f62, 25, Rules.mapFusion)
    val f64 = Rewrite.applyRuleAtId(f63, 33, Rules.mapFission)
    val f65 = Rewrite.applyRuleAtId(f64, 31, Rules.mapTransposeTransposeMapTranspose)
    val f66 = Rewrite.applyRuleAtId(f65, 25, Rules.mapFusion)
    val f67 = Rewrite.applyRuleAtId(f66, 32, Rules.transposeTransposeId)
    val f68 = Rewrite.applyRuleAtId(f67, 31, Rules.mapFusion)
    val f69 = Rewrite.applyRuleAtId(f68, 49, Rules.transposeTransposeId)
    val f70 = Rewrite.applyRuleAtId(f69, 31, Rules.removeEmptyMap)
    val f71 = Rewrite.applyRuleAtId(f70, 30, Rules.transposeTransposeId)

    // Most transposes eliminated, just one left

    val f72 = Rewrite.applyRuleAtId(f71, 54, Rules.partialReduceToReduce)
    val f73 = Rewrite.applyRuleAtId(f72, 51, Rules.mapReduceInterchange)
    val f74 = Rewrite.applyRuleAtId(f73, 53, Rules.transposeTransposeId)

    // All unwanted transposes eliminated, start fusing

    val f75 = Rewrite.applyRuleAtId(f74, 16, Rules.mapFusion)
    val f76 = Rewrite.applyRuleAtId(f75, 24, Rules.mapFusion)
    val f77 = Rewrite.applyRuleAtId(f76, 29, Rules.mapFusion)
    val f78 = Rewrite.applyRuleAtId(f77, 35, MacroRules.reduceMapFusion)
    val f79 = Rewrite.applyRuleAtId(f78, 15, MacroRules.reduceMapFusion)

    println(f79)
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

    val f1 = Rewrite.applyRuleAtId(f, 0, MacroRules.tileTranspose(x, y))

    // Add the copy
    val f4 = Rewrite.applyRuleAtId(f1, 17, Rules.addCopy)

    // Lower to OpenCL execution model
    val fw0 = Lower.lowerNextLevelWithRule(f4, Rules.mapWrg(0))
    val fw1 = Lower.lowerNextLevelWithRule(fw0, Rules.mapWrg(1))
    val fl1 = Lower.lowerNextLevelWithRule(fw1, Rules.mapLcl(1))
    val fl0 = Lower.lowerNextLevelWithRule(fl1, Rules.mapLcl(0))

    // Lower to OpenCL memory model
    val f11 = Rewrite.applyRuleAtId(fl0, 16, Rules.globalMemory)
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
  def mmSquareTilesLocalMemory(): Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f0 = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, K), N), (p1538399081, p1800890735) => FunCall(Join(), FunCall(Map(fun((p1957502751) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1177377518) => FunCall(TransposeW(), FunCall(Map(fun((p1122805102) => FunCall(TransposeW(), p1122805102))), FunCall(TransposeW(), FunCall(toGlobal(fun((p183284570) => FunCall(MapSeq(fun((p2109874862) => FunCall(Map(fun((p275310919) => FunCall(Map(fun((p797925218) => FunCall(id, p797925218))), p275310919))), p2109874862))), p183284570))), FunCall(ReduceSeq(fun((p1136497418, p1943325854) => FunCall(Map(fun((p1413378318) => FunCall(Map(fun((p1181869371) => FunCall(add, FunCall(Get(0), p1181869371), FunCall(Get(1), p1181869371)))), FunCall(Zip(2), FunCall(Get(0), p1413378318), FunCall(Join(), FunCall(Map(fun((p1256728724) => FunCall(toPrivate(fun((p1157058691) => FunCall(MapSeq(fun((p1667689440) => FunCall(id, p1667689440))), p1157058691))), FunCall(ReduceSeq(fun((p852687460, p1138193439) => FunCall(add, p852687460, FunCall(mult, FunCall(Get(0), p1138193439), FunCall(Get(1), p1138193439))))), FunCall(id, Value(0.0f, Float)), FunCall(Zip(2), FunCall(Get(1), p1413378318), p1256728724))))), FunCall(Transpose(), FunCall(Get(1), p1943325854)))))))), FunCall(Zip(2), p1136497418, FunCall(Transpose(), FunCall(Get(0), p1943325854)))))), FunCall(Map(fun((p194706439) => FunCall(Map(fun((p1686369710) => FunCall(id, p1686369710))), p194706439))), Value(0.0f, ArrayType(ArrayType(Float, 4), 4))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p1957502751)), FunCall(Split(4), FunCall(Transpose(), p1177377518)))))))))), FunCall(Split(4), p1800890735)))))), FunCall(Split(4), p1538399081))))

    // Add a copy
    val f1 = Rewrite.applyRuleAtId(f0, 13, Rules.addIdForCurrentValueInReduce)
    val f2 = Rewrite.applyRuleAtId(f1, 28, Rules.implementIdAsDeepCopy)

    // Lower to OpenCL execution model
    val f3 = Lower.lowerNextLevelWithRule(f2, Rules.mapWrg(0))
    val f4 = Lower.lowerNextLevelWithRule(f3, Rules.mapWrg(1))

    val f5 = Lower.lowerNextLevelWithRule(f4, Rules.mapLcl(1))
    val f6 = Lower.lowerNextLevelWithRule(f5, Rules.mapLcl(0))

    // Lower to OpenCL memory model
    val f7 = Rewrite.applyRuleAtId(f6, 38, Rules.localMemory)
    val f8 = Rewrite.applyRuleAtId(f7, 31, Rules.localMemory)

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val (output: Array[Float], _) = Execute(4, 4, mSize, kSize, (true, true))(f8, matrixA, matrixB.transpose)

    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB)

    assertArrayEquals(gold.flatten, output, 0.0001f)
  }

  @Test
  def rectangularTiles(): Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    // Derivation the same, except split on the zip is different than the others
    val h0 = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, N), K), (p951880373, p1752203484) => FunCall(Join(), FunCall(Map(fun((p243745864) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p699780352) => FunCall(toGlobal(fun((p1613255205) => FunCall(MapSeq(fun((p1897115967) => FunCall(Map(fun((p1166151249) => FunCall(Map(fun((p1121453612) => FunCall(id, p1121453612))), p1166151249))), p1897115967))), p1613255205))), FunCall(ReduceSeq(fun((p2619171, p1728790703) => FunCall(Map(fun((p1227074340) => FunCall(Map(fun((p1154002927) => FunCall(add, FunCall(Get(0), p1154002927), FunCall(Get(1), p1154002927)))), FunCall(Zip(2), FunCall(Get(0), p1227074340), FunCall(Get(1), p1227074340))))), FunCall(Zip(2), p2619171, FunCall(fun((p2070529722) => FunCall(Map(fun((p1188753216) => FunCall(Join(), FunCall(Map(fun((p317986356) => FunCall(toPrivate(fun((p331510866) => FunCall(MapSeq(fun((p640363654) => FunCall(id, p640363654))), p331510866))), FunCall(ReduceSeq(fun((p924477420, p99451533) => FunCall(add, p924477420, FunCall(fun((p84739718) => FunCall(mult, FunCall(Get(0), p84739718), FunCall(Get(1), p84739718))), p99451533)))), FunCall(toPrivate(id), Value(0.0f, Float)), FunCall(Zip(2), p317986356, p1188753216))))), FunCall(Transpose(), FunCall(Get(0), p2070529722)))))), FunCall(Transpose(), FunCall(Get(1), p2070529722)))), p1728790703))))), FunCall(toPrivate(Map(fun((p2050835901) => FunCall(Map(fun((p511473681) => FunCall(id, p511473681))), p2050835901)))), Value(0.0f, ArrayType(ArrayType(Float, 8), 8))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p243745864)), FunCall(Split(4), FunCall(Transpose(), p699780352))))))), FunCall(Split(8), FunCall(Transpose(), p1752203484))))))), FunCall(Split(8), p951880373))))

    val h1 = Rewrite.applyRuleAtId(h0, 1, MacroRules.finishRectangularTiles)

    println(h1)

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
    val f6 = Rewrite.applyRuleAtId(f5, 7, MacroRules.reduceMapFusion)

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

    val f1 = Rewrite.applyRuleAtId(f0, 2, Rules.reorderBothSidesWithStride)
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
    val f16 = Rewrite.applyRuleAtId(f15, 14, MacroRules.reduceMapFusion)

    println(f16)
  }

  @Test
  def mmTiled() = {
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

    val tileSizeMN = 32
    val tileSizeK = 32

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.tileInputAndOutput(tileSizeMN, tileSizeK))

    val f2 = Rewrite.applyRuleAtId(f1, 6, Rules.mapFissionWithZipInside)
    val f3 = Rewrite.applyRuleAtId(f2, 7, MacroRules.moveTransposeInsideTiling)

    // Split Reduce in 2
    val f4 = Rewrite.applyRuleAtId(f3, 48, Rules.partialReduce)
    val f5 = Rewrite.applyRuleAtId(f4, 49, Rules.partialReduceSplitJoin(tileSizeK))

    // Pull out the last one
    val f6 = Rewrite.applyRuleAtId(f5, 45, MacroRules.mapFissionAtPosition(2))
    val f7 = Rewrite.applyRuleAtId(f6, 45, Rules.mapReducePartialReduce)
    val f8 = Rewrite.applyRuleAtId(f7, 17, Rules.mapFission)
    val f9 = Rewrite.applyRuleAtId(f8, 18, Rules.mapFission)
    val f10 = Rewrite.applyRuleAtId(f9, 18, Rules.mapReduceInterchange)


    // Move split from partial reduce out and eliminate transposes on the left hand side
    val f11 = Rewrite.applyRuleAtId(f10, 50, Rules.mapSplitTranspose)
    val f12 = Rewrite.applyRuleAtId(f11, 49, Rules.transposeTransposeId)
    val f13 = Rewrite.applyRuleAtId(f12, 21, Rules.mapFission)

    // TODO: Swapping the following 2 gives an incorrect result...
    val f14 = Rewrite.applyRuleAtId(f13, 21, Rules.transposeBothSides)
    val f15 = Rewrite.applyRuleAtId(f14, 24, Rules.mapSplitTranspose)

    lowerAndExecute(f1)
    lowerAndExecute(f2)
    lowerAndExecute(f3)
    lowerAndExecute(f4)
    lowerAndExecute(f5)
    lowerAndExecute(f6)
    lowerAndExecute(f7)
    lowerAndExecute(f8)
    lowerAndExecute(f9)
    lowerAndExecute(f10)
    lowerAndExecute(f11)
    lowerAndExecute(f12)
    lowerAndExecute(f13)
    lowerAndExecute(f14)
    lowerAndExecute(f15)
  }

  def lowerAndExecute(lambda: Lambda): Unit = {
    val lowered = Lower.lowerNoAddressSpaces(lambda)

    val mSize = 512
    val kSize = 512
    val nSize = 512
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val values = Seq(matrixA, matrixB)

    val (localRange, globalRange) = InferNDRange(lowered, values:_*)

    val (output: Array[Float], runtime) = Execute(localRange(0).eval, localRange(1).eval,
      globalRange(0).eval, globalRange(1).eval, (true, true))(lowered, values:_*)

    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB)

    assertArrayEquals(gold.flatten, output, 0.0001f)

    println(runtime)
  }

}
