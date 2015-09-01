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
    val workPerThreadN = 4
    val workPerThreadM = 8

    val tileTranspositionM = 32
    val tileTranspositionK = 16

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.tileInputAndOutput(tileSizeMN, tileSizeK))

    val g0 = Rewrite.applyRuleAtId(f1, 44, Rules.partialReduce)
    val g1 = Rewrite.applyRuleAtId(g0, 45, Rules.partialReduceSplitJoin(tileSizeK))

    // Experimenting

    val f7 = Rewrite.applyRuleAtId(g1, 23, MacroRules.apply2DRegisterBlocking(workPerThreadN, workPerThreadM))

    // Input's good

    val f16 = Rewrite.applyRuleAtId(f7, 11, MacroRules.apply2DRegisterBlocking(workPerThreadN, workPerThreadM))

    val f34 = Rewrite.applyRuleAtId(f16, 66, MacroRules.moveReduceOutOneLevel)
    val f37 = Rewrite.applyRuleAtId(f34, 13, MacroRules.moveReduceOutOneLevel)

    val f38 = Rewrite.applyRuleAtId(f37, 110, Rules.partialReduceToReduce)

    val f39 = Rewrite.applyRuleAtId(f38, 104, MacroRules.moveReduceOutOneLevel)
    val f42 = Rewrite.applyRuleAtId(f39, 97, MacroRules.moveReduceOutOneLevel)

    // Output's good, nested reduces use same memory, performance drops by a third for NVIDIA and
    // half for AMD if not.
    // Can be brought into a form, where a large chunk (splits, joins, transposes and reorders)
    // can be commented out and runs fine.

    val numExpressionsIntermediate = NumberExpression.breadthFirst(f42).values.max

    assertEquals(138, numExpressionsIntermediate)


    val f43 = Rewrite.applyRuleAtId(f42, 18, Rules.splitTranspose)
    val f44 = Rewrite.applyRuleAtId(f43, 20, Rules.splitJoin(tileSizeK))
    val f45 = Rewrite.applyRuleAtId(f44, 22, Rules.splitJoinId)
    val f46 = Rewrite.applyRuleAtId(f45, 21, Rules.mapFusion)
    val f47 = Rewrite.applyRuleAtId(f46, 29, Rules.mapSplitTranspose)
    val f48 = Rewrite.applyRuleAtId(f47, 32, Rules.splitJoin(workPerThreadN))
    val f49 = Rewrite.applyRuleAtId(f48, 31, Rules.splitJoinId)
    val f50 = Rewrite.applyRuleAtId(f49, 32, Rules.splitJoinId)

    println(NumberPrinter(f50))

    // TODO: Find missing derivation
    // Continuing from where the splits, joins, transposes and reorders have been eliminated.
    // Obviously possible, don't yet know how to.
    val h0 = fun(ArrayType(ArrayType(Float, K), M), ArrayType(ArrayType(Float, N), K),(p1418385211, p1282811396) => FunCall(Join(), FunCall(Map(fun((p789219251) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p832279283) => FunCall(TransposeW(), FunCall(Map(fun((p265119009) => FunCall(Scatter(ReorderWithStride(tileSizeMN/workPerThreadM)), p265119009))), FunCall(Join(), FunCall(Map(fun((p2050404090) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p388043093) => FunCall(TransposeW(), p388043093))), p2050404090))))), FunCall(Map(fun((p188576144) => FunCall(Map(fun((p1608230649) => FunCall(Map(fun((p282432134) => FunCall(TransposeW(), p282432134))), FunCall(TransposeW(), p1608230649)))), FunCall(TransposeW(), p188576144)))), FunCall(TransposeW(), FunCall(Reduce(fun((p266437232, p1873859565) => FunCall(Map(fun((p1843289228) => FunCall(Map(fun((p1361289747) => FunCall(Map(fun((p1381128261) => FunCall(Join(), FunCall(Transpose(), p1381128261)))), FunCall(Transpose(), FunCall(Reduce(fun((p999609945, p615634843) => FunCall(Map(fun((p1758386724) => FunCall(Map(fun((p673068808) => FunCall(add, FunCall(Get(0), p673068808), FunCall(Get(1), p673068808)))), FunCall(Zip(2), FunCall(Get(0), p1758386724), FunCall(Get(1), p1758386724))))), FunCall(Zip(2), p999609945, p615634843)))), FunCall(Get(0), p1361289747), FunCall(Transpose(), FunCall(Map(fun((p900008524) => FunCall(Transpose(), p900008524))), FunCall(Get(1), p1361289747)))))))), FunCall(Zip(2), FunCall(Get(0), p1843289228), FunCall(Get(1), p1843289228))))), FunCall(Zip(2), p266437232, p1873859565)))), FunCall(Map(fun((p520232556) => FunCall(Map(fun((p17037394) => FunCall(Map(fun((p1484531981) => FunCall(Map(fun((p1159114532) => FunCall(id, p1159114532))), p1484531981))), p17037394))), p520232556))), Value(0.0f, ArrayType(ArrayType(ArrayType(ArrayType(Float, workPerThreadM), workPerThreadN), tileSizeMN/workPerThreadM), tileSizeMN/workPerThreadN))), FunCall(Map(fun((p1256728724) => FunCall(Map(fun((p1412925683) => FunCall(Map(fun((p1832580921) => FunCall(Map(fun((p497359413) => FunCall(TransposeW(), p497359413))), FunCall(TransposeW(), p1832580921)))), FunCall(Map(fun((p369241501) => FunCall(Map(fun((p2124046270) => FunCall(Map(fun((p1151593579) => FunCall(Map(fun((p1902260856) => FunCall(mult, p1151593579, p1902260856))), FunCall(Get(1), p2124046270)))), FunCall(Get(0), p2124046270)))), FunCall(Zip(2), FunCall(Transpose(), p1412925683), FunCall(Transpose(), p369241501))))), FunCall(Split(workPerThreadM), FunCall(Gather(ReorderWithStride(tileSizeMN/workPerThreadM)), FunCall(Transpose(), FunCall(Get(1), p1256728724)))))))), FunCall(Split(workPerThreadN), FunCall(Transpose(), FunCall(Get(0), p1256728724)))))), FunCall(Zip(2), FunCall(Split(tileSizeK), FunCall(Transpose(), p789219251)), FunCall(Split(tileSizeK), FunCall(Transpose(), p832279283))))))))))))), FunCall(Split(tileSizeMN), FunCall(Transpose(), p1282811396))))))), FunCall(Split(tileSizeMN), p1418385211))))

    val h9 = SimplifyAndFuse(h0)

    // Final steps, move transpose inside tiling + tiling (kernel) for A

    val h15 = Rewrite.applyRuleAtId(h9, 1, MacroRules.finishRectangularTiles)

    // Tile the transposition

    val h16 = Rewrite.applyRuleAtId(h15, 5, Rules.addCopy)
    val h17 = Rewrite.applyRuleAtId(h16, 6, MacroRules.tileTranspose(tileTranspositionM, tileTranspositionK))

    val numExpressionsFinal = NumberExpression.breadthFirst(h17).values.max

    assertEquals(134, numExpressionsFinal)
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
    val f15 = Lower.simpleMapLoweringStrategy(f9)

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
  def simpleLowering(): Unit = {
    val tileSizeMN = 128
    val tileSizeK = 8
    val workPerThreadN = 2
    val workPerThreadM = 8

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val h0 = fun(ArrayType(ArrayType(Float, M), K), ArrayType(ArrayType(Float, N), K),(p1795960102, p477289012) => FunCall(Join(), FunCall(Map(fun((p1889248251) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p2023938592) => FunCall(TransposeW(), FunCall(Map(fun((p225290371) => FunCall(Scatter(ReorderWithStride(tileSizeMN/workPerThreadM)), p225290371))), FunCall(Join(), FunCall(Map(fun((p297927961) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p733672688) => FunCall(TransposeW(), FunCall(Map(fun((p756185697) => FunCall(TransposeW(), p756185697))), FunCall(TransposeW(), p733672688))))), FunCall(TransposeW(), p297927961)))))), FunCall(TransposeW(), FunCall(MapSeq(fun((p1691875296) => FunCall(Id(), p1691875296))), FunCall(ReduceSeq(fun((p500179317, p1225197672) => FunCall(Map(fun((p1500608548) => FunCall(Map(fun((p513700442) => FunCall(Map(fun((p912011468) => FunCall(Join(), FunCall(Transpose(), p912011468)))), FunCall(Transpose(), FunCall(MapSeq(fun((p1195067075) => FunCall(Id(), p1195067075))), FunCall(ReduceSeq(fun((p1983025922, p1007309018) => FunCall(Map(fun((p2038148563) => FunCall(Map(fun((p2142080121) => FunCall(add, FunCall(Get(0), p2142080121), FunCall(Get(1), p2142080121)))), FunCall(Zip(2), FunCall(Get(0), p2038148563), FunCall(Map(fun((p112619572) => FunCall(mult, FunCall(Get(1), p2038148563), p112619572))), FunCall(Get(1), p1007309018)))))), FunCall(Zip(2), p1983025922, FunCall(Get(0), p1007309018))))), FunCall(Get(0), p513700442), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p1500608548)), FunCall(Transpose(), FunCall(Get(1), p513700442))))))))), FunCall(Zip(2), FunCall(Get(0), p1500608548), FunCall(Split(workPerThreadM), FunCall(Gather(ReorderWithStride(tileSizeMN/workPerThreadM)), FunCall(Transpose(), FunCall(Get(1), p1225197672)))))))), FunCall(Zip(2), p500179317, FunCall(Split(workPerThreadN), FunCall(Transpose(), FunCall(Get(0), p1225197672))))))), FunCall(Map(fun((p1786364562) => FunCall(Map(fun((p326298949) => FunCall(Map(fun((p876926621) => FunCall(Map(fun((p1268959798) => FunCall(id, p1268959798))), p876926621))), p326298949))), p1786364562))), Value(0.0f, ArrayType(ArrayType(ArrayType(ArrayType(Float, workPerThreadM), workPerThreadN), tileSizeMN/workPerThreadM), tileSizeMN/workPerThreadN))), FunCall(Zip(2), p1889248251, p2023938592)))))))))), FunCall(Transpose(), FunCall(Map(fun((p1935972447) => FunCall(Transpose(), p1935972447))), FunCall(Split(tileSizeK), FunCall(Map(fun((p1890627974) => FunCall(Split(tileSizeMN), p1890627974))), p477289012))))))))), FunCall(Transpose(), FunCall(Map(fun((p1641313620) => FunCall(Transpose(), p1641313620))), FunCall(Split(tileSizeK), FunCall(Map(fun((p192881625) => FunCall(Split(tileSizeMN), p192881625))), p1795960102)))))))
    val h1 = Lower.lowerNoAddressSpaces(h0)

    val mSize = 256
    val kSize = 256
    val nSize = 256
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val values = Seq(matrixA.transpose, matrixB)

    val (localRange, globalRange) = InferNDRange(h1, values:_*)

    val (output: Array[Float], _) = Execute(localRange(0).eval, localRange(1).eval,
      globalRange(0).eval, globalRange(1).eval, (true, true))(h1, values:_*)

    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB)

    assertArrayEquals(gold.flatten, output, 0.0001f)


    val tileSizeMNVar = Var("tileSizeMNVar")
    val tileSizeKVar = Var("tileSizeKVar")
    val workPerThreadNVar = Var("workPerThreadNVar")
    val workPerThreadMVar = Var("workPerThreadMVar")

    val f0 = fun(ArrayType(ArrayType(Float, M), K), ArrayType(ArrayType(Float, N), K),(p1795960102, p477289012) => FunCall(Join(), FunCall(Map(fun((p1889248251) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p2023938592) => FunCall(TransposeW(), FunCall(Map(fun((p225290371) => FunCall(Scatter(ReorderWithStride(tileSizeMNVar/^workPerThreadMVar)), p225290371))), FunCall(Join(), FunCall(Map(fun((p297927961) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p733672688) => FunCall(TransposeW(), FunCall(Map(fun((p756185697) => FunCall(TransposeW(), p756185697))), FunCall(TransposeW(), p733672688))))), FunCall(TransposeW(), p297927961)))))), FunCall(TransposeW(), FunCall(MapSeq(fun((p1691875296) => FunCall(Id(), p1691875296))), FunCall(ReduceSeq(fun((p500179317, p1225197672) => FunCall(Map(fun((p1500608548) => FunCall(Map(fun((p513700442) => FunCall(Map(fun((p912011468) => FunCall(Join(), FunCall(Transpose(), p912011468)))), FunCall(Transpose(), FunCall(MapSeq(fun((p1195067075) => FunCall(Id(), p1195067075))), FunCall(ReduceSeq(fun((p1983025922, p1007309018) => FunCall(Map(fun((p2038148563) => FunCall(Map(fun((p2142080121) => FunCall(add, FunCall(Get(0), p2142080121), FunCall(Get(1), p2142080121)))), FunCall(Zip(2), FunCall(Get(0), p2038148563), FunCall(Map(fun((p112619572) => FunCall(mult, FunCall(Get(1), p2038148563), p112619572))), FunCall(Get(1), p1007309018)))))), FunCall(Zip(2), p1983025922, FunCall(Get(0), p1007309018))))), FunCall(Get(0), p513700442), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p1500608548)), FunCall(Transpose(), FunCall(Get(1), p513700442))))))))), FunCall(Zip(2), FunCall(Get(0), p1500608548), FunCall(Split(workPerThreadMVar), FunCall(Gather(ReorderWithStride(tileSizeMNVar/^workPerThreadMVar)), FunCall(Transpose(), FunCall(Get(1), p1225197672)))))))), FunCall(Zip(2), p500179317, FunCall(Split(workPerThreadNVar), FunCall(Transpose(), FunCall(Get(0), p1225197672))))))), FunCall(Map(fun((p1786364562) => FunCall(Map(fun((p326298949) => FunCall(Map(fun((p876926621) => FunCall(Map(fun((p1268959798) => FunCall(id, p1268959798))), p876926621))), p326298949))), p1786364562))), Value(0.0f, ArrayType(ArrayType(ArrayType(ArrayType(Float, workPerThreadMVar), workPerThreadNVar), tileSizeMNVar/^workPerThreadMVar), tileSizeMNVar/^workPerThreadNVar))), FunCall(Zip(2), p1889248251, p2023938592)))))))))), FunCall(Transpose(), FunCall(Map(fun((p1935972447) => FunCall(Transpose(), p1935972447))), FunCall(Split(tileSizeKVar), FunCall(Map(fun((p1890627974) => FunCall(Split(tileSizeMNVar), p1890627974))), p477289012))))))))), FunCall(Transpose(), FunCall(Map(fun((p1641313620) => FunCall(Transpose(), p1641313620))), FunCall(Split(tileSizeKVar), FunCall(Map(fun((p192881625) => FunCall(Split(tileSizeMNVar), p192881625))), p1795960102)))))))

    TypeChecker(f0)
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

    val f11 = Rewrite.applyRuleAtId(f1, 44, Rules.partialReduce)
    val f12 = Rewrite.applyRuleAtId(f11, 45, Rules.partialReduceSplitJoin(tileSizeK))

    // Experimenting from here on

    val f8 = Rewrite.applyRuleAtId(f12, 23, MacroRules.apply1DRegisterBlocking(workPerThreadN))

    // Input's good

    val h0 = Rewrite.applyRuleAtId(f8, 11, MacroRules.apply1DRegisterBlocking(workPerThreadN))

    val h1 = Rewrite.applyRuleAtId(h0, 53, MacroRules.moveReduceOutOneLevel)
    val h2 = Rewrite.applyRuleAtId(h1, 12, MacroRules.moveReduceOutOneLevel)

    val g0 = Rewrite.applyRuleAtId(h2, 86, Rules.partialReduceToReduce)
    val g1 = Rewrite.applyRuleAtId(g0, 80, Rules.mapReduceInterchangeWithZipOutside)

    // Now there's a massive bunch of transposes, splits and joins, which are id.
    // Can comment out and runs fine. Otherwise good.

    val h3 = Rewrite.applyRuleAtId(g1, 63, Rules.transposeMapSplit)

    // TODO: macro?
    val h4 = Rewrite.applyRuleAtId(h3, 57, Rules.mapFission)
    val h5 = Rewrite.applyRuleAtId(h4, 58, Rules.mapFission)
    val h6 = Rewrite.applyRuleAtId(h5, 59, Rules.mapTransposeTransposeMapTranspose)
    val h7 = Rewrite.applyRuleAtId(h6, 58, Rules.mapSplitTranspose)

    val h8 = Rewrite.applyRuleAtId(h7, 57, Rules.transposeBothSides)
    val h9 = Rewrite.applyRuleAtId(h8, 56, Rules.transposeTransposeId)
    val h10 = Rewrite.applyRuleAtId(h9, 57, Rules.transposeTransposeId)

    // TODO: macro?
    val h11 = Rewrite.applyRuleAtId(h10, 16, Rules.mapFission)
    val h12 = Rewrite.applyRuleAtId(h11, 16, Rules.transposeBothSides)

    val h13 = Rewrite.applyRuleAtId(h12, 15, Rules.transposeTransposeId)

    val f41 = Rewrite.applyRuleAtId(h13, 16, MacroRules.transposeMapMapTransposeFission)

    // TODO: macro?
    val f42 = Rewrite.applyRuleAtId(f41, 18, Rules.mapFission)
    val f43 = Rewrite.applyRuleAtId(f42, 19, Rules.mapFission)
    val f44 = Rewrite.applyRuleAtId(f43, 20, Rules.mapTransposeSplit)

    val f79 = SimplifyAndFuse(f44)

    println(f79)

    val numExpressions = NumberExpression.breadthFirst(f79).values.max
    assertEquals(81, numExpressions)
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
    val fl0 = Lower.simpleMapLoweringStrategy(f4)

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
    val f6 = Lower.simpleMapLoweringStrategy(f2)

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
    val f2 = Rewrite.applyRuleAtId(f1, 6, MacroRules.moveReduceOutOneLevel)
    val f4 = Rewrite.applyRuleAtId(f2, 9, Rules.mapMapTransposeZipInside)
    val f6 = SimplifyAndFuse(f4)

    println(f6)
  }

  @Test
  def mmReuseB(): Unit = {
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

    val f2 = Rewrite.applyRuleAtId(f0, 0, MacroRules.apply1DRegisterBlocking)
    val f5 = SimplifyAndFuse(f2)

    println(f5)
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

    val f14 = Rewrite.applyRuleAtId(f0, 0, MacroRules.apply2DRegisterBlocking)
    val f16 = SimplifyAndFuse(f14)

    val numExpressions = NumberExpression.breadthFirst(f14).values.max
    assertEquals(55, numExpressions)
    println(f16.body)
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
    val f6 = Rewrite.applyRuleAtId(f5, 45, MacroRules.moveReduceOutOneLevel)
    val f8 = Rewrite.applyRuleAtId(f6, 17, MacroRules.moveReduceOutOneLevel)

    // Move split from partial reduce out and eliminate transposes on the left hand side
    val f11 = Rewrite.applyRuleAtId(f8, 50, Rules.mapSplitTranspose)
    val f12 = Rewrite.applyRuleAtId(f11, 49, Rules.transposeTransposeId)

    // TODO: macro?
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
    lowerAndExecute(f8)
    lowerAndExecute(f11)
    lowerAndExecute(f12)
    lowerAndExecute(f13)
    lowerAndExecute(f14)
    lowerAndExecute(f15)
  }

  def lowerAndExecute(lambda: Lambda): Unit = {
    val lowered = Lower.lowerNoAddressSpaces(lambda)

    val mSize = 256
    val kSize = 256
    val nSize = 256
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
