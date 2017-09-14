package rewriting

import exploration.HighLevelRewrite
import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._
import rewriting.rules._
import rewriting.utils.NumberExpression

object TestRewriteMatrixMatrix extends TestWithExecutor

class TestRewriteMatrixMatrix {

  def checkDepth(lambda: Lambda, ruleSeq: Seq[rules.Rule] = Seq()): Unit =
    assertTrue("Depth filter.", HighLevelRewrite.filterByDepth(lambda, ruleSeq))

  def checkDistance(lambda: Lambda): Unit =
    assertTrue("Distance filter.", HighLevelRewrite.filterByDistance(lambda))

  val N = SizeVar("N")
  val M = SizeVar("M")
  val K = SizeVar("K")

  @Test
  def reuseBothWithTiling(): Unit = {

    LongTestsEnabled()

    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        Map(fun(aRow =>
          Map(fun(bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
          )) o Transpose() $ B
        )) $ A
      })

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.tileMapMap)
    val f2 = Rewrite.applyRuleAtId(f1, 12, MacroRules.finishTiling)
    val f3 = Rewrite.applyRuleAtId(f2, 24, MacroRules.apply2DRegisterBlocking)
    val f4 = Rewrite.applyRuleAtId(f3, 11, MacroRules.apply2DRegisterBlocking)
    val f5 = Rewrite.applyRuleAtId(f4, 13, MacroRules.finishTiling)
    val f6 = Rewrite.applyRuleAtId(f5, 98, ReduceRules.partialReduceToReduce)
    val f7 = SimplifyAndFuse(f6)

    // Final steps, move transpose inside tiling + tiling (kernel) for A
    val f8 = Rewrite.applyRuleAtId(f7, 1, MacroRules.finishRectangularTiles)

    // Tile the transposition
    val f9 = Rewrite.applyRuleAtId(f8, 5, CopyRules.addCopy)
    val f10 = Rewrite.applyRuleAtId(f9, 6, MacroRules.tileTranspose)

    val ruleSeq = Seq(
      MacroRules.tileMapMap, MacroRules.finishTiling,
      MacroRules.apply2DRegisterBlocking, MacroRules.apply2DRegisterBlocking,
      MacroRules.finishTiling
    )

    val numExpressionsFinal = NumberExpression.breadthFirst(f10).values.max
    assertEquals(113, numExpressionsFinal)
    checkDepth(f10, ruleSeq)
    checkDistance(f10)
  }

  @Test
  def vectorLoads(): Unit = {
    val tileSizeMN = 16
    val tileSizeK = 8
    val workPerThreadN = 2
    val workPerThreadM = 4
    val vectorWidth = 4

    val tileTranspositionM = 32
    val tileTranspositionK = 32

    val f0 = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K), (p450003680, p2134991632) => FunCall(Join(), FunCall(Map(fun((p756185697) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1237598030) => FunCall(TransposeW(), FunCall(Map(fun((p660879561) => FunCall(Scatter(ReorderWithStride(tileSizeMN/workPerThreadM)), p660879561))), FunCall(Join(), FunCall(Map(fun((p376416077) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1089504328) => FunCall(TransposeW(), p1089504328))), p376416077))))), FunCall(Map(fun((p294184992) => FunCall(Map(fun((p793315160) => FunCall(Map(fun((p270397815) => FunCall(TransposeW(), p270397815))), FunCall(TransposeW(), p793315160)))), FunCall(TransposeW(), p294184992)))), FunCall(TransposeW(), FunCall(toGlobal(fun((p1485697819) => FunCall(MapSeq(fun((p867398280) => FunCall(Map(fun((p2007331442) => FunCall(Map(fun((p1904324159) => FunCall(Map(fun((p1176735295) => FunCall(Map(fun((p1848415041) => FunCall(id, p1848415041))), p1176735295))), p1904324159))), p2007331442))), p867398280))), p1485697819))), FunCall(ReduceSeq(fun((p1032000752, p843467284) => FunCall(Map(fun((p124407148) => FunCall(Map(fun((p1825027294) => FunCall(Map(fun((p19986569) => FunCall(Join(), FunCall(Transpose(), p19986569)))), FunCall(Transpose(), FunCall(toGlobal(fun((p852445367) => FunCall(MapSeq(fun((p1738236591) => FunCall(Map(fun((p1558021762) => FunCall(Map(fun((p225290371) => FunCall(id, p225290371))), p1558021762))), p1738236591))), p852445367))), FunCall(ReduceSeq(fun((p1004095028, p1169146729) => FunCall(Map(fun((p1948863195) => FunCall(Map(fun((p1890187342) => FunCall(add, FunCall(Get(0), p1890187342), FunCall(Get(1), p1890187342)))), FunCall(Zip(2), FunCall(Get(0), p1948863195), FunCall(Get(1), p1948863195))))), FunCall(Zip(2), p1004095028, FunCall(Map(fun((p876213901) => FunCall(Map(fun((p230528013) => FunCall(mult, p876213901, p230528013))), FunCall(Get(1), p1169146729)))), FunCall(Get(0), p1169146729)))))), FunCall(Get(0), p1825027294), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p124407148)), FunCall(Transpose(), FunCall(Get(1), p1825027294))))))))), FunCall(Zip(2), FunCall(Get(0), p124407148), FunCall(Split(workPerThreadM), FunCall(Gather(ReorderWithStride(tileSizeMN/workPerThreadM)), FunCall(Transpose(), FunCall(Get(1), p843467284)))))))), FunCall(Zip(2), p1032000752, FunCall(Split(workPerThreadN), FunCall(Transpose(), FunCall(Get(0), p843467284))))))), FunCall(Map(fun((p1822383117) => FunCall(Map(fun((p233021551) => FunCall(Map(fun((p1991313236) => FunCall(Map(fun((p736778932) => FunCall(id, p736778932))), p1991313236))), p233021551))), p1822383117))), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThreadM), workPerThreadN), tileSizeMN/workPerThreadM), tileSizeMN/workPerThreadN))), FunCall(Zip(2), p756185697, p1237598030))))))))))), FunCall(Transpose(), FunCall(Map(fun((p24606376) => FunCall(Transpose(), p24606376))), FunCall(Split(tileSizeK), FunCall(Map(fun((p302155142) => FunCall(Split(tileSizeMN), p302155142))), p2134991632))))))))), FunCall(Transpose(), FunCall(Map(fun((p1891546521) => FunCall(Transpose(), p1891546521))), FunCall(Split(tileSizeK), FunCall(Map(fun((p297927961) => FunCall(Split(tileSizeMN), p297927961))), FunCall(Join(), FunCall(Map(fun((p1881129850) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p912011468) => FunCall(TransposeW(), FunCall(Map(fun((p1312884893) => FunCall(Map(fun((p849373393) => FunCall(id, p849373393))), p1312884893))), FunCall(Transpose(), p912011468))))), p1881129850))))), FunCall(Transpose(), FunCall(Map(fun((p2142080121) => FunCall(Transpose(), p2142080121))), FunCall(Split(tileTranspositionK), FunCall(Map(fun((p673186785) => FunCall(Split(tileTranspositionM), p673186785))), p450003680)))))))))))))

    val f1 = Rewrite.applyRuleAtId(f0, 52, CopyRules.addIdForCurrentValueInReduce)
    val f2 = Rewrite.applyRuleAtId(f1, 67, CopyRules.implementIdAsDeepCopy)
    val f3 = Rewrite.applyRuleAtId(f2, 67, FusionRules.tupleMap)
    val f4 = Rewrite.applyRuleAtId(f3, 80, OpenCLRules.vectorize(vectorWidth))
    val f5 = Rewrite.applyRuleAtId(f4, 75, OpenCLRules.vectorize(vectorWidth))
    val f6 = Rewrite.applyRuleAtId(f5, 74, FissionRules.tupleFission)
    val f7 = Rewrite.applyRuleAtId(f6, 75, FissionRules.tupleFission)
    val f8 = Rewrite.applyRuleAtId(f7, 83, FusionRules.tupleMap)

    val numExpressions = NumberExpression.breadthFirst(f8).values.max
    assertEquals(194, numExpressions)
  }

  @Test
  def loweringTiledAndBlockedBInnermost(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val tileSizeMN = 16
    val tileSizeK = 8
    val workPerThreadN = 2
    val workPerThreadM = 4

    val f0 = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),(p450003680, p2134991632) => FunCall(Join(), FunCall(Map(fun((p756185697) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1237598030) => FunCall(TransposeW(), FunCall(Map(fun((p660879561) => FunCall(Scatter(ReorderWithStride(tileSizeMN/workPerThreadM)), p660879561))), FunCall(Join(), FunCall(Map(fun((p376416077) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1089504328) => FunCall(TransposeW(), p1089504328))), p376416077))))), FunCall(Map(fun((p294184992) => FunCall(Map(fun((p793315160) => FunCall(Map(fun((p270397815) => FunCall(TransposeW(), p270397815))), FunCall(TransposeW(), p793315160)))), FunCall(TransposeW(), p294184992)))), FunCall(TransposeW(), FunCall(toGlobal(fun((p1485697819) => FunCall(MapSeq(fun((p867398280) => FunCall(Map(fun((p2007331442) => FunCall(Map(fun((p1904324159) => FunCall(Map(fun((p1176735295) => FunCall(Map(fun((p1848415041) => FunCall(id, p1848415041))), p1176735295))), p1904324159))), p2007331442))), p867398280))), p1485697819))), FunCall(ReduceSeq(fun((p1032000752, p843467284) => FunCall(Map(fun((p124407148) => FunCall(Map(fun((p1825027294) => FunCall(Map(fun((p19986569) => FunCall(Join(), FunCall(Transpose(), p19986569)))), FunCall(Transpose(), FunCall(toPrivate(fun((p852445367) => FunCall(MapSeq(fun((p1738236591) => FunCall(Map(fun((p1558021762) => FunCall(Map(fun((p225290371) => FunCall(id, p225290371))), p1558021762))), p1738236591))), p852445367))), FunCall(ReduceSeq(fun((p1004095028, p1169146729) => FunCall(Map(fun((p1948863195) => FunCall(Map(fun((p1890187342) => FunCall(add, FunCall(Get(0), p1890187342), FunCall(Get(1), p1890187342)))), FunCall(Zip(2), FunCall(Get(0), p1948863195), FunCall(Get(1), p1948863195))))), FunCall(Zip(2), p1004095028, FunCall(Map(fun((p876213901) => FunCall(Map(fun((p230528013) => FunCall(mult, p876213901, p230528013))), FunCall(Get(1), p1169146729)))), FunCall(Get(0), p1169146729)))))), FunCall(Get(0), p1825027294), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p124407148)), FunCall(Transpose(), FunCall(Get(1), p1825027294))))))))), FunCall(Zip(2), FunCall(Get(0), p124407148), FunCall(Split(workPerThreadM), FunCall(Gather(ReorderWithStride(tileSizeMN/workPerThreadM)), FunCall(Transpose(), FunCall(Get(1), p843467284)))))))), FunCall(Zip(2), p1032000752, FunCall(Split(workPerThreadN), FunCall(Transpose(), FunCall(Get(0), p843467284))))))), FunCall(Map(fun((p1822383117) => FunCall(Map(fun((p233021551) => FunCall(Map(fun((p1991313236) => FunCall(Map(fun((p736778932) => FunCall(id, p736778932))), p1991313236))), p233021551))), p1822383117))), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThreadM), workPerThreadN), tileSizeMN/workPerThreadM), tileSizeMN/workPerThreadN))), FunCall(Zip(2), p756185697, p1237598030))))))))))), FunCall(Transpose(), FunCall(Map(fun((p24606376) => FunCall(Transpose(), p24606376))), FunCall(Split(tileSizeK), FunCall(Map(fun((p302155142) => FunCall(Split(tileSizeMN), p302155142))), p2134991632))))))))), FunCall(Transpose(), FunCall(Map(fun((p1891546521) => FunCall(Transpose(), p1891546521))), FunCall(Split(tileSizeK), FunCall(Map(fun((p297927961) => FunCall(Split(tileSizeMN), p297927961))), FunCall(Transpose(), p450003680))))))))

    // Add and simplify copies
    val f1 = Rewrite.applyRuleAtId(f0, 64, CopyRules.addIdForCurrentValueInReduce)
    val f2 = Rewrite.applyRuleAtId(f1, 31, CopyRules.addIdForCurrentValueInReduce)
    val f3 = Rewrite.applyRuleAtId(f2, 46, CopyRules.implementIdAsDeepCopy)
    val f4 = Rewrite.applyRuleAtId(f3, 91, CopyRules.implementOneLevelOfId)
    val f5 = Rewrite.applyRuleAtId(f4, 95, Rules.dropId)
    val f6 = Rewrite.applyRuleAtId(f5, 92, CopyRules.implementIdAsDeepCopy)
    val f7 = Rewrite.applyRuleAtId(f6, 101, CopyRules.addCopy)
    val f8 = Rewrite.applyRuleAtId(f7, 46, FusionRules.tupleMap)
    val f9 = Rewrite.applyRuleAtId(f8, 53, FusionRules.tupleMap)

    // Lower to OpenCL execution model
    val f10 = Lower.simpleMapLoweringStrategy(f9)


    // Lower to OpenCL memory model
    val f11 = Rewrite.applyRuleAtId(f10, 47, OpenCLRules.localMemory)
    val f12 = Rewrite.applyRuleAtId(f11, 110, OpenCLRules.privateMemory)
    val f13 = Rewrite.applyRuleAtId(f12, 103, OpenCLRules.privateMemory)

    val mSize = 256
    val kSize = 256
    val nSize = 256
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val values = Seq(matrixA, matrixB)

    val (localRange, globalRange) = InferNDRange(f13, values:_*)

    val (output: Array[Float], _) = Execute(localRange(0).eval, localRange(1).eval,
      globalRange(0).eval, globalRange(1).eval, (true, true))(f13, values:_*)

    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB)

    assertArrayEquals(gold.flatten, output, 0.0001f)
  }

  @Test
  def simpleLowering(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val tileSizeMN = 16
    val tileSizeK = 8
    val workPerThreadN = 2
    val workPerThreadM = 8

    val h0 = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),(p1795960102, p477289012) => FunCall(Join(), FunCall(Map(fun((p1889248251) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p2023938592) => FunCall(TransposeW(), FunCall(Map(fun((p225290371) => FunCall(Scatter(ReorderWithStride(tileSizeMN/workPerThreadM)), p225290371))), FunCall(Join(), FunCall(Map(fun((p297927961) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p733672688) => FunCall(TransposeW(), FunCall(Map(fun((p756185697) => FunCall(TransposeW(), p756185697))), FunCall(TransposeW(), p733672688))))), FunCall(TransposeW(), p297927961)))))), FunCall(TransposeW(), FunCall(MapSeq(fun((p1691875296) => FunCall(Id(), p1691875296))), FunCall(ReduceSeq(fun((p500179317, p1225197672) => FunCall(Map(fun((p1500608548) => FunCall(Map(fun((p513700442) => FunCall(Map(fun((p912011468) => FunCall(Join(), FunCall(Transpose(), p912011468)))), FunCall(Transpose(), FunCall(MapSeq(fun((p1195067075) => FunCall(Id(), p1195067075))), FunCall(ReduceSeq(fun((p1983025922, p1007309018) => FunCall(Map(fun((p2038148563) => FunCall(Map(fun((p2142080121) => FunCall(add, FunCall(Get(0), p2142080121), FunCall(Get(1), p2142080121)))), FunCall(Zip(2), FunCall(Get(0), p2038148563), FunCall(Map(fun((p112619572) => FunCall(mult, FunCall(Get(1), p2038148563), p112619572))), FunCall(Get(1), p1007309018)))))), FunCall(Zip(2), p1983025922, FunCall(Get(0), p1007309018))))), FunCall(Get(0), p513700442), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p1500608548)), FunCall(Transpose(), FunCall(Get(1), p513700442))))))))), FunCall(Zip(2), FunCall(Get(0), p1500608548), FunCall(Split(workPerThreadM), FunCall(Gather(ReorderWithStride(tileSizeMN/workPerThreadM)), FunCall(Transpose(), FunCall(Get(1), p1225197672)))))))), FunCall(Zip(2), p500179317, FunCall(Split(workPerThreadN), FunCall(Transpose(), FunCall(Get(0), p1225197672))))))), FunCall(Map(fun((p1786364562) => FunCall(Map(fun((p326298949) => FunCall(Map(fun((p876926621) => FunCall(Map(fun((p1268959798) => FunCall(id, p1268959798))), p876926621))), p326298949))), p1786364562))), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThreadM), workPerThreadN), tileSizeMN/workPerThreadM), tileSizeMN/workPerThreadN))), FunCall(Zip(2), p1889248251, p2023938592)))))))))), FunCall(Transpose(), FunCall(Map(fun((p1935972447) => FunCall(Transpose(), p1935972447))), FunCall(Split(tileSizeK), FunCall(Map(fun((p1890627974) => FunCall(Split(tileSizeMN), p1890627974))), p477289012))))))))), FunCall(Transpose(), FunCall(Map(fun((p1641313620) => FunCall(Transpose(), p1641313620))), FunCall(Split(tileSizeK), FunCall(Map(fun((p192881625) => FunCall(Split(tileSizeMN), p192881625))), p1795960102)))))))
    val h1 = Lower.simpleMapStrategy(h0)

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

    val tileSizeMNVar = SizeVar("tileSizeMNVar")
    val tileSizeKVar = SizeVar("tileSizeKVar")
    val workPerThreadNVar = SizeVar("workPerThreadNVar")
    val workPerThreadMVar = SizeVar("workPerThreadMVar")

    val fast = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K), (p54495403, p1260134048) => FunCall(Join(), FunCall(MapWrg(1)(fun((p1408652377) => FunCall(TransposeW(), FunCall(Join(), FunCall(MapWrg(0)(fun((p990416209) => FunCall(TransposeW(), FunCall(Map(fun((p1651855867) => FunCall(Scatter(ReorderWithStride(tileSizeMNVar/^workPerThreadMVar)), p1651855867))), FunCall(Join(), FunCall(Map(fun((p1468303011) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p523691575) => FunCall(TransposeW(), p523691575))), p1468303011))))), FunCall(Map(fun((p1354011814) => FunCall(Map(fun((p1852584274) => FunCall(Map(fun((p1857815974) => FunCall(TransposeW(), p1857815974))), FunCall(TransposeW(), p1852584274)))), FunCall(TransposeW(), p1354011814)))), FunCall(TransposeW(), FunCall(toGlobal(fun((p520016214) => FunCall(MapSeq(fun((p1675763772) => FunCall(MapLcl(1)(fun((p841283083) => FunCall(MapLcl(0)(fun((p990398217) => FunCall(MapSeq(fun((p1468357786) => FunCall(MapSeq(fun((p36333492) => FunCall(id, p36333492))), p1468357786))), p990398217))), p841283083))), p1675763772))), p520016214))), FunCall(ReduceSeq(fun((p1511785794, p527446182) => FunCall(fun((p1205555397) => FunCall(MapLcl(1)(fun((p1454031203) => FunCall(MapLcl(0)(fun((p407858146) => FunCall(Map(fun((p817406040) => FunCall(Join(), FunCall(Transpose(), p817406040)))), FunCall(Transpose(), FunCall(ReduceSeq(fun((p603650290, p1754638213) => FunCall(fun((p278934944) => FunCall(MapSeq(fun((p222624801) => FunCall(MapSeq(fun((p85777802) => FunCall(add, FunCall(Get(0), p85777802), FunCall(Get(1), p85777802)))), FunCall(Zip(2), FunCall(Get(0), p222624801), FunCall(Get(1), p222624801))))), FunCall(Zip(2), p603650290, FunCall(MapSeq(fun((p280744458) => FunCall(MapSeq(fun((p1213216872) => FunCall(mult, p280744458, p1213216872))), FunCall(Get(1), p278934944)))), FunCall(toPrivate(fun((p1686369710) => FunCall(MapSeq(fun((p385337537) => FunCall(id, p385337537))), p1686369710))), FunCall(Get(0), p278934944)))))), FunCall(fun((p1282811396) => FunCall(Tuple(2), FunCall(Get(0), p1282811396), FunCall(toPrivate(fun((p439928219) => FunCall(MapSeq(fun((p1883840933) => FunCall(id, p1883840933))), p439928219))), FunCall(Get(1), p1282811396)))), p1754638213)))), FunCall(Get(0), p407858146), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p1454031203)), FunCall(Transpose(), FunCall(Get(1), p407858146)))))))), FunCall(Zip(2), FunCall(Get(0), p1454031203), FunCall(Split(workPerThreadMVar), FunCall(Gather(ReorderWithStride(tileSizeMNVar/^workPerThreadMVar)), FunCall(Transpose(), FunCall(Get(1), p1205555397)))))))), FunCall(Zip(2), p1511785794, FunCall(Split(workPerThreadNVar), FunCall(Transpose(), FunCall(Get(0), p1205555397)))))), FunCall(fun((p1209669119) => FunCall(Unzip(), FunCall(toLocal(fun((p1607305514) => FunCall(MapLcl(1)(fun((p832279283) => FunCall(Unzip(), FunCall(MapLcl(0)(fun((p668210649) => FunCall(Tuple(2), FunCall(id, FunCall(Get(0), p668210649)), FunCall(id, FunCall(Get(1), p668210649))))), FunCall(Zip(2), FunCall(Get(0), p832279283), FunCall(Get(1), p832279283)))))), p1607305514))), FunCall(Zip(2), FunCall(Get(0), p1209669119), FunCall(Get(1), p1209669119))))), p527446182)))), FunCall(MapLcl(1)(fun((p1301664418) => FunCall(MapLcl(0)(fun((p513169028) => FunCall(MapSeq(fun((p377478451) => FunCall(MapSeq(fun((p1596467899) => FunCall(id, p1596467899))), p377478451))), p513169028))), p1301664418))), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThreadMVar), workPerThreadNVar), tileSizeMNVar/^workPerThreadMVar), tileSizeMNVar/^workPerThreadNVar))), FunCall(Zip(2), p1408652377, p990416209))))))))))), FunCall(Transpose(), FunCall(Map(fun((p1952779858) => FunCall(Transpose(), p1952779858))), FunCall(Split(tileSizeKVar), FunCall(Map(fun((p1791868405) => FunCall(Split(tileSizeMNVar), p1791868405))), p1260134048))))))))), FunCall(Transpose(), FunCall(Map(fun((p81009902) => FunCall(Transpose(), p81009902))), FunCall(Split(tileSizeKVar), FunCall(Map(fun((p674483268) => FunCall(Split(tileSizeMNVar), p674483268))), p54495403)))))))
    TypeChecker(fast)

    val f0 = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),(p1795960102, p477289012) => FunCall(Join(), FunCall(Map(fun((p1889248251) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p2023938592) => FunCall(TransposeW(), FunCall(Map(fun((p225290371) => FunCall(Scatter(ReorderWithStride(tileSizeMNVar/^workPerThreadMVar)), p225290371))), FunCall(Join(), FunCall(Map(fun((p297927961) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p733672688) => FunCall(TransposeW(), FunCall(Map(fun((p756185697) => FunCall(TransposeW(), p756185697))), FunCall(TransposeW(), p733672688))))), FunCall(TransposeW(), p297927961)))))), FunCall(TransposeW(), FunCall(MapSeq(fun((p1691875296) => FunCall(Id(), p1691875296))), FunCall(ReduceSeq(fun((p500179317, p1225197672) => FunCall(Map(fun((p1500608548) => FunCall(Map(fun((p513700442) => FunCall(Map(fun((p912011468) => FunCall(Join(), FunCall(Transpose(), p912011468)))), FunCall(Transpose(), FunCall(MapSeq(fun((p1195067075) => FunCall(Id(), p1195067075))), FunCall(ReduceSeq(fun((p1983025922, p1007309018) => FunCall(Map(fun((p2038148563) => FunCall(Map(fun((p2142080121) => FunCall(add, FunCall(Get(0), p2142080121), FunCall(Get(1), p2142080121)))), FunCall(Zip(2), FunCall(Get(0), p2038148563), FunCall(Map(fun((p112619572) => FunCall(mult, FunCall(Get(1), p2038148563), p112619572))), FunCall(Get(1), p1007309018)))))), FunCall(Zip(2), p1983025922, FunCall(Get(0), p1007309018))))), FunCall(Get(0), p513700442), FunCall(Zip(2), FunCall(Transpose(), FunCall(Get(1), p1500608548)), FunCall(Transpose(), FunCall(Get(1), p513700442))))))))), FunCall(Zip(2), FunCall(Get(0), p1500608548), FunCall(Split(workPerThreadMVar), FunCall(Gather(ReorderWithStride(tileSizeMNVar/^workPerThreadMVar)), FunCall(Transpose(), FunCall(Get(1), p1225197672)))))))), FunCall(Zip(2), p500179317, FunCall(Split(workPerThreadNVar), FunCall(Transpose(), FunCall(Get(0), p1225197672))))))), FunCall(Map(fun((p1786364562) => FunCall(Map(fun((p326298949) => FunCall(Map(fun((p876926621) => FunCall(Map(fun((p1268959798) => FunCall(id, p1268959798))), p876926621))), p326298949))), p1786364562))), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, workPerThreadMVar), workPerThreadNVar), tileSizeMNVar/^workPerThreadMVar), tileSizeMNVar/^workPerThreadNVar))), FunCall(Zip(2), p1889248251, p2023938592)))))))))), FunCall(Transpose(), FunCall(Map(fun((p1935972447) => FunCall(Transpose(), p1935972447))), FunCall(Split(tileSizeKVar), FunCall(Map(fun((p1890627974) => FunCall(Split(tileSizeMNVar), p1890627974))), p477289012))))))))), FunCall(Transpose(), FunCall(Map(fun((p1641313620) => FunCall(Transpose(), p1641313620))), FunCall(Split(tileSizeKVar), FunCall(Map(fun((p192881625) => FunCall(Split(tileSizeMNVar), p192881625))), p1795960102)))))))
    TypeChecker(f0)
  }

  @Test
  def reuseWithTiling(): Unit = {

    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) o Transpose() $ B
        )) $ A
      })

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.tileMapMap)
    val f2 = Rewrite.applyRuleAtId(f1, 12, MacroRules.finishTiling)


    val f3 = Rewrite.applyRuleAtId(f2, 24, MacroRules.apply1DRegisterBlocking)
    val f4 = Rewrite.applyRuleAtId(f3, 11, MacroRules.apply1DRegisterBlocking)
    val f5 = Rewrite.applyRuleAtId(f4, 12, MacroRules.finishTiling)
    val f6 = Rewrite.applyRuleAtId(f5, 6, MacroRules.moveTransposeInsideTiling)

    val f7 = Rewrite.applyRuleAtId(f6, 82, ReduceRules.partialReduceToReduce)
    val f8 = SimplifyAndFuse(f7)

    val ruleSeq = Seq(
      MacroRules.tileMapMap, MacroRules.finishTiling,
      MacroRules.apply1DRegisterBlocking, MacroRules.apply1DRegisterBlocking,
      MacroRules.finishTiling
    )

    val numExpressions = NumberExpression.breadthFirst(f8).values.max
    assertEquals(66, numExpressions)
    checkDepth(f8, ruleSeq)
    checkDistance(f8)
  }

  @Test
  def tiledTransposeWithLocalMemory(): Unit = {

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      input => Map(Map(id)) o Transpose() $ input
    )

    val x = 8
    val y = 4

    val f1 = Rewrite.applyRuleAtId(f, 0, MacroRules.tileTranspose(x, y))

    // Add the copy
    val f2 = Rewrite.applyRuleAtId(f1, 17, CopyRules.addCopy)

    // Lower to OpenCL execution model
    val f3 = Lower.simpleMapLoweringStrategy(f2)

    // Lower to OpenCL memory model
    val f4 = Rewrite.applyRuleAtId(f3, 16, OpenCLRules.globalMemory)
    val f5 = Rewrite.applyRuleAtId(f4, 18, OpenCLRules.localMemory)

    val nSize = 12
    val mSize = 8
    val matrix = Array.tabulate(nSize, mSize)((r, c) => c * 1.0f + r * 8.0f)
    val gold = matrix.transpose

    val (output: Array[Float], _) =
      Execute(y, x, nSize, mSize, (false, false))(f5, matrix)
    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test
  def mmSquareTilesLocalMemory(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val f0 = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), (p1538399081, p1800890735) => FunCall(Join(), FunCall(Map(fun((p1957502751) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p1177377518) => FunCall(TransposeW(), FunCall(Map(fun((p1122805102) => FunCall(TransposeW(), p1122805102))), FunCall(TransposeW(), FunCall(toGlobal(fun((p183284570) => FunCall(MapSeq(fun((p2109874862) => FunCall(Map(fun((p275310919) => FunCall(Map(fun((p797925218) => FunCall(id, p797925218))), p275310919))), p2109874862))), p183284570))), FunCall(ReduceSeq(fun((p1136497418, p1943325854) => FunCall(Map(fun((p1413378318) => FunCall(Map(fun((p1181869371) => FunCall(add, FunCall(Get(0), p1181869371), FunCall(Get(1), p1181869371)))), FunCall(Zip(2), FunCall(Get(0), p1413378318), FunCall(Join(), FunCall(Map(fun((p1256728724) => FunCall(toPrivate(fun((p1157058691) => FunCall(MapSeq(fun((p1667689440) => FunCall(id, p1667689440))), p1157058691))), FunCall(ReduceSeq(fun((p852687460, p1138193439) => FunCall(add, p852687460, FunCall(mult, FunCall(Get(0), p1138193439), FunCall(Get(1), p1138193439))))), FunCall(id, Value(0.0f, Float)), FunCall(Zip(2), FunCall(Get(1), p1413378318), p1256728724))))), FunCall(Transpose(), FunCall(Get(1), p1943325854)))))))), FunCall(Zip(2), p1136497418, FunCall(Transpose(), FunCall(Get(0), p1943325854)))))), FunCall(Map(fun((p194706439) => FunCall(Map(fun((p1686369710) => FunCall(id, p1686369710))), p194706439))), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, 4), 4))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p1957502751)), FunCall(Split(4), FunCall(Transpose(), p1177377518)))))))))), FunCall(Split(4), p1800890735)))))), FunCall(Split(4), p1538399081))))

    // Add a copy
    val f1 = Rewrite.applyRuleAtId(f0, 13, CopyRules.addIdForCurrentValueInReduce)
    val f2 = Rewrite.applyRuleAtId(f1, 28, CopyRules.implementIdAsDeepCopy)

    // Lower to OpenCL execution model
    val f3 = Lower.simpleMapLoweringStrategy(f2)

    // Lower to OpenCL memory model
    val f4 = Rewrite.applyRuleAtId(f3, 36, OpenCLRules.localMemory)
    val f5 = Rewrite.applyRuleAtId(f4, 29, OpenCLRules.localMemory)

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val (output: Array[Float], _) = Execute(4, 4, mSize, kSize, (true, true))(f5, matrixA, matrixB.transpose)

    val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB)

    assertArrayEquals(gold.flatten, output, 0.0001f)
  }

  @Test
  def rectangularTiles(): Unit = {

    // Derivation the same, except split on the zip is different than the others
    val h0 = fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K), (p951880373, p1752203484) => FunCall(Join(), FunCall(Map(fun((p243745864) => FunCall(TransposeW(), FunCall(Join(), FunCall(Map(fun((p699780352) => FunCall(toGlobal(fun((p1613255205) => FunCall(MapSeq(fun((p1897115967) => FunCall(Map(fun((p1166151249) => FunCall(Map(fun((p1121453612) => FunCall(id, p1121453612))), p1166151249))), p1897115967))), p1613255205))), FunCall(ReduceSeq(fun((p2619171, p1728790703) => FunCall(Map(fun((p1227074340) => FunCall(Map(fun((p1154002927) => FunCall(add, FunCall(Get(0), p1154002927), FunCall(Get(1), p1154002927)))), FunCall(Zip(2), FunCall(Get(0), p1227074340), FunCall(Get(1), p1227074340))))), FunCall(Zip(2), p2619171, FunCall(fun((p2070529722) => FunCall(Map(fun((p1188753216) => FunCall(Join(), FunCall(Map(fun((p317986356) => FunCall(toPrivate(fun((p331510866) => FunCall(MapSeq(fun((p640363654) => FunCall(id, p640363654))), p331510866))), FunCall(ReduceSeq(fun((p924477420, p99451533) => FunCall(add, p924477420, FunCall(fun((p84739718) => FunCall(mult, FunCall(Get(0), p84739718), FunCall(Get(1), p84739718))), p99451533)))), FunCall(toPrivate(id), Value(0.0f, Float)), FunCall(Zip(2), p317986356, p1188753216))))), FunCall(Transpose(), FunCall(Get(0), p2070529722)))))), FunCall(Transpose(), FunCall(Get(1), p2070529722)))), p1728790703))))), FunCall(toPrivate(Map(fun((p2050835901) => FunCall(Map(fun((p511473681) => FunCall(id, p511473681))), p2050835901)))), Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 8))), FunCall(Zip(2), FunCall(Split(4), FunCall(Transpose(), p243745864)), FunCall(Split(4), FunCall(Transpose(), p699780352))))))), FunCall(Split(8), FunCall(Transpose(), p1752203484))))))), FunCall(Split(8), p951880373))))

    val h1 = Rewrite.applyRuleAtId(h0, 1, MacroRules.finishRectangularTiles)

    val numExpressions = NumberExpression.breadthFirst(h1).values.max
    assertEquals(91, numExpressions)
    checkDepth(h1)
  }

  @Test
  def mmReuseB(): Unit = {

    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.apply1DRegisterBlocking)
    val f2 = SimplifyAndFuse(f1)

    val numExpressions = NumberExpression.breadthFirst(f2).values.max
    assertEquals(26, numExpressions)
    checkDepth(f2)
    checkDistance(f2)
  }

  @Test
  def mmReuseBoth(): Unit = {

    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.apply2DRegisterBlocking)
    val f2 = SimplifyAndFuse(f1)

    val numExpressions = NumberExpression.breadthFirst(f2).values.max
    assertEquals(43, numExpressions)
    checkDepth(f2)
    checkDistance(f2)
  }

  @Test
  def transposeInsideTiling(): Unit = {

    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) o Transpose() $ B
        )) o Transpose() $ A
      })

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.tileMapMap)
    val f2 = Rewrite.applyRuleAtId(f1, 13, MacroRules.finishTiling)
    val f3 = Rewrite.applyRuleAtId(f2, 1, MacroRules.moveTransposeInsideTiling)

    val f4 = Rewrite.applyRuleAtId(f3, 18, MacroRules.finishTiling)
    val f5 = Rewrite.applyRuleAtId(f4, 70, ReduceRules.partialReduceToReduce)
    val f6 = SimplifyAndFuse(f5)

    val numExpressions = NumberExpression.breadthFirst(f3).values.max
    assertEquals(56, numExpressions)
    checkDepth(f3)
    checkDistance(f6)
  }

  @Test
  def mmTiled(): Unit = {

    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) o Transpose() $ B
        )) $ A
      })

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.tileMapMap)
    val f2 = Rewrite.applyRuleAtId(f1, 12, MacroRules.finishTiling)
    val f3 = Rewrite.applyRuleAtId(f2, 6, MacroRules.moveTransposeInsideTiling)
    val f4 = Rewrite.applyRuleAtId(f3, 17, MacroRules.finishTiling)
    val f5 = SimplifyAndFuse(f4)

    val numExpressions = NumberExpression.breadthFirst(f5).values.max
    assertEquals(58, numExpressions)
    checkDepth(f5)
    checkDistance(f5)
  }

  @Test
  def partiallyVectorisedTiled(): Unit = {

    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Transposed
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.tileMapMap)

    val f2 = Rewrite.applyRuleAtId(f1, 10, MacroRules.finishTiling)
    val f3 = Rewrite.applyRuleAtId(f2, 15, MacroRules.finishTiling)
    val f4 = Rewrite.applyRuleAtId(f3, 36, OpenCLRules.vectorizeMapZip(4))
    val f5 = HighLevelRewrite.finishRewriting(f4)

    val numExpressions = NumberExpression.breadthFirst(f5).values.max
    assertEquals(56, numExpressions)
    checkDepth(f5)
    checkDistance(f5)
  }

  @Test
  def vectorised(): Unit = {

    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Transposed
      (A, B) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)) )) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val f1 = Rewrite.applyRuleAtId(f0, 5, OpenCLRules.vectorizeMapZip(4))
    val f2 = Rewrite.applyRuleAtId(f1, 4, MacroRules.vectorizeReduce(4))
    val f3 = Rewrite.applyRuleAtId(f2, 6, ReduceRules.partialReduceToReduce)
    val f4 = SimplifyAndFuse(f3)

    val numExpressions = NumberExpression.breadthFirst(f4).values.max
    assertEquals(23, numExpressions)
    checkDepth(f4)
    checkDistance(f4)
  }

  @Test
  def clBLAST_kepler(): Unit = {

    LongTestsEnabled()

    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        Map(fun(aRow =>
          Map(fun(bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
          )) o Transpose() $ B
        )) o Transpose() $ A
      })

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.tileMapMap)
    val f2 = Rewrite.applyRuleAtId(f1, 13, MacroRules.finishTiling)
    val f3 = Rewrite.applyRuleAtId(f2, 25, MacroRules.apply2DRegisterBlockingNoReorder)
    val f4 = Rewrite.applyRuleAtId(f3, 12, MacroRules.apply2DRegisterBlockingNoReorder)
    val f5 = Rewrite.applyRuleAtId(f4, 13, MacroRules.finishTiling)
    val f6 = Rewrite.applyRuleAtId(f5, 93, ReduceRules.partialReduceToReduce)
    val f7 = SimplifyAndFuse(f6)

    // Final steps, move transpose inside tiling
    val f8 = HighLevelRewrite.applyAlwaysRules(f7)

    val numExpressionsHighLevel = NumberExpression.breadthFirst(f8).values.max
    assertEquals(87, numExpressionsHighLevel)

    // Lower and vectorise
    val g0 = Rewrite.applyRuleAtId(f8, 27, CopyRules.addIdAfterReduce)
    val g1 = Rewrite.applyRuleAtId(g0, 78, CopyRules.implementIdAsDeepCopy)
    val g2 = Rewrite.applyRuleAtId(g1, 78, OpenCLRules.globalMemory)
    val g3 = Rewrite.applyRuleAtId(g2, 86, OpenCLRules.vectorize(4))
    val g4 = Rewrite.applyRuleAtId(g3, 32, CopyRules.addIdValue)
    val g5 = Lower.simpleMapLoweringStrategy(g4)

    val g6 = Rewrite.applyRuleAtId(g5, 28, CopyRules.addIdForCurrentValueInReduce)
    val g7 = Rewrite.applyRuleAtId(g6, 43, OpenCLRules.localMemory)
    val g8 = Rewrite.applyRuleAtId(g7, 45, CopyRules.implementIdAsDeepCopy)
    val g10 = Rewrite.applyRuleAtId(g8, 53, MacroRules.reshapeMapMap)

    val g11 = Rewrite.applyRuleAtId(g10, 60, OpenCLRules.vectorize(4))
    val g12 = Rewrite.applyRuleAtId(g11, 49, OpenCLRules.vectorize(4))

    val g13 = Lower.lowerNextLevelWithRule(g12, OpenCLRules.mapLcl(1))
    val g14 = Lower.lowerNextLevelWithRule(g13, OpenCLRules.mapLcl(0))

    val g15 = Rewrite.applyRuleAtId(g14, 84, CopyRules.addIdForCurrentValueInReduce)
    val g16 = Rewrite.applyRuleAtId(g15, 95, OpenCLRules.privateMemory)
    val g17 = Rewrite.applyRuleAtId(g16, 97, CopyRules.implementIdAsDeepCopy)
    val g18 = Lower.lowerNextLevelWithRule(g17, OpenCLRules.mapSeq)

    val numExpressionsFinal = NumberExpression.breadthFirst(g18).values.max
    assertEquals(151, numExpressionsFinal)
  }

  @Ignore
  @Test
  def gemmTiled(): Unit = {

    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      Float,
      Float,
      (A, B, C, alpha, beta) => {
        Map(fun( aRow =>
          Map(fun( bCol =>
              Map(fun(x =>
                add(
                  mult(x, alpha),
                  mult(Get(bCol, 1), beta)
                )
              )) o Reduce(add, 0.0f) o
                Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(Get(aRow, 0), Get(bCol, 0))
          )) $ Zip(Transpose() $ B, Get(aRow, 1))
        )) $ Zip(A, C)
      })

    val f1 = Rewrite.applyRuleAtId(f0, 0, MacroRules.tileMapMap)

    val f2 = Rewrite.applyRuleAtId(f1, 21, MacroRules.finishTiling)
    val f3 = Rewrite.applyRuleAtId(f2, 20, MacroRules.finishTiling)
    val f4 = Rewrite.applyRuleAtId(f3, 19, MacroRules.finishTiling)
    SimplifyAndFuse(f4)
  }

  @Test
  def dot(): Unit = {
    val v_K0_0 = SizeVar("K")
    val v_M1_1 = SizeVar("M")
    val v_N2_2 = SizeVar("N")
    val v_3_3 = SizeVar("")
    val v_4_4 = SizeVar("")
    val v_5_5 = SizeVar("")

    val idfloat = UserFun("idfloat", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
    val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
    val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
    val f0 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, v_K0_0), v_M1_1),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, v_K0_0), v_N2_2),
      (p_0, p_1) =>
        FunCall(Join(),
          FunCall(Map(fun((p_2) =>
            FunCall(TransposeW(),
              FunCall(Join(),
                FunCall(Map(fun((p_3) =>
                  FunCall(TransposeW(),
                    FunCall(Map(fun((p_4) =>
                      FunCall(TransposeW(), p_4))),
                      FunCall(TransposeW(),
                        FunCall(MapSeq(fun((p_5) =>
                          FunCall(Id(), p_5))),
                          FunCall(ReduceSeq(fun((p_6, p_7) =>
                            FunCall(Map(fun((p_8) =>
                              FunCall(Join(),
                                FunCall(Map(fun((p_9) =>
                                  FunCall(Reduce(fun((p_10, p_11) =>
                                    FunCall(add, p_10, p_11))),
                                    FunCall(Get(0), p_9),
                                    FunCall(asScalar(),
                                      FunCall(Map(fun((p_12) =>
                                        FunCall(VectorizeUserFun(4,mult),
                                          FunCall(Get(0), p_12),
                                          FunCall(Get(1), p_12)))),
                                        FunCall(Zip(2),
                                          FunCall(asVector(4),
                                            FunCall(Get(1), p_8)),
                                          FunCall(asVector(4),
                                            FunCall(Get(1), p_9)))))))),
                                  FunCall(Zip(2),
                                    FunCall(Get(0), p_8),
                                    FunCall(Transpose(),
                                      FunCall(Get(1), p_7))))))),
                              FunCall(Zip(2), p_6,
                                FunCall(Transpose(),
                                  FunCall(Get(0), p_7)))))),
                            FunCall(Map(fun((p_13) =>
                              FunCall(Map(fun((p_14) =>
                                FunCall(idfloat, p_14))), p_13))),
                              Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float, v_3_3), v_4_4))),
                            FunCall(Zip(2),
                              FunCall(Split(v_5_5),
                                FunCall(Transpose(), p_2)),
                              FunCall(Split(v_5_5),
                                FunCall(Transpose(), p_3)))))))))),
                  FunCall(Split(v_3_3), p_1)))))),
            FunCall(Split(v_4_4), p_0))))

    val f1 = Rewrite.applyRuleAtId(f0, 41, OpenCLRules.dotBuiltin)

    checkDepth(f1)
    checkDistance(f1)
  }

}
