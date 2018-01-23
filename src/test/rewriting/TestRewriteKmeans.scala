package rewriting

import ir._
import ir.ast._
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.Assert._
import org.junit._
import rewriting.macrorules.MacroRules
import rewriting.rules.{CopyRules, FissionRules, OpenCLRules, Rules}
import rodinia.Kmeans._

object TestRewriteKmeans extends TestWithExecutor

class TestRewriteKmeans {

  val currentDistance = UserFun("currentDistance", Array("x", "y"),
    "{ return (x - y) * (x - y); }",
      Seq(Float, Float), Float)

  @Test
  def kMeansLocalMemory(): Unit = {

    val f = fun(
      featuresType, clustersType,
      (features, clusters) =>
        Map( \( feature =>
          Map(Map(select)) o
          ReduceSeq( \( (tuple, cluster) =>
            Map(\((x) => test(x._0,x._1)))
              $ Zip(Reduce(add, 0.0f) o
              Map(fun(x => currentDistance(x._0, x._1))) $ Zip(feature, cluster), tuple)
          ), Value("{3.40282347e+38, 0, 0}", ArrayTypeWSWC(TupleType(Float, Int, Int),1))
          ) $ clusters
        )) o Transpose() $ features
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(128))

    // TODO: Next 3 should come after parallelism mapping
    val f2 = Rewrite.applyRuleAtId(f1, 8, CopyRules.addIdRed)
    val f4 = Rewrite.applyRuleAtId(f2, 5, FissionRules.mapFission)
    val f5 = Rewrite.applyRuleAtId(f4, 6, FissionRules.extractFromMap)

    val f6 = SimplifyAndFuse(f5)

    val lastToGlobal = Lower.lastWriteToGlobal(f6)
    val f7 = Lower.lowerNextLevelWithRule(lastToGlobal, OpenCLRules.mapWrg)
    val f8 = Lower.lowerNextLevelWithRule(f7, OpenCLRules.mapLcl)
    val f9 = Lower.lowerNextLevelWithRule(f8, OpenCLRules.mapSeq)
    val f10 = Lower.lowerNextLevelWithRule(f9, OpenCLRules.mapSeq)

    val f12 = Rewrite.applyRuleAtId(f10, 6, OpenCLRules.localMemory)
    val f3 = Rewrite.applyRuleAtId(f12, 8, CopyRules.implementIdAsDeepCopy)
    val l0 = Lower.lowerNextLevelWithRule(f3, OpenCLRules.mapLcl)
    val l1 = Lower.lowerNextLevelWithRule(l0, OpenCLRules.mapSeq)

    val l2 = Rewrite.applyRuleUntilCannot(l1, MacroRules.userFunCompositionToPrivate)

    val numPoints = 1024
    val numClusters = 5
    val numFeatures = 8

    val points = Array.fill(numPoints, numFeatures)(util.Random.nextFloat())
    val clusters = Array.fill(numClusters, numFeatures)(util.Random.nextFloat())

    val gold = calculateMembership(points, clusters)

    val (output, _) = Execute(numPoints)[Array[Int]](l2, points.transpose, clusters)
    assertArrayEquals(gold, output)
  }
}
