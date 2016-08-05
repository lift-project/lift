package rewriting

import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.Assert._
import org.junit._
import rodinia.Kmeans._

object TestRewriteKmeans {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    Executor.shutdown()
  }
}

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
          ), Value("{3.40282347e+38, 0, 0}", ArrayType(TupleType(Float, Int, Int),1))
          ) $ clusters
        )) o Transpose() $ features
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(128))
    val f2 = Rewrite.applyRuleAtId(f1, 8, Rules.addIdRed)
    val f3 = Rewrite.applyRuleAtId(f2, 9, Rules.implementIdAsDeepCopy)
    val f4 = Rewrite.applyRuleAtId(f3, 5, Rules.mapFission)
    val f5 = Rewrite.applyRuleAtId(f4, 6, Rules.extractFromMap)
    val f6 = Rewrite.applyRuleAtId(f5, 5, Rules.mapFusion)
    val f7 = Lower.lowerNextLevelWithRule(f6, Rules.mapWrg)
    val f8 = Lower.lowerNextLevelWithRule(f7, Rules.mapLcl)
    val f9 = Lower.lowerNextLevelWithRule(f8, Rules.mapSeq)
    val f10 = Lower.lowerNextLevelWithRule(f9, Rules.mapSeq)
    val f11 = Rewrite.applyRuleAtId(f10, 14, Rules.globalMemory)
    val f12 = Rewrite.applyRuleAtId(f11, 6, Rules.localMemory)
    val f13 = Rewrite.applyRuleAtId(f12, 23, MacroRules.reduceMapFusion)

    val numPoints = 1024
    val numClusters = 5
    val numFeatures = 8

    val points = Array.fill(numPoints, numFeatures)(util.Random.nextFloat())
    val clusters = Array.fill(numClusters, numFeatures)(util.Random.nextFloat())

    val gold = calculateMembership(points, clusters)

    val (output: Array[Int], _) = Execute(numPoints)(f13, points.transpose, clusters)
    assertArrayEquals(gold, output)
  }
}
