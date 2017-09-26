package rewriting

import benchmarks.MolecularDynamics
import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import rewriting.rules.{CopyRules, OpenCLRules, Rules, SimplificationRules}

object TestRewriteMD {
  @BeforeClass
  def before(): Unit =
    Executor.loadAndInit()

  @AfterClass
  def after(): Unit =
    Executor.shutdown()
}

class TestRewriteMD {

  @Test
  def shoc(): Unit = {
    val mdCompute = UserFun("updateF",
      Array("f", "ipos", "jpos", "cutsq", "lj1", "lj2"),
      """|{
         |  // Calculate distance
         |  float delx = ipos.x - jpos.x;
         |  float dely = ipos.y - jpos.y;
         |  float delz = ipos.z - jpos.z;
         |  float r2inv = delx*delx + dely*dely + delz*delz;
         |  // If distance is less than cutoff, calculate force
         |  if (r2inv < cutsq) {
         |    r2inv = 1.0f/r2inv;
         |    float r6inv = r2inv * r2inv * r2inv;
         |    float forceC = r2inv*r6inv*(lj1*r6inv - lj2);
         |    f.x += delx * forceC;
         |    f.y += dely * forceC;
         |    f.z += delz * forceC;
         |  }
         |  return f;
         |}
      """.stripMargin,
      Seq(Float4, Float4, Float4, Float, Float, Float),
      Float4)

    val N = SizeVar("N") // number of particles
    val M = SizeVar("M") // number of neighbors

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(ArrayTypeWSWC(Int, N), M),
      Float,
      Float,
      Float,
      (particles, neighbourIds, cutsq, lj1, lj2) =>
        Zip(particles, Transpose() $ neighbourIds) :>>
          Map( \(p =>
            Filter(particles, p._1) :>>
              // TODO: Separate into Map and Reduce?
              ReduceSeq(\((force, n) =>
                mdCompute(force, p._0, n, cutsq, lj1, lj2)
              ), Value(0.0f, Float4))
          ) )
    )

    val f1 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(128))
    val f2 = Rewrite.applyRuleAtId(f1, 7, CopyRules.addIdForMapParam)
    val f3 = Rewrite.applyRuleAtId(f2, 10, CopyRules.implementOneLevelOfId)
    val f4 = Rewrite.applyRuleAtId(f3, 1, OpenCLRules.mapWrg)
    val f5 = Rewrite.applyRuleAtId(f4, 7, OpenCLRules.mapLcl)
    val f6 = Rewrite.applyRuleAtId(f5, 14, OpenCLRules.privateMemory)
    val f7 = Rewrite.applyRuleAtId(f6, 17, CopyRules.implementOneLevelOfId)
    val f8 = Rewrite.applyRuleAtId(f7, 11, SimplificationRules.dropId)
    val f9 = Rewrite.applyRuleAtId(f8, 18, CopyRules.addIdAfterReduce)
    val f10 = Rewrite.applyRuleAtId(f9, 18, OpenCLRules.globalMemory)
    val f11 = Rewrite.applyRuleAtId(f10, 35, CopyRules.implementIdAsDeepCopy)

    val inputSize = 1024
    val maxNeighbours = 128

    val particles = Array.fill(inputSize, 4)(util.Random.nextFloat() * 20.0f)
    val particlesTuple = particles.map { case Array(a, b, c, d) => (a, b, c, d) }
    val neighbours = MolecularDynamics.buildNeighbourList(particlesTuple, maxNeighbours).transpose
    val cutsq = 16.0f
    val lj1 = 1.5f
    val lj2 = 2.0f

    val gold = MolecularDynamics.mdScala(particlesTuple, neighbours, cutsq, lj1, lj2)
               .map(_.productIterator).reduce(_ ++ _).asInstanceOf[Iterator[Float]].toArray

    val (output, _) =
      Execute(inputSize)[Array[Float]](f11, particles, neighbours, cutsq, lj1, lj2)

    assertEquals(gold.length, output.length)

    (output, gold).zipped.map((x, y) => {

      var diff = (x - y) / x

      if (x == 0.0f)
        diff = 0.0f

      math.sqrt(diff * diff).toFloat
    }).zipWithIndex.foreach(x => assertEquals("Error at pos " + x._2, 0.0f, x._1, 0.1f))
  }

}
