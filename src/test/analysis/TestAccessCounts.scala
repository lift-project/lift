package analysis

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.generator._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.InferNDRange

class TestAccessCounts {

  val N = SizeVar("N")
  val globalSize0 = get_global_size(0)
  val globalSize1 = get_global_size(1)

  val numGroups0 = get_num_groups(0)
  val localSize0 = get_local_size(0)

  @Test
  def simple(): Unit = {

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(MapSeq(id)) $ x
    )

    val accessCounts = AccessCounts(f)

    assertEquals(N * (N /^ globalSize0), accessCounts.getStores(f.body.mem))
  }

  @Test
  def simple2(): Unit = {

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) $ x
    )

    val accessCounts = AccessCounts(f)

    assertEquals((N /^ globalSize1) * (N /^ globalSize0),
      accessCounts.getStores(f.body.mem))
  }

  @Test
  def moreReads(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float, N),
      x => MapSeq(add) $ Zip(x,x)
    )

    val accessCounts = AccessCounts(f)

    assertEquals(N, accessCounts.getStores(f.body.mem))
    assertEquals(2*N, accessCounts.getLoads(f.params.head.mem))

    assertEquals(Cst(0), accessCounts.getLoads(f.body.mem))
    assertEquals(Cst(0), accessCounts.getStores(f.params.head.mem))
  }

  @Test
  def simpleLocal(): Unit = {
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 16), N),
      x => MapWrg(toGlobal(MapLcl(id)) o toLocal(MapLcl(id))) $ x
    )

    val counts = AccessCounts(f)
    val localReads = counts.getLoads(LocalMemory, exact = false)
    val localWrites = counts.getStores(LocalMemory, exact = false)

    assertEquals((N /^ numGroups0) * (Cst(16) /^ localSize0) , localReads)
    assertEquals((N /^ numGroups0) * (Cst(16) /^ localSize0) , localWrites)
  }

  @Test
  def withPattern(): Unit = {

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) o Transpose() $ x
    )

    val accessCounts = AccessCounts(f)

    assertEquals((N /^ globalSize1) * (N /^ globalSize0),
      accessCounts.getLoads(GlobalMemory, UnknownPattern, exact = false))
    assertEquals((N /^ globalSize1) * (N /^ globalSize0),
      accessCounts.getStores(GlobalMemory, CoalescedPattern, exact = false))

    assertEquals(Cst(0),
      accessCounts.getLoads(GlobalMemory, CoalescedPattern, exact = false))
    assertEquals(Cst(0),
      accessCounts.getStores(GlobalMemory, UnknownPattern, exact = false))
  }

  @Test
  def vector(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      x => MapGlb(0)(idF4) $ x
    )

    val accessCounts = AccessCounts(f)

    assertEquals(N /^ globalSize0,
      accessCounts.vectorLoads(GlobalMemory, CoalescedPattern))
    assertEquals(N /^ globalSize0,
      accessCounts.vectorStores(GlobalMemory, CoalescedPattern))
  }

  @Test
  def vector2(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float, 4*N),
      x => asScalar() o MapGlb(0)(idF4) o asVector(4) $ x
    )

    val accessCounts = AccessCounts(f)

    assertEquals(N /^ globalSize0,
      accessCounts.vectorLoads(GlobalMemory, CoalescedPattern))
    assertEquals(N /^ globalSize0,
      accessCounts.vectorStores(GlobalMemory, CoalescedPattern))
  }

  @Test
  def vectorizeUserFun(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      x => MapGlb(0)(VectorizeUserFun(4, id)) $ x
    )

    val accessCounts = AccessCounts(f)

    assertEquals(N /^ globalSize0,
      accessCounts.vectorLoads(GlobalMemory, CoalescedPattern))
    assertEquals(N /^ globalSize0,
      accessCounts.vectorStores(GlobalMemory, CoalescedPattern))
  }

  @Test
  def vectorizeUserFun2(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float, 4*N),
      x => asScalar() o MapGlb(0)(VectorizeUserFun(4, id)) o asVector(4) $ x
    )

    val accessCounts = AccessCounts(f)

    assertEquals(N /^ globalSize0,
      accessCounts.vectorLoads(GlobalMemory, CoalescedPattern))
    assertEquals(N /^ globalSize0,
      accessCounts.vectorStores(GlobalMemory, CoalescedPattern))
  }

  @Test
  def vectorNotEvaluable(): Unit = {
    val v_N_0 = SizeVar("N")

    val idfloat4 = UserFun("idfloat4", Array("x"), """|{ return x; }""".stripMargin, Seq(VectorType(Float, 4)), VectorType(Float, 4))
    val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
    val calcAcc = UserFun("calcAcc", Array("p1", "p2", "deltaT", "espSqr"),
      """|{
         |  float4 r;
         |  r.xyz = p2.xyz - p1.xyz ;
         |  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
         |  float invDist = 1.0f / sqrt(distSqr + espSqr);
         |  float invDistCube = invDist * invDist * invDist;
         |  float s = invDistCube * p2.w;
         |  float4 res;
         |  res.xyz = s * r.xyz;
         |  return res;
         |}
         | """.stripMargin, Seq(VectorType(Float, 4), VectorType(Float, 4), Float, Float), VectorType(Float, 4))
    val update = UserFun("update", Array("pos", "vel", "deltaT", "acceleration"),
      """|{
         |  float4 newPos;
         |  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
         |  newPos.w = pos.w;
         |  float4 newVel;
         |  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
         |  newVel.w = vel.w;
         |  Tuple t = {newPos, newVel};
         |  return t;
         |}
         |    """.stripMargin, Seq(VectorType(Float, 4), VectorType(Float, 4), Float, VectorType(Float, 4)), TupleType(VectorType(Float, 4), VectorType(Float, 4)))

    val f = fun(ArrayTypeWSWC(VectorType(Float, 4), v_N_0), ArrayTypeWSWC(VectorType(Float, 4), v_N_0), Float, Float,(p_0, p_1, p_2, p_3) => FunCall(MapWrg(0)(fun((p_4) => FunCall(toGlobal(fun((p_5) => FunCall(MapLcl(0)(fun((p_6) => FunCall(update, FunCall(Get(0), p_4), FunCall(Get(1), p_4), p_3, p_6))), p_5))), FunCall(MapSeq(fun((p_7) => FunCall(toLocal(fun((p_8) => FunCall(idfloat4, p_8))), p_7))), FunCall(ReduceSeq(fun((p_9, p_10) => FunCall(fun((p_11) => FunCall(fun((p_12) => FunCall(VectorizeUserFun(4,add), p_9, p_12)), p_11)), p_10))), FunCall(idfloat4, Value("0.0f", VectorType(Float, 4))), FunCall(Join(), FunCall(MapLcl(0)(fun((p_13) => FunCall(MapSeq(fun((p_14) => FunCall(toLocal(fun((p_15) => FunCall(idfloat4, p_15))), p_14))), FunCall(ReduceSeq(fun((p_16, p_17) => FunCall(fun((p_18) => FunCall(fun((p_19) => FunCall(VectorizeUserFun(4,add), p_16, FunCall(calcAcc, FunCall(Get(0), p_4), p_19, p_3, p_2))), p_18)), p_17))), FunCall(idfloat4, Value("0.0f", VectorType(Float, 4))), p_13)))), FunCall(Split(v_N_0 * 1 /^ 512), FunCall(Gather(ReorderWithStride(512)), p_0))))))))), FunCall(Zip(2), p_0, p_1)))

    TypeChecker(f)

    val (l, g) = InferNDRange(f)

    val exprToExpr = scala.collection.immutable.Map[ArithExpr, ArithExpr](v_N_0 -> 1024)
    val g2 = g.map(ArithExpr.substitute(_, exprToExpr))

    val accessCounts = AccessCounts(f, l, g2, exprToExpr)

    val count = accessCounts.vectorLoads(GlobalMemory, UnknownPattern, exact = true)

    count.evalDouble
  }

}
