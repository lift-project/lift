package exploration.detection

import exploration.MemoryMappingRewrite
import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.rules.OpenCLRules

class TestImplement {

  @Test
  def nbody(): Unit = {
    val v_N_0 = Var("N", StartFromRange(1))
    val v__1 = Var("", StartFromRange(1))
    val v__2 = Var("", StartFromRange(1))

    val idfloat4 = UserFun("idfloat4", Array("x"), """|{ return x; }""".stripMargin, Seq(VectorType(Float, 4)), VectorType(Float, 4))
    val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
    val calcAcc = UserFun("calcAcc", Array("p1", "p2", "deltaT", "espSqr"), """|{
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
    val update = UserFun("update", Array("pos", "vel", "deltaT", "acceleration"), """|{
                                                                                     |  float4 newPos;
                                                                                     |  newPos.xyz = pos.xyz + vel.xyz * deltaT + 0.5f * acceleration.xyz * deltaT * deltaT;
                                                                                     |  newPos.w = pos.w;
                                                                                     |  float4 newVel;
                                                                                     |  newVel.xyz = vel.xyz + acceleration.xyz * deltaT;
                                                                                     |  newVel.w = vel.w;
                                                                                     |  Tuple t = {newPos, newVel};
                                                                                     |  return t;
                                                                                     |}""".stripMargin, Seq(VectorType(Float, 4), VectorType(Float, 4), Float, VectorType(Float, 4)), TupleType(VectorType(Float, 4), VectorType(Float, 4)))
    val f = fun(ArrayType(VectorType(Float, 4), v_N_0), ArrayType(VectorType(Float, 4), v_N_0), Float, Float,(p_0, p_1, p_2, p_3) => FunCall(Join(), FunCall(MapWrg(0)(fun((p_4) => FunCall(TransposeW(), FunCall(MapSeq(fun((p_5) => FunCall(toGlobal(fun((p_6) => FunCall(MapLcl(0)(fun((p_7) => FunCall(update, FunCall(Get(0), FunCall(Get(0), p_7)), FunCall(Get(1), FunCall(Get(0), p_7)), p_3, FunCall(Get(1), p_7)))), p_6))), FunCall(Zip(2), p_4, p_5)))), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_10) => FunCall(ReduceSeq(fun((p_11, p_12) => FunCall(VectorizeUserFun(Cst(4),add), p_11, FunCall(calcAcc, FunCall(Get(0), FunCall(Get(1), p_10)), p_12, p_3, p_2)))), FunCall(Get(0), p_10), p_9))), FunCall(Zip(2), p_8, p_4))))), FunCall(MapLcl(0)(fun((p_13) => FunCall(idfloat4, p_13))), Value("0.0f", ArrayType(VectorType(Float, 4), v__1))), FunCall(Split(v__2), p_0)))))), FunCall(Split(v__1), FunCall(Zip(2), p_0, p_1)))))


    val strategicLocationsMarked =
      MemoryMappingRewrite.addIdsForPrivate(MemoryMappingRewrite.addIdsForLocal(f))

    val localMemCandidates = (DetectReuseWithinThread.getCandidates(strategicLocationsMarked) ++
      DetectReuseAcrossThreads.getCandidates(strategicLocationsMarked)).
      distinct.
      filter(x => OpenCLRules.localMemory.isDefinedAt(x._1))


    println(strategicLocationsMarked)
    println(localMemCandidates)

//    localMemCandidates.
    val lambdas = ImplementReuse(strategicLocationsMarked, localMemCandidates, OpenCLRules.localMemory)

    val cleanedLambdas = lambdas.map(MemoryMappingRewrite.cleanup) :+ f
  }

  @Test
  def mm(): Unit = {
    val f = mmTiled2DBlocked
    val strategicLocationsMarked =
      MemoryMappingRewrite.addIdsForPrivate(MemoryMappingRewrite.addIdsForLocal(f))

    val localMemCandidates =
      DetectReuseAcrossThreads.getCandidates(strategicLocationsMarked).
      distinct.
      filter(x => OpenCLRules.localMemory.isDefinedAt(x._1))

    val realCandidates = Seq(localMemCandidates(0), localMemCandidates(2))

    val lambdas = ImplementReuse(strategicLocationsMarked, realCandidates, OpenCLRules.localMemory)

    val cleanedLambdas = lambdas.map(MemoryMappingRewrite.cleanup) :+ f

    val good = cleanedLambdas(2)

    {
      val strategicLocationsMarked =
        MemoryMappingRewrite.addIdsForPrivate(good)

      val privateMemCandidates = (DetectReuseWithinThread.getCandidates(strategicLocationsMarked) ++
        DetectReuseAcrossThreads.getCandidates(strategicLocationsMarked)).
        distinct.
        filter(x => OpenCLRules.privateMemory.isDefinedAt(x._1))

    println(strategicLocationsMarked)
    println(privateMemCandidates)

      val realCandidates = Seq(privateMemCandidates(0), privateMemCandidates(2))

      val lambdas = ImplementReuse(strategicLocationsMarked, realCandidates, OpenCLRules.privateMemory)

      println(lambdas(2))
    }
  }

  @Test
  def reuseMappingCount(): Unit = {
    val f = mmTiled2DBlocked
    val strategicLocationsMarked =
      MemoryMappingRewrite.addIdsForPrivate(MemoryMappingRewrite.addIdsForLocal(f))

    val localMemCandidates = (DetectReuseWithinThread.getCandidates(strategicLocationsMarked) ++
      DetectReuseAcrossThreads.getCandidates(strategicLocationsMarked)).
      distinct.
      filter(x => OpenCLRules.localMemory.isDefinedAt(x._1))

    val lambdas = ImplementReuse(strategicLocationsMarked, localMemCandidates, OpenCLRules.localMemory)

    val cleanedLambdas = lambdas.map(MemoryMappingRewrite.cleanup) :+ f

    val cleanedWithPrivate = cleanedLambdas.flatMap(lowered => {
      val strategicLocationsMarked =
        MemoryMappingRewrite.addIdsForPrivate(lowered)

      val privateMemCandidates = (DetectReuseWithinThread.getCandidates(strategicLocationsMarked) ++
        DetectReuseAcrossThreads.getCandidates(strategicLocationsMarked)).
        distinct.
        filter(x => OpenCLRules.privateMemory.isDefinedAt(x._1))

      val lambdas = ImplementReuse(strategicLocationsMarked, privateMemCandidates, OpenCLRules.privateMemory)

      lambdas.map(MemoryMappingRewrite.cleanup)
    }) ++ cleanedLambdas

    assertTrue(cleanedWithPrivate.length > 1)
  }
}
