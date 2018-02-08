package openmp.executor

import lift.arithmetic.SizeVar
import benchmarks.NBody
import ir._
import ir.ast.{Get, Join, Lambda2, Pad, Split, Transpose, Tuple, Unzip, UserFun, Value, Zip, fun}
import opencl.executor.Execute
import opencl.generator.OpenCLGeneratorNew
import opencl.ir._
import opencl.ir.pattern.{MapLcl, _}
import openmp.ir.AuxTypes._
import openmp.ir.pattern.{:+, MapOMP, ReduceOMP}

import scala.util.Random

/**
  * Created by Federico on 02-Aug-16.
  */
object Benchmarks {

  val increment = UserFun("increment","x", "return x;", Float,Float)

  def genID(t:Type) = UserFun(s"${t.toString}ID", "x", "return x;", t,t)

  def ultraTrivial(N:Int) = fun(
    ArrayTypeWSWC(Float,N),
    ArrayTypeWSWC(Float,N),
    (inA,inB) => {
      MapSeq(add) $ Zip(inA,inB)
    }
  )

  def nestedTrivial(N:Int,M:Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float,N),M),
    (inA) => {
      toGlobal(MapSeq(MapSeq(increment))) $ inA
    }
  )


  def matrixMultSeqAcc(N:Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
    (A, B) => {
      toGlobal(MapSeq(MapSeq(MapSeq(id)))) o MapSeq(fun( Arow =>
        MapSeq(fun( Bcol =>
          toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
        )) o Transpose() $ B
      )) $ A
    })

  def matrixMultSeq(N:Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
    (A, B) => {
      toGlobal(MapSeq(MapSeq(MapSeq(id)))) o MapSeq(fun( Arow =>
        MapSeq(fun( Bcol =>
          toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)  $ Zip(Arow, Bcol)
        )) o Transpose() $ B
      )) $ A
    })


  def matrixMultPar(N:Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
    (A, B) => {
      toGlobal(MapSeq(MapSeq(MapSeq(id)))) o  MapOMP(fun(Arow =>
        MapOMP(fun(Bcol =>
          toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)  $ Zip(Arow, Bcol)
        )) o Transpose() $ B
      )) $ A
    })

  val sN = SizeVar("N")

  def matrixMultCL(N:Int) =  fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
    (A, B) => {
      MapGlb(1)(fun( Arow =>
        MapGlb(0)(fun( Bcol =>
          toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(Arow, Bcol)
        )) o Transpose() $ B
      )) $ A
    })

  //NBody

  val mapCalc =
    UserFun("mapCalc", Array("p1", "p2"),
    """
      |{ Tuple_float_float_float_float r;
      |  r._0 = p1._0 - p2._0;
      |  r._1 = p1._1 - p2._1;
      |  r._2 = p1._2 - p2._2;
      |  return r;
      |}
    """.stripMargin, Seq(float4, float4), float4)

  val reduceCalc =
    UserFun("reduceCalc", Array("r", "detlaT", "espSqr", "acc"),
      """
        | {
        |  float distSqr = r._0*r._0 + r._1*r._1 + r._2*r._2;
        |  float invDist = 1.0f / sqrt(distSqr + espSqr);
        |  float invDistCube = invDist * invDist * invDist;
        |  float s = invDistCube * p2._3;
        |  Tuple_float_float_float_float res;
        |  res._0 = acc._0 + s * r._0;
        |  res._1 = acc._1 + s * r._1;
        |  res._2 = acc._2 + s * r._2;
        |  return res;
        | }
      """.stripMargin,
      Seq(float4, Float, Float, float4), float4)

  val calcAcc =
    UserFun("calcAcc", Array("p1", "p2", "deltaT", "espSqr", "acc"),
      """|{
        |  Tuple_float_float_float_float r;
        |  r._0 = p1._0 - p2._0;
        |  r._1 = p1._1 - p2._1;
        |  r._2 = p1._2 - p2._2;
        |  float distSqr = r._0*r._0 + r._1*r._1 + r._2*r._2;
        |  float invDist = 1.0f / sqrt(distSqr + espSqr);
        |  float invDistCube = invDist * invDist * invDist;
        |  float s = invDistCube * p2._3;
        |  Tuple_float_float_float_float res;
        |  res._0 = acc._0 + s * r._0;
        |  res._1 = acc._1 + s * r._1;
        |  res._2 = acc._2 + s * r._2;
        |  return res;
        |}
        | """.stripMargin,
      Seq(float4, float4, Float, Float, float4), float4)

  val update =
    UserFun("update", Array("pos", "vel", "deltaT", "acceleration"),
      """|{
        |  Tuple_float_float_float_float newPos;
        |  newPos._0 = pos._0 + vel._0 * deltaT + 0.5f * acceleration._0 * deltaT * deltaT;
        |  newPos._1 = pos._1 + vel._1 * deltaT + 0.5f * acceleration._1 * deltaT * deltaT;
        |  newPos._2 = pos._2 + vel._2 * deltaT + 0.5f * acceleration._2 * deltaT * deltaT;
        |  newPos._3 = pos._3;
        |  Tuple_float_float_float_float newVel;
        |  newVel._0 = vel._0 + acceleration._0 * deltaT;
        |  newVel._1 = vel._1 + acceleration._1 * deltaT;
        |  newVel._2 = vel._2 + acceleration._2 * deltaT;
        |  newVel._3 = vel._3;
        |  Tuple_Tuple_float_float_float_float_Tuple_float_float_float_float t = {newPos, newVel};
        |  return t;
        |}
      """.stripMargin,
      Seq(float4, float4, Float, float4), TupleType(float4, float4))

  def nbodyID = genID(TupleType(float4,float4))

  val float4zero = Value("(Tuple_float_float_float_float) { 0.0f, 0.0f, 0.0f, 0.0f }",float4)

  def nbodySeq(N:Int) = fun(
        ArrayTypeWSWC(float4, N),
        ArrayTypeWSWC(float4, N),
        Float,
        Float,
        (pos, vel, espSqr, deltaT) =>
          MapSeq(fun(p1 =>

            toGlobal(MapSeq(fun(acceleration =>
              update(Get(p1, 0), Get(p1, 1), deltaT, acceleration))))

              o ReduceSeq(fun((acc, p2) =>
              calcAcc(Get(p1,0), p2, deltaT, espSqr, acc)),
              float4zero) $ pos

          )) $ Zip(pos, vel)
  )

  def nbodyPar(N:Int) = fun(
    ArrayTypeWSWC(float4, N),
    ArrayTypeWSWC(float4, N),
    Float,
    Float,
    (pos, vel, espSqr, deltaT) =>
      toGlobal(MapOMP(fun(p1 =>

        (MapSeq(fun(acceleration =>
          update(Get(p1, 0), Get(p1, 1), deltaT, acceleration))))

          o ReduceSeq(fun((acc, p2) =>
          calcAcc(Get(p1,0), p2, deltaT, espSqr, acc)),
          float4zero) $ pos

      ))) $ Zip(pos, vel)
  )

  val calcAccCL =
    UserFun("calcAcc", Array("p1", "p2", "deltaT", "espSqr", "acc"),
      """|{
        |  float4 r;
        |  r.x = p1.x - p2.x;
        |  r.y = p1.y - p2.y;
        |  r.z = p1.z - p2.z;
        |  float distSqr = r.x*r.x + r.y*r.y + r.z*r.z;
        |  float invDist = 1.0f / sqrt(distSqr + espSqr);
        |  float invDistCube = invDist * invDist * invDist;
        |  float s = invDistCube * p2.w;
        |  float4 res;
        |  res.x = acc.x + s * r.x;
        |  res.y = acc.y + s * r.y;
        |  res.z = acc.z + s * r.z;
        |  return res;
        |}
        | """.stripMargin,
      Seq(Float4, Float4, Float, Float, Float4), Float4)

  val updateCL =
    UserFun("update", Array("pos", "vel", "deltaT", "acceleration"),
      """|{
        |  float4 newPos;
        |  newPos.x = pos.x + vel.x * deltaT + 0.5f * acceleration.x * deltaT * deltaT;
        |  newPos.y = pos.y + vel.y * deltaT + 0.5f * acceleration.y * deltaT * deltaT;
        |  newPos.z = pos.z + vel.z * deltaT + 0.5f * acceleration.z * deltaT * deltaT;
        |  newPos.w = pos.w;
        |  float4 newVel;
        |  newVel.x = vel.x + acceleration.x * deltaT;
        |  newVel.y = vel.y + acceleration.y * deltaT;
        |  newVel.z = vel.z + acceleration.z * deltaT;
        |  newVel.w = vel.w;
        |  Tuple t = {newPos, newVel};
        |  return t;
        |}
      """.stripMargin,
      Seq(Float4, Float4, Float, Float4), TupleType(Float4, Float4))

  def nbodyCL(N:Int) = fun(
    ArrayTypeWSWC(Float4, N),
    ArrayTypeWSWC(Float4, N),
    Float,
    Float,
    (pos, vel, espSqr, deltaT) =>
      MapGlb(fun(p1 =>

        toGlobal(MapSeq(fun(acceleration =>
          updateCL(Get(p1, 0), Get(p1, 1), deltaT, acceleration))))

          o ReduceSeq(fun((acc, p2) =>
          calcAccCL(Get(p1,0), p2, deltaT, espSqr, acc)),
          Value("(float4) 0.0f", Float4)) $ pos

      )) $ Zip(pos, vel)
  )

  val blackScholesComp =
    UserFun("blackScholesComp", "inRand",
      """|{
        |  #define S_LOWER_LIMIT 10.0f
        |  #define S_UPPER_LIMIT 100.0f
        |  #define K_LOWER_LIMIT 10.0f
        |  #define K_UPPER_LIMIT 100.0f
        |  #define T_LOWER_LIMIT 1.0f
        |  #define T_UPPER_LIMIT 10.0f
        |  #define R_LOWER_LIMIT 0.01f
        |  #define R_UPPER_LIMIT 0.05f
        |  #define SIGMA_LOWER_LIMIT 0.01f
        |  #define SIGMA_UPPER_LIMIT 0.10f
        |  Tuple p;
        |
        |  float S = S_LOWER_LIMIT * inRand + S_UPPER_LIMIT * (1.0f - inRand);
        |  float K = K_LOWER_LIMIT * inRand + K_UPPER_LIMIT * (1.0f - inRand);
        |  float T = T_LOWER_LIMIT * inRand + T_UPPER_LIMIT * (1.0f - inRand);
        |  float R = R_LOWER_LIMIT * inRand + R_UPPER_LIMIT * (1.0f - inRand);
        |  float V = SIGMA_LOWER_LIMIT * inRand + SIGMA_UPPER_LIMIT * (1.0f - inRand);
        |
        |  float sqrtT = sqrt(T);
        |  float d1 = (log(S / K) + ((R + V * V * 0.05f) * T)) / V * sqrtT;
        |  float d2 = d1 - (V * sqrtT);
        |
        |  float CNDD1;
        |  {
        |    float L;
        |    float K1;
        |    float w;
        |    float a1 = 0.319381530f;
        |    float a2 = -0.356563782f;
        |    float a3 = 1.781477937f;
        |    float a4 = -1.821255978f;
        |    float a5 = 1.330274429f;
        |    float a6 = 2.506628273f;
        |    L = fabs(d1);
        |    K1 = 1.0f / (1.0f + 0.2316419f * L);
        |    w = 1.0f - 1.0f / 1 * a6 * exp((-1 * L) * L / 2) * (a1 * K1 + a2 * K1 * K1 * 1 + a3 * K1 * K1 * K1 * +a4 * K1 * K1 * K1 * K1 * 1 + a5 * K1 * K1 * K1 * K1 * K1);
        |    if (d1 < 0) {
        |      CNDD1 = 1.0f - w;
        |    } else {
        |      CNDD1 = w;
        |    }
        |  }
        |  float CNDD2;
        |  {
        |    float L;
        |    float K2;
        |    float w;
        |    float a1 = 0.319381530f;
        |    float a2 = -0.356563782f;
        |    float a3 = 1.781477937f;
        |    float a4 = -1.821255978f;
        |    float a5 = 1.330274429f;
        |    float a6 = 2.506628273f;
        |    L = fabs(d2);
        |    K2 = 1.0f / (1.0f + 0.2316419f * L);
        |    w = 1.0f - 1.0f / 1 * a6 * exp((-1 * L) * L / 2) * (a1 * K2 + a2 * K2 * K2 * 1 + a3 * K2 * K2 * K2 * +a4 * K2 * K2 * K2 * K2 * 1 + a5 * K2 * K2 * K2 * K2 * K2);
        |    if (d2 < 0) {
        |      CNDD2 = 1.0f - w;
        |    } else {
        |      CNDD2 = w;
        |    }
        |  }
        |  float expRT = exp(-T * R);
        |  Tuple result;
        |  result._0 = S * CNDD1 - K * expRT * CNDD2;
        |  result._1 = K * expRT * (1.0f - CNDD2) - S * (1.0f - CNDD1);
        |  return result;
        |}
      """.stripMargin
      , Float, TupleType(Float, Float))


  def blackScholesSeq(N:Int) = fun(
    ArrayTypeWSWC(Float, N),
    inRand => MapSeq(blackScholesComp)  $ inRand
  )

  def blackScholesPar(N:Int) = fun(
    ArrayTypeWSWC(Float,N),
    inRand => MapOMP(blackScholesComp) $ inRand
  )

  def blackScholesCL(N:Int)  = fun(
    ArrayTypeWSWC(Float, N),
    inRand => Join() o MapWrg(MapLcl(blackScholesComp)) o Split(1280) $ inRand
  )

  val squareAdd = UserFun("squareAdd", Array("x","y"),"return x + sqrt(((y * y)/52));",Seq(Float,Float),Float)
  val square = UserFun("square", "x", "return sqrt(((x * x)/52));;",Float,Float)
  def squareAccSeq(N:Int) = fun(
    ArrayTypeWSWC(Float,N),
    in =>  toGlobal(MapSeq(id)) o ReduceSeq(add,0.0f) o MapOMP(square) $ in
  )
  def squareAccPar(N:Int) = fun(
    ArrayTypeWSWC(Float,N),
    in =>  toGlobal(MapSeq(id)) o ReduceOMP(:+(Float),squareAdd,0.0f) $ in
  )

  def other(args: Array[String]) {
    val N = 40000
    val big = 800
    val ls = List.iterate(0,N)(x => x + 1)
    val bigList = (List.iterate(0,big)(x => x + 1)).map(_ => (List.iterate(0,big)(x => x + 1).map(_ => 1)))
    val NSize = 1000
    //Executor.compileAndGenerateScript(matrixMultPar(big),bigList ++ bigList,"D:/Test")
  }


  def benchPath(tName:String, tSize:String, parSeq:String) = s"D:/Benchmarks/$tName$tSize/$parSeq"

  abstract class Algotype
  object Sequential extends Algotype {
    override def toString = "Seq"
  }
  object Parallel extends Algotype {
    override def toString = "Par"
  }

  def matrixMult(size:Int, algotype: Algotype) = {
    val rand = new Random(22)
    val input1 = randomSquare(size,size,rand)
    val input2 = randomSquare(size,size,rand)
    val params = input1 ++ input2
    val kernel = algotype match {
      case Sequential => matrixMultSeq(size)
      case Parallel => matrixMultPar(size)
    }
    Executor.compileAndGenerateScript(kernel,params, benchPath("MM", size.toString, algotype.toString))
  }

  def nBody(size:Int, algo:Algotype):Unit = {
    val rand = new Random(32)
    val deltaT = 0.005f
    val espSqr = 500.0f
    val input1 = List.fill(size)(List(rand.nextFloat(),rand.nextFloat(),rand.nextFloat(),rand.nextFloat())).flatten
    val input2 = List.fill(size)(List(rand.nextFloat(),rand.nextFloat(),rand.nextFloat(),rand.nextFloat())).flatten
    val params = input1 ++ input2 ++ List(espSqr) ++ List(deltaT)
    val kernel = algo match {
      case Sequential => nbodySeq(size)
      case Parallel => nbodyPar(size)
    }

    Executor.compileAndGenerateScript(kernel, params, benchPath("NBody",size.toString,algo.toString))
  }

  def blackScholes(size:Int, algo:Algotype):Unit = {
    val rand = new Random(32)
    val input = List.fill(size)(rand.nextFloat())
    val kernel = algo match {
      case Sequential => blackScholesSeq(size)
      case Parallel => blackScholesPar(size)
    }
    Executor.compileAndGenerateScript(kernel, input, benchPath("BS", size.toString, algo.toString))
  }

  def squareAcc(size:Int, algo:Algotype):Unit = {
    val rand = new Random(32)
    val input = List.fill(size)(rand.nextFloat())
    val kernel = algo match {
      case Sequential => squareAccSeq(size)
      case Parallel => squareAccPar(size)
    }
    Executor.compileAndGenerateScript(kernel, input, benchPath("SquareAcc", size.toString, algo.toString))
  }

  /*def dotProduct(size:Int, algo:Algotype) = {
    val rand = new Random(2)
    val input1 = List.fill(size)(rand.nextFloat())
    val input2 = List.fill(size)(rand.nextFloat())
    val kernel = algo match {
      case Sequential => dotProductSeq(size)
      case Parallel => dotProductPar(size)
    }
    Executor.compileAndGenerateScript(kernel, input1 ++ input2, benchPath("DP", size.toString, algo.toString))
  }*/

  private def randomSquare(sizeX:Int, sizeY:Int, random:Random) = List.fill(sizeX, sizeY)(random.nextFloat()).flatten

  def runBlackScholesCL(N:Int):Unit = {
    val rand = new Random
    val input = Array.fill(N)(rand.nextFloat())
    val (total, runtime) = Execute(N)[Array[Float]](blackScholesCL(N),input)
    println(s"BS$N total = ${total.toString}, runtime = $runtime")
  }

  def runMMCL(N:Int):Unit = {
    val rand = new Random
    val matrixA = Array.tabulate(N, N)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(N, N)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val (total, runtime) = Execute(N*N)[Array[Float]](matrixMultCL(N),matrixA, matrixB)
    println(s"MM$N runtime = $runtime")
  }

  def runNBody(N:Int):Unit = {
    val rand = new Random()
    val inputsA = Array.fill(N)(rand.nextFloat())
    val inputsB = Array.fill(N)(rand.nextFloat())
    val deltaT = 0.005f
    val espSqr = 500.0f
    val (_, runtime) = Execute(N)[Array[Float]](nbodyCL(N), inputsA, inputsB, deltaT, espSqr)
    println(s"NBody$N runtime = $runtime")
  }

  def main(args:Array[String]): Unit = {
    squareAcc(10000,Parallel)
  }

  def mainOCL(args: Array[String]): Unit = {
    //matrixMult(200,Parallel)
    opencl.executor.Executor.loadLibrary()
    opencl.executor.Executor.init()
    runBlackScholesCL(1280)
    runBlackScholesCL(12800)
    runBlackScholesCL(51200)
    runBlackScholesCL(128000)
    runMMCL(128)
    runMMCL(256)
    runMMCL(512)
    runMMCL(640)
    //runMMCL(896)
    println("MMCL 896 goes here...sigh :(")
    runNBody(512)
    runNBody(1280)
    runNBody(5120)
    runNBody(12800)
    opencl.executor.Executor.shutdown()

  }
}
