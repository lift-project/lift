package analysis

import benchmarks.{BlackScholes, MolecularDynamics, NBody, NearestNeighbour}
import ir.TupleType
import ir.ast.UserFun
import opencl.executor.Executor
import opencl.ir._

object UserFunsToBenchmark {

  private val qFun = UserFun("computeQ",
    Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag", "acc"),
    """{
      |    #define PIx2 6.2831853071795864769252867665590058f
      |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
      |    acc._0 = acc._0 + PhiMag * cos(expArg);
      |    acc._1 = acc._1 + PhiMag * sin(expArg);
      |
      |    return acc;
      |}""".
      stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float, TupleType
    (Float, Float)),
    TupleType(Float, Float))

  private val mapFun = UserFun("mapFun",
    Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag"),
    """{
      |    #define PIx2 6.2831853071795864769252867665590058f
      |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
      |    Tuple2_float_float bla = { PhiMag * cos(expArg), PhiMag * sin(expArg) };
      |    return  bla;
      |}""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float), TupleType(Float, Float))

  private val reduceFun = UserFun("reduceFun",
    Array("x", "y"),
    """{
          | x._0 += y._0;
          | x._1 += y._1;
          | return x;
        }""".stripMargin,
    Seq(TupleType(Float, Float), TupleType(Float, Float)), TupleType(Float, Float))

  private val phiMag = UserFun("phiMag",
    Array("phiR", "phiI"),
    "{ return phiR * phiR + phiI * phiI; }",
    Seq(Float, Float),
    Float)


  val userFuns = Seq(
    // linear algebra
    add,
    mult,
    add.vectorize(4),
    mult.vectorize(4),
    // MRI-Q compute Q
    qFun,
    mapFun,
    reduceFun,
    // MRI-Q phi mag
    phiMag,
    // molecular dynamics
    MolecularDynamics.mdCompute,
    // nearest neighbour,
    NearestNeighbour.distance,
    // black scholes
    BlackScholes.blackScholesComp,
    // nbody
    NBody.update,
    NBody.calcAccNoAdd
  )

  def main(args: Array[String]): Unit = {

    Executor.loadAndInit()

    val baseCallsPerThread = 1024
    val iterations = 101

    val weights = userFuns.map(uf => {

      val small = BenchmarkUserFun.benchmark(uf, baseCallsPerThread, iterations)
      val large = BenchmarkUserFun.benchmark(uf, 2*baseCallsPerThread, iterations)

      val change = large/small

      if (change > 1.8 && change < 2.2) {
        println("hurray")
      } else {
        println("boo")
      }


      large / (2 * baseCallsPerThread)
    })

    val csv = (userFuns, weights).zipped.map((uf, time) => s"${uf.name},$time").mkString("\n")

    println(csv)

    Executor.shutdown()
  }
}
