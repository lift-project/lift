package benchmarks

import ir._
import opencl.ir._

class BlackScholes(override val f: Seq[(String, Seq[Lambda])]) extends Benchmark("Black-Scholes", Seq(4096), f, 0.01f) {
  override def runScala(inputs: Any*): Array[Float] = {
    BlackScholes.runScala(inputs(0).asInstanceOf[Array[Float]])
  }

  override def generateInputs(): Seq[Any] = {
    val inputSize = inputSizes().head

    val inputData = Array.fill(inputSize)(util.Random.nextFloat())

    Seq(inputData)
  }
}

object BlackScholes {

  def runScala(input: Array[Float]): Array[Float] = {
    input.asInstanceOf[Array[Float]].map(inRand => {
      val S_LOWER_LIMIT = 10.0f
      val S_UPPER_LIMIT = 100.0f
      val K_LOWER_LIMIT = 10.0f
      val K_UPPER_LIMIT = 100.0f
      val T_LOWER_LIMIT = 1.0f
      val T_UPPER_LIMIT = 10.0f
      val R_LOWER_LIMIT = 0.01f
      val R_UPPER_LIMIT = 0.05f
      val SIGMA_LOWER_LIMIT = 0.01f
      val SIGMA_UPPER_LIMIT = 0.10f
      val S = S_LOWER_LIMIT * inRand + S_UPPER_LIMIT * (1.0f - inRand)
      val K = K_LOWER_LIMIT * inRand + K_UPPER_LIMIT * (1.0f - inRand)
      val T = T_LOWER_LIMIT * inRand + T_UPPER_LIMIT * (1.0f - inRand)
      val R = R_LOWER_LIMIT * inRand + R_UPPER_LIMIT * (1.0f - inRand)
      val V = SIGMA_LOWER_LIMIT * inRand + SIGMA_UPPER_LIMIT * (1.0f - inRand)

      val sqrtT = math.sqrt(T).toFloat
      val d1 = (math.log(S / K).toFloat + ((R + V * V * 0.05f) * T)) / V * sqrtT
      val d2 = d1 - (V * sqrtT)

      val CNDD1 = CND(d1)
      val CNDD2 = CND(d2)

      val expRT = math.exp(-T * R).toFloat
      val callResult = S * CNDD1 - K * expRT * CNDD2
      val putResult = K * expRT * (1.0f - CNDD2) - S * (1.0f - CNDD1)

      (callResult, putResult)
    }).map(_.productIterator).reduce(_++_).asInstanceOf[Iterator[Float]].toArray
  }

  private def CND(X : Float) : Float = {
    val a1 = 0.319381530f
    val a2 = -0.356563782f
    val a3 = 1.781477937f
    val a4 = -1.821255978f
    val a5 = 1.330274429f
    val a6 = 2.506628273f
    val L = X.abs
    val K = 1.0f / (1.0f + 0.2316419f * L)
    val w = 1.0f - 1.0f / 1 * a6 * math.exp((-L) * L / 2).toFloat * (a1 * K + a2 * K * K + a3 * K * K * K * +a4 * K * K * K * K + a5 * K * K * K * K * K)
    if (X < 0) {
      1.0f - w
    } else {
      w
    }
  }

  val blackScholesComp =
    UserFunDef("blackScholesComp", "inRand",
      "{\n" +
        "  #define S_LOWER_LIMIT 10.0f\n" +
        "  #define S_UPPER_LIMIT 100.0f\n" +
        "  #define K_LOWER_LIMIT 10.0f\n" +
        "  #define K_UPPER_LIMIT 100.0f\n" +
        "  #define T_LOWER_LIMIT 1.0f\n" +
        "  #define T_UPPER_LIMIT 10.0f\n" +
        "  #define R_LOWER_LIMIT 0.01f\n" +
        "  #define R_UPPER_LIMIT 0.05f\n" +
        "  #define SIGMA_LOWER_LIMIT 0.01f\n" +
        "  #define SIGMA_UPPER_LIMIT 0.10f\n" +
        "  Tuple p;\n" +
        "  \n" +
        "  float S = S_LOWER_LIMIT * inRand + S_UPPER_LIMIT * (1.0f - inRand);\n" +
        "  float K = K_LOWER_LIMIT * inRand + K_UPPER_LIMIT * (1.0f - inRand);\n" +
        "  float T = T_LOWER_LIMIT * inRand + T_UPPER_LIMIT * (1.0f - inRand);\n" +
        "  float R = R_LOWER_LIMIT * inRand + R_UPPER_LIMIT * (1.0f - inRand);\n" +
        "  float V = SIGMA_LOWER_LIMIT * inRand + SIGMA_UPPER_LIMIT * (1.0f - inRand);\n" +
        "  \n" +
        "  float sqrtT = sqrt(T);\n" +
        "  float d1 = (log(S / K) + ((R + V * V * 0.05f) * T)) / V * sqrtT;\n" +
        "  float d2 = d1 - (V * sqrtT);\n" +
        "  \n" +
        "  float CNDD1;\n" +
        "  {\n" +
        "    float L;\n" +
        "    float K1;\n" +
        "    float w;\n" +
        "    float a1 = 0.319381530f;\n" +
        "    float a2 = -0.356563782f;\n" +
        "    float a3 = 1.781477937f;\n" +
        "    float a4 = -1.821255978f;\n" +
        "    float a5 = 1.330274429f;\n" +
        "    float a6 = 2.506628273f;\n" +
        "    L = fabs(d1);\n" +
        "    K1 = 1.0f / (1.0f + 0.2316419f * L);\n" +
        "    w = 1.0f - 1.0f / 1 * a6 * exp((-1 * L) * L / 2) * (a1 * K1 + a2 * K1 * K1 * 1 + a3 * K1 * K1 * K1 * +a4 * K1 * K1 * K1 * K1 * 1 + a5 * K1 * K1 * K1 * K1 * K1);\n" +
        "    if (d1 < 0) {\n" +
        "      CNDD1 = 1.0f - w;\n" +
        "    } else {\n" +
        "      CNDD1 = w;\n" +
        "    }\n" +
        "  }\n" +
        "  float CNDD2;\n" +
        "  {\n" +
        "    float L;\n" +
        "    float K2;\n" +
        "    float w;\n" +
        "    float a1 = 0.319381530f;\n" +
        "    float a2 = -0.356563782f;\n" +
        "    float a3 = 1.781477937f;\n" +
        "    float a4 = -1.821255978f;\n" +
        "    float a5 = 1.330274429f;\n" +
        "    float a6 = 2.506628273f;\n" +
        "    L = fabs(d2);\n" +
        "    K2 = 1.0f / (1.0f + 0.2316419f * L);\n" +
        "    w = 1.0f - 1.0f / 1 * a6 * exp((-1 * L) * L / 2) * (a1 * K2 + a2 * K2 * K2 * 1 + a3 * K2 * K2 * K2 * +a4 * K2 * K2 * K2 * K2 * 1 + a5 * K2 * K2 * K2 * K2 * K2);\n" +
        "    if (d2 < 0) {\n" +
        "      CNDD2 = 1.0f - w;\n" +
        "    } else {\n" +
        "      CNDD2 = w;\n" +
        "    }\n" +
        "  }\n" +
        "  float expRT = exp(-T * R);\n" +
        "  Tuple result;\n" +
        "  result._0 = S * CNDD1 - K * expRT * CNDD2;\n" +
        "  result._1 = K * expRT * (1.0f - CNDD2) - S * (1.0f - CNDD1);\n" +
        "  return result;\n" +
        "}\n"
      , Float, TupleType(Float, Float))

  val blackScholes = fun(
    ArrayType(Float, Var("N")),
    inRand => Join() o MapWrg(Barrier() o MapLcl(blackScholesComp)) o Split(8192) $ inRand
  )

  val blackScholesAMD = fun(
    ArrayType(Float, Var("N")),
    inRand => Join() o MapWrg(Barrier() o MapLcl(blackScholesComp)) o Split(256) $ inRand
  )

  def apply() = new BlackScholes(
    Seq(
      ("BLACK_SCHOLES", Seq(blackScholes)),
      ("BLACK_SCHOLES_AMD", Seq(blackScholesAMD))))

  def main(args: Array[String]) = {
    BlackScholes().run(args)
  }

}
