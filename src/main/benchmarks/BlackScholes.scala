package benchmarks

import lift.arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

@deprecated("Uses an old benchmark infrastructure", "")
class BlackScholes(override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("Black-Scholes", Seq(4096), f, 0.01f) {
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

  val blackScholes = fun(
    ArrayTypeWSWC(Float, SizeVar("N")),
    inRand => Join() o MapWrg(MapLcl(blackScholesComp)) o Split(8192) $ inRand
  )

  val blackScholesAMD = fun(
    ArrayTypeWSWC(Float, SizeVar("N")),
    inRand => Join() o MapWrg(MapLcl(blackScholesComp)) o Split(256) $ inRand
  )

  def apply() = new BlackScholes(
    Seq(
      ("BLACK_SCHOLES", Array[Lambda](blackScholes)),
      ("BLACK_SCHOLES_AMD", Array[Lambda](blackScholesAMD))))

  def main(args: Array[String]): Unit = {
    BlackScholes().run(args)
  }

}
