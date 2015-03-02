package opencl.generator

import java.awt.image.BufferedImage
import java.io.{IOException, File}
import javax.imageio.ImageIO

import ir._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestBenchmark {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestBenchmark {
  // A mix between AMD and nVidia versions, with the CND function inlined in the UserFunDef
  @Test def blackScholes() : Unit = {

    val inputSize = 512
    val input = Array.fill(inputSize)(util.Random.nextFloat())

    val gold = input.map(inRand => {
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

    val kernel = fun(
      ArrayType(Float, Var("N")),
      inRand => MapGlb(blackScholesComp) $ inRand
    )

    val (output, runtime) = Execute(inputSize)(kernel, input, inputSize)

    assertArrayEquals(gold, output, 0.01f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
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

  @Test def kmeansMembership(): Unit = {
    val inputSize = 512
    val k = 16

    val pointsX = Array.fill(inputSize)(util.Random.nextFloat())
    val pointsY = Array.fill(inputSize)(util.Random.nextFloat())
    val centresX = Array.fill(k)(util.Random.nextFloat())
    val centresY = Array.fill(k)(util.Random.nextFloat())
    val indices = Array.range(0, inputSize)

    val distance = UserFunDef("dist", Array("x", "y", "a", "b", "id"), "{ Tuple t = {(x - a) * (x - a) + (y - b) * (y - b), id}; return t; }", Seq(Float, Float, Float, Float, Int), TupleType(Float, Int))
    val minimum = UserFunDef("minimum", Array("x", "y"), "{ return x._0 < y._0 ? x : y; }", Seq(TupleType(Float, Int), TupleType(Float, Int)), TupleType(Float, Int))
    val getSecond = UserFunDef("getSecond", "x", "{ return (float) x._1; }", TupleType(Float, Int), Float)

    val points = pointsX zip pointsY
    val centres = (centresX, centresY, indices).zipped

    val gold = points.map(x => {
      centres
        .map((a,b,id) => ((x._1 - a) * (x._1 - a) + (x._2 - b) * (x._2 - b),id))
        .reduce( (p1, p2) => if (p1._1 < p2._1) p1 else p2)
        ._2
        .toFloat
    })

    val N = Var("N")
    val K = Var("K")

    // TODO: integer output
    val function = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, K),
      ArrayType(Float, K),
      ArrayType(Int, K),
      (x, y, a, b, i) => MapGlb(fun( xy => MapSeq(getSecond) o ReduceSeq(minimum, (scala.Float.MaxValue, -1)) o MapSeq(fun( ab => distance(Get(xy, 0), Get(xy, 1), Get(ab, 0), Get(ab, 1), Get(ab, 2)))) $ Zip(a,b,i))) $ Zip(x, y)
    )

    val (output, runtime) = Execute(inputSize)(function, pointsX, pointsY, centresX, centresY, indices, inputSize, k)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  // Output type
  @Test def mandelbrot(): Unit = {
    val inputSize = 512

    val iterations = 100

    val input = Array.range(0, inputSize)

    val gold = input.map(i => {
      input.map(j => {
        val space = 2.0f / inputSize

        var Zr = 0.0f
        var Zi = 0.0f
        val Cr = j * space - 1.5f
        val Ci = i * space - 1.0f

        var ZrN = 0.0f
        var ZiN = 0.0f
        var y = 0
        while (ZiN + ZrN <= 4.0f && y < iterations) {
          Zi = 2.0f * Zr * Zi + Ci
          Zr = ZrN - ZiN + Cr
          ZiN = Zi * Zi
          ZrN = Zr * Zr
          y += 1
        }

        ((y * 255) / iterations).toFloat
        //        y.toFloat
      })
    }).flatten

    val md = UserFunDef("md", Array("i", "j", "niters", "size"),
      "{ \n" +
        "  float space = 2.0f / size;\n" +
        "  float Zr = 0.0f;\n" +
        "  float Zi = 0.0f;\n" +
        "  float Cr = (j * space - 1.5f);\n" +
        "  float Ci = (i * space - 1.0f);\n" +
        "  \n" +
        "  float ZrN = 0;\n" +
        "  float ZiN = 0;\n" +
        "  int y = 0;\n" +
        "  \n" +
        "  for (y = 0; y < niters && ZiN + ZrN <= 4.0f; y++) {\n" +
        "    Zi = 2.0f * Zr * Zi + Ci;\n" +
        "    Zr = ZrN - ZiN + Cr;\n" +
        "    ZiN = Zi * Zi;\n" +
        "    ZrN = Zr * Zr;\n" +
        "  }\n" +
        "  return (float) ((y * 255) / niters);\n" +
        //        "  return (float) y;\n" +
        "}\n", Seq(Int, Int, Int, Int), Float)

    val f = fun(
      ArrayType(Int, Var("N")),
      Int,
      Int,
      (in, niters, size) => MapGlb(fun(i=>MapSeq(fun(j => md(i, j, niters, size))) $ in)) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, input, iterations, inputSize, inputSize)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)


    assertArrayEquals(gold, output, 0.0f)
  }

  private def writeMD(width : Int, height : Int, data : Array[Float], name : String): Unit = {
    val out = new File(name + ".png")
    val img = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
    val r = img.getRaster

    for (i <- 0 until height) {
      for (j <- 0 until width) {
        r.setSample(j, i, 0, data(i*height + j).toByte)
      }
    }

    try {
      ImageIO.write (img, "png", out)
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  @Test def nbody(): Unit = {

    val inputSize = 512

    val deltaT = 0.005f
    val espSqr = 500.0f

    // x, y, z, velocity x, y, z, mass
    val input = Array.fill(inputSize)((util.Random.nextFloat(),util.Random.nextFloat(),util.Random.nextFloat(),0.0f,0.0f,0.0f,util.Random.nextFloat()))

    val x = input.map(_._1)
    val y = input.map(_._2)
    val z = input.map(_._3)

    val velX = input.map(_._4)
    val velY = input.map(_._5)
    val velZ = input.map(_._6)

    val mass =input.map(_._7)

    val gold = input.map(x => {
      val acceleration = input.map(y => {
        val r = Array(0.0f, 0.0f, 0.0f)

        r(0) = x._1 - y._1
        r(1) = x._2 - y._2
        r(2) = x._3 - y._3

        val distSqr = r.sum

        val invDist = 1.0f / math.sqrt(distSqr + espSqr)

        val s = invDist * invDist * invDist * y._7

        (s*r(0), s*r(1), s*r(2))
      }).reduce((y, z) => (y._1 + z._1, y._2 + z._2, y._3 + z._3))

      val px = x._4 * deltaT + 0.5f * acceleration._1 * deltaT * deltaT
      val py = x._5 * deltaT + 0.5f * acceleration._2 * deltaT * deltaT
      val pz = x._6 * deltaT + 0.5f * acceleration._3 * deltaT * deltaT

      (x._1 + px.toFloat, x._2 + py.toFloat, x._3 + pz.toFloat, x._4 + acceleration._1.toFloat * deltaT, x._5 + acceleration._2.toFloat * deltaT, x._6 + acceleration._3.toFloat * deltaT, x._7)
    }).map(_.productIterator).reduce(_++_).asInstanceOf[Iterator[Float]].toArray

    val calcAcc = UserFunDef("calcAcc", Array("x1", "y1", "z1", "x2", "y2", "z2", "mass", "espSqr"),
      "{\n" +
        "  float4 r = (x1 - x2, y1 - y2, z1 - z2, 0.0f);\n" +
        "  float distSqr = r.x + r.y + r.z;\n" +
        "  float invDist = 1.0f / sqrt(distSqr + espSqr);\n" +
        "  float invDistCube = invDist * invDist * invDist;\n" +
        "  float s = invDistCube * mass;\n" +
        "  Tuple acc = {s * r.x, s * r.y, s * r.z};" +
        "  return acc;\n" +
        "}\n", Seq(Float,Float, Float, Float, Float, Float, Float, Float), TupleType(Float, Float, Float))
    val reduce = UserFunDef("reduce", Array("x", "y"), "{ Tuple t = {x._0 + y._0, x._1 + y._1, x._2 + y._2}; return t;}", Seq(TupleType(Float, Float, Float), TupleType(Float, Float, Float)), TupleType(Float, Float, Float))
    val update = UserFunDef("update", Array("x", "y", "z", "velX", "velY", "velZ", "mass", "deltaT", "acceleration"),
      "{\n" +
        "  float px = velX * deltaT + 0.5f * acceleration._0 * deltaT * deltaT;\n" +
        "  float py = velY * deltaT + 0.5f * acceleration._1 * deltaT * deltaT;\n" +
        "  float pz = velZ * deltaT + 0.5f * acceleration._2 * deltaT * deltaT;\n" +
        "  Tuple1 t = {x + px, y + py, z + pz, velX + acceleration._0 * deltaT, velY + acceleration._1 * deltaT, velZ + acceleration._2 * deltaT, mass};\n" +
        "  return t;\n" +
        "}\n", Seq(Float,Float, Float, Float, Float, Float, Float, Float, TupleType(Float, Float, Float)), TupleType(Float,Float, Float, Float, Float, Float, Float) )

    val N = Var("N")

    val function = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      Float,
      Float,
      (x, y, z, velX, velY, velZ, mass, espSqr, deltaT) =>
        MapGlb(fun(xyz => MapSeq(fun(acceleration => update(Get(xyz, 0), Get(xyz, 1), Get(xyz, 2), Get(xyz, 3), Get(xyz, 4), Get(xyz, 5), Get(xyz, 6), deltaT, acceleration))) o ReduceSeq(reduce, (0.0f, 0.0f, 0.0f)) o MapSeq(fun(abc => calcAcc(Get(xyz, 0), Get(xyz, 1), Get(xyz, 2), Get(abc, 0), Get(abc, 1), Get(abc, 2), Get(abc, 6), espSqr))) $ Zip(x, y, z, velX, velY, velZ, mass))) $ Zip(x, y, z, velX, velY, velZ, mass)
    )

    val (output, runtime) = Execute(inputSize)(function, x, y, z, velX, velY, velZ, mass, espSqr, deltaT, inputSize)

    assertArrayEquals(gold, output, 0.0001f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

}
