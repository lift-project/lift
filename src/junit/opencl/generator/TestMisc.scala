package opencl.generator

import org.junit.Assert._
import org.junit.{Ignore, AfterClass, BeforeClass, Test}
import opencl.executor._
import opencl.ir._
import ir._
import ir.UserFunDef._


object TestMisc {
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

class TestMisc {

  @Ignore
  @Test def compositionTest(): Unit = {
    // TODO: Crashes the VM, compilation fails on the native side
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val gold = inputData.map(x => - x)

    val composition = id o neg
    val N = Var("N")

    val compFun = fun(
        ArrayType(Float, N),
      (input) =>
        MapGlb(composition) $ input
    )

    val (output, runtime) = Execute(inputSize)(compFun, inputData, inputData.length)
    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def accessingMultidimArrayAfterZip(): Unit = {
    val Nsize = 8
    val Msize = 4
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)
    val vector = Array.fill(Nsize)(1.0f)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, K), M), N),
      ArrayType(Float, N),
      (matrix, vector) => MapGlb(fun(r =>
        MapSeq(fun(t =>
          MapSeq(id) $ Get(t, 0)
        )) $ Zip(Get(r,0), vector)
      )) $ Zip(matrix, vector)
    )

    val (output, runtime) = Execute(4, Nsize)(f, matrix, vector, Nsize, Msize, Ksize)
    assertArrayEquals(matrix.flatten.flatten, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def vectorType(): Unit = {
    val inputSize = 1024
    val inputData = Array.tabulate(inputSize*4)(_.toFloat)

    val N = Var("N")

    val f = fun(
      ArrayType(Float4, N),
      (input) =>
        MapGlb(Vectorize(4)(id)) $ input
    )

    val (output, runtime) = Execute(inputSize)(f, inputData, inputSize)
    assertArrayEquals(inputData, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def composeUserFunctionWithPattern(): Unit = {

    val Nsize = 512
    val Msize = 512
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => 1.0f * c * r)
    val gold   = matrix.map(- _.sum)

    val function = fun(
          ArrayType(ArrayType(Float, Var("N")), Var("M")),
          (input) => MapGlb(MapSeq(neg) o ReduceSeq(add, 0.0f)) $ input
    )

    val (output, runtime) = Execute(Nsize * Msize)(function, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def vectorize(): Unit = {
    val inputSize = 512
    val inputData = Array.tabulate(inputSize)(_.toFloat)

    val gold = inputData.map(_+1)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => asScalar() o MapGlb(Vectorize(4)(plusOne)) o asVector(4) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, inputData, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def asVectorFollowedByAsScalar(): Unit = {
    val inputSize = 512
    val inputData = Array.tabulate(inputSize)(_.toFloat)

    val gold = inputData.map(_+1)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapGlb(plusOne) o asScalar() o asVector(4) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, inputData, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def joinThenSplit2D(): Unit = {

    val Nsize = 256
    val Msize = 128
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * Msize.toFloat)

    val N = Var("N")
    val M = Var("M")


    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => Split(Msize) o MapGlb(0)(id) o Join() $ matrix
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.flatten, output, 0.0f)
  }

  @Test def joinThenSplit3D(): Unit = {

    val Nsize = 256
    val Msize = 128
    val Ksize = 64
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 1.0f + r * Msize.toFloat + z * Msize * Ksize)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")


    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, K), M), N),
      (matrix) => Split(Msize) o MapGlb(0)(MapSeq(id)) o Join() $ matrix
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize, Ksize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.flatten.flatten, output, 0.0f)
  }

  @Test def joinThenSplitInsideMap3D(): Unit = {

    val Nsize = 256
    val Msize = 128
    val Ksize = 64
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 1.0f + r * Msize.toFloat + z * Msize * Ksize)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")


    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, K), M), N),
      (matrix) => MapGlb(0)(Split(Ksize) o MapSeq(id) o Join()) $ matrix
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize, Ksize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.flatten.flatten, output, 0.0f)
  }

  @Test def joinJoinThenSplitSplit3D(): Unit = {

    val Nsize = 256
    val Msize = 128
    val Ksize = 64
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 1.0f + r * Msize.toFloat + z * Msize * Ksize)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")


    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, K), M), N),
      (matrix) => Split(Msize) o Split(Ksize) o MapGlb(0)(id) o Join() o Join() $ matrix
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize, Ksize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.flatten.flatten, output, 0.0f)
  }

  @Test def iterate(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.map(_+1).map(_+1).map(_+1).map(_+1).map(_+1)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Iterate(5)(MapGlb(plusOne)) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, input, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def iterateLocalOnly(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.map(_+1).map(_+1).map(_+1).map(_+1).map(_+1)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(toGlobal(MapLcl(id)) o Iterate(5)(MapLcl(plusOne)) o toLocal(MapLcl(id))) o Split(16) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, input, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def localGlobalMemory(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)

    val gold = input.map(_+1)

    val  f = fun(
      ArrayType(Float, Var("N")),
      in => Join() o MapWrg(toGlobal(MapLcl(plusOne)) o toLocal(MapLcl(id))) o Split(4) $ in
    )

    val (output, runtime) = Execute(inputSize)(f, input, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def decompose(): Unit = {

    val nSize = 512
    val mSize = 256
    val A = Array.tabulate(nSize, mSize)((x, y) => x + y.toFloat)
    val B = Array.tabulate(nSize, mSize)((x, y) => x + y.toFloat)

    val gold = A.map(a => B.map(b => (a, b).zipped)).map(_.map(_.map(_+_)))


    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(ArrayType(Float, M), N),
      (X, Y) => MapGlb(MapSeq(MapSeq(fun(z => add.apply(Get(z, 0), Get(z, 1)))))) o Map(fun(x => Map(fun(y => Zip(x, y))) $ Y )) $ X
    )

    val (output, runtime) = Execute(nSize)(f, A, B, nSize, mSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }


  /* TODO: Not there yet ... Transpose the tiles as well ...
  @Test def MATRIX_TRANSPOSE_TILED(): Unit = {

    val Nsize = 16
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix: ")
    myPrint(matrix)

    val N = Var("N")
    val M = Var("M")

    val r = 8
    val c = 4

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        Join() o MapWrg(0)(fun( rows =>
          MapSeq(Join()) o Swap() o Scatter(transpose)(Gather(transpose)(MapWrg(1)(fun( tile =>

            // step 2: write back to global memory
            toGlobal(Gather(transpose)(MapLcl(0)(MapLcl(1)(fun(id(_)))))) o Swap() o

            // step 1: load tile to local memory
            toLocal(MapLcl(0)(MapLcl(1)(fun(id(_))))) o tile

          )))) o Swap() o MapSeq(Split(c)) o rows
        )) o Split(r) o matrix
      })

    val comp = Compile(f)

    val (output, runtime) = Execute(8, Nsize * Msize)(f, matrix, Msize, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    myPrint(gold.flatten, Nsize)

    println("output: ")
    myPrint(output, Nsize)

    (gold.flatten, output).zipped.map(assertEquals(_,_,0.0))
  }
  */


  /*
            @Test def BLACK_SCHOLES_NVIDIA_VERSION() {

              val pricesType = UserType("typedef struct { float call; float put; } prices;")

              val cnd =
                UserFun("CND", Array("d"),
                    "{ const float A1       =  0.319381530f;\n" +
                      "const float A2       = -0.356563782f;\n  " +
                      "const float A3       =  1.781477937f;\n  " +
                      "const float A4       = -1.821255978f;\n  " +
                      "const float A5       =  1.330274429f;\n  " +
                      "const float RSQRT2PI =  0.39894228040143267793994605993438f;\n\n  " +
                      "float K = 1.0f / (1.0f + 0.2316419f * fabs(d));\n\n  " +
                      "float cnd = RSQRT2PI * exp(-0.5f * d * d)\n" +
                      "            * (K * (A1 + K * (A2 + K * (A3 + K * (A4 + K * A5)))));\n  \n  " +
                      "if (d > 0) cnd = 1.0f - cnd;\n\n  " +
                      "return cnd; }", Float, Float)

              val blackScholesComp =
                UserFun("blackScholesComp", Array("S", "X", "T", "R", "V"),
                    "{ float sqrtT = sqrt(T);\n  " +
                      "float    d1 = (log(S / X) + (R + 0.5f * V * V) * T) / (V * sqrtT);\n  " +
                      "float    d2 = d1 - V * sqrtT;\n  " +
                      "float CNDD1 = CND(d1);\n  " +
                      "float CNDD2 = CND(d2);\n\n  " +
                      "float expRT = exp(- R * T);\n  " +
                      "prices p;\n  " +
                      "p.call = (S * CNDD1 - X * expRT * CNDD2);\n  " +
                      "p.put  = (X * expRT * (1.0f - CNDD2) - S * (1.0f - CNDD1));\n  " +
                      "return p; }", TupleType(Float, Float, Float, Float, Float), pricesType)

              val firstKernel = Join() o Join() o MapWrg(
                MapLcl(MapSeq(blackScholesComp))
              ) o Split(8192) o Split(1) o Zip(Svec, Xvec, Tvec, Rvec, Vvec)

            }

            @Test def BLACK_SCHOLES_AMD_VERSION() {

              val pricesType = UserType("typedef struct { float call; float put; } prices;")

              val blackScholesComp =
                UserFun("blackScholesComp", Array("inRand"),
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
                          "  \n" +
                          "  float d1, d2;\n" +
                          "  float phiD1, phiD2;\n" +
                          "  float sigmaSqrtT;\n" +
                          "  float KexpMinusRT;\n" +
                          "  prices p;\n" +
                          "  \n" +
                          "  float two = (float)2.0f;\n" +
                          "  float S = S_LOWER_LIMIT * inRand + S_UPPER_LIMIT * (1.0f - inRand);\n" +
                          "  float K = K_LOWER_LIMIT * inRand + K_UPPER_LIMIT * (1.0f - inRand);\n" +
                          "  float T = T_LOWER_LIMIT * inRand + T_UPPER_LIMIT * (1.0f - inRand);\n" +
                          "  float R = R_LOWER_LIMIT * inRand + R_UPPER_LIMIT * (1.0f - inRand);\n" +
                          "  float sigmaVal = SIGMA_LOWER_LIMIT * inRand + SIGMA_UPPER_LIMIT * (1.0f - inRand);\n" +
                          "  \n" +
                          "  sigmaSqrtT = sigmaVal * sqrt(T);\n" +
                          "  \n" +
                          "  d1 = (log(S/K) + (R + sigmaVal * sigmaVal / two)* T)/ sigmaSqrtT;\n" +
                          "  d2 = d1 - sigmaSqrtT;\n" +
                          "  \n" +
                          "  KexpMinusRT = K * exp(-R * T);\n" +
                          "  phi(d1, &phiD1);\n" +
                          "  phi(d2, &phiD2);\n" +
                          "  p.call = S * phiD1 - KexpMinusRT * phiD2;\n" +
                          "  \n" +
                          "  phi(-d1, &phiD1);\n" +
                          "  phi(-d2, &phiD2);\n" +
                          "  p.put  = KexpMinusRT * phiD2 - S * phiD1;\n" +
                          "  return p;\n" +
                          "}", Float, pricesType)

              val firstKernel = Join() o Join() o MapWrg(
                MapLcl(MapSeq(blackScholesComp))
              ) o Split(256) o Split(1) o input

            }

            @Test def SCAL_AMD() {

              /*
              val firstKernel = Join() o Join() o MapWrg(
                MapLcl(MapSeq(Bind(mult, alpha)))
              ) o Split(128) o Split(1) o input
              */

            }

            @Test def MD() {

              //val firstKernel = ...???

            }

            */

  @Test def stuff() {
    val scal = fun(Float, ArrayType(Float, Var("N")),
      (alpha, input) => {
        Map(fun((x) => mult(x, alpha))) $ input })

    val asum = fun(ArrayType(Float, Var("N")),
      (input) => { Reduce(add, 0.0f) o Map(abs) $ input })

    val dot = fun(ArrayType(Float, Var("N")), ArrayType(Float, Var("N")),
      (x,y) => { Reduce(add, 0.0f) o Map(mult) $ Zip(x,y) })

    val vecAdd = fun(ArrayType(Float, Var("N")), ArrayType(Float, Var("N")), (x,y) => { Map(add) $ Zip(x,y) })

    val gemv = fun(ArrayType(ArrayType(Float, Var("M")), Var("N")),
      ArrayType(Float, Var("N")),
      ArrayType(Float, Var("M")),
      Float, Float,
      (A, x, y, alpha, beta) => {
        val scalledY = scal(beta, y)
        val AtimesX = Map(fun( row => scal(alpha) $ dot(x, row) ), A)
        vecAdd(AtimesX, scalledY)
      })

    /*
    private def BlackScholes(s: Input): Fun =
    {
      val BSModel = UserFun("BSmodel", Array("s"), "{ return s; }", Float, Float)

      Map(BSModel, s)
    }

    private def MD(particles: Input, neighArray: Input): Fun =
    {
      val calculateForce = UserFun("calculateForce", Array("s"), "{ return s; }", Float, Float)

      Map(Reduce(calculateForce, 0)) o Zip(particles, neighArray)
    }
    */
  }

}
