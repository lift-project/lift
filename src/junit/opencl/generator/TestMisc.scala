package junit.opencl.generator

import java.io._
import java.awt.image._

import javax.imageio.ImageIO

import opencl.executor._
import org.junit.Assert._
import org.junit.{Ignore, AfterClass, BeforeClass, Test}
import opencl.ir._
import ir._

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

  val id = UserFunDef("id", "x", "{ return x; }", Float, Float)

  val add = UserFunDef("add", Array("x", "y"), "{ return x+y; }", Seq(Float, Float), Float)

  val plusOne = UserFunDef("plusOne", "x", "{ return x+1; }", Float, Float)

  val sumUp = UserFunDef("sumUp", Array("x", "y"), "{ return x+y; }", Seq(Float, Float), Float)

  val doubleItAndSumUp = UserFunDef("doubleItAndSumUp", Array("x", "y"), "{ return x + (y * y); }", Seq(Float, Float), Float)

  val sqrtIt = UserFunDef("sqrtIt", "x", "{ return sqrt(x); }", Float, Float)

  val abs = UserFunDef("abs", "x", "{ return x >= 0 ? x : -x; }", Float, Float)

  val neg = UserFunDef("neg", "x", "{ return -x; }", Float, Float)

  val mult = UserFunDef("mult", Array("l", "r"), "{ return l * r; }", Seq(Float, Float), Float)

  val addPair = UserFunDef(
    "pair",
    Array("x", "y"),
    "{ x._0 = x._0 + y._0;" +
      "x._1 = x._1 + y._1;" +
      "return x; }",
    Seq(TupleType(Float, Float), TupleType(Float, Float)),
    TupleType(Float, Float))

  val transpose = AccessFunction.transpose

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

  @Test def testScatterGlb1D(): Unit = {
    val Nsize = 128
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Scatter(AccessFunction.reverse)(MapGlb(id)) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.reverse, output, 0.0f)
  }

  @Test def testScatterGlbSeqInnerDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => MapGlb(Scatter(AccessFunction.reverse)(MapSeq(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testScatterGlbSeqOuterDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Scatter(AccessFunction.reverse)(MapGlb(MapSeq(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.flatten, output, 0.0f)
  }

  @Test def testScatterGlbSeqBothDims(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Scatter(AccessFunction.reverse)(MapGlb(Scatter(AccessFunction.reverse)(MapSeq(id)))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherGlb1D(): Unit = {
    val Nsize = 128
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Gather(AccessFunction.reverse)(MapGlb(id)) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.reverse, output, 0.0f)
  }

  @Test def testGatherGlbSeqInnerDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => MapGlb(Gather(AccessFunction.reverse)(MapSeq(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherGlbSeqOuterDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Gather(AccessFunction.reverse)(MapGlb(MapSeq(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.flatten, output, 0.0f)
  }

  @Test def testGatherGlbSeqBothDims(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Gather(AccessFunction.reverse)(MapGlb(Gather(AccessFunction.reverse)(MapSeq(id)))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherWrg1D(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Gather(AccessFunction.reverse)(MapWrg(id)) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.reverse, output, 0.0f)
  }

  @Test def testGatherWrgLclInnerDim(): Unit = {
    val Nsize = 256
    val Msize = 128
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => MapWrg(Gather(AccessFunction.reverse)(MapLcl(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherWrgLclOuterDim(): Unit = {
    val Nsize = 256
    val Msize = 128
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Gather(AccessFunction.reverse)(MapWrg(MapLcl(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.flatten, output, 0.0f)
  }

  @Test def testGatherWrgLclBothDims(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Gather(AccessFunction.reverse)(MapWrg(Gather(AccessFunction.reverse)(MapLcl(id)))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherSplit(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Gather(AccessFunction.reverse)(MapGlb(MapSeq(id))) o Split(splitSize) $ in
    )

    val (output, runtime) = Execute(1,Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.grouped(splitSize).toArray.reverse.flatten, output, 0.0f)
  }

  @Test def testSplitGather(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapGlb(Gather(AccessFunction.reverse)(MapSeq(id))) o Split(splitSize) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.grouped(splitSize).toArray.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testScatterSplit(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Scatter(AccessFunction.reverse)(MapGlb(MapSeq(id))) o Split(splitSize) $ in
    )

    val (output, runtime) = Execute(1,Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.grouped(splitSize).toArray.reverse.flatten, output, 0.0f)
  }

  @Test def testSplitScatter(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapGlb(Scatter(AccessFunction.reverse)(MapSeq(id))) o Split(splitSize) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.grouped(splitSize).toArray.map(_.reverse).flatten, output, 0.0f)
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
      (X, Y) => MapGlb(MapSeq(MapSeq(fun(z => add.apply(Get(z, 0), Get(z, 1)))))) o MapGlb(fun(x => MapSeq(fun(y => Zip(x, y))) $ Y )) $ X
    )

    val (output, runtime) = Execute(nSize)(f, A, B, nSize, mSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def reduceOverTuples(): Unit = {

    val maxFirstArg = UserFunDef("maxFirstArg", Array("x", "y"), "{ return x._0 > y._0 ? x : y; }", Array(TupleType(Float, Float), TupleType(Float, Float)), TupleType(Float, Float))

    val inputSize = 512

    val N = Var("N")

    val input2 = Array.fill(inputSize)(util.Random.nextInt(5).toFloat,util.Random.nextInt(5).toFloat)

    val gold = input2.reduce((x, y) => if (x._1 > y._1) x else y).productIterator.asInstanceOf[Iterator[Float]].toArray

    val input = input2.map(_.productIterator).reduce(_++_).asInstanceOf[Iterator[Float]].toArray

    val function = fun(
        ArrayType(TupleType(Float, Float), N),
        input => ReduceSeq(maxFirstArg, (0.0f, 0.0f)) $ input
    )

    val (output, runtime) = Execute(inputSize)(function, input, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def sumOverTuples(): Unit = {

    val inputSize = 512

    val N = Var("N")

    val input2 = Array.fill(inputSize)(util.Random.nextInt(5).toFloat,util.Random.nextInt(5).toFloat)

    val gold = input2.reduce((x, y) => (x._1 + y._1, x._2 + y._2)).productIterator.asInstanceOf[Iterator[Float]].toArray

    val input = input2.map(_.productIterator).reduce(_++_).asInstanceOf[Iterator[Float]].toArray

    val function = fun(
      ArrayType(TupleType(Float, Float), N),
      input => ReduceSeq(addPair, (0.0f, 0.0f)) $ input
    )

    val (output, runtime) = Execute(inputSize)(function, input, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

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

    assertArrayEquals(gold, output, 0.001f)

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

  @Test def VECTOR_ADD_SIMPLE() {

    val inputSize = 1024
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val gold = (leftInputData, rightInputData).zipped.map(_+_)

    val N = Var("N")

    val addFun = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) =>

        Join() o MapWrg(
          Join() o MapLcl(MapSeq(add)) o Split(4)
        ) o Split(1024) $ Zip(left, right)

    )

    val code = Compile(addFun)
    val (output, runtime) = Execute(inputSize)(code, addFun, leftInputData, rightInputData, leftInputData.size)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

  }

  @Test def VECTOR_NEG_SIMPLE() {

    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inputArray.map(-_)

    val neg = UserFunDef("neg", "x", "{ return -x; }", Float, Float)

    val negFun = fun(ArrayType(Float, Var("N")), (input) =>

      Join() o MapWrg(
        Join() o MapLcl(MapSeq(neg)) o Split(4)
      ) o Split(1024) $ input

    )

    val (output, runtime) = Execute(inputArray.length)(negFun, inputArray, inputArray.size)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

  }

  @Test def VECTOR_PAIR() {
    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val gold = inputArray.map((f) => Array(f, f)).flatten

    val pair = UserFunDef("pair", "x", "{ Tuple t = {x, x}; return t; }", Float, TupleType(Float, Float))

    val pairFun = fun(ArrayType(Float, Var("N")), (input) =>
      Join() o MapWrg(
        Join() o MapLcl(MapSeq(pair)) o Split(4)
      ) o Split(1024) $ input
    )

    val (output, runtime) = Execute(inputArray.length)(pairFun, inputArray, inputArray.length)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def VECTOR_NEG_PAIR() {
    val inputSize = 1024
    val inputArray = Array.fill(inputSize * 2)(util.Random.nextInt(5).toFloat)

    val gold = inputArray.map(-_)

    val negPair = UserFunDef("pair", "x", "{ x._0 = -x._0; x._1 = -x._1; return x; }",
                             TupleType(Float, Float), TupleType(Float, Float))

    val f = fun(ArrayType(TupleType(Float, Float), Var("N")), (input) =>
      Join() o MapWrg(
        Join() o MapLcl(MapSeq(fun(x => negPair(x)))) o Split(4)
      ) o Split(1024) $ input
    )

    val (output, runtime) = Execute(inputSize)(f, inputArray, inputSize)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def VECTOR_ADD_PAIRS() {
    val inputSize = 1024
    val leftArray = Array.fill(inputSize * 2)(util.Random.nextInt(5).toFloat)
    val rightArray = Array.fill(inputSize * 2)(util.Random.nextInt(5).toFloat)

    val gold = (leftArray zip rightArray).map({case (l, r) => l + r})

    val N = Var("N")

    val f = fun(
      ArrayType(TupleType(Float, Float), N),
      ArrayType(TupleType(Float, Float), N),
      (left, right) =>
        Join() o MapWrg(
          Join() o MapLcl(MapSeq(addPair)) o Split(4)
        ) o Split(1024) $ Zip(left, right)
    )

    val (output, runtime) = Execute(inputSize)(f, leftArray, rightArray, inputSize)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def VECTOR_NEG_SIMPLE_GLOBAL_ID() {

    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inputArray.map(-_)

    val neg = UserFunDef("neg", "x", "{ return -x; }", Float, Float)

    val negFun = fun(
      ArrayType(Float, Var("N")),
      (input) => Join() o MapGlb(
        MapSeq(neg)
      ) o Split(4) $ input
    )

    val (output, runtime) = Execute(inputArray.length)(negFun, inputArray, inputArray.size)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

  }

  @Test def VECTOR_NEG_SIMPLE_GLOBAL_ID_REORDER_REVERSE() {

    val reverse = AccessFunction.reverse

    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inputArray.map(-_).reverse

    val neg = UserFunDef("neg", "x", "{ return -x; }", Float, Float)

    val negFun = fun(ArrayType(Float, Var("N")), (input) =>

      Gather(reverse)(Join() o MapGlb(
        MapSeq(neg)
      ) o Split(4)) $ input
    )

    val (output, runtime) = Execute(16, inputArray.length)(negFun, inputArray, inputArray.size)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def VECTOR_SCAL() {

    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f
    val gold = inputArray.map(_ * alpha)

    val scalFun = fun( ArrayType(Float, Var("N")), Float, (input, alpha) =>
      Join() o MapWrg(
        Join() o MapLcl(MapSeq(
          fun( x => mult(alpha, x) )
        )) o Split(4)
      ) o Split(1024) $ input
    )

    val (output, runtime) = Execute(inputArray.length)(scalFun, inputArray, alpha, inputArray.size)

    (gold, output).zipped.map(assertEquals(_,_,0.0))

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def VECTOR_SCAL_REDUCE() {

    val inputSize = 2048
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f
    val gold = inputArray.map(_ * alpha).sum

    val scalFun = fun( ArrayType(Float, Var("N")), Float, (input, alpha) =>
      Join() o MapWrg(
        Join() o MapLcl(ReduceSeq(sumUp, 0.0f) o MapSeq(
          fun( x => mult(alpha, x) )
        )) o Split(4)
      ) o Split(1024) $ input
    )

    val (output, runtime) = Execute(inputArray.length)(scalFun, inputArray, alpha, inputArray.size)

    assertEquals(gold,output.sum,0.0)
    //(gold, output).zipped.map(assertEquals(_,_,0.0))

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def VECTOR_NORM() {

    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = scala.math.sqrt(inputArray.map(x => x*x).sum).toFloat

    val f = fun(
      ArrayType(Float, Var("N")),
      (input) =>

        Join() o MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(sqrtIt))) o Split(1) o
            Iterate(5)( Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2) ) o
            Join() o toLocal(MapLcl(ReduceSeq(doubleItAndSumUp, 0.0f))) o Split(32) o ReorderStride(1024/32)
        ) o Split(1024) $ input

    )

    val (output, runtime) = Execute(inputSize)(f, inputArray, inputSize)

    assertEquals(gold, output(0), 0.1)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def MATRIX_PLUS_ONE(): Unit = {

    val Msize = 512
    val Ksize = 512
    val matrix = Array.tabulate(Msize, Ksize)((r, c) => 1.0f * c * r)
    val gold   = matrix.map(_.map(_+1.0f))

    val M = Var("M")
    val K = Var("K")

    val r = 2
    val c = 4

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      (matrix) => {
        Join() o MapWrg(0)(fun( rows =>
          MapLcl(0)(fun( row =>
            Join() o MapWrg(1)(fun( cols =>
              MapLcl(1)(fun( col =>
                plusOne(col)
              )) $ cols
            )) o Split(c) $ row
          )) $ rows
        )) o Split(r) $ matrix
      })

    val (output, runtime) = Execute(Ksize * Msize)(f, matrix, Ksize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def MATRIX_PLUS_ONE_TILED(): Unit = {

    val Msize = 8
    val Ksize = 16
    val matrix = Array.tabulate(Msize, Ksize)((r, c) => 1.0f * (c + r))
    val gold   = matrix.map(_.map(_+1.0f))

    val M = Var("M")
    val K = Var("K")

    val r = 4
    val c = 8

    val plusOne = UserFunDef("plusOne", "x", "{ return x+1; }", Float, Float)
    val id = UserFunDef("id", "x", "{ return x; }", Float, Float)

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      (matrix) => {
        Join() o MapWrg(0)(fun( cols =>
          MapSeq(Join()) o Swap() o Scatter(transpose)(Gather(transpose)(MapWrg(1)(fun( tile =>

            // step 2: compute plus one
            toGlobal(
              MapLcl(0)(fun( row =>
                MapLcl(1)(fun( elem =>
                  id(elem)
                )) $ row
              ))
            ) o
            // step 1: load tile to local memory
            toLocal(
              MapLcl(0)(fun( row =>
                MapLcl(1)(fun( elem =>
                  plusOne(elem)
                )) $ row
              ))
            ) $ tile

          )))) o Swap() o MapSeq(Split(c)) $ cols
        )) o Split(r) $  matrix
      })

    val (output, runtime) = Execute(32, Ksize * Msize)(f, matrix, Ksize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    myPrint(gold.flatten, Ksize)

    println("output: ")
    myPrint(output, Ksize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def MATRIX_PLUS_ONE_TILED_TRANSPOSE_WITH_JOIN_REORDER_SPLIT(): Unit = {

    val Msize = 8
    val Ksize = 16
    val matrix = Array.tabulate(Msize, Ksize)((r, c) => 1.0f * (c + r))
    val gold   = matrix.map(_.map(_+1.0f))

    val M = Var("M")
    val K = Var("K")

    val r = 4
    val c = 8

    val plusOne = UserFunDef("plusOne", "x", "{ return x+1; }", Float, Float)

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      (matrix) => {
        Join() o MapWrg(0)(fun( cols =>
          MapSeq(Join()) o Swap() o Scatter(transpose)(Gather(transpose)(MapWrg(1)(fun( tile =>

            MapLcl(0)(fun( row =>
              MapLcl(1)(fun( elem =>
                plusOne(elem)
              )) $ row
            )) $ tile

          )))) o Swap() o MapSeq(Split(c)) $ cols
        )) o Split(r) $  matrix
      })

    val (output, runtime) = Execute(32, Ksize * Msize)(f, matrix, Ksize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    myPrint(gold.flatten, Ksize)

    println("output: ")
    myPrint(output, Ksize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  private def myPrint(m: Array[Array[Array[Float]]]): Unit = {
    m.map( r => {
      println(r.map( e => {
        "(" + e.map("%2.0f".format(_)).reduce(_ + ", " + _) + ")"
      }).reduce(_ + " " + _))
    } )
  }

  private def myPrint(m: Array[Array[Float]]): Unit = {
    m.map( r => {
      println(r.map("%2.0f".format(_)).reduce(_ + " " + _))
    } )
  }

  private def myPrint(m: Array[Float], cols: Int): Unit = {
    val (row, rest) = m.splitAt(cols)
    if (row.nonEmpty) println(row.map("%2.0f".format(_)).reduce(_ + " " + _))
    if (rest.nonEmpty) myPrint(rest, cols)
  }

  private def printRow(r: Array[Float], elems: Int): Unit = {
    val (elem, rest) = r.splitAt(elems)
    if (elem.nonEmpty) print("(" + elem.map("%2.0f".format(_)).reduce(_ + ", " + _) + ") ")
    if (rest.nonEmpty) printRow(rest, elems)
  }

  private def myPrint(m: Array[Float], cols: Int, elems: Int): Unit = {
    val (row, rest) = m.splitAt(cols*elems)
    if (row.nonEmpty) printRow(row, elems); println("")
    if (rest.nonEmpty) myPrint(rest, cols, elems)
  }

  @Test def MATRIX_TRANSPOSE(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix: ")
    myPrint(matrix)

    val N = Var("N")
    val M = Var("M")

    val f1 = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapGlb(0)(MapGlb(1)(id)) o Transpose() $ matrix
      })

    val f2 = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        Gather(transpose)(MapGlb(0)(MapGlb(1)(id))) o Swap() $ matrix
      })

    val f3 = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        Swap() o Scatter(transpose)(MapGlb(0)(MapGlb(1)(id))) $ matrix
      })

    // transpose twice == id
    val f4 = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        Swap() o Scatter(transpose)(Gather(transpose)(MapGlb(0)(MapGlb(1)(id)))) o Swap() $ matrix
      })

    val f5 = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        Gather(transpose)(MapGlb(0)(MapGlb(1)(id)) o Split(Nsize)) o Join() $ matrix
      })

    val f = f5

    val (output, runtime) = Execute(32, Nsize * Msize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("output: ")
    myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def MATRIX_TRANSPOSE_3D(): Unit = {

    val Nsize = 8
    val Msize = 4
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    println("matrix: ")
    myPrint(matrix)

    val gold   = matrix.transpose

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, K), M), N),
      (matrix) => {
        Gather(transpose)(
          MapGlb(0)(
            MapGlb(1)(
              MapSeq(id)
            )
          ) o Split(Nsize)
        ) o Join() $ matrix
      })

    val (output, runtime) = Execute(4, Nsize * Msize)(f, matrix, Nsize, Msize, Ksize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    myPrint(gold.flatten.flatten, Nsize, Ksize)

    println("output: ")
    myPrint(output, Nsize, Ksize)

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

    val id = UserFunDef("id", "x", "{ return x; }", Float, Float)

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
      (input) => { Reduce(sumUp, 0.0f) o Map(abs) $ input })

    val dot = fun(ArrayType(Float, Var("N")), ArrayType(Float, Var("N")),
      (x,y) => { Reduce(sumUp, 0.0f) o Map(mult) $ Zip(x,y) })

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

    (asum, gemv)

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
