package opencl.generator

import arithmetic.Var
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

  // Simple 1D increment, used to check the syntax of unary UserFunDef
  @Test def increment(): Unit = {
    // domain size
    val inputSize = 128
    val N = Var("N")

    // Input variables
    val xs = Array.fill(inputSize)(util.Random.nextFloat())

    // Cross validation
    val gold = xs.map(_ + 1)

    // user function
    val fct = UserFunDef("inc", Array("x"),
      " return x+1.0; ", Seq(Float), Float)

    // Expression
    val f = fun(
      ArrayType(Float, N),
      (xs) => MapGlb(
        fun(x => fct(x))
      ) $ xs
    )

    // execute
    val (output: Array[Float], runtime) = Execute(inputSize)(f, xs)

    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.001f)
  }

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

    val (output: Array[Float], runtime) = Execute(inputSize)(compFun, inputData)
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

    val (output: Array[Float], runtime) = Execute(4, Nsize)(f, matrix, vector)
    assertArrayEquals(matrix.flatten.flatten, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test
  def zipUnzip(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)

    val N = Var("N")

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), N),
      (A, B) =>
        MapGlb(fun(tuple => MapSeq(plusOne) $ Get(tuple, 0))) o
        MapGlb(fun( tuple =>
          fun(tuple => Tuple(Join() $ Get(tuple, 0), Join() $ Get(tuple, 1))) o Unzip() o MapSeq(fun( tuple =>
            Tuple(MapSeq(id) $ Get(tuple, 0), MapSeq(id) $ Get(tuple, 1))
          )) $ Zip(Split(1) $ Get(tuple, 0), Split(1) $ Get(tuple, 1))
        )) $ Zip(A, B)
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, inputData, inputData)

    assertArrayEquals(inputData.flatten.map(_ + 1), output, 0.0f)
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

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputData)
    assertArrayEquals(inputData, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def mapValueArg(): Unit = {
    val inputSize = 1024
    val inputData = Array.tabulate(inputSize)(_.toFloat)

    val gold = inputData.map(_ + 3.0f)

    val N = Var("N")

    val f = fun(
      ArrayType(Float, N),
      (input) =>
        MapGlb(fun(x => add(x, Value.FloatToValue(3.0f)))) $ input
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputData)
    assertArrayEquals(gold, output, 0.0f)

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
      (input) => MapGlb(toGlobal(MapSeq(neg)) o ReduceSeq(add, 0.0f)) $ input
    )

    val (output: Array[Float], runtime) = Execute(Nsize * Msize)(function, matrix)

    println("output.size = " + output.length)
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

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputData)

    println("output.size = " + output.length)
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

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputData)

    println("output.size = " + output.length)
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

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
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

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
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

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
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

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.flatten.flatten, output, 0.0f)
  }

  @Test def iterate(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.map(_+(1*7))

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Iterate(7)(MapGlb(plusOne)) $ in
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(f, input)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)


    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def iterateFixedSecondArg() : Unit = {
    val inputSize = 512
    val inputA = Array.tabulate(inputSize)(_.toFloat)
    val inputB = Array.tabulate(inputSize)(_.toFloat).reverse
    val gold = inputA.zip(inputB.map(_*5.0f)).map((t:(Float, Float)) => t match{ case (x:Float,y:Float) => x+y})

    val N = Var("N")

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (inA,inB) => Iterate(5)(fun( (va) =>
        fun( (vb) =>
          MapWrg(add) $ Zip(va,vb)
        ) $ inB
      )) $ inA
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputA, inputB)

    println("output.size = " + output.length)
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
      in => Join() o MapWrg(Barrier() o toGlobal(MapLcl(id)) o
        Iterate(5)(Barrier() o MapLcl(plusOne)) o
        toLocal(MapLcl(id))) o Split(16) $ in
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(f, input)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def decompose(): Unit = {

    val nSize = 128
    val mSize = 128
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

    val (output: Array[Float], runtime) = Execute(nSize)(f, A, B)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }
}
