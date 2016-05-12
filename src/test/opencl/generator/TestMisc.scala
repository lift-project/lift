package opencl.generator

import apart.arithmetic.{?, ArithExpr, Cst, Var}
import apart.arithmetic.SizeVar
import exploration.ParameterRewrite
import rewriting.InferNDRange
import ir._
import ir.ast._
import opencl.executor._
import opencl.generator.OpenCLGenerator._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._

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

  @Test
  def testDouble(): Unit = {
    Assume.assumeTrue("Needs double support", Executor.supportsDouble())

    val inputSize = 256
    val input = Array.fill(inputSize)(util.Random.nextInt(5).toDouble)

    val incr = UserFun("incr", "x", "{ return x+1; }", Double, Double)

    val f = fun(
      ArrayType(Double, SizeVar("N")),
      in => MapGlb(incr) $ in
    )

    val (output: Array[Double], _) = Execute(inputSize)(f, input)

    assertArrayEquals(input.map(_ + 1), output, 0.0)
  }

  @Test def issue20(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inputData.map(_+5)

    val incr = UserFun("incr", "x", "{ return x+1; }", Float, Float)

    val f = fun(
      ArrayType(Float, SizeVar("N")),
      (inArr) => {
        Join() o MapGlb(
          Iterate(5)(fun((e) => MapSeq(incr) $ e))
        ) o Split(1) $ inArr
      }
    )

    val f2 = fun(
      ArrayType(Float, SizeVar("N")),
      (inArr) => {
        Iterate(5)(fun((arr) =>
          MapGlb(incr) $ arr
        )) $ inArr
      }
    )


    val (output1: Array[Float], _) = Execute(inputData.length)(f, inputData)
    assertArrayEquals(gold, output1, 0.0f)

    val (output2: Array[Float], _) = Execute(inputData.length)(f2, inputData)
    assertArrayEquals(gold, output2, 0.0f)
  }

  @Test def issue22(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (
      ArrayType(Float, SizeVar("N")),
      Float,
      (in, init) => {
        fun( x0 => Join()(MapWrg(
          fun(x1 =>
            fun(x2 =>
              fun( x3 => Join()(x3)
              )(MapLcl(
                fun( x4 => toGlobal(MapSeq(id))(ReduceSeq(add, init)(x4)))
              )(x2)
                )
            )(Split(4)(x1))
          ))(x0))
        )(Split(128)(in))
      })

    val (output: Array[Float], _) = Execute(inputData.length)( l, inputData, 0.0f)

    assertEquals(inputData.sum, output.sum, 0.0)
  }

  @Test def issue23(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayType(Float, SizeVar("N")),
      in => {
        Join() o MapWrg(
          Join() o  MapLcl(
            fun( x4 => toGlobal(MapSeq(id))(ReduceSeq(add, id(0.0f))(x4)))
          ) o Split(4)
        ) o Split(128) $ in
      })

    val (output: Array[Float], _) = Execute(inputData.length)(l, inputData)

    assertEquals(inputData.sum, output.sum, 0.0)
  }

  @Test def issue23ForwardStyle(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l =
      fun (ArrayType(Float, SizeVar("N")),
         in => {
           in :>>
           Split(128) :>>
           MapWrg(
             Split(4) >>>
             MapLcl(
               ReduceSeq(add, id(0.0f)) >>>
               toGlobal(MapSeq(id)) ) >>>
             Join() ) :>>
           Join()
         })

    val (output: Array[Float], _) = Execute(inputData.length)(l, inputData)

    assertEquals(inputData.sum, output.sum, 0.0)
  }

  @Test def issue23BackwardStyle(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l =
      fun (ArrayType(Float, SizeVar("N")),
          in => {
            Join() <<:
            MapWrg(
              Join() o
              MapLcl(
                toGlobal(MapSeq(id)) o
                ReduceSeq(add, id(0.0f)) ) o
              Split(4) ) <<:
            Split(128) <<:
            in
          })

    val (output: Array[Float], _) = Execute(inputData.length)(l, inputData)

    assertEquals(inputData.sum, output.sum, 0.0)
  }

  @Test def issue24(): Unit = {
    val input = Array.tabulate(2, 4, 8)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, SizeVar("N")), SizeVar("M")), SizeVar("L")),
      input => MapWrg(
        fun( x0 => toGlobal(MapLcl(MapSeq(id)))(x0) ) o
          Transpose() o TransposeW() o
           toLocal(MapLcl(MapSeq(id)))
      ) $ input
    )

    val (output: Array[Float], _) = Execute(4, 4)(f, input)

    assertArrayEquals(input.flatten.flatten, output, 0.0f)
  }

  @Test
  def issue25(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun(ArrayType(Float, SizeVar("N")),
      in => {
        MapSeq(id o id) $ in
      })

    val (output: Array[Float], _) = Execute(inputData.length)(l, inputData)

    assertArrayEquals(inputData, output, 0.0f)
  }

  @Test
  def issue28(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun(ArrayType(Float, SizeVar("N")),
      in => {
        in :>> ReduceSeq(add, toPrivate(add)(0.0f, 1.0f)) :>> toGlobal(MapSeq(id))
      })

    val (output: Array[Float], _) = Execute(inputSize)(l, inputData)

    assertEquals(inputData.sum + 1, output.head, 0.0f)
  }

  @Test
  def issue37(): Unit = {
    val inputSize = 8
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun(ArrayType(Float, Cst(inputSize)),
                in => {
                  in :>>
                  asVector(4) :>>
                  toPrivate(MapSeq(VectorizeUserFun(4, id))) :>>
                  asScalar() :>>
                  toGlobal(MapSeq(id))
                })

    val (output: Array[Float], _) = Execute(1, 1)(l, inputData)

    assertArrayEquals(inputData, output, 0.0f)
  }

  @Test
  def asScalarInPrivateMemoryAfterReduce(): Unit = {
    val inputSize = 8
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun(ArrayType(Float, Cst(inputSize)),
      in => {
        in :>>
          asVector(4) :>>
          ReduceSeq(VectorizeUserFun(4, add), Value(0.0f).vectorize(4)) :>>
          asScalar() :>>
          toGlobal(MapSeq(id))
      })
    val (output: Array[Float], _) = Execute(1, 1)(l, inputData)

    val gold = inputData.splitAt(inputSize/2).zipped.map(_+_)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def reduceVector(): Unit = {
    val inputSize = 8
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun(ArrayType(Float, Cst(inputSize)),
      in => {
        in :>>
          asVector(4) :>>
          ReduceSeq(VectorizeUserFun(4, add), Value(0.0f).vectorize(4)) :>>
          asScalar() :>>
          ReduceSeq(add, 0.0f) :>>
          toGlobal(MapSeq(id))
      })
    val (output: Array[Float], _) = Execute(1, 1)(l, inputData)

    val gold = Array(inputData.sum)

    assertArrayEquals(gold, output, 0.0f)
  }

  // Simple 1D increment, used to check the syntax of unary UserFunDef
  @Test def increment(): Unit = {
    // domain size
    val inputSize = 128
    val N = SizeVar("N")

    // Input variables
    val xs = Array.fill(inputSize)(util.Random.nextFloat())

    // Cross validation
    val gold = xs.map(_ + 1)

    // user function
    val fct = UserFun("inc", Array("x"),
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

  @Test def compositionTest(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val gold = inputData.map(x => - x)

    val composition = id o neg
    val N = SizeVar("N")

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

  @Test def accessingMultiDimArrayAfterZip(): Unit = {
    val Nsize = 8
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Nsize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)
    val vector = Array.fill(Nsize)(1.0f)

    val N = SizeVar("N")
    val K = SizeVar("K")

    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, K), N), N),
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

    val N = SizeVar("N")

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

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float4, N),
      (input) =>
        MapGlb(id.vectorize(4)) $ input
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputData)
    assertArrayEquals(inputData, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def vectorizePattern(): Unit = {
    val inputSize = 1024
    val inputData = Array.tabulate(inputSize*4)(_.toFloat)

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float4, N),
      (input) =>
        MapGlb(VectorizeUserFun(4, id)) $ input
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

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Float, N),
      (input) =>
        MapGlb(fun(x => add(x, 3.0f))) $ input
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
      ArrayType(ArrayType(Float, SizeVar("N")), SizeVar("M")),
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
      ArrayType(Float, SizeVar("N")),
      in => asScalar() o MapGlb(plusOne.vectorize(4)) o asVector(4) $ in
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
      ArrayType(Float, SizeVar("N")),
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

    val N = SizeVar("N")
    val M = SizeVar("M")


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

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")


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

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")


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

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")


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
      ArrayType(Float, SizeVar("N")),
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

    val N = SizeVar("N")

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
      ArrayType(Float, SizeVar("N")),
      in => Join() o MapWrg( toGlobal(MapLcl(id)) o
        Iterate(5)( MapLcl(plusOne)) o
        toLocal(MapLcl(id))) o Split(16) $ in
    )

    val f_nested = fun(
      ArrayType(Float, SizeVar("N")),
      in => fun(x1 => Join()(MapWrg(
        fun(x2 =>
          fun(x3 =>
            fun(x4 => toGlobal(MapLcl(id))(x4))(
              Iterate(5)(fun(x5 => MapLcl(plusOne)(x5)))(x3)))(
                toLocal(MapLcl(id))(x2)))
          )(x1)))(Split(16)(in))
    )

    val f_nested2 = fun(
      ArrayType(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x2 =>
        fun(x3 =>
          fun(x4 => toGlobal(MapLcl(id))(x4))(
            Iterate(5)(fun(x5 => MapLcl(plusOne)(x5)))(x3)))(
              toLocal(MapLcl(id))(x2)))
              )(Split(16)(in)))
    )

    val f_nested3 = fun(
      ArrayType(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x2 =>
        fun(x4 => toGlobal(MapLcl(id))(x4))(
          Iterate(5)(fun(x5 => MapLcl(plusOne)(x5)))(
            toLocal(MapLcl(id))(x2)))
              ))(Split(16)(in)))
      )

    val f_nested4 = fun(
      ArrayType(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x2 =>
        toGlobal(MapLcl(id))(
          Iterate(5)(fun(x5 => MapLcl(plusOne)(x5)))(
            toLocal(MapLcl(id))(x2)))
              ))(Split(16)(in)))
    )

    val f_full = fun(
      ArrayType(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x0 =>
        toGlobal(MapLcl(id))(
          Iterate(5)(fun(x1 => MapLcl(plusOne)(x1)))(
            toLocal(MapLcl(id))(x0)))
              ))(Split(16)(in)))
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


    val N = SizeVar("N")
    val M = SizeVar("M")

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

  @Test def localMemoryRegression(): Unit = {
    val f =
      fun(
        ArrayType(Float, SizeVar("N")),
        x => Join() o MapWrg(
          Join() o MapLcl(
            Join() o MapSeq(
              toGlobal(MapSeq(id)) o Gather(reverse) o toLocal(MapSeq(id))
            ) o Split(2)
          ) o Split(2)
        ) o Split(512) $ x

      )

    val input = Array.fill(2048)(util.Random.nextInt(5).toFloat)

    val kernel = Compile(f)

    val (output: Array[Float], _) = Execute(16, 2048)(kernel.code, kernel.f, input)

    assertEquals(input.sum, output.sum, 0.0f)
    assertEquals(9, "l_id".r.findAllMatchIn(kernel.code).length)
  }

  @Ignore
  @Test def issue47(): Unit = {
    val factory = (variables: Seq[ArithExpr]) => {
      val v_M0_0 = variables(0)
      val v_N1_1 = variables(1)
      val v_2_2 = variables(2)
      val v_3_3 = variables(3)
      val v_4_4 = variables(4)
      val v_5_5 = variables(5)

      val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
      val id = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
      val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
      fun(
        ArrayType(ArrayType(Float, v_M0_0), v_N1_1),
        ArrayType(Float, v_M0_0),
        ArrayType(Float, v_N1_1),
        Float,
        Float,
        (p_0, p_1, p_2, p_3, p_4) =>
        FunCall(MapWrg(0)(fun((p_5) =>
          FunCall(toGlobal(fun((p_6) =>
            FunCall(MapLcl(0)(fun((p_7) =>
              FunCall(add,
                FunCall(mult, p_7, p_3),
                FunCall(mult,
                  FunCall(Get(1), p_5), p_4)))), p_6))),
            FunCall(MapSeq(fun((p_8) =>
              FunCall(toLocal(fun((p_9) =>
                FunCall(id, p_9))), p_8))),
              FunCall(ReduceSeq(fun((p_10, p_11) =>
                FunCall(add, p_10, p_11))),
                FunCall(id, Value("0.0f", Float)),
                FunCall(Join(),
                  FunCall(MapLcl(0)(fun((p_12) =>
                    FunCall(MapSeq(fun((p_13) => p_13)),
                      FunCall(ReduceSeq(fun((p_14, p_15) =>
                        FunCall(add, p_14,
                          FunCall(toPrivate(fun((p_16) =>
                            FunCall(id, p_16))), p_15)))),
                        FunCall(id, Value("0.0f", Float)), p_12)))),
                    FunCall(Split(v_2_2 * 1 /^ v_3_3),
                      FunCall(Gather(ReorderWithStride(v_3_3)),
                        FunCall(Join(),
                          FunCall(MapLcl(0)(fun((p_17) =>
                            FunCall(MapSeq(fun((p_18) => p_18)),
                              FunCall(ReduceSeq(fun((p_19, p_20) =>
                                FunCall(add, p_19, p_20))),
                                FunCall(id, Value("0.0f", Float)), p_17)))),
                            FunCall(toLocal(fun((p_21) =>
                              FunCall(MapLcl(0)(fun((p_22) =>
                                FunCall(MapSeq(fun((p_23) =>
                                  FunCall(id, p_23))), p_22))), p_21))),
                              FunCall(Join(),
                                FunCall(MapLcl(0)(fun((p_24) =>
                                  FunCall(TransposeW(),
                                    FunCall(Join(),
                                      FunCall(MapSeq(fun((p_25) =>
                                        FunCall(TransposeW(),
                                          FunCall(MapSeq(fun((p_26) =>
                                            FunCall(MapSeq(fun((p_27) =>
                                              FunCall(mult,
                                                FunCall(Get(0), p_27),
                                                FunCall(Get(1), p_27)))), p_26))),
                                            FunCall(Transpose(), p_25))))),
                                        FunCall(Split(v_4_4),
                                          FunCall(Transpose(), p_24))))))),
                                  FunCall(Split(v_5_5),
                                    FunCall(Split(v_M0_0 * 1 /^ v_2_2),
                                      FunCall(Gather(ReorderWithStride(v_2_2)),
                                        FunCall(Zip(2), p_1,
                                          FunCall(Get(0), p_5))))))))))))))))))),
          FunCall(Zip(2), p_0, p_2)))
    }

    val inputSizeN = 1024
    val inputSizeM = 1024

    val matrix = Array.fill(inputSizeN, inputSizeM)(util.Random.nextInt(5).toFloat)
    val vectorX = Array.fill(inputSizeM)(util.Random.nextInt(5).toFloat)
    val vectorY = Array.fill(inputSizeN)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f
    val beta = 1.5f
    val values = Seq(matrix, vectorX, vectorY, alpha, beta)

    val expr = factory(Array[ArithExpr](1024,1024,128,128,8,2))
    /*val kernel = opencl.executor.Utils.compile(
      expr, Seq(matrix, vectorX),
      128, 1, 1,
      128*1024, 1, 1, (true, true))

    println(kernel)*/

    Execute(128, 1, 128*1024, 1, (true, true))(expr, values: _*)
  }

  /**
   * Expected behavior: generates code
   * Actual behavior: Throws scala.MatchError: (Arr(Arr(Arr(float,128),v__2),(v__1*1/^(128))),float4) (of class scala.Tuple2)
   */
  @Ignore
  @Test def issue50_MatchError(): Unit = {
    val v_K0_0 = SizeVar("")
    val v_M1_1 = SizeVar("")
    val v_N2_2 = SizeVar("")
    val v_3_3 = Cst(128)

    val id = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
    val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
    val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
    val expr = fun(
        ArrayType(ArrayType(Float, v_K0_0), v_M1_1),
        ArrayType(ArrayType(Float, v_N2_2), v_K0_0),
        (p_0, p_1) => FunCall(Join(), FunCall(MapWrg(0)(fun((p_2) => FunCall(TransposeW(), FunCall(MapWrg(1)(fun((p_3) => FunCall(TransposeW(), FunCall(toGlobal(fun((p_4) => FunCall(MapSeq(fun((p_5) => FunCall(MapLcl(0)(fun((p_6) => FunCall(id, p_6))), p_5))), p_4))), FunCall(MapSeq(fun((p_7) => p_7)), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(fun((p_10) => FunCall(MapLcl(0)(fun((p_11) => FunCall(add, FunCall(Get(0), p_11), FunCall(mult, FunCall(Get(1), p_11), FunCall(Get(1), p_10))))), FunCall(toLocal(fun((p_12) => FunCall(MapLcl(0)(fun((p_13) => FunCall(Tuple(2), FunCall(id, FunCall(Get(0), p_13)), FunCall(id, FunCall(Get(1), p_13))))), p_12))), FunCall(Zip(2), p_8, FunCall(Get(0), p_10))))), p_9))), FunCall(MapLcl(0)(fun((p_14) => FunCall(id, p_14))), FunCall(toLocal(fun((p_15) => FunCall(asScalar(), FunCall(MapLcl(0)(fun((p_16) => FunCall(VectorizeUserFun(4,id), p_16))), FunCall(asVector(4), p_15))))), Value("0.0f", ArrayType(Float, v_3_3)))), FunCall(Zip(2), FunCall(Transpose(), p_2), p_3))))))), FunCall(Transpose(), p_1))))), FunCall(Split(v_3_3), p_0))))

    var local: NDRange = Array(128, 1, 1)
    var global: NDRange = Array(?, ?, ?)
    InferNDRange(expr) match { case (l, g) => local = l; global = g }
    val valueMap = ParameterRewrite.createValueMap(expr)

    val globalSubstituted = InferNDRange.substituteInNDRange(global, valueMap)
    OpenCLGenerator.generate(expr, local, globalSubstituted, valueMap)
  }
}
