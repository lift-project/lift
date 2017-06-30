package opencl.generator

import exploration.ParameterRewrite
import ir._
import ir.ast._
import lift.arithmetic.{?, ArithExpr, Cst, SizeVar}
import opencl.executor._
import opencl.generator.OpenCLAST.ArithExpression
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._
import rewriting.InferNDRange

object TestMisc {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestMisc {

  val incr = UserFun("incr", "x", "{ return x+1; }", Float, Float)

  @Ignore
  @Test
  def reverseScatterAndVec(): Unit = {

    val size = 1024
    val input = Array.fill(size)(util.Random.nextFloat())

    val N = SizeVar("N")

    val f = \(
      ArrayTypeWSWC(Float, N),
      Scatter(reverse) o asScalar() o MapGlb(VectorizeUserFun(4, id)) o asVector(4) $ _
    )

    val (output: Array[Float], _) = Execute(size)(f, input)

    assertArrayEquals(input.reverse, output, 0.0f)
  }

  @Ignore
  @Test
  def reverseGatherAndVec(): Unit = {

    val size = 1024
    val input = Array.fill(size)(util.Random.nextFloat())

    val N = SizeVar("N")

    val f = \(
      ArrayTypeWSWC(Float, N),
      asScalar() o MapGlb(VectorizeUserFun(4, id)) o asVector(4) o Gather(reverse) $ _
    )
    val (output: Array[Float], _) = Execute(size)(f, input)

    assertArrayEquals(input.reverse, output, 0.0f)
  }

  @Ignore
  @Test
  def strideScatterAndVec(): Unit = {

    val size = 1024
    val input = Array.fill(size)(util.Random.nextFloat())

    val N = SizeVar("N")

    val f = \(
      ArrayTypeWSWC(Float, N),
      Scatter(ReorderWithStride(32)) o asScalar() o
        MapGlb(VectorizeUserFun(4, id)) o asVector(4) $ _
    )

    val (output: Array[Float], _) = Execute(size)(f, input)

    assertArrayEquals(input.reverse, output, 0.0f)
  }

  @Ignore
  @Test
  def strideGatherAndVec(): Unit = {

    val size = 1024
    val input = Array.fill(size)(util.Random.nextFloat())

    val N = SizeVar("N")

    val f = \(
      ArrayTypeWSWC(Float, N),
      asScalar() o MapGlb(VectorizeUserFun(4, id)) o
        asVector(4) o Gather(ReorderWithStride(32)) $ _
    )
    val (output: Array[Float], _) = Execute(size)(f, input)

    assertArrayEquals(input.reverse, output, 0.0f)
  }

  @Ignore
  @Test
  def issue38(): Unit = {
    val N = SizeVar("N")
    val size = 1024
    val input = Array.fill(size)(util.Random.nextFloat())
    val a = 2.0f

    val f = \(
      ArrayTypeWSWC(Float, N),
      Float,
      (input, a) =>
        asScalar() o MapGlb(\(b => VectorizeUserFun(4, mult)(a, b))) o
          asVector(4) $ input
    )

    val (output: Array[Float], _) = Execute(size)(f, input, a)
    val gold = input.map(_*a)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def sameParam(): Unit = {

    val input = Array.fill(128)(util.Random.nextFloat())

    val f = fun(ArrayTypeWSWC(Float, SizeVar("N")),
      input => MapGlb(fun(a => add(a, a))) $ input
    )

    val (output: Array[Float], _) = Execute(input.length)(f, input)

    val gold = (input, input).zipped.map(_+_)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def testIterateAmdBug(): Unit = {

    Assume.assumeFalse("Wrong AMD IL generated", Utils.isAmdGpu)

    val inputSize = 1
    val input = Array.fill(inputSize)(0.0f)

    val iterCount = 100

    val f = fun(
        ArrayTypeWSWC(Float, SizeVar("N")),
        x => MapGlb(Iterate(iterCount)(MapSeq(incr))) o Split(inputSize) $ x
      )

    val (output: Array[Float], _) = Execute(1,1)(f, input)

    assertArrayEquals(Array.fill(inputSize)(100.0f), output, 0.0f)
  }

  @Test
  def testDouble(): Unit = {
    Assume.assumeTrue("Needs double support", Executor.supportsDouble())

    val inputSize = 256
    val input = Array.fill(inputSize)(util.Random.nextInt(5).toDouble)

    val incr = UserFun("incr", "x", "{ return x+1; }", Double, Double)

    val f = fun(
      ArrayTypeWSWC(Double, SizeVar("N")),
      in => MapGlb(incr) $ in
    )

    val (output: Array[Double], _) = Execute(inputSize)(f, input)

    assertArrayEquals(input.map(_ + 1), output, 0.0)
  }

  @Test
  def testBool(): Unit = {
    val inputSize = 256
    val input = Array.fill(inputSize)(util.Random.nextBoolean)

    val neg = UserFun("neg", "x", "{ if (x) { return false; } else { return true; } }", Bool, Bool)

    val f = fun(
      ArrayTypeWSWC(Bool, SizeVar("N")),
      in => MapGlb(neg) $ in
    )

    val (output: Array[Boolean], _) = Execute(inputSize)(f, input)

    assert( (input, output).zipped.forall((i, o) => i == !o) )
  }

  @Test def issue20(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inputData.map(_+5)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (inArr) => {
        Join() o MapGlb(
          Iterate(5)(fun((e) => MapSeq(incr) $ e))
        ) o Split(1) $ inArr
      }
    )

    val f2 = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
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

  @Test
  def issue76(): Unit = {
    val f = \(
      Float,
      ArrayTypeWSWC(Float, 32),
      (const, arr) =>
        MapGlb(\(a => add(const, add(const, a)))) $ arr
    )

    val input = Array.tabulate(32)(_.toFloat)
    val const = 2.0f

    val gold = input.map(_ + 2*const)

    val (result: Array[Float], _) = Execute(1,1)(f, const, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def issue76_2(): Unit = {
    val f = \(
      Float,
      ArrayTypeWSWC(Float, 32),
      (const, arr) =>
        MapGlb(\(a => add(toGlobal(id) $ const, add(const, a)))) $ arr
    )

    val input = Array.tabulate(32)(_.toFloat)
    val const = 2.0f

    val gold = input.map(_ + 2*const)

    val (result: Array[Float], _) = Execute(1,1)(f, const, input)

    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def issue76_3(): Unit = {
    val f = \(
      ArrayTypeWSWC(Float, 32),
      ArrayTypeWSWC(Float, 32),
      (arr1, arr2) =>
        MapGlb(\(a => MapSeq(\(b => add(toPrivate(id) $ b, add(a, b)))) $ arr2)) $ arr1
    )

    val input = Array.tabulate(32)(_.toFloat)

    val gold = input.flatMap(a => input.map(b => a + b + b))

    val (result: Array[Float], _) = Execute(1,1)(f, input, input)
    assertArrayEquals(gold, result, 0.0f)
  }

  @Test
  def issue76_4(): Unit = {
    val f = \(
      ArrayTypeWSWC(Float, 32),
      ArrayTypeWSWC(Float, 32),
      (arr1, arr2) =>
        MapGlb(\(a => MapSeq(\(b => add(add(a, b), toPrivate(id) $ b))) $ arr2)) $ arr1
    )

    val input = Array.tabulate(32)(_.toFloat)

    val gold = input.flatMap(a => input.map(b => a + b + b))

    val (result: Array[Float], _) = Execute(1,1)(f, input, input)
    assertArrayEquals(gold, result, 0.0f)
  }

  @Test def issue22(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (
      ArrayTypeWSWC(Float, SizeVar("N")),
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

    val l = fun (ArrayTypeWSWC(Float, SizeVar("N")),
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
      fun (ArrayTypeWSWC(Float, SizeVar("N")),
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
      fun (ArrayTypeWSWC(Float, SizeVar("N")),
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
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("N")), SizeVar("M")), SizeVar("L")),
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

    val l = fun(ArrayTypeWSWC(Float, SizeVar("N")),
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

    val l = fun(ArrayTypeWSWC(Float, SizeVar("N")),
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

    val l = fun(ArrayTypeWSWC(Float, Cst(inputSize)),
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

    val l = fun(ArrayTypeWSWC(Float, Cst(inputSize)),
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

    val l = fun(ArrayTypeWSWC(Float, Cst(inputSize)),
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
      " return x+1.0; ", Seq(Float), Float).
      setScalaFun( xs => xs.head.asInstanceOf[Float]+1.0f )

    // Expression
    val f = fun(
      ArrayTypeWSWC(Float, N),
      (xs) => MapGlb(
        fun(x => fct(x))
      ) $ xs
    )

    // execute
    val (output: Array[Float], _) = Execute(inputSize)(f, xs)
    assertArrayEquals(gold, output, 0.001f)
  }

  @Test def compositionTest(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val gold = inputData.map(x => - x)

    val composition = id o neg
    val N = SizeVar("N")

    val compFun = fun(
        ArrayTypeWSWC(Float, N),
      (input) =>
        MapGlb(composition) $ input
    )

    val (output: Array[Float], _) = Execute(inputSize)(compFun, inputData)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def accessingMultiDimArrayAfterZip(): Unit = {
    val Nsize = 8
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Nsize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)
    val vector = Array.fill(Nsize)(1.0f)

    val N = SizeVar("N")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), N),
      ArrayTypeWSWC(Float, N),
      (matrix, vector) => MapGlb(fun(r =>
        MapSeq(fun(t =>
          MapSeq(id) $ Get(t, 0)
        )) $ Zip(Get(r,0), vector)
      )) $ Zip(matrix, vector)
    )

    val (output: Array[Float], _) = Execute(4, Nsize)(f, matrix, vector)
    assertArrayEquals(matrix.flatten.flatten, output, 0.0f)
  }

  @Test
  def zipUnzip(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (A, B) =>
        MapGlb(fun(tuple => MapSeq(plusOne) $ Get(tuple, 0))) o
        MapGlb(fun( tuple =>
          fun(tuple =>
            Tuple(Join() $ Get(tuple, 0), Join() $ Get(tuple, 1))) o
            Unzip() o
            MapSeq(fun( tuple =>
            Tuple(MapSeq(id) $ Get(tuple, 0), MapSeq(id) $ Get(tuple, 1))
          )) $ Zip(Split(1) $ Get(tuple, 0), Split(1) $ Get(tuple, 1))
        )) $ Zip(A, B)
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, inputData, inputData)
    assertArrayEquals(inputData.flatten.map(_ + 1), output, 0.0f)
  }

  @Test def vectorType(): Unit = {
    val inputSize = 1024
    val inputData = Array.tabulate(inputSize, 4)((i, j) => (i * 4 + j).toFloat)

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      (input) =>
        MapGlb(id.vectorize(4)) $ input
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, inputData)
    assertArrayEquals(inputData.flatten, output, 0.0f)
  }

  @Test def vectorizePattern(): Unit = {
    val inputSize = 1024
    val inputData = Array.tabulate(inputSize, 4)((i, j) => (i * 4 + j).toFloat)

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      (input) =>
        MapGlb(VectorizeUserFun(4, id)) $ input
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, inputData)
    assertArrayEquals(inputData.flatten, output, 0.0f)
  }

  @Test def mapValueArg(): Unit = {
    val inputSize = 1024
    val inputData = Array.tabulate(inputSize)(_.toFloat)

    val gold = inputData.map(_ + 3.0f)

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      (input) =>
        MapGlb(fun(x => add(x, 3.0f))) $ input
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, inputData)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def composeUserFunctionWithPattern(): Unit = {

    val Nsize = 512
    val Msize = 512
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => 1.0f * c * r)
    val gold   = matrix.map(- _.sum)

    val function = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("N")), SizeVar("M")),
      (input) => MapGlb(toGlobal(MapSeq(neg)) o ReduceSeq(add, 0.0f)) $ input
    )

    val (output: Array[Float], _) = Execute(Nsize * Msize)(function, matrix)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def vectorize(): Unit = {
    val inputSize = 512
    val inputData = Array.tabulate(inputSize)(_.toFloat)

    val gold = inputData.map(_+1)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => asScalar() o MapGlb(plusOne.vectorize(4)) o asVector(4) $ in
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, inputData)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def asVectorFollowedByAsScalar(): Unit = {
    val inputSize = 512
    val inputData = Array.tabulate(inputSize)(_.toFloat)

    val gold = inputData.map(_+1)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(plusOne) o asScalar() o asVector(4) $ in
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, inputData)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def joinThenSplit2D(): Unit = {

    val Nsize = 256
    val Msize = 128
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * Msize.toFloat)

    val N = SizeVar("N")
    val M = SizeVar("M")


    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (matrix) => Split(Msize) o MapGlb(0)(id) o Join() $ matrix
    )

    val (output: Array[Float], _) = Execute(Nsize)(f, matrix)
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
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), N),
      (matrix) => Split(Msize) o MapGlb(0)(MapSeq(id)) o Join() $ matrix
    )

    val (output: Array[Float], _) = Execute(Nsize)(f, matrix)
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
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), N),
      (matrix) => MapGlb(0)(Split(Ksize) o MapSeq(id) o Join()) $ matrix
    )

    val (output: Array[Float], _) = Execute(Nsize)(f, matrix)
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
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), N),
      (matrix) => Split(Msize) o Split(Ksize) o MapGlb(0)(id) o Join() o Join() $ matrix
    )

    val (output: Array[Float], _) = Execute(Nsize)(f, matrix)
    assertArrayEquals(matrix.flatten.flatten, output, 0.0f)
  }

  @Test def iterate(): Unit = {

    Assume.assumeFalse("Disabled on AMD GPUs.", Utils.isAmdGpu)

    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.map(_+(1*7))

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Iterate(7)(MapGlb(plusOne)) $ in
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, input)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def iterateFixedSecondArg() : Unit = {

    Assume.assumeFalse("Disabled on AMD GPUs.", Utils.isAmdGpu)

    val inputSize = 512
    val inputA = Array.tabulate(inputSize)(_.toFloat)
    val inputB = Array.tabulate(inputSize)(_.toFloat).reverse
    val gold = inputA.zip(inputB.map(_*5.0f)).map((t:(Float, Float)) => t match{ case (x:Float,y:Float) => x+y})

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (inA,inB) => Iterate(5)(fun( (va) =>
        fun( (vb) =>
          MapWrg(add) $ Zip(va,vb)
        ) $ inB
      )) $ inA
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, inputA, inputB)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def iterateLocalOnly(): Unit = {

    Assume.assumeFalse("Disabled on AMD GPUs.", Utils.isAmdGpu)

    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.map(_+1).map(_+1).map(_+1).map(_+1).map(_+1)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg( toGlobal(MapLcl(id)) o
        Iterate(5)( MapLcl(plusOne)) o
        toLocal(MapLcl(id))) o Split(16) $ in
    )

    val f_nested = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => fun(x1 => Join()(MapWrg(
        fun(x2 =>
          fun(x3 =>
            fun(x4 => toGlobal(MapLcl(id))(x4))(
              Iterate(5)(fun(x5 => MapLcl(plusOne)(x5)))(x3)))(
                toLocal(MapLcl(id))(x2)))
          )(x1)))(Split(16)(in))
    )

    val f_nested2 = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x2 =>
        fun(x3 =>
          fun(x4 => toGlobal(MapLcl(id))(x4))(
            Iterate(5)(fun(x5 => MapLcl(plusOne)(x5)))(x3)))(
              toLocal(MapLcl(id))(x2)))
              )(Split(16)(in)))
    )

    val f_nested3 = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x2 =>
        fun(x4 => toGlobal(MapLcl(id))(x4))(
          Iterate(5)(fun(x5 => MapLcl(plusOne)(x5)))(
            toLocal(MapLcl(id))(x2)))
              ))(Split(16)(in)))
      )

    val f_nested4 = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x2 =>
        toGlobal(MapLcl(id))(
          Iterate(5)(fun(x5 => MapLcl(plusOne)(x5)))(
            toLocal(MapLcl(id))(x2)))
              ))(Split(16)(in)))
    )

    val f_full = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join()(MapWrg(fun(x0 =>
        toGlobal(MapLcl(id))(
          Iterate(5)(fun(x1 => MapLcl(plusOne)(x1)))(
            toLocal(MapLcl(id))(x0)))
              ))(Split(16)(in)))
    )

    val (output1: Array[Float], _) = Execute(inputSize)(f, input)
    assertArrayEquals(gold, output1, 0.0f)
    val (output2: Array[Float], _) = Execute(inputSize)(f_nested, input)
    assertArrayEquals(gold, output2, 0.0f)
    val (output3: Array[Float], _) = Execute(inputSize)(f_nested2, input)
    assertArrayEquals(gold, output3, 0.0f)
    val (output4: Array[Float], _) = Execute(inputSize)(f_nested3, input)
    assertArrayEquals(gold, output4, 0.0f)
    val (output5: Array[Float], _) = Execute(inputSize)(f_nested4, input)
    assertArrayEquals(gold, output5, 0.0f)
    val (output6: Array[Float], _) = Execute(inputSize)(f_full, input)
    assertArrayEquals(gold, output6, 0.0f)
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
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (X, Y) =>
        MapGlb(
          MapSeq(
            MapSeq(fun(z => add.apply(Get(z, 0), Get(z, 1)))))) o
          Map(fun(x => Map(fun(y => Zip(x, y))) $ Y )
        ) $ X
    )

    val (output: Array[Float], _) = Execute(nSize)(f, A, B)
    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def localMemoryRegression(): Unit = {
    val f =
      fun(
        ArrayTypeWSWC(Float, SizeVar("N")),
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

    val (output: Array[Float], _) = Execute(16, 2048)(kernel, f, input)

    assertEquals(input.sum, output.sum, 0.0f)
    assertEquals(9, "l_id".r.findAllMatchIn(kernel).length)
  }

  @Test(expected = classOf[OpenCLGeneratorException])
  def issue47(): Unit = {
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
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M0_0), v_N1_1),
        ArrayTypeWSWC(Float, v_M0_0),
        ArrayTypeWSWC(Float, v_N1_1),
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
    Execute(128, 1, 128*1024, 1, (true, true))(expr, values: _*)
  }

  @Test
  def initFromValue(): Unit = {
    val f = Lambda( Array(),
      toGlobal(MapGlb(id))
        $ Value(1.0f, ArrayTypeWSWC(Float, Cst(128)))
    )

    val (output: Array[Float], _) = Execute(128)(f)

    val gold = Array.fill(128)(1.0f)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def initFromVectorValue(): Unit = {
    val f = Lambda( Array(),
      toGlobal(MapGlb(VectorizeUserFun(4, id)))
        $ Value(1.0f, ArrayTypeWSWC(Float4, Cst(128/4)))
    )

    val (output: Array[Float], _) = Execute(128)(f)

    val gold = Array.fill(128)(1.0f)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Ignore
  @Test
  def initFromValueAsVector(): Unit = {
    val f = Lambda( Array(),
      toGlobal(MapGlb(VectorizeUserFun(4, id)))
        o asVector(4) $ Value(1.0f, ArrayTypeWSWC(Float, Cst(128)))
    )

    val (output: Array[Float], _) = Execute(128)(f)

    val gold = Array.fill(128)(1.0f)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Ignore
  @Test def issue50_MatchError(): Unit = {
    val v_K0_0 = SizeVar("")
    val v_M1_1 = SizeVar("")
    val v_N2_2 = SizeVar("")
    // TODO: trying to vectorise a value of ArrayType, for initialising it in local memory
    val v_3_3 = Cst(128)

    val id = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
    val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
    val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
    val expr = fun(
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_K0_0), v_M1_1),
        ArrayTypeWSWC(ArrayTypeWSWC(Float, v_N2_2), v_K0_0),
        (p_0, p_1) => FunCall(Join(), FunCall(MapWrg(0)(fun((p_2) => FunCall(TransposeW(), FunCall(MapWrg(1)(fun((p_3) => FunCall(TransposeW(), FunCall(toGlobal(fun((p_4) => FunCall(MapSeq(fun((p_5) => FunCall(MapLcl(0)(fun((p_6) => FunCall(id, p_6))), p_5))), p_4))), FunCall(MapSeq(fun((p_7) => p_7)), FunCall(ReduceSeq(fun((p_8, p_9) => FunCall(fun((p_10) => FunCall(MapLcl(0)(fun((p_11) => FunCall(add, FunCall(Get(0), p_11), FunCall(mult, FunCall(Get(1), p_11), FunCall(Get(1), p_10))))), FunCall(toLocal(fun((p_12) => FunCall(MapLcl(0)(fun((p_13) => FunCall(Tuple(2), FunCall(id, FunCall(Get(0), p_13)), FunCall(id, FunCall(Get(1), p_13))))), p_12))), FunCall(Zip(2), p_8, FunCall(Get(0), p_10))))), p_9))), FunCall(MapLcl(0)(fun((p_14) => FunCall(id, p_14))), FunCall(toLocal(fun((p_15) => FunCall(asScalar(), FunCall(MapLcl(0)(fun((p_16) => FunCall(VectorizeUserFun(4,id), p_16))), FunCall(asVector(4), p_15))))), Value("0.0f", ArrayTypeWSWC(Float, v_3_3)))), FunCall(Zip(2), FunCall(Transpose(), p_2), p_3))))))), FunCall(Transpose(), p_1))))), FunCall(Split(v_3_3), p_0))))

    println(expr)

    var local: NDRange = NDRange(128, 1, 1)
    var global: NDRange = NDRange(?, ?, ?)
    InferNDRange(expr) match { case (l, g) => local = l; global = g }
    val valueMap = ParameterRewrite.createValueMap(expr)

    val globalSubstituted = InferNDRange.substituteInNDRange(global, valueMap)
    OpenCLGenerator.generate(expr, local, globalSubstituted, valueMap)
  }
  
  @Test def issue104(): Unit = {
    val size = 128
    val N = SizeVar("N")
    val input = Array.fill(size, 4)(util.Random.nextInt(5))
  
    val flatten = UserFun(
      "flatten", "t",
      "Tuple1 t2 = {t._0._0, t._0._1, t._0._2, t._1}; return t2;",
      TupleType(TupleType(Int, Int, Int), Int), TupleType(Int, Int, Int, Int)
    )
  
    val reshape2 = UserFun(
      "reshape2", "x",
      "Tuple1 t = {{x._0._0, x._0._1, x._1}, x._2}; return t;",
      TupleType(TupleType(Int, Int), Int, Int),
      TupleType(TupleType(Int, Int, Int), Int)
    )
  
    val reshape1 = UserFun(
      "reshape1", Array("a", "b", "c", "d"),
      "Tuple t = {{a, b}, c, d}; return t;",
      Seq(Int, Int, Int, Int), TupleType(TupleType(Int, Int), Int, Int)
    )
  
    val expr = fun(
      ArrayTypeWSWC(TupleType(Int, Int, Int, Int), N),
      arr => MapGlb(toGlobal(flatten) o reshape2 o reshape1) $ arr
    )
  
    val (output: Array[Int], _) = Execute(size)(expr, input.flatten)
    assertArrayEquals(input.flatten, output)
  }

  @Test
  def arrayFromValue(): Unit = {

    val input = Array.fill(128)(util.Random.nextFloat())

    val at = ArrayTypeWSWC(Float, SizeVar("N"))

    val f = fun(at,
      input => MapGlb(add) $ Zip(input, ArrayFromValue(1, at))
    )

    val (output: Array[Float], _) = Execute(input.length)(f, input)

    val gold = (input, Array.fill(input.length)(1)).zipped.map(_+_)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def arrayFromGenerator(): Unit = {

    val input = Array.fill(128)(util.Random.nextFloat())

    val at = ArrayTypeWSWC(Float, SizeVar("N"))

    val f = fun(at,
      input => MapGlb(add) $ Zip(input, ArrayFromGenerator( (i, _) => ArithExpression(i), at))
    )

    val (output: Array[Float], _) = Execute(input.length)(f, input)

    val gold = (input, Array.tabulate(input.length)( i => i )).zipped.map(_+_)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def arrayFromUserFunGenerator(): Unit = {

    val input = Array.fill(128)(util.Random.nextFloat())

    val at = ArrayTypeWSWC(Float, SizeVar("N"))

    val idxF = UserFun("idxF", Array("i", "n"), "{ return i; }", Seq(Int, Int), Int)

    val f = fun(at,
      input => MapGlb(add) $ Zip(input, ArrayFromUserFunGenerator(idxF, at))
    )

    val (output: Array[Float], _) = Execute(input.length)(f, input)

    val gold = (input, Array.tabulate(input.length)( i => i )).zipped.map(_+_)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def arrayFromUserFunGenerator2(): Unit = {

    val input = Array.fill(128)(util.Random.nextInt())

    val at = ArrayTypeWSWC(Int, SizeVar("N"))

    val idxF = UserFun("idxF", Array("i", "n"), "{ return i; }", Seq(Int, Int), Int)

    val f = fun(at,
      input =>
        ArrayFromUserFunGenerator(idxF, at) :>>
          Split(64) :>>
            toGlobal(MapGlb(MapSeq(idI)))
    )

    val (output: Array[Int], _) = Execute(input.length)(f, input)

    val gold = Array.tabulate(input.length)( i => i )

    assertArrayEquals(gold, output)
  }

  @Test
  def arrayFromUserFunGenerator2D(): Unit = {
    val m = 128
    val n = 8

    val input = Array.fill(m, n)(util.Random.nextInt())

    val M = SizeVar("M")
    val N = SizeVar("N")

    val at = ArrayTypeWSWC(ArrayTypeWSWC(Int, N), M)

    val idxF = UserFun("idxF", Array("i", "j", "m", "n"), "{ return i+j; }",
      Seq(Int, Int, Int, Int), Int)

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(Int, N), M),
      input =>
        Array2DFromUserFunGenerator(idxF, at) :>>
          toGlobal(MapGlb(MapSeq(idI)))
    )

    val (output: Array[Int], _) = Execute(input.length)(f, input)

    val gold = Array.tabulate(m, n)( (i, j) => i+j ).flatten

    assertArrayEquals(gold, output)
  }

  @Test
  def arrayFromUserFunGenerator3D(): Unit = {
    val m = 128
    val n = 8
    val o = 4

    val input = Array.fill(m, n, o)(util.Random.nextInt())

    val M = SizeVar("M")
    val N = SizeVar("N")
    val O = SizeVar("O")

    val at = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, O), N), M)

    val idxF = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ return i+j+k; }",
      Seq(Int, Int, Int, Int, Int, Int), Int)

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, O), N), M),
      input =>
        Array3DFromUserFunGenerator(idxF, at) :>>
          toGlobal(MapGlb(MapSeq(MapSeq(idI))))
    )

    val (output: Array[Int], _) = Execute(input.length)(f, input)

    val gold = Array.tabulate(m, n, o)( (i, j, k) => i+j+k ).flatten.flatten

    assertArrayEquals(gold, output)
  }
}
