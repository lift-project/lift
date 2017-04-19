package opencl.ir.interpreter

import ir._
import ir.ast.{UserFun, fun, _}
import ir.interpreter.Interpreter
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir.pattern.{MapGlb, MapLcl, MapWrg, ReduceSeq, _}
import opencl.ir.{Float4, _}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

import scala.language.reflectiveCalls

object TestInterpreter {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    Executor.shutdown()
  }
}

class TestInterpreter {

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

    val output = Interpreter(l).->[Vector[Float]].run(inputData, 0.0f)

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

    val output = Interpreter(l).->[Vector[Float]].run(inputData)

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

    val output = Interpreter(l).->[Vector[Float]].runAndFlatten(inputData).toArray[Float]

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

    val output = Interpreter(l).->[Vector[Float]].run(inputData)

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

    val output = Interpreter(f).->[Vector[Vector[Vector[Float]]]].runAndFlatten(input).toArray[Float]

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

    val output = Interpreter(l).->[Vector[Float]].run(inputData).toArray

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

    val output = Interpreter(l).->[Vector[Float]].run(inputData).toArray

    assertEquals(inputData.sum + 1, output.head, 0.0f)
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

    val output = Interpreter(f).->[Vector[Float]].run(xs).toArray

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

    val output = Interpreter(compFun).->[Vector[Float]].run(inputData).toArray

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

    val output = Interpreter(f).->[Vector[Vector[Vector[Float]]]].runAndFlatten(matrix, vector).toArray[Float]
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
          fun(tuple => Tuple(Join() $ Get(tuple, 0), Join() $ Get(tuple, 1))) o Unzip() o MapSeq(fun( tuple =>
            Tuple(MapSeq(id) $ Get(tuple, 0), MapSeq(id) $ Get(tuple, 1))
          )) $ Zip(Split(1) $ Get(tuple, 0), Split(1) $ Get(tuple, 1))
        )) $ Zip(A, B)
    )

    val output = Interpreter(f)
                    .->[Vector[Vector[Float]]]
                    .runAndFlatten(inputData, inputData)
                    .toArray[Float]

    assertArrayEquals(inputData.flatten.map(_ + 1), output, 0.0f)
  }

  @Test def vectorType(): Unit = {
    val inputSize = 1024
    val inputData = Array.tabulate(inputSize*4)(_.toFloat)

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      (input) =>
        MapGlb(id.vectorize(4)) $ input
    )

    val output = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(inputData.grouped(4).toArray).toArray[Float]

    assertArrayEquals(inputData, output, 0.0f)
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

    val output = Interpreter(f).->[Vector[Float]].run(inputData).toArray
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

    val output = Interpreter(function).->[Vector[Vector[Float]]].runAndFlatten(matrix).toArray[Float]
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

    val output = Interpreter(f).->[Vector[Float]].run(inputData).toArray
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

    val output = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(matrix).toArray[Float]
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

    val output = Interpreter(f).->[Vector[Vector[Vector[Float]]]].runAndFlatten(matrix).toArray[Float]
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

    val output = Interpreter(f).->[Vector[Vector[Vector[Float]]]].runAndFlatten(matrix).toArray[Float]
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

    val output = Interpreter(f).->[Vector[Vector[Vector[Float]]]].runAndFlatten(matrix).toArray[Float]
    assertArrayEquals(matrix.flatten.flatten, output, 0.0f)
  }

  @Test def iterate(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.map(_+(1*7))

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Iterate(7)(MapGlb(plusOne)) $ in
    )

    val output = Interpreter(f).->[Vector[Float]].run(input).toArray
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def iterateFixedSecondArg() : Unit = {
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

    val output = Interpreter(f).->[Vector[Float]].run(inputA, inputB).toArray
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def iterateLocalOnly(): Unit = {
    val inputSize = 512
    val input = Array.tabulate(inputSize)(_.toFloat)
    val gold = input.map(_+1).map(_+1).map(_+1).map(_+1).map(_+1)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Join() o MapWrg( toGlobal(MapLcl(id)) o
        Iterate(5)( MapLcl(plusOne)) o
        toLocal(MapLcl(id))) o Split(16) $ in
    )

    val output = Interpreter(f).->[Vector[Float]].run(input).toArray
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
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (X, Y) => MapGlb(MapSeq(MapSeq(fun(z => add.apply(Get(z, 0), Get(z, 1)))))) o Map(fun(x => Map(fun(y => Zip(x, y))) $ Y )) $ X
    )

    val outputVec = Interpreter(f).->[Vector[Vector[Vector[Float]]]].run(A,B) //AndFlatten(A, B).toArray[Float]

    val output = outputVec.flatten.flatten.toArray
    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }
}
