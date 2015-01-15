package junit.opencl.generator

import opencl.executor._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
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

  @Test def compositionTest(): Unit = {
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

  @Test def composeUserFunctionWithPattern(): Unit = {

    val Nsize = 512
    val Msize = 512
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => 1.0f * c * r)
    val gold   = matrix.map(- _.reduce(_+_))

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

    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f
    val gold = inputArray.map(_ * alpha).reduce(_+_)

    val scalFun = fun( ArrayType(Float, Var("N")), Float, (input, alpha) =>
      Join() o MapWrg(
        Join() o MapLcl(ReduceSeq(sumUp, 0.0f) o MapSeq(
          fun( x => mult(alpha, x) )
        )) o Split(4)
      ) o Split(1024) $ input
    )

    val (output, runtime) = Execute(inputArray.length)(scalFun, inputArray, alpha, inputArray.size)

    assertEquals(gold,output.reduce(_+_),0.0)
    //(gold, output).zipped.map(assertEquals(_,_,0.0))

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def VECTOR_NORM() {

    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = scala.math.sqrt(inputArray.map(x => x*x).reduce(_ + _)).toFloat

    val f = fun(
      ArrayType(Float, Var("N")),
      (input) =>

        Join() o MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(sqrtIt))) o Split(1) o
            Iterate(5)( Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2) ) o
            Join() o toLocal(MapLcl(ReduceSeq(doubleItAndSumUp, 0.0f))) o Split(32) o ReorderStride()
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

    val plusOne = UserFunDef("plusOne", "x", "{ return x+1; }", Float, Float)

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

    val f = f3

    val (output, runtime) = Execute(32, Nsize * Msize)(f, matrix, Msize, Nsize)

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
          )
        ) o Swap() $ matrix
      })

    val (output, runtime) = Execute(4, Nsize * Msize)(f, matrix, Msize, Nsize, Ksize)

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
