package opencl.generator

import apart.arithmetic.Var
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{Ignore, AfterClass, BeforeClass, Test}

object TestTuple {
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

class TestTuple {

  val makeTupleFromZip = UserFun("id",
    Array("x", "y"), "{ Tuple t = {x, y}; return t; }",
    Seq(Float, Float),
    TupleType(Float, Float))

  @Test def  MAKE_TUPLE_FROM_ZIP_EXPLICIT() {
    val inputSize = 1024
    val inArrA = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val inArrB = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inArrA.zip(inArrB).flatMap{case (a,b) => Array(a, b)}

    val N = Var("N")
    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (a,b) => {
        toGlobal(MapGlb(makeTupleFromZip)) $ Zip(a, b)
      }
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, inArrA, inArrB)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  @Ignore
  def zip(){
    val inputSize = 1024
    val inArrA = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val inArrB = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inArrA.zip(inArrB).flatMap{case (a,b) => Array(a, b)}

    val N = Var("N")
    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (a,b) => {
        toGlobal(MapGlb(makeTupleFromZip)) o Zip(2) $ Tuple(a, b)
      }
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, inArrA, inArrB)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  @Ignore
  def zip2(){
    val inputSize = 1024
    val inArrA = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val inArrB = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inArrA.zip(inArrB).flatMap{case (a,b) => Array(a, b)}

    val N = Var("N")
    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (a,b) => {
        toGlobal(MapGlb(makeTupleFromZip)) o Zip(2) o Unzip() $ Zip(a, b)
      }
    )

    val (output: Array[Float], _) = Execute(inputSize)(f, inArrA, inArrB)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def  MAKE_TUPLE_FROM_ZIP_IMPLICIT() {
    val inputSize = 1024
    val inArrA = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val inArrB = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inArrA.zip(inArrB).flatMap{case (a,b) => Array(a, b)}

    val id =  UserFun("id", "x", "{ return x; }", TupleType(Float, Float), TupleType(Float, Float))

    val N = Var("N")
    val f = fun(
                 ArrayType(Float, N),
                 ArrayType(Float, N),
                 (a,b) => {
                   toGlobal(MapGlb(id)) $ Zip(a, b)
                 }
               )

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inArrA, inArrB)

    assertArrayEquals(gold, output, 0.0f)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }
  
  @Test def VECTOR_NEG_PAIR() {
    val inputSize = 1024
    val inputArray = Array.fill(inputSize * 2)(util.Random.nextInt(5).toFloat)

    val gold = inputArray.map(-_)

    val negPair = UserFun("pair", "x", "{ x._0 = -x._0; x._1 = -x._1; return x; }",
      TupleType(Float, Float), TupleType(Float, Float))

    val f = fun(ArrayType(TupleType(Float, Float), Var("N")), (input) =>
      Join() o MapWrg(
        Join() o  MapLcl(MapSeq(fun(x => negPair(x)))) o Split(4)
      ) o Split(1024) $ input
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputArray)

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
          Join() o  MapLcl(MapSeq(addPair)) o Split(4)
        ) o Split(1024) $ Zip(left, right)
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(f, leftArray, rightArray)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def VECTOR_PAIR() {
    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val gold = inputArray.flatMap((f) => Array(f, f))

    val pair = UserFun("pair", "x", "{ Tuple t = {x, x}; return t; }",
                          Float, TupleType(Float, Float))

    val pairFun = fun(ArrayType(Float, Var("N")), (input) =>
      Join() o MapWrg(
        Join() o  MapLcl(MapSeq(pair)) o Split(4)
      ) o Split(1024) $ input
    )

    val (output: Array[Float], runtime) = Execute(inputArray.length)(pairFun, inputArray)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def reduceOverTuples(): Unit = {

    val maxFirstArg = UserFun("maxFirstArg", Array("x", "y"), "{ return x._0 > y._0 ? x : y; }",
                                 Array(TupleType(Float, Float),
                                       TupleType(Float, Float)), TupleType(Float, Float))

    val inputSize = 512

    val N = Var("N")

    val input2 = Array.fill(inputSize)(util.Random.nextInt(5).toFloat,util.Random.nextInt(5).toFloat)

    val gold = input2.reduce((x, y) => if (x._1 > y._1) x else y).productIterator.asInstanceOf[Iterator[Float]].toArray

    val input = input2.map(_.productIterator).reduce(_++_).asInstanceOf[Iterator[Float]].toArray

    val function = fun(
      ArrayType(TupleType(Float, Float), N),
      input => toGlobal(MapSeq(idFF)) o ReduceSeq(maxFirstArg, (0.0f, 0.0f)) $ input
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(function, input)

    println("output.length = " + output.length)
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
      input => toGlobal(MapSeq(idFF)) o ReduceSeq(addPair, (0.0f, 0.0f)) $ input
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(function, input)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def tuplePattern(): Unit = {
    val nSize = 256
    val mSize = 128
    val input = Array.fill(nSize, mSize)(util.Random.nextInt(5).toFloat)
    val array = Array.fill(nSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, array).zipped.map((x, y) => (x.map(_+1), y)._1.map(_+y)).flatten

    val N = Var("N")
    val M = Var("M")

    val function = fun(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(Float, N),
      (A, B) => {
        MapGlb(fun(t => {
          MapSeq(fun(x => add.apply(x, Get(t, 1)))) $ Get(t, 0)
        }) o fun(t => {
          Tuple(MapSeq(plusOne) $ Get(t, 0), Get(t, 1))
        })) $ Zip(A, B)
      }
    )

    val (output: Array[Float], runtime) = Execute(nSize)(function, input, array)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def patternGetOfTuple(): Unit = {
    val inputSize = 1024
    val inputArray = Array.fill(inputSize * 2)(util.Random.nextInt(5).toFloat)

    val gold = inputArray.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)

    val f = fun(ArrayType(TupleType(Float, Float), Var("N")), (input) =>
      Join() o MapWrg(
        Join() o  MapLcl(MapSeq(fun(x => id $ Get(x, 0)))) o Split(4)
      ) o Split(1024) $ input
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputArray)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def projectFirstComponentFromTuple() : Unit = {
    val idII = UserFun("idII", "x", "{ return x; }", TupleType(Int, Int), TupleType(Int, Int))

    val N = Var("N")
    val f = fun(ArrayType(TupleType(Int, Int), N), A => {
      MapSeq( fun((a) => idI(a._0)) ) o MapSeq(idII) $ A
    })

    val inputSize = 1024
    val input2 = Array.fill(inputSize)(util.Random.nextInt(5),util.Random.nextInt(5))
    val input = input2.map(_.productIterator).reduce(_++_).asInstanceOf[Iterator[Int]].toArray
    val gold = input2.map(_._1)

    val (output: Array[Int], _) = Execute(128)(f, input)

    assertArrayEquals(gold, output)
  }

}
