package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

object TestTuple {
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

class TestTuple {

  val makeTupleFromZip = UserFun("id",
    Array("x", "y"), "{ Tuple t = {x, y}; return t; }",
    Seq(Float, Float),
    TupleType(Float, Float))

  @Test def  MAKE_TUPLE_FROM_ZIP_EXPLICIT(): Unit = {
    val inputSize = 1024
    val inArrA = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val inArrB = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (inArrA zip inArrB).toVector

    val N = SizeVar("N")
    val f = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (a,b) => {
        toGlobal(MapGlb(makeTupleFromZip)) $ Zip(a, b)
      }
    )

    val (output, _) = Execute(inputSize)[Vector[(Float, Float)]](f, inArrA, inArrB)
    assertEquals(gold, output)
  }

  @Test
  @Ignore
  def zip(): Unit = {
    val inputSize = 1024
    val inArrA = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val inArrB = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inArrA.zip(inArrB).flatMap{case (a,b) => Array(a, b)}

    val N = SizeVar("N")
    val f = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (a,b) => {
        toGlobal(MapGlb(makeTupleFromZip)) o Zip(2) $ Tuple(a, b)
      }
    )

    val (output, _) = Execute(inputSize)[Array[Float]](f, inArrA, inArrB)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  @Ignore
  def zip2(): Unit = {
    val inputSize = 1024
    val inArrA = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val inArrB = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inArrA.zip(inArrB).flatMap{case (a,b) => Array(a, b)}

    val N = SizeVar("N")
    val f = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (a,b) => {
        toGlobal(MapGlb(makeTupleFromZip)) o Zip(2) o Unzip() $ Zip(a, b)
      }
    )

    val (output, _) = Execute(inputSize)[Array[Float]](f, inArrA, inArrB)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def  MAKE_TUPLE_FROM_ZIP_IMPLICIT(): Unit = {
    val inputSize = 1024
    val inArrA = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val inArrB = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = (inArrA zip inArrB).toVector

    val id =  UserFun("id", "x", "{ return x; }", TupleType(Float, Float), TupleType(Float, Float))

    val N = SizeVar("N")
    val f = fun(
                 ArrayTypeWSWC(Float, N),
                 ArrayTypeWSWC(Float, N),
                 (a,b) => {
                   toGlobal(MapGlb(id)) $ Zip(a, b)
                 }
               )

    val (output, runtime) = Execute(inputSize)[Vector[(Float, Float)]](f, inArrA, inArrB)

    assertEquals(gold, output)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }
  
  @Test def VECTOR_NEG_PAIR(): Unit = {
    val inputSize = 1024
    val inputArray = Array.fill(inputSize)((util.Random.nextInt(5).toFloat, util.Random.nextInt(5).toFloat))

    val gold = inputArray.map(t => (-t._1, -t._2)).toVector

    val negPair = UserFun("pair", "x", "{ x._0 = -x._0; x._1 = -x._1; return x; }",
      TupleType(Float, Float), TupleType(Float, Float))

    val f = fun(ArrayTypeWSWC(TupleType(Float, Float), SizeVar("N")), (input) =>
      Join() o MapWrg(
        Join() o  MapLcl(MapSeq(fun(x => negPair(x)))) o Split(4)
      ) o Split(1024) $ input
    )

    val (output, runtime) = Execute(inputSize)[Vector[(Float, Float)]](f, inputArray)

    assertEquals(gold, output)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def VECTOR_ADD_PAIRS(): Unit = {
    val inputSize = 1024
    val leftArray = Array.fill(inputSize, 2)(util.Random.nextInt(5).toFloat).map{ case Array(x, y) => (x, y) }
    val rightArray = Array.fill(inputSize, 2)(util.Random.nextInt(5).toFloat).map{ case Array(x, y) => (x, y) }

    val gold = (leftArray zip rightArray).map({ case ((lx, ly), (rx, ry)) => (lx + rx, ly + ry) }).toVector

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(TupleType(Float, Float), N),
      ArrayTypeWSWC(TupleType(Float, Float), N),
      (left, right) =>
        Join() o MapWrg(
          Join() o  MapLcl(MapSeq(addPair)) o Split(4)
        ) o Split(1024) $ Zip(left, right)
    )

    val (output, runtime) = Execute(inputSize)[Vector[(Float, Float)]](f, leftArray, rightArray)

    assertEquals(gold, output)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def VECTOR_PAIR(): Unit = {
    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val gold = inputArray.map(f => (f, f)).toVector

    val pair = UserFun("pair", "x", "{ Tuple t = {x, x}; return t; }",
                          Float, TupleType(Float, Float))

    val pairFun = fun(ArrayTypeWSWC(Float, SizeVar("N")), (input) =>
      Join() o MapWrg(
        Join() o  MapLcl(MapSeq(pair)) o Split(4)
      ) o Split(1024) $ input
    )

    val (output, runtime) = Execute(inputArray.length)[Vector[(Float, Float)]](pairFun, inputArray)

    assertEquals(gold, output)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def reduceOverTuples(): Unit = {

    val maxFirstArg = UserFun("maxFirstArg", Array("x", "y"), "{ return x._0 > y._0 ? x : y; }",
                                 Array(TupleType(Float, Float),
                                       TupleType(Float, Float)), TupleType(Float, Float))

    val inputSize = 512

    val N = SizeVar("N")

    val input = Array.fill(inputSize)(util.Random.nextInt(5).toFloat,util.Random.nextInt(5).toFloat)

    val gold = input.reduce((x, y) => if (x._1 > y._1) x else y)

    val function = fun(
      ArrayTypeWSWC(TupleType(Float, Float), N),
      input => toGlobal(MapSeq(idFF)) o ReduceSeq(maxFirstArg, (0.0f, 0.0f)) $ input
    )

    val (Vector(output), runtime) = Execute(inputSize)[Vector[(Float, Float)]](function, input)

    println(s"output = $output")
    println(s"runtime = $runtime")

    assertEquals(gold, output)
  }

  @Test def sumOverTuples(): Unit = {

    val inputSize = 512

    val N = SizeVar("N")

    val input = Array.fill(inputSize)(util.Random.nextInt(5).toFloat,util.Random.nextInt(5).toFloat)

    val gold = input.reduce((x, y) => (x._1 + y._1, x._2 + y._2))

    val function = fun(
      ArrayTypeWSWC(TupleType(Float, Float), N),
      input => toGlobal(MapSeq(idFF)) o ReduceSeq(addPair, (0.0f, 0.0f)) $ input
    )

    val (Vector(output), runtime) = Execute(inputSize)[Vector[(Float, Float)]](function, input)

    println("output = " + output)
    println("runtime = " + runtime)

    assertEquals(gold, output)
  }

  @Test def tuplePattern(): Unit = {
    val nSize = 256
    val mSize = 128
    val input = Array.fill(nSize, mSize)(util.Random.nextInt(5).toFloat)
    val array = Array.fill(nSize)(util.Random.nextInt(5).toFloat)
    val gold = (input, array).zipped.map((x, y) => (x.map(_+1), y)._1.map(_+y)).flatten

    val N = SizeVar("N")
    val M = SizeVar("M")

    val function = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, N),
      (A, B) => {
        MapGlb(fun(t => {
          MapSeq(fun(x => add.apply(x, Get(t, 1)))) $ Get(t, 0)
        }) o fun(t => {
          Tuple(MapSeq(plusOne) $ Get(t, 0), Get(t, 1))
        })) $ Zip(A, B)
      }
    )

    val (output, runtime) = Execute(nSize)[Array[Float]](function, input, array)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test
  def patternGetOfTuple(): Unit = {
    val inputSize = 1024
    val inputArray = Array.fill(inputSize, 2)(util.Random.nextInt(5).toFloat).map{ case Array(x, y) => (x, y) }

    val gold = inputArray.map(_._1)

    val f = fun(ArrayTypeWSWC(TupleType(Float, Float), SizeVar("N")), (input) =>
      Join() o MapWrg(
        Join() o  MapLcl(MapSeq(fun(x => id $ Get(x, 0)))) o Split(4)
      ) o Split(1024) $ input
    )

    val (output, runtime) = Execute(inputSize)[Array[Float]](f, inputArray)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def projectFirstComponentFromTuple() : Unit = {
    val idII = UserFun("idII", "x", "{ return x; }", TupleType(Int, Int), TupleType(Int, Int))

    val N = SizeVar("N")
    val f = fun(ArrayTypeWSWC(TupleType(Int, Int), N), A => {
      MapSeq( fun((a) => idI(a._0)) ) o MapSeq(idII) $ A
    })

    val inputSize = 1024
    val input = Array.fill(inputSize)(util.Random.nextInt(5),util.Random.nextInt(5))
    val gold = input.map(_._1)

    val (output, _) = Execute(128)[Array[Int]](f, input)

    assertArrayEquals(gold, output)
  }

}
