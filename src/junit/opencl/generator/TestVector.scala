package opencl.generator

import benchmarks.VectorScaling
import ir.UserFunDef._
import ir._
import opencl.executor.{Execute, Compile, Executor}
import opencl.ir._
import opencl.ir.IndexFunction.reverse
import org.junit.Assert._
import org.junit.{Test, AfterClass, BeforeClass}

object TestVector {
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

class TestVector {

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
          Join() o Barrier() o MapLcl(MapSeq(add)) o Split(4)
        ) o Split(1024) $ Zip(left, right)

    )

    val code = Compile(addFun)
    val (output, runtime) = Execute(inputSize)(code, addFun, leftInputData, rightInputData, leftInputData.length)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

  }

  @Test def VECTOR_NEG_SIMPLE() {

    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inputArray.map(-_)

    val negFun = fun(ArrayType(Float, Var("N")), (input) =>

      Join() o MapWrg(
        Join() o Barrier() o MapLcl(MapSeq(neg)) o Split(4)
      ) o Split(1024) $ input

    )

    val (output, runtime) = Execute(inputArray.length)(negFun, inputArray, inputArray.length)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

  }

  @Test def VECTOR_NEG_SIMPLE_GLOBAL_ID() {

    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inputArray.map(-_)

    val negFun = fun(
      ArrayType(Float, Var("N")),
      (input) => Join() o MapGlb(
        MapSeq(neg)
      ) o Split(4) $ input
    )

    val (output, runtime) = Execute(inputArray.length)(negFun, inputArray, inputArray.length)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

  }

  @Test def VECTOR_NEG_SIMPLE_GLOBAL_ID_REORDER_REVERSE() {

    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inputArray.map(-_).reverse

    val negFun = fun(ArrayType(Float, Var("N")), (input) =>

      Gather(reverse)(Join() o MapGlb(
        MapSeq(neg)
      ) o Split(4)) $ input
    )

    val (output, runtime) = Execute(16, inputArray.length)(negFun, inputArray, inputArray.length)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def VECTOR_SCAL() {

    val inputSize = 1024
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f
    val gold = inputArray.map(_ * alpha)

    val scalFun = VectorScaling.vectorScal

    val (output, runtime) = Execute(inputArray.length)(scalFun, inputArray, alpha, inputArray.length)

    (gold, output).zipped.map(assertEquals(_,_,0.0))

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def SCAL_NVIDIA() {

    val inputSize = 4096
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f
    val gold = inputArray.map(_ * alpha)

    val scalFun = VectorScaling.scalNVIDIA

    val (output, runtime) = Execute(inputArray.length)(scalFun, inputArray, alpha, inputArray.length)

    (gold, output).zipped.map(assertEquals(_,_,0.0))

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def SCAL_AMD() {

    val inputSize = 2048
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f
    val gold = inputArray.map(_ * alpha)

    val scalFun = VectorScaling.scalAMD

    val (output, runtime) = Execute(inputArray.length)(scalFun, inputArray, alpha, inputArray.length)

    (gold, output).zipped.map(assertEquals(_,_,0.0))

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def SCAL_INTEL() {

    val inputSize = 65536
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f
    val gold = inputArray.map(_ * alpha)

    val scalFun = VectorScaling.scalINTEL

    val (output, runtime) = Execute(inputArray.length)(scalFun, inputArray, alpha, inputArray.length)

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
        Join() o Barrier() o MapLcl(ReduceSeq(add, 0.0f) o MapSeq(
          fun( x => mult(alpha, x) )
        )) o Split(4)
      ) o Split(1024) $ input
    )

    val (output, runtime) = Execute(inputArray.length)(scalFun, inputArray, alpha, inputArray.length)

    assertEquals(gold,output.sum,0.0)
    //(gold, output).zipped.map(assertEquals(_,_,0.0))

    println("output.size = " + output.length)
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
          Join() o Barrier() o toGlobal(MapLcl(MapSeq(sqrtIt))) o Split(1) o
            Iterate(5)( Join() o Barrier() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2) ) o
            Join() o Barrier() o toLocal(MapLcl(ReduceSeq(doubleItAndSumUp, 0.0f))) o Split(32) o ReorderStride(1024/32)
        ) o Split(1024) $ input

    )

    val (output, runtime) = Execute(inputSize)(f, inputArray, inputSize)

    assertEquals(gold, output(0), 0.1)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def addArrayOfVectors(): Unit = {
    val inputSize = 1024
    val numVectors = 1024
    val inputArray = Array.fill(numVectors, inputSize)(util.Random.nextInt(5).toFloat)

    val gold = inputArray.reduce((x, y) => (x, y).zipped.map(_+_))

    val test = inputArray.transpose.map(_.sum)
    assertArrayEquals(gold, test, 0.001f)

    val f = fun(
      ArrayType(ArrayType(Float, new Var("M")), new Var("N")),
      input => MapGlb(ReduceSeq(add, 0.0f)) o Transpose() $ input
    )

    val (output, _) = Execute(inputSize)(f, inputArray, inputSize, numVectors)

    assertArrayEquals(gold, output, 0.0f)
  }

}
