package junit.opencl.generator

import opencl.executor._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import opencl.ir._
import ir._

object TestMatrixVector {
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

class TestMatrixVector {

  val id = UserFunDef("id", "x", "{ return x; }", Float, Float)

  val sumUp = UserFunDef("sumUp", Array("x", "y"), "{ return x+y; }", Seq(Float, Float), Float)

  val add = UserFunDef("add", Array("x", "y"), "{ return x+y; }", Seq(Float, Float), Float)

  val mult = UserFunDef("mult", Array("l", "r"), "{ return l * r; }", Seq(Float, Float), Float)

  val multAndSumUp = UserFunDef("multAndSumUp", Array("acc", "l", "r"),
    "{ return acc + (l * r); }",
    Seq(Float, Float, Float), Float)

  def matrixVector(matrix: Array[Array[Float]], vector: Array[Float]): Array[Float] = {
    matrix.map(
      (row) => (row, vector).zipped.map(_ * _).reduce(_ + _)
    )
  }

  def matrixVector(matrix: Array[Array[Float]], vectorX: Array[Float], vectorY: Array[Float]): Array[Float] = {
    val tmp = matrix.map(
      (row) => (row, vectorX).zipped.map(_ * _).reduce(_ + _)
    )
    (tmp, vectorY).zipped.map(_ + _)
  }

  def matrixVector(matrix: Array[Array[Float]], vector: Array[Float], alpha: Float): Array[Float] = {
    matrix.map(
      (row) => (row, vector).zipped.map(_ * _).reduce(_ + _) * alpha
    )
  }

  def matrixVector(matrix: Array[Array[Float]], vectorX: Array[Float], vectorY: Array[Float], alpha: Float, beta: Float): Array[Float] = {
    val tmp = matrix.map(
      (row) => (row, vectorX).zipped.map(_ * _).reduce(_ + _) * alpha
    )

    val scaledY = vectorY.map(_ * beta)

    (tmp, scaledY).zipped.map(_ + _)
  }


  @Test def MATRIX_VECTOR_FIXED_SIZE() {

    val inputSize = 1024
    val matrix = Array.tabulate(inputSize, inputSize)((r,c) => 1.0f)
    val vector = Array.fill(inputSize)(2.0f)

    val f = fun(
      ArrayType(ArrayType(Float, 1024), 1024),
      ArrayType(Float, 1024),
      (matrix, vector) => {
        Join() o MapWrg(
          MapLcl( fun( (r) => ReduceSeq(sumUp, 0.0f) o MapSeq(mult) $ Zip(vector, r) ) )
        ) o Split(128) $ matrix

      })

    val (output, runtime) = Execute(inputSize * inputSize)(f, matrix, vector )

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    (matrixVector(matrix, vector), output).zipped.map(assertEquals(_,_,0.0))
  }

  @Test def MATRIX_VECTOR_FIXED_SIZE_LOCAL_MEMORY() {

    val inputSize = 1024
    val matrix = Array.tabulate(inputSize, inputSize)((r,c) => 1.0f)
    val vector = Array.fill(inputSize)(2.0f)

    val f = fun(
      ArrayType(ArrayType(Float, 1024), 1024),
      ArrayType(Float, 1024),
      (matrix, vector) => {
        MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(10)( Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2) ) o
            Join() o toLocal(MapLcl(MapSeq(mult))) o Split(1) o fun( (r) => Zip(vector, r) )
        ) $ matrix

      })

    val (output, runtime) = Execute(inputSize * inputSize)(f, matrix, vector)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    (matrixVector(matrix, vector), output).zipped.map(assertEquals(_,_,0.0))

    (output, runtime)

  }


  @Test def MATRIX_VECTOR() {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r,c) => 1.0f)
    val vector = Array.fill(inputSize)(2.0f)

    val f = fun(
      ArrayType(ArrayType(Float, Var("N1")), Var("M")),
      ArrayType(Float, Var("N2")),
      (matrix, vector) => {
        Join() o MapWrg(
          MapLcl( fun( (r) => ReduceSeq(sumUp, 0.0f) o MapSeq(mult) $ Zip(vector, r) ) )
        ) o Split(128) $ matrix
      })

    val (output, runtime) = Execute(inputSize * inputSize)(f, matrix, vector, inputSize, inputSize, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    (matrixVector(matrix, vector), output).zipped.map(assertEquals(_,_,0.0))

    (output, runtime)

  }

  @Test def MATRIX_VECTOR_LOCAL_MEMORY() {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r,c) => 1.0f)
    val vector = Array.fill(inputSize)(2.0f)

    val N = SizeVar("N")
    val M = SizeVar("M")
    val f = fun(
      ArrayType(ArrayType(Float, N), M),
      ArrayType(Float, N),
      (matrix, vector) => {
        MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(Log(2, N))(Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2)) o
            Join() o toLocal(MapLcl(MapSeq(mult))) o Split(1) o fun( (r) => Zip(vector, r) )
        ) $ matrix
      })

    val (output, runtime) = Execute(inputSize * inputSize)(f, matrix, vector, inputSize, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    (matrixVector(matrix, vector), output).zipped.map(assertEquals(_,_,0.0))

    (output, runtime)

  }

  @Test def MATRIX_VECTOR_LOCAL_MEMORY_FUSED() {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val vector = Array.tabulate(inputSize)(i => ((i % 10) + 1) * 2.0f)

    val N = SizeVar("N")
    val M = SizeVar("M")
    val f = fun(
      ArrayType(ArrayType(Float, N), M),
      ArrayType(Float, N),
      (matrix, vector) => {
        MapWrg(
          Join() o toGlobal(MapLcl(ReduceSeq(sumUp, 0.0f))) o Split(N / 32) o
            Join() o toLocal(MapLcl(ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f))) o ReorderStride() o Split(32) o fun( r => Zip(vector, r) )
        ) $ matrix
      })

    val (output, runtime) = Execute(inputSize * inputSize)(f, matrix, vector, inputSize, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    (matrixVector(matrix, vector), output).zipped.map(assertEquals(_,_,0.1))

  }


  @Test def MATRIX_VECTOR_FUSED() {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val vectorX = Array.tabulate(inputSize)(i => ((i % 10) + 1) * 2.0f)
    val vectorY = Array.tabulate(inputSize)(i => ((i*3 % 10) + 1) + 1.5f)
    val alpha = 2.5f
    val beta = 1.5f

    val N = SizeVar("N")
    val M = SizeVar("M")
    val f1 = fun(
      ArrayType(ArrayType(Float, N), M),
      ArrayType(Float, N),
      Float,
      (matrix, vectorX, alpha) => {
        MapWrg(
          Join() o MapLcl(
            MapSeq(fun( x => mult(alpha, x) )) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)
          ) o Split(4096) o fun( (r) => Zip(vectorX, r) )
        ) $ matrix
      })

    val (firstOutput, firstRuntime) = Execute(inputSize * inputSize)(f1, matrix, vectorX, alpha, inputSize, inputSize)

    println("output.size = " + firstOutput.size)
    println("output(0) = " + firstOutput(0))
    println("runtime = " + firstRuntime)

    (matrixVector(matrix, vectorX, alpha), firstOutput).zipped.map(assertEquals(_,_,0.0))

    val f2 = fun(
      ArrayType(Float, M),
      ArrayType(Float, M),
      Float,
      (tmp, vectorY, beta) => {
        Join() o Join() o MapWrg(
          MapLcl(MapSeq(fun( x => multAndSumUp(Get(x, 0), Get(x, 1), beta) )))
        ) o Split(128) o Split(32) $ Zip(tmp, vectorY)
      })

    val (output, secondRuntime) = Execute(inputSize)(f2, firstOutput, vectorY, beta, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + secondRuntime)

    (matrixVector(matrix, vectorX, vectorY, alpha, beta), output).zipped.map(assertEquals(_,_,0.0))

  }

  @Test def FULL_MATRIX_VECTOR_FUSED_OPENCL() {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val vectorX = Array.tabulate(inputSize)(i => ((i % 10) + 1) * 2.0f)
    val vectorY = Array.tabulate(inputSize)(i => ((i*3 % 10) + 1) + 1.5f)
    val alpha = 2.5f
    val beta = 1.5f

    val N = SizeVar("N")
    val M = SizeVar("M")
    val f = fun(
      ArrayType(ArrayType(Float, N), M),
      ArrayType(Float, N),
      ArrayType(ArrayType(Float, 1), M),
      Float,
      Float,
      (matrix, vectorX, vectorY, alpha, beta) => {
        MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(fun( x => multAndSumUp(Get(x, 0), Get(x, 1), beta))))) o Split(1) o
            fun( t => Zip(
              Join() o MapLcl(MapSeq(fun( x => mult(alpha, x) ))) o Split(1) o
                Join() o toLocal(MapLcl(ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f))) o Split(N) $ Zip(vectorX, Get(t, 0)),
              Get(t, 1)) )
        ) $ Zip(matrix, vectorY)
      })

    val (output, runtime) = Execute(inputSize * inputSize)(f, matrix, vectorX, vectorY, alpha, beta, inputSize, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    (matrixVector(matrix, vectorX, vectorY, alpha, beta), output).zipped.map(assertEquals(_,_,0.0))

  }

  @Test def FULL_MATRIX_VECTOR_FUSED() {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val vectorX = Array.tabulate(inputSize)(i => ((i % 10) + 1) * 2.0f)
    val vectorY = Array.tabulate(inputSize)(i => ((i*3 % 10) + 1) + 1.5f)

    val N = SizeVar("N")
    val M = SizeVar("M")
    val f = fun(
      ArrayType(ArrayType(Float, N), M),
      ArrayType(Float, N),
      ArrayType(ArrayType(Float, 1), M),
      (matrix, vectorX, vectorY) => {
        MapWrg(
          Join() o MapLcl(MapSeq(add)) o Split(1) o
            fun( t => Zip(
              Join() o MapLcl(ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)) o Split(N) $ Zip(vectorX, Get(t, 0)),
              Get(t, 1) ) )
        ) $ Zip(matrix, vectorY)
      })

    val (output, runtime) = Execute(inputSize * inputSize)(f, matrix, vectorX, vectorY, inputSize, inputSize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    (matrixVector(matrix, vectorX, vectorY), output).zipped.map(assertEquals(_,_,0.0))

  }

}
