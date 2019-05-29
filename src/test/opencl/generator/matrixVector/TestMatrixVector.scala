package opencl.generator.matrixVector

import benchmarks.MatrixVector
import ir._
import ir.ast._
import lift.arithmetic._
import opencl.executor._
import opencl.generator.AllocateLocalMemoryStatically
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.utils.ScalaPrinter

object TestMatrixVector extends TestWithExecutor

class TestMatrixVector {

  @Test
  def hawaiiBest(): Unit = {
    val factory = (variables: Seq[ArithExpr]) => {
      val v_M0_0 = variables(0)
      val v_N1_1 = variables(1)
      val v_2_2 = variables(2)

      val id = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
      val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
      val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
      fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M0_0), v_N1_1), ArrayTypeWSWC(Float, v_M0_0), ArrayTypeWSWC(Float, v_N1_1), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(MapWrg(0)(fun((p_5) => FunCall(toGlobal(fun((p_6) => FunCall(MapLcl(0)(fun((p_7) => FunCall(add, FunCall(mult, p_7, p_3), FunCall(mult, FunCall(Get(1), p_5), p_4)))), p_6))), FunCall(MapSeq(fun((p_8) => FunCall(toLocal(fun((p_9) => FunCall(id, p_9))), p_8))), FunCall(ReduceSeq(fun((p_10, p_11) => FunCall(add, p_10, p_11))), FunCall(id, Value("0.0f", Float)), FunCall(Join(), FunCall(MapLcl(0)(fun((p_12) => FunCall(MapSeq(fun((p_13) => FunCall(toLocal(fun((p_14) => FunCall(id, p_14))), p_13))), FunCall(ReduceSeq(fun((p_15, p_16) => FunCall(fun((p_17) => FunCall(add, p_15, FunCall(mult, FunCall(Get(0), p_17), FunCall(Get(1), p_17)))), p_16))), FunCall(id, Value("0.0f", Float)), p_12)))), FunCall(Split(v_M0_0 * 1 /^ v_2_2), FunCall(Gather(ReorderWithStride(v_2_2)), FunCall(Zip(2), p_1, FunCall(Get(0), p_5))))))))))), FunCall(Zip(2), p_0, p_2)))
    }

    val M = SizeVar("M")
    val N = SizeVar("N")

    val f = factory(Seq[ArithExpr](M,N, 128))

    println(ScalaPrinter()(f))

    Compile(f, 128,1,1,128*4096,1,1,scala.collection.immutable.Map[ArithExpr,ArithExpr](M -> 4096, N -> 4096))
  }

  @Test
  def keplerBest(): Unit = {

    val factory = (variables: Seq[ArithExpr]) => {
      val v_M0_0 = variables(0)
      val v_N1_1 = variables(1)
      val v_2_2 = variables(2)

      val id = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
      val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
      val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
      fun(ArrayTypeWSWC(ArrayTypeWSWC(Float, v_M0_0), v_N1_1), ArrayTypeWSWC(Float, v_M0_0), ArrayTypeWSWC(Float, v_N1_1), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(MapWrg(0)(fun((p_5) => FunCall(toGlobal(fun((p_6) => FunCall(MapLcl(0)(fun((p_7) => FunCall(add, FunCall(mult, p_7, p_3), FunCall(mult, FunCall(Get(1), p_5), p_4)))), p_6))), FunCall(MapSeq(fun((p_8) => FunCall(toLocal(fun((p_9) => FunCall(id, p_9))), p_8))), FunCall(ReduceSeq(fun((p_10, p_11) => FunCall(add, p_10, p_11))), FunCall(id, Value("0.0f", Float)), FunCall(Join(), FunCall(MapLcl(0)(fun((p_12) => FunCall(MapSeq(fun((p_13) => FunCall(toLocal(fun((p_14) => FunCall(id, p_14))), p_13))), FunCall(ReduceSeq(fun((p_15, p_16) => FunCall(fun((p_17) => FunCall(add, p_15, FunCall(mult, FunCall(Get(0), p_17), FunCall(Get(1), p_17)))), FunCall(toPrivate(fun((p_18) => FunCall(fun((p_19) => FunCall(Tuple(2), FunCall(id, FunCall(Get(0), p_19)), FunCall(id, FunCall(Get(1), p_19)))), p_18))), p_16)))), FunCall(id, Value("0.0f", Float)), p_12)))), FunCall(Split(v_M0_0 * 1 /^ v_2_2), FunCall(Gather(ReorderWithStride(v_2_2)), FunCall(Zip(2), p_1, FunCall(Get(0), p_5))))))))))), FunCall(Zip(2), p_0, p_2)))
    }

    val M = SizeVar("M")
    val N = SizeVar("N")

    val f = factory(Seq[ArithExpr](M,N, 128))
    println(ScalaPrinter()(f))

    Compile(f, 128,1,1,128*4096,1,1,scala.collection.immutable.Map[ArithExpr,ArithExpr](M -> 4096, N -> 4096))
  }

  @Test def MATRIX_VECTOR_FIXED_SIZE(): Unit = {

    val inputSize = 1024
    val matrix = Array.tabulate(inputSize, inputSize)((r,c) => 1.0f)
    val vector = Array.fill(inputSize)(2.0f)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 1024), 1024),
      ArrayTypeWSWC(Float, 1024),
      (matrix, vector) => {
        Join() o MapWrg(
           MapLcl( fun( (r) => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(vector, r) ) )
        ) o Split(128) $ matrix

      })

    val (output, runtime) = Execute(inputSize * inputSize)[Array[Float]](f, matrix, vector )

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(Utils.matrixVector(matrix, vector), output, 0.0f)
  }

  @Test def MATRIX_VECTOR_FIXED_SIZE_LOCAL_MEMORY(): Unit = {

    // TODO: Workaround for AMD GPUs. See issue 42.
    if (Utils.isAmdGpu)
      AllocateLocalMemoryStatically(false)

    val inputSize = 1024
    val matrix = Array.fill(inputSize, inputSize)(1.0f)
    val vector = Array.fill(inputSize)(2.0f)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 1024), 1024),
      ArrayTypeWSWC(Float, 1024),
      (matrix, vector) => {
        MapWrg(
          Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(10)( Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2) ) o
            Join() o  toLocal(MapLcl(MapSeq(mult))) o Split(1) o fun( (r) => Zip(vector, r) )
        ) $ matrix

      })

    val (output, _) = Execute(inputSize * inputSize)[Array[Float]](f, matrix, vector)

    AllocateLocalMemoryStatically(true)
    assertArrayEquals(Utils.matrixVector(matrix, vector), output, 0.0f)
  }


  @Test def MATRIX_VECTOR(): Unit = {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r,c) => 1.0f)
    val vector = Array.fill(inputSize)(2.0f)

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), SizeVar("M")),
      ArrayTypeWSWC(Float, N),
      (matrix, vector) => {
        Join() o MapWrg(
           MapLcl( fun( (r) => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(vector, r) ) )
        ) o Split(128) $ matrix
      })

    val (output, runtime) = Execute(inputSize * inputSize)[Array[Float]](f, matrix, vector)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(Utils.matrixVector(matrix, vector), output, 0.0f)
  }

  @Test def MATRIX_VECTOR_LOCAL_MEMORY(): Unit = {

    val inputSize = 1024
    val matrix = Array.tabulate(inputSize, inputSize)((r,c) => 1.0f)
    val vector = Array.fill(inputSize)(2.0f)

    val N = SizeVar("N")
    val M = SizeVar("M")
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      ArrayTypeWSWC(Float, N),
      (matrix, vector) => {
        MapWrg(
          Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(Log(2, N))(Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)) o
            Join() o  toLocal(MapLcl(MapSeq(mult))) o Split(1) o fun( (r) => Zip(vector, r) )
        ) $ matrix
      })

    val (output, runtime) = Execute(inputSize * inputSize)[Array[Float]](f, matrix, vector)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(Utils.matrixVector(matrix, vector), output, 0.0f)
  }

  @Test def MATRIX_VECTOR_LOCAL_MEMORY_FUSED(): Unit = {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val vector = Array.tabulate(inputSize)(i => ((i % 10) + 1) * 2.0f)

    val N = SizeVar("N")
    val M = SizeVar("M")
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      ArrayTypeWSWC(Float, N),
      (matrix, vector) => {
        MapWrg(
          Join() o
            toGlobal(MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o
            Split(N /^ 32) o
            Join() o
            toLocal(MapLcl(
              toLocal(MapSeq(id)) o
                ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f))
            ) o
            Split(32) o ReorderStride(N/^32) o fun( r => Zip(vector, r) )
        ) $ matrix
      })

    val (output, _) = Execute(inputSize * inputSize)[Array[Float]](f, matrix, vector)
    assertArrayEquals(Utils.matrixVector(matrix, vector), output, 0.0f)

  }


  @Test def MATRIX_VECTOR_FUSED(): Unit = {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val vectorX = Array.tabulate(inputSize)(i => ((i % 10) + 1) * 2.0f)
    val vectorY = Array.tabulate(inputSize)(i => ((i*3 % 10) + 1) + 1.5f)
    val alpha = 2.5f
    val beta = 1.5f

    val N = SizeVar("N")
    val M = SizeVar("M")
    val f1 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      ArrayTypeWSWC(Float, N),
      Float,
      (matrix, vectorX, alpha) => {
        MapWrg(
          Join() o  MapLcl(
            MapSeq(fun( x => mult(alpha, x) )) o toGlobal(MapSeq(id)) o  ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)
          ) o Split(4096) o fun( (r) => Zip(vectorX, r) )
        ) $ matrix
      })

    val (firstOutput, firstRuntime) = Execute(inputSize * inputSize)[Array[Float]](f1, matrix, vectorX, alpha)

    println("output.size = " + firstOutput.length)
    println("output(0) = " + firstOutput(0))
    println("runtime = " + firstRuntime)

    assertArrayEquals(Utils.matrixVector(matrix, vectorX, alpha), firstOutput, 0.0f)

    val f2 = fun(
      ArrayTypeWSWC(Float, M),
      ArrayTypeWSWC(Float, M),
      Float,
      (tmp, vectorY, beta) => {
        Join() o Join() o MapWrg(
           MapLcl(MapSeq(fun( x => multAndSumUp(Get(x, 0), Get(x, 1), beta) )))
        ) o Split(128) o Split(32) $ Zip(tmp, vectorY)
      })

    val (output, secondRuntime) = Execute(inputSize)[Array[Float]](f2, firstOutput, vectorY, beta)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + secondRuntime)

    assertArrayEquals(Utils.matrixVector(matrix, vectorX, vectorY, alpha, beta), output, 0.0f)

  }

  @Test def FULL_MATRIX_VECTOR_FUSED_OPENCL(): Unit = {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val vectorX = Array.tabulate(inputSize)(i => ((i % 10) + 1) * 2.0f)
    val vectorY = Array.tabulate(inputSize)(i => ((i*3 % 10) + 1) + 1.5f)
    val alpha = 2.5f
    val beta = 1.5f

    val f = MatrixVector.fullMatrixVectorFusedOpenCL

    val (output, runtime) = Execute(inputSize * inputSize)[Array[Float]](f, matrix, vectorX, vectorY, alpha, beta)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(Utils.matrixVector(matrix, vectorX, vectorY, alpha, beta), output,0.0f)
  }

  @Test def FULL_MATRIX_VECTOR_FUSED_OPENCL_AMD(): Unit = {

    val inputSize = 4096
    val matrix = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)
    val vectorX = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val vectorY = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val alpha = 2.5f
    val beta = 1.5f

    val f = MatrixVector.fullMatrixVectorFusedOpenCLAMD

    val (output, runtime) = Execute(inputSize * inputSize)[Array[Float]](f, matrix, vectorX, vectorY, alpha, beta)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(Utils.matrixVector(matrix, vectorX, vectorY, alpha, beta), output,0.0f)
  }

  @Test def FULL_MATRIX_VECTOR_FUSED(): Unit = {

    val inputSize = 4096
    val matrix = Array.tabulate(inputSize, inputSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val vectorX = Array.tabulate(inputSize)(i => ((i % 10) + 1) * 2.0f)
    val vectorY = Array.tabulate(inputSize)(i => Array(((i*3 % 10) + 1) + 1.5f))

    val N = SizeVar("N")
    val M = SizeVar("M")
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 1), M),
      (matrix, vectorX, vectorY) => {
        MapWrg(
          Join() o  MapLcl(MapSeq(add)) o Split(1) o
            fun( t => Zip(
              Join() o  MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)) o Split(N) $ Zip(vectorX, Get(t, 0)),
              Get(t, 1) ) )
        ) $ Zip(matrix, vectorY)
      })

    val (output, runtime) = Execute(inputSize * inputSize)[Array[Float]](f, matrix, vectorX, vectorY)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(Utils.matrixVector(matrix, vectorX, vectorY.flatten), output,0.0f)
  }

}
