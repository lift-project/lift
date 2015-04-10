package opencl.generator

import arithmetic.Var
import benchmarks.SumAbsoluteValues
import org.junit._
import org.junit.Assert._
import opencl.ir._
import ir._
import ir.UserFunDef._

import opencl.executor._

object TestReduce {
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

class TestReduce {

  @Test def reduceParamInitial(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (
      ArrayType(Float, Var("N")),
      Float,
      (in, init) => {
      Join() o MapWrg(
        Join() o Barrier() o MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, init)) o Split(4)
      ) o Split(128) $ in
    })

    val (output, runtime) = Execute(inputData.length)( l, inputData, 0.0f, inputData.length )

    assertEquals(inputData.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def reduceArrayParamInitial(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(0.0f)
    val matrix = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)

    val N = Var("N")
    val M = Var("M")

    val l = fun(
      ArrayType(ArrayType(Float, N), M),
      ArrayType(Float, N),
      (in, init) => {
        Join() o MapSeq(MapSeq(MapSeq(id)) o ReduceSeq(fun((x, y) => MapSeq(add) $ Zip(x, y)), MapSeq(id) $ init)) o Split(M) $ in
      })

    val (output, runtime) = Execute(1, 1)(l, matrix, inputData, inputSize, inputSize)

    val gold = matrix.reduce((x, y) => (x, y).zipped.map(_+_))

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)

  }

  @Test def reduceArrayValueInitial(): Unit = {
    val inputSize = 1024
    val matrix = Array.fill(inputSize, inputSize)(util.Random.nextInt(5).toFloat)

    val N = Var("N")
    val M = Var("M")

    val l = fun(
      ArrayType(ArrayType(Float, N), M),
      in => {
        Join() o MapSeq(
          MapSeq(MapSeq(id)) o
            ReduceSeq(fun((x, y) => MapSeq(add) $ Zip(x, y)),
              toGlobal(MapSeq(id)) $ Value.apply(0.0f, ArrayType(Float, N)))
        ) o Split(M) $ in
      })

    val (output, runtime) = Execute(1, 1)(l, matrix, inputSize, inputSize)

    val gold = matrix.reduce((x, y) => (x, y).zipped.map(_+_))

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def reduceIdParamInitial(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayType(Float, Var("N")),
      Float,
      (in, init) => {
        Join() o MapWrg(
          Join() o Barrier() o MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, id(init))) o Split(4)
        ) o Split(128) $ in
      })

    val (output, runtime) = Execute(inputData.length)(l, inputData, 0.0f, inputData.length)

    assertEquals(inputData.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def reduceIdValueInitial(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayType(Float, Var("N")),
      in => {
        Join() o MapWrg(
          Join() o Barrier() o MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, id(Value.FloatToValue(0.0f)))) o Split(4)
        ) o Split(128) $ in
      })

    val (output, runtime) = Execute(inputData.length)(l, inputData, inputData.length)

    assertEquals(inputData.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def SIMPLE_REDUCE_FIRST() {

    val inputSize = 4194304
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayType(Float, Var("N")), (in) => {
      Join() o MapWrg(
        Join() o Barrier() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2048)
      ) o Split(262144) $ in
    } )

    val (output, runtime) = Execute(inputData.length)( l, inputData, inputData.length )

    assertEquals(inputData.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }


  @Test def SIMPLE_REDUCE_SECOND() {

    val inputSize = 4194304
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {
      Join() o Join() o  MapWrg(
        Barrier() o MapLcl(ReduceSeq(add, 0.0f))
      ) o Split(128) o Split(2048) $ in
    }), inputData, inputData.length)

    assertEquals(inputData.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }


  @Test def REDUCE_HOST() {


    val inputSize = 128
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (output, runtime) = opencl.executor.Execute(1, inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {
      ReduceHost(add, 0.0f) $ in
    }), inputData, inputData.length)

    assertEquals(inputData.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }


  @Test def NVIDIA_A() {

    // for input size of 16777216 the first kernel has to be executed 3 times and the second kernel once to perform a
    // full reduction

    val inputSize = 1024
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {
        Join() o MapWrg(
          Join() o Barrier() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(7)(Join() o Barrier() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2) ) o
            Join() o Barrier() o toLocal(MapLcl(MapSeq(id))) o Split(1)
        ) o Split(128) $ in
      }), inputData, inputData.length)

      assertEquals(inputData.sum, output.sum, 0.0)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(8, firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {
        Join() o MapWrg(
          Join() o Barrier() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(3)(Join() o Barrier() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2)) o
            Join() o Barrier() o toLocal(MapLcl(MapSeq(id))) o Split(1)
        ) o Split(8) $ in
      }), firstOutput, firstOutput.length)

      assertEquals(inputData.sum, output.sum, 0.0)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

    }

  }


  @Test def NVIDIA_B() {
    // for an input size of 16777216 the kernel has to executed 3 times to perform a full reduction

    val inputSize = 1024
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {
      Join() o MapWrg(
        Join() o Barrier() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
          Iterate(7)( Join() o Barrier() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2) ) o
          Join() o Barrier() o toLocal(MapLcl(ReduceSeq(add, 0.0f))) o Split(2)
      ) o Split(256) $ in
    }), inputData, inputData.length)

    assertEquals(inputData.sum, output.sum, 0.0)

    println("outputArray(0) = " + output(0))
    println("Runtime = " + runtime)
  }

  @Ignore
  @Test def NVIDIA_C() {
    // TODO: Only valid with warp size of 32. Perhaps query the device and use org.junit.Assume to conditionally ignore the test

    val inputSize = 16777216
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o Barrier() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Join() o Barrier() o MapWarp( Iterate(5)( Join() o MapLane(ReduceSeq(add, 0.0f)) o Split(2) ) ) o Split(32) o
            Iterate(2)( Join() o Barrier() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2) ) o
            Join() o Barrier() o toLocal(MapLcl(ReduceSeq(add, 0.0f))) o Split(2048) o ReorderStride(262144 / 2048)
        ) o Split(262144) $ in

      }), inputData, inputData.length)

      assertEquals("Note that this benchmark is only valid on device with a warp_size of 32!",inputData.sum, output.sum, 0.1)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(64, firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o Barrier() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(5)( Join() o Barrier() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2) ) o
            Join() o Barrier() o toLocal(MapLcl(ReduceSeq(add, 0.0f))) o Split(2)
        ) o Split(64) $ in

      }), firstOutput, firstOutput.length)


      assertEquals(inputData.sum, output.sum, 0.1)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

    }

  }

  @Test def NVIDIA_C_NO_WARPS() {

    val inputSize = 16777216
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {

      val f = fun(
        ArrayType(Float, Var("N")),
        (in) =>
        Join() o MapWrg(
          Join() o Barrier() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(7)(Join() o Barrier() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2)) o
            Join() o Barrier() o toLocal(MapLcl(ReduceSeq(add, 0.0f))) o Split(2048) o ReorderStride(262144/2048)
        ) o Split(262144) $ in
      )

      val (output, runtime) = Execute(inputData.length)(f, inputData, inputData.length)

      assertEquals(inputData.sum, output.sum, 0.0)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(64, firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o Barrier() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(5)( Join() o Barrier() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2) ) o
            Join() o Barrier() o toLocal(MapLcl(ReduceSeq(add, 0.0f))) o Split(2)
        ) o Split(64) $ in

      }), firstOutput, firstOutput.length)

      assertEquals(inputData.sum, output.sum, 0.0)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

    }

  }

  @Test def AMD_A() {
    // for an input size of 16777216 the first kernel has to be executed twice and the second kernel once to perform a
    // full reduction

    val inputSize = 32768
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (firstOutput, _) = {

      val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          asScalar() o Join() o Barrier() o toGlobal(MapLcl(MapSeq(Vectorize(4)(id)))) o Split(1) o
            Iterate(8)( Join() o Barrier() o MapLcl(ReduceSeq(Vectorize(4)(add), Vectorize(4)(0.0f))) o Split(2) ) o
            Join() o Barrier() o toLocal(MapLcl(ReduceSeq(Vectorize(4)(add), Vectorize(4)(0.0f)))) o Split(2) o asVector(4)
        ) o Split(2048) $ in

      }), inputData, inputData.length)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.1)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(64, firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o Barrier() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(6)( Join() o Barrier() o MapLcl(ReduceSeq(add, 0.0f)) o Split(2) ) o
            Join() o Barrier() o toLocal(MapLcl(MapSeq(id))) o Split(1)
        ) o Split(64) $ in

      }), firstOutput, firstOutput.length)

      assertEquals(inputData.sum, output.sum, 0.0)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

    }

  }

  @Test def NVIDIA_DERIVED() {

    val inputSize = 16777216
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = opencl.executor.Execute(inputData.length)(SumAbsoluteValues.nvidiaDerived1,
        inputData, inputData.length)

      assertEquals(inputData.sum, output.sum, 0.0)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(firstOutput.length)(SumAbsoluteValues.amdNvidiaDerived2,
        firstOutput, firstOutput.length)

      assertEquals(inputData.sum, output.sum, 0.0)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

    }

  }

  @Test def AMD_DERIVED() {

    val inputSize = 16777216
//    val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = Execute(inputData.length)(SumAbsoluteValues.amdDerived1,
        inputData, inputData.length)

      println("output size = " + output.length)
      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.0)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(firstOutput.length)(SumAbsoluteValues.amdNvidiaDerived2,
        firstOutput, firstOutput.length)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.0)

    }

  }

  @Test def INTEL_DERIVED() {

    val inputSize = 16777216
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
            asScalar() o Join() o Join() o MapWarp(
            MapLane(MapSeq(Vectorize(4)(id)) o ReduceSeq(Vectorize(4)(absAndSumUp), Vectorize(4)(0.0f)))
          ) o Split(1) o Split(8192) o asVector(4)
        ) o Split(32768) $ in

      }), inputData, inputData.length)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.0)

      (output, runtime)
    }

    {

      val (output, runtime) = Execute(firstOutput.length)(SumAbsoluteValues.intelDerived2,
        firstOutput, firstOutput.length)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.0)

    }

  }

  @Test def INTEL_DERIVED_NO_WARP() {

    val inputSize = 16777216
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = Execute(inputData.length)(SumAbsoluteValues.intelDerivedNoWarp1,
        inputData, inputData.length)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.0)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(firstOutput.length)(SumAbsoluteValues.intelDerived2,
        firstOutput, firstOutput.length)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.0)

    }

  }

  /*
    @Test def SEQ_TEST() {

      /*
       // this is working fine
      val kernel = Join() o MapWrg(
        Join() o Barrier() o MapLcl(MapSeq(id)) o Barrier() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(1024)
      ) o Split(1024) o input
      */

      /* // triggers wrong allocation ...
      val kernel = Join() o MapWrg(
        Join() o Barrier() o MapLcl(MapSeq(id)) o Split(1024)
      ) o Split(1024) o input
      */


      // triggers generation of wrong code (the indices are broken)
      val kernel = Join() o MapWrg(
        Join() o Barrier() o MapLcl(MapSeq(id) o ReduceSeq(sumUp, 0.0f)) o Split(1024)
      ) o Split(1024) o input


      val inputSize = 4194304
      //val inputData = Array.fill(inputSize)(1.0f)
      val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

      val (output, runtime) = {
        val outputSize = inputSize / 1024

        val (output, runtime) = execute(kernel, inputData, outputSize)

        println("output size = " + output.size)
        println("first output(0) = " + output(0))
        println("first runtime = " + runtime)

        assertEquals(inputData.sum, output.sum, 0.0)

        (output, runtime)
      }

    }
    */

}
