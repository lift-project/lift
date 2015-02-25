package junit.opencl.generator

import org.junit._
import org.junit.Assert._
import opencl.ir._
import ir._

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

  val sumUp = UserFunDef("sumUp", Array("x", "y"), "{ return x+y; }", Seq(Float, Float), Float)

  val id = UserFunDef("id", "x", "{ return x; }", Float, Float)

  val absAndSumUp = UserFunDef("absAndSumUp", Array("acc", "x"), "{ return acc + fabs(x); }", Seq(Float, Float), Float)


  @Test def SIMPLE_REDUCE_FIRST() {

    val inputSize = 4194304
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayType(Float, Var("N")), (in) => {
      Join() o MapWrg(
        Join() o MapLcl(ReduceSeq(sumUp, 0.0f) /*o MapSeq(id)*/) o Split(2048)
      ) o Split(262144) $ in
    } )

    val (output, runtime) = Execute(inputData.length)( l, inputData, inputData.length )

    assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }


  @Test def SIMPLE_REDUCE_SECOND() {

    val inputSize = 4194304
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {
      Join() o Join() o  MapWrg(
        MapLcl(ReduceSeq(sumUp, 0.0f))
      ) o Split(128) o Split(2048) $ in
    }), inputData, inputData.length)

    assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }


  @Test def REDUCE_HOST() {


    val inputSize = 128
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (output, runtime) = opencl.executor.Execute(1, inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {
      ReduceHost(sumUp, 0.0f) $ in
    }), inputData, inputData.length)

    assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

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
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(7)(Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2) ) o
            Join() o toLocal(MapLcl(MapSeq(id))) o Split(1)
        ) o Split(128) $ in
      }), inputData, inputData.length)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(8, firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {
        Join() o MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(3)(Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2)) o
            Join() o toLocal(MapLcl(MapSeq(id))) o Split(1)
        ) o Split(8) $ in
      }), firstOutput, firstOutput.length)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      (output, runtime)
    }

  }


  @Test def NVIDIA_B() {
    // for an input size of 16777216 the kernel has to executed 3 times to perform a full reduction

    val inputSize = 1024
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {
      Join() o MapWrg(
        Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
          Iterate(7)( Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2) ) o
          Join() o toLocal(MapLcl(ReduceSeq(sumUp, 0.0f))) o Split(2)
      ) o Split(256) $ in
    }), inputData, inputData.length)

    assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

    println("outputArray(0) = " + output(0))
    println("Runtime = " + runtime)
  }

  @Test def NVIDIA_C() {

    val inputSize = 16777216
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Join() o MapWarp( Iterate(5)( Join() o MapLane(ReduceSeq(sumUp, 0.0f)) o Split(2) ) ) o Split(32) o
            Iterate(2)( Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2) ) o
            Join() o toLocal(MapLcl(ReduceSeq(sumUp, 0.0f))) o Split(2048) o ReorderStride(2048 / 262144)
        ) o Split(262144) $ in

      }), inputData, inputData.length)

      assertEquals("Note that this benchmark is only valid on device with a warp_size of 32!",inputData.reduce(_ + _), output.reduce(_ + _), 0.1)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(64, firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(5)( Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2) ) o
            Join() o toLocal(MapLcl(ReduceSeq(sumUp, 0.0f))) o Split(2)
        ) o Split(64) $ in

      }), firstOutput, firstOutput.length)


      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.1)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      (output, runtime)
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
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(7)(Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2)) o
            Join() o toLocal(MapLcl(ReduceSeq(sumUp, 0.0f))) o Split(2048) o ReorderStride(262144/2048)
        ) o Split(262144) $ in
      )

      val (output, runtime) = Execute(inputData.length)(f, inputData, inputData.length)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(64, firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(5)( Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2) ) o
            Join() o toLocal(MapLcl(ReduceSeq(sumUp, 0.0f))) o Split(2)
        ) o Split(64) $ in

      }), firstOutput, firstOutput.length)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      (output, runtime)
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

        Join() o asScalar() o MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(Vectorize(4)(id)))) o Split(1) o
            Iterate(8)( Join() o MapLcl(ReduceSeq(Vectorize(4)(sumUp), Vectorize(4)(0.0f))) o Split(2) ) o
            Join() o toLocal(MapLcl(ReduceSeq(Vectorize(4)(sumUp), Vectorize(4)(0.0f)))) o Split(2)
        ) o asVector(4) o Split(2048) $ in

      }), inputData, inputData.length)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.1)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(64, firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(6)( Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2) ) o
            Join() o toLocal(MapLcl(MapSeq(id))) o Split(1)
        ) o Split(64) $ in

      }), firstOutput, firstOutput.length)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      (output, runtime)
    }

  }

  @Test def NVIDIA_DERIVED() {

    val inputSize = 16777216
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = opencl.executor.Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {

        // the original derived one does not generate correct code ...
        Join() o Join() o MapWrg(
          MapLcl(ReduceSeq(sumUp, 0.0f))
          //toGlobal(MapLcl(Iterate(7)(MapSeq(id) o ReduceSeq(sumUp, 0.0f)) o ReduceSeq(sumUp, 0.0f))) o ReorderStride()
        ) o Split(128) o ReorderStride(2048/128) o Split(2048) $ in

      }), inputData, inputData.length)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(6)( Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2) ) o
            Join() o toLocal(MapLcl(ReduceSeq(sumUp, 0.0f))) o Split(128)
        ) o Split(8192) $ in

      }), firstOutput, firstOutput.length)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      (output, runtime)
    }

  }

  @Test def AMD_DERIVED() {

    val inputSize = 16777216
    val inputData = Array.fill(inputSize)(1.0f)
    //val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o asScalar() o Join() o MapWrg(
          MapLcl(MapSeq(Vectorize(2)(id)) o ReduceSeq(Vectorize(2)(sumUp), Vectorize(2)(0.0f)))// o ReorderStride())
        ) o Split(128) o asVector(2) o Split(4096) $ in

      }), inputData, inputData.length)

      println("output size = " + output.size)
      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(6)( Join() o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(2) ) o
            Join() o toLocal(MapLcl(ReduceSeq(sumUp, 0.0f))) o Split(128)
        ) o Split(8192) $ in

      }), firstOutput, firstOutput.length)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      (output, runtime)
    }

  }

  @Test def INTEL_DERIVED() {

    val inputSize = 16777216
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o asScalar() o Join() o MapWarp(
            MapLane(MapSeq(Vectorize(4)(id)) o ReduceSeq(Vectorize(4)(absAndSumUp), Vectorize(4)(0.0f)))
          ) o Split(1) o asVector(4) o Split(32768)
        ) o Split(32768) $ in

      }), inputData, inputData.length)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      (output, runtime)
    }

    {

      val (output, runtime) = Execute(firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o MapLcl(
            ReduceSeq(sumUp, 0.0f)
          ) o Split(2048)
        ) o Split(2048) $ in

      }), firstOutput, firstOutput.length)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      (output, runtime)
    }

  }

  @Test def INTEL_DERIVED_NO_WARP() {

    val inputSize = 16777216
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {
      val (output, runtime) = Execute(inputData.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o asScalar() o MapLcl(
            MapSeq(Vectorize(4)(id)) o ReduceSeq(Vectorize(4)(absAndSumUp), Vectorize(4)(0.0f))
          ) o asVector(4) o Split(32768)
        ) o Split(32768) $ in

      }), inputData, inputData.length)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      (output, runtime)
    }

    {
      val (output, runtime) = Execute(firstOutput.length)( fun(ArrayType(Float, Var("N")), (in) => {

        Join() o MapWrg(
          Join() o MapLcl(
            ReduceSeq(sumUp, 0.0f)
          ) o Split(2048)
        ) o Split(2048) $ in

      }), firstOutput, firstOutput.length)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

      (output, runtime)
    }

  }

  /*
    @Test def SEQ_TEST() {

      /*
       // this is working fine
      val kernel = Join() o MapWrg(
        Join() o MapLcl(MapSeq(id)) o MapLcl(ReduceSeq(sumUp, 0.0f)) o Split(1024)
      ) o Split(1024) o input
      */

      /* // triggers wrong allocation ...
      val kernel = Join() o MapWrg(
        Join() o MapLcl(MapSeq(id)) o Split(1024)
      ) o Split(1024) o input
      */


      // triggers generation of wrong code (the indices are broken)
      val kernel = Join() o MapWrg(
        Join() o MapLcl(MapSeq(id) o ReduceSeq(sumUp, 0.0f)) o Split(1024)
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

        assertEquals(inputData.reduce(_ + _), output.reduce(_ + _), 0.0)

        (output, runtime)
      }

    }
    */

}
