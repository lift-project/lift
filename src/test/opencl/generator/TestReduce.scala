package opencl.generator

import apart.arithmetic.Var
import benchmarks.SumAbsoluteValues
import ir._
import ir.ast._
import opencl.executor._
import opencl.ir._
import org.junit.Assert._
import org.junit._
import opencl.ir.pattern._

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
        Join() o  MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, init)) o Split(4)
      ) o Split(128) $ in
    })

    val (output: Array[Float], runtime) = Execute(inputData.length)( l, inputData, 0.0f)

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
        Join() o MapSeq(MapSeq(MapSeq(id)) o ReduceSeq(fun((x, y) => {
          MapSeq(add) $ Zip(x, y)
        }), MapSeq(id) $ init)) o Split(M) $ in
      })

    val (output: Array[Float], runtime) = Execute(1, 1)(l, matrix, inputData)

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
              toGlobal(MapSeq(id)) $ Value(0.0f, ArrayType(Float, N)))
        ) o Split(M) $ in
      })

    val (output: Array[Float], runtime) = Execute(1, 1)(l, matrix)

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
          Join() o  MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, id(init))) o Split(4)
        ) o Split(128) $ in
      })

    val (output: Array[Float], runtime) = Execute(inputData.length)(l, inputData, 0.0f)

    assertEquals(inputData.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  /** @see Issue #21 */
  @Ignore
  @Test def reduceIterate(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayType(Float, Var("N")),
      Float,
      (in, init) => {
        Join() o MapWrg(
          Join() o  MapLcl(toGlobal(MapSeq(id)) o Iterate(1,ReduceSeq(add, id(init)))) o Split(4)
        ) o Split(128) $ in
      })

    val (output: Array[Float], runtime) = Execute(inputData.length)(l, inputData, 0.0f)

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
          Join() o  MapLcl(toGlobal(MapSeq(id)) o
                                      ReduceSeq(add, id(0.0f))) o Split(4)
        ) o Split(128) $ in
      })

    val (output: Array[Float], runtime) = Execute(inputData.length)(l, inputData)

    assertEquals(inputData.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def reduceValueToGlobal(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(Array(util.Random.nextInt(5).toFloat))

    val l = fun (ArrayType(ArrayType(Float, 1), Var("N")),
      in => {
        Join() o MapWrg(
          Join() o  MapLcl(ReduceSeq(fun((acc, x) => {
            MapSeq(add) $ Zip(acc, x)
          }), toGlobal(MapSeq(id)) $ Value(0.0f, ArrayType(Float, 1)))) o Split(4)
        ) o Split(128) $ in
      })

    val (output: Array[Float], runtime) = Execute(inputData.length)(l, inputData)

    assertEquals(inputData.flatten.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def SIMPLE_REDUCE_FIRST() {

    val inputSize = 4194304
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun (ArrayType(Float, Var("N")), (in) => {
      Join() o MapWrg(
        Join() o  MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2048)
      ) o Split(262144) $ in
    } )

    val (output: Array[Float], runtime) = Execute(inputData.length)( l, inputData )

    assertEquals(inputData.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }


  @Test def SIMPLE_REDUCE_SECOND() {

    val inputSize = 4194304
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (output: Array[Float], runtime) = Execute(inputData.length)(
      fun(ArrayType(Float, Var("N")), (in) => {
        Join() o Join() o  MapWrg(
           MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f))
        ) o Split(128) o Split(2048) $ in
      }), inputData)

    assertEquals(inputData.sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def NVIDIA_A() {

    // for input size of 16777216 the first kernel has to be executed 3 times and the second
    // kernel once to perform a full reduction

    val inputSize = 1024
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (firstOutput, _) = {
      val (output: Array[Float], runtime) = Execute(inputData.length)(
        fun(ArrayType(Float, Var("N")), (in) => {
          Join() o MapWrg(
            Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
              Iterate(7)(
                Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)
              ) o Join() o  toLocal(MapLcl(MapSeq(id))) o Split(1)
          ) o Split(128) $ in
        }), inputData)

      assertEquals(inputData.sum, output.sum, 0.0)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output: Array[Float], runtime) = Execute(8, firstOutput.length)(
        fun(ArrayType(Float, Var("N")), (in) => {
          Join() o MapWrg(
            Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
              Iterate(3)(
                Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)
              ) o Join() o  toLocal(MapLcl(MapSeq(id))) o Split(1)
          ) o Split(8) $ in
        }), firstOutput)

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

    val (output: Array[Float], runtime) = Execute(inputData.length)(
      fun(ArrayType(Float, Var("N")), (in) => {
        Join() o MapWrg(
          Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
          Iterate(7)(
            Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)
          ) o Join() o  toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o
          Split(2)
        ) o Split(256) $ in
      }), inputData)

    assertEquals(inputData.sum, output.sum, 0.0)

    println("outputArray(0) = " + output(0))
    println("Runtime = " + runtime)
  }

  @Test def NVIDIA_C() {
    Assume.assumeTrue("Only valid on NVIDIA", Executor.getPlatformName == "NVIDIA CUDA")

    val inputSize = 16777216
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(2).toFloat)

    val (firstOutput, _) = {
      val (output: Array[Float], runtime) = Execute(128, inputData.length, (true, true))(
        fun(ArrayType(Float, Var("N")), (in) => {

          Join() o MapWrg(
            Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Join() o  MapWarp(
              Iterate(5)( Join() o MapLane(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2) )
            ) o Split(32) o
            Iterate(2)(
              Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)
            ) o Join() o  toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o
            Split(2048) o ReorderStride(262144 / 2048)
          ) o Split(262144) $ in

        }), inputData)

      assertEquals("Note that this benchmark is only valid on device with a warp_size of 32!",
                   inputData.sum, output.sum, 0.1)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output: Array[Float], runtime) = Execute(64, firstOutput.length)(
        fun(ArrayType(Float, Var("N")), (in) => {

          Join() o MapWrg(
            Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(5)(
              Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)
            ) o Join() o  toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o
            Split(2)
          ) o Split(64) $ in

        }), firstOutput)


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
        (in) => {
          Join() o MapWrg(
            Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(7)(
              Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)
            ) o Join() o  toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o
            Split(2048) o ReorderStride(262144 / 2048)
          ) o Split(262144) $ in
        })

      val (output: Array[Float], runtime) = Execute(inputData.length)(f, inputData)

      assertEquals(inputData.sum, output.sum, 0.0)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output: Array[Float], runtime) = Execute(64, firstOutput.length)(
        fun(ArrayType(Float, Var("N")), (in) => {

          Join() o MapWrg(
            Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate(5)(
              Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)
            ) o Join() o  toLocal(MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o
            Split(2)
          ) o Split(64) $ in

        }), firstOutput)

      assertEquals(inputData.sum, output.sum, 0.0)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

    }

  }

  @Test def AMD_A() {
    // for an input size of 16777216 the first kernel has to be executed twice and the second
    // kernel once to perform a full reduction

    val inputSize = 32768
    //val inputData = Array.fill(inputSize)(1.0f)
    val inputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (firstOutput, _) = {

      val (output: Array[Float], runtime) = Execute(inputData.length)(
        fun(ArrayType(Float, Var("N")), (in) => {

          Join() o MapWrg( asScalar() o
            Join() o  toGlobal(MapLcl(MapSeq(id.vectorize(4)))) o Split(1) o
            Iterate(8)(
              Join() o  MapLcl(toLocal(MapSeq(id.vectorize(4))) o
                                          ReduceSeq(add.vectorize(4), Value(0.0f).vectorize(4))) o
              Split(2)
            ) o Join() o  toLocal(MapLcl(toLocal(MapSeq(id.vectorize(4))) o
                                                    ReduceSeq(add.vectorize(4), Value(0.0f).vectorize(4)))) o
            Split(2) o asVector(4)
          ) o Split(2048) $ in

        }), inputData)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.1)

      (output, runtime)
    }

    {
      val (output: Array[Float], runtime) = Execute(64, firstOutput.length)(
        fun(ArrayType(Float, Var("N")), (in) => {

          Join() o MapWrg(
            Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
              Iterate(6)(
                Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)
              ) o Join() o  toLocal(MapLcl(toLocal(MapSeq(id)) o MapSeq(id))) o Split(1)
          ) o Split(64) $ in

        }), firstOutput)

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
      val (output: Array[Float], runtime) =
        Execute(inputData.length)(SumAbsoluteValues.nvidiaDerived1, inputData)

      assertEquals(inputData.sum, output.sum, 0.0)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      (output, runtime)
    }

    {
      val (output: Array[Float], runtime) =
        Execute(firstOutput.length)(SumAbsoluteValues.amdNvidiaDerived2, firstOutput)

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
      val (output: Array[Float], runtime) =
        Execute(inputData.length)(SumAbsoluteValues.amdDerived1, inputData)

      println("output size = " + output.length)
      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.0)

      (output, runtime)
    }

    {
      val (output: Array[Float], runtime) =
        Execute(firstOutput.length)(SumAbsoluteValues.amdNvidiaDerived2, firstOutput)

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
      val (output: Array[Float], runtime) = Execute(128, inputData.length, (true, true))(
        fun(ArrayType(Float, Var("N")), (in) => {

          Join() o MapWrg(
            asScalar() o Join() o Join() o MapWarp(
              MapLane( toGlobal(MapSeq(id.vectorize(4))) o
                       ReduceSeq(absAndSumUp.vectorize(4), Value(0.0f).vectorize(4)) )
            ) o Split(1) o Split(8192) o asVector(4)
          ) o Split(32768) $ in

        }), inputData)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.0)

      (output, runtime)
    }

    {

      val (output: Array[Float], runtime) =
        Execute(firstOutput.length)(SumAbsoluteValues.intelDerived2, firstOutput)

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
      val (output: Array[Float], runtime) =
        Execute(inputData.length)(SumAbsoluteValues.intelDerivedNoWarp1, inputData)

      println("first output(0) = " + output(0))
      println("first runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.0)

      (output, runtime)
    }

    {
      val (output: Array[Float], runtime) =
        Execute(firstOutput.length)(SumAbsoluteValues.intelDerived2, firstOutput)

      println("second output(0) = " + output(0))
      println("second runtime = " + runtime)

      assertEquals(inputData.sum, output.sum, 0.0)

    }

  }

  /**
   * This generates an out-of-bound memory access in iterate.
   */
  @Test def issue_30(): Unit = {
    val inputSize = 512
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inputArray.sum

    val f = fun(
      ArrayType(Float, Var("N")),
      (input) => {
        Join() o MapWrg(
          Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate((scala.math.log(inputSize)/scala.math.log(2)).toInt)(
              Join() o  MapLcl(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)
            ) o Join() o toLocal(MapLcl(toLocal(MapSeq(id)))) o Split(1)
        ) o Split(inputSize) $ input
      })

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputArray)

    assertEquals(gold, output(0), 0.1)
    assertEquals(1, output.length)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def issue_31(): Unit = {
    val inputSize = 512
    val inputArray = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val gold = inputArray.sum

    val f = fun(
      ArrayType(Float, Var("N")),
      (input) => {
        Join() o MapWrg(
          Join() o  toGlobal(MapLcl(MapSeq(id))) o Split(1) o
            Iterate((scala.math.log(inputSize)/scala.math.log(2)).toInt)(
              Join() o  MapLcl(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Split(2)
            ) o Join() o MapLcl(toGlobal(MapSeq(id))) o Split(1)
        ) o Split(inputSize) $ input
      })

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputArray)

    assertEquals(gold, output(0), 0.1)
    assertEquals(1, output.length)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def SPLIT_REDUCE() : Unit = {
    val inputSize = Math.pow(2, 12).toInt
    val search_arr = Array.tabulate(inputSize)((i:Int) => i.toFloat)
    val gold = search_arr.sum
    val N = Var("N")

    val reduce_kernel = fun(
      ArrayType(Float, N),
      (array) => {
        toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join() o MapSeq(
          fun((subarr) =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) $ subarr
          )
        ) o Split(8) $ array
      }
    )

    val (output:Array[Float], _) = Execute(1,1, (true, true))(reduce_kernel, search_arr)
    assertEquals(gold, output(0), 0.0f)
  }

  /**
   * This test currently fails as the generated unroll does not reset the accumulator properly.
   */
  @Ignore @Test def SPLIT_REDUCE_UNROLLED() : Unit = {
    val inputSize = 128
    val search_arr = Array.tabulate(inputSize)((i:Int) => i)
    val gold = search_arr.sum
    val int_add = UserFun("int_add", Array("a", "b"), "return a+b;", Array(Int, Int), Int)

    val reduce_kernel = fun(
      ArrayType(Int, inputSize),
      (array) => {
        toGlobal(MapSeq(idI)) o ReduceSeq(int_add, 0) o Join() o MapSeq(
          fun((subarr) =>
            ReduceSeq(int_add, 0) $ subarr
          )
        ) o Split(8) $ array
      }
    )
    val (output:Array[Int], _) = Execute(1,1, (true, true))(reduce_kernel, search_arr)

    assertEquals(gold, output(0))
  }
}
