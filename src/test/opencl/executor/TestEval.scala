package opencl.executor

import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}


object TestEval {
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

class TestEval {

  @Test def VECTOR_ADD_SIMPLE_STRING(): Unit = {
    val inputSize = 1024
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val gold = (leftInputData, rightInputData).zipped.map(_+_)

    val addFun = """
      import apart.arithmetic._
      val add = UserFun("add", Array("x", "y"), "{ return x+y; }", Seq(Float, Float), Float)

      val N = SizeVar("N")

      fun(
        ArrayType(Float, N),
        ArrayType(Float, N),
        (left, right) =>
          Join() o MapWrg(
            Join() o MapLcl(MapSeq(add)) o Split(4)
          ) o Split(1024) $ Zip(left, right)
      )
    """

    val (output: Array[Float], runtime) = Execute(inputSize)(addFun, leftInputData, rightInputData)

    (gold, output).zipped.map(assertEquals(_,_,0.0))

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

  }

  @Test def Accelerate1(): Unit = {
    val inputSize = 1024
    val inputData = Array.fill(inputSize)(util.Random.nextInt())

    val gold = inputData.map(_+1)

    val code = """
val gensym0 = UserFun("gensym0", "x0_0", "{int gensym3 = x0_0 + (int) 1; return gensym3;}", Int, Int)

val gensym1 = fun(ArrayType(Int, SizeVar("N")), gensym2 => MapGlb(gensym0) $ gensym2)

gensym1
    """

    val (output: Array[Int], _) = Execute(inputSize)(code, inputData)

    (gold, output).zipped.map(assertEquals(_,_))

    println("output(0) = " + output(0))
    println("gold(0) = " + gold(0))
  }

}
