package junit.opencl.generator

import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

import opencl.executor._


object TestEval {
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

class TestEval {

  @Test def VECTOR_ADD_SIMPLE_STRING() {
    val inputSize = 1024
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val gold = (leftInputData, rightInputData).zipped.map(_+_)

    val addFun = """
      val add = UserFunDef("add", Array("x", "y"), "{ return x+y; }", TupleType(Float, Float), Float)

      val N = Var("N")

      fun(
        ArrayType(Float, N),
        ArrayType(Float, N),
        (left, right) =>
          Join() o MapWrg(
            Join() o MapLcl(MapSeq(add)) o Split(4)
          ) o Split(1024) o Zip(left, right)
      )
    """

    val (output, runtime) = Execute(inputSize)(addFun, leftInputData, rightInputData, leftInputData.size)

    (gold, output).zipped.map(assertEquals(_,_,0.0))

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

  }

}
