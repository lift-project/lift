package opencl.generator

import ir.UserFunDef._
import ir._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestBarrier {
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

class TestBarrier {
  @Test def basicBarrier(): Unit ={
    val inputSize = 1024
    val input = Array.tabulate(inputSize)(_.toFloat)

    val f = fun(
      ArrayType(Float, new Var("N")),
      input => Join() o MapWrg(Barrier() o MapLcl(id)) o Split(128) $ input
    )

    Barriers.mark(f)

    val (output, _) = Execute(inputSize)(f, input, inputSize)

    assertArrayEquals(input, output, 0.0f)
  }
}
