package opencl.generator

import apart.arithmetic.Var
import ir.ArrayType
import ir.ast._
import opencl.executor.{Executor, Execute}
import opencl.ir.Float
import opencl.ir.pattern.MapGlb
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestEpsilon {
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

class TestEpsilon {
  @Test def saxpy(): Unit = {
    // domain size
    val inputSize = 128
    val N = Var("N")

    // Input variables
    val a: Float = 5.0f
    val xs = Array.fill(inputSize)(util.Random.nextFloat())
    val ys = Array.fill(inputSize)(util.Random.nextFloat())

    // Cross validation
    val gold = (xs.map(_ * a), ys).zipped map (_ + _)

    // user function
    val fct = UserFun("saxpy", Array("a", "x", "y"),
      " return a * x + y; ",
      Seq(Float, Float, Float), Float)

    // Expression
    val f = fun(
      Float,
      ArrayType(Float, N),
      ArrayType(Float, N),
      (a, xs, ys) => MapGlb(
        fun(xy => fct(
          a, Get(xy, 0), Get(xy, 1)
        ))
      ) o MapGlb(Epsilon()) o Epsilon() $ Zip(xs, ys)
    )

    // execute
    val (output: Array[Float], runtime) = Execute(inputSize)(f, a, xs, ys)

    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.001f)
  }
}
