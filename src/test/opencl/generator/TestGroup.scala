package opencl.generator

import apart.arithmetic.Var
import ir.ArrayType
import ir.ast.{Pad, Zip, Group}
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import spl.Stencil
import ir.ast.fun


object TestGroup {
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

/**
  * Created by Bastian Hagedorn on 04.04.16.
  */
class TestGroup {
  // TODO Transform Group primitive to accept negative relative indices.
  @Test def GROUP_1D(): Unit = {
    val data = Array.tabulate(5)(_*1.0f)
    val gold = Array(0.0f, 1.0f, 2.0f, 1.0f, 2.0f, 3.0f, 2.0f, 3.0f, 4.0f)
    val indices = Array(-1 , 0 , 1)
    //val indices = Array(1,2)


    val groupFun = fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(MapSeq(id)) o Group(indices) $ domain
    )

    //val (output: Array[Float], runtime) = Execute(data.length)(groupFun, data, weights)
    val (output: Array[Float],runtime) = Execute(data.length, 10)(groupFun, data)
    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println(output.mkString(", "))

    assertArrayEquals(gold, output, 0.00001f)
  }
}
