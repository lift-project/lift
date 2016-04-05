package opencl.generator

import apart.arithmetic.Var
import ir.ArrayType
import ir.ast.{Group}
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
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
  @Test def groupLeftCurrentRight(): Unit = {
    val data = Array.tabulate(5)(_*1.0f)
    val gold = Array(0.0f, 1.0f, 2.0f, 1.0f, 2.0f, 3.0f, 2.0f, 3.0f, 4.0f)
    val indices = Array(-1, 0 , 1)

    val groupFun = fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(MapSeq(id)) o Group(indices) $ domain
    )

    val (output: Array[Float],runtime) = Execute(data.length, 10)(groupFun, data)

    println("runtime = " + runtime)
    println(output.mkString(", "))
    assertArrayEquals(gold, output, 0.00001f)
  }

  // TODO (@Bastian) fix me
  @Test def groupCurrentWithTwoRightNeighbours(): Unit = {
    val data = Array.tabulate(5)(_*1.0f)
    val gold = Array(0.0f, 1.0f, 2.0f, 1.0f, 2.0f, 3.0f, 2.0f, 3.0f, 4.0f)
    val indices = Array(0, 1, 2)

    val groupFun = fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(MapSeq(id)) o Group(indices) $ domain
    )

    val (output: Array[Float],runtime) = Execute(data.length, 10)(groupFun, data)

    println("runtime = " + runtime)
    println(output.mkString(", "))
    assertArrayEquals(gold, output, 0.00001f)
  }

  @Test def groupCurrentWithTwoLeftNeighbours(): Unit = {
    val data = Array.tabulate(5)(_*1.0f)
    val gold = Array(0.0f, 1.0f, 2.0f, 1.0f, 2.0f, 3.0f, 2.0f, 3.0f, 4.0f)
    val indices = Array(-2, -1 ,0)

    val groupFun = fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(MapSeq(id)) o Group(indices) $ domain
    )

    val (output: Array[Float],runtime) = Execute(data.length, 10)(groupFun, data)

    println("runtime = " + runtime)
    println(output.mkString(", "))
    assertArrayEquals(gold, output, 0.00001f)
  }

  @Test def groupIdentity(): Unit = {
    val data = Array.tabulate(5)(_*1.0f)
    val indices = Array(0)

    val groupFun = fun(
      ArrayType(Float, Var("N")),
      (domain) => MapGlb(MapSeq(id)) o Group(indices) $ domain
    )

    val (output: Array[Float],runtime) = Execute(data.length, 10)(groupFun, data)

    println("runtime = " + runtime)
    println(output.mkString(", "))
    assertArrayEquals(data, output, 0.00001f)
  }
}
