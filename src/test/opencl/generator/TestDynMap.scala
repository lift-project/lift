package opencl.generator

import apart.arithmetic.Var
import ir._
import ir.ast._
import ir.ast.UserFun._
import opencl.executor._
import opencl.ir._
import opencl.ir.ast._
import org.junit.Assert._
import org.junit.{Ignore, AfterClass, BeforeClass, Test}

import opencl.ir.pattern._

object TestDynMap {
  @BeforeClass def TestDynMap() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}



class TestDynMap {
  @Test def FLAT_MAPS() : Unit = {
    val inputSize = Math.pow(2, 14).toInt
    val splitSize = Math.pow(2, 5).toInt
    val arr = Array.tabulate(inputSize)((i:Int) => i)
    val gold = arr.map((i:Int) => i + i)
    val idIterate = UserFun("idIterate", "x", 
    """
    int y = x;
    for(int i = 0;i<x;i++){
      y = y + 1;
    }
    return y;
    """.stripMargin
    , Int, Int)

    val N = Var("N")
    val kernel = fun(
      ArrayType(Int, N),
      (array) => {
        Join() o MapWrg(
          MapLcl(idIterate)
        ) o Split(splitSize) $ array
      }
    )

    val (output: Array[Int], runtime) = Execute(25,300)(kernel, arr)
    println("Time: " + runtime)
    println("input[0:10]:  " + arr.take(10).toList.toString())
    println("output[0:10]: " + output.take(10).toList.toString())
    println("gold[0:10]:   " + gold.take(10).toList.toString())
    assertArrayEquals(output, gold)
  }

  @Test def ATOM_LOCAL_MAP(): Unit = {
    val inputSize = Math.pow(2, 14).toInt
    val splitSize = Math.pow(2, 5).toInt
    val arr = Array.tabulate(inputSize)((i:Int) => i)
    val gold = arr.map((i:Int) => i + i)
    val idIterate = UserFun("idIterate", "x", 
    """
    int y = x;
    for(int i = 0;i<x;i++){
      y = y + 1;
    }
    return y;
    """.stripMargin
    , Int, Int)

    val N = Var("N")
    val kernel = fun(
      ArrayType(Int, N),
      (array) => {
        Join() o MapWrg(
          MapAtomLcl(idIterate)
        ) o Split(splitSize) $ array
      }
    )

    val (output: Array[Int], runtime) = Execute(25,300)(kernel, arr)
    println("Time: " + runtime)
    println("input[0:10]:  " + arr.take(10).toList.toString())
    println("output[0:10]: " + output.take(10).toList.toString())
    println("gold[0:10]:   " + gold.take(10).toList.toString())
    assertArrayEquals(output, gold)
  }

  @Test def ATOM_WRG_MAP(): Unit = {
    val inputSize = Math.pow(2, 14).toInt
    val splitSize = Math.pow(2, 5).toInt
    val arr = Array.tabulate(inputSize)((i:Int) => i)
    val gold = arr.map((i:Int) => i + i)
    val idIterate = UserFun("idIterate", "x", 
    """
    int y = x;
    for(int i = 0;i<x;i++){
      y = y + 1;
    }
    return y;
    """.stripMargin
    , Int, Int)

    val N = Var("N")
    val kernel = fun(
      ArrayType(Int, N),
      (array) => {
        MapAtomWrg(
          MapLcl(idIterate)
        ) o Split(splitSize) $ array
      }
    )

    val (output: Array[Int], runtime) = Execute(25,300)(kernel, arr)
    println("Time: " + runtime)
    println("input[0:10]:  " + arr.take(10).toList.toString())
    println("output[0:10]: " + output.take(10).toList.toString())
    println("gold[0:10]:   " + gold.take(10).toList.toString())
    assertArrayEquals(output, gold)
  }

  @Test def ATOM_WRG_LCL_MAP() : Unit = {
        val inputSize = Math.pow(2, 14).toInt
    val splitSize = Math.pow(2, 5).toInt
    val arr = Array.tabulate(inputSize)((i:Int) => i)
    val gold = arr.map((i:Int) => i + i)
    val idIterate = UserFun("idIterate", "x", 
    """
    int y = x;
    for(int i = 0;i<x;i++){
      y = y + 1;
    }
    return y;
    """.stripMargin
    , Int, Int)

    val N = Var("N")
    val kernel = fun(
      ArrayType(Int, N),
      (array) => {
        Join() o MapAtomWrg(
          MapAtomLcl(idIterate)
        ) o Split(splitSize) $ array
      }
    )
    
    val (output: Array[Int], runtime) = Execute(25,300)(kernel, arr)
    println("Time: " + runtime)
    println("input[0:10]:  " + arr.take(10).toList.toString())
    println("output[0:10]: " + output.take(10).toList.toString())
    println("gold[0:10]:   " + gold.take(10).toList.toString())
    assertArrayEquals(output, gold)
  }
}
