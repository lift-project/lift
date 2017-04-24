package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._

object TestDynMap {
  @BeforeClass def TestDynMap(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
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

    val N = SizeVar("N")
    val kernel = fun(
      ArrayTypeWSWC(Int, N),
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

    val N = SizeVar("N")
    val kernel = fun(
      ArrayTypeWSWC(Int, N),
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
    // this currently only seems to work on GPUs...
    Assume.assumeTrue("Only valid on GPUs", Executor.getDeviceType == "GPU")

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

    val N = SizeVar("N")
    val kernel = fun(
      ArrayTypeWSWC(Int, N),
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

    val N = SizeVar("N")
    val kernel = fun(
      ArrayTypeWSWC(Int, N),
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
