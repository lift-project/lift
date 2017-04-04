package opencl.generator

import ir.ArrayType
import ir.ast.{UserFun, fun}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir.pattern.FilterSeq
import org.junit.Assert.assertArrayEquals
import org.junit.{AfterClass, BeforeClass, Test}

object TestFilterSeq {
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

class TestFilterSeq {
  @Test def filterLt(): Unit = {
    val size = 128
    val input = Array.fill(size)(util.Random.nextInt(10))
    val N = SizeVar("N")
    
    val lt5 = UserFun("lt5", "x", "return x < 5;", opencl.ir.Int, opencl.ir.Int)
  
    val expr = fun(
      ArrayType(opencl.ir.Int, N),
      l => FilterSeq(lt5) $ l
    )
    
    val (output: Array[Int], _) = Execute(size)(expr, input)
    val gold = input.filter(_ < 5)
    
    assertArrayEquals(gold, output.slice(0, gold.length))
  }
}
