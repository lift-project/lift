package opencl.generator

import ir.ArrayType
import ir.ast.{Join, Split, UserFun, fun}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir.pattern.{InsertionSortSeq, MapWrg, toGlobal}
import org.junit.{AfterClass, BeforeClass, Test}
import org.junit.Assert.assertArrayEquals

object TestSort {
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

class TestSort {
  private val int = opencl.ir.Int
  private val int_compare = UserFun(
    "int_compare", Array("x", "y"), "return x < y;", Seq(int, int), int
  )
  
  @Test def sortInt1D(): Unit = {
    val size = 128
    val input = Array.fill(size)(util.Random.nextInt(1024))
    val N = SizeVar("N")
    
    val kernel = fun(
      ArrayType(int, N),
      arr => InsertionSortSeq(int_compare) $ arr
    )
    
    val (output: Array[Int], runtime) = Execute(size)(kernel, input)
    println(s"Runtime: $runtime")
    
    assertArrayEquals(input.sortWith(_ < _), output)
  }
  
  @Test def sortInt2D(): Unit = {
    val size = 4096
    val input = Array.fill(size)(util.Random.nextInt(1024))
    val N = SizeVar("N")
    
    val kernel = fun(
      ArrayType(int, N),
      arr =>
        Join()
          o MapWrg(toGlobal(InsertionSortSeq(int_compare)))
          o Split(32) $ arr
    )
   
    val gold = input.grouped(32).flatMap(_.sortWith(_ < _)).toArray
    val (output: Array[Int], runtime) = Execute(size)(kernel, input)
    println(s"Runtime: $runtime")
    
    assertArrayEquals(gold, output)
  }
}
