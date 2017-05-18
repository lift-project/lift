package opencl.generator

import ir.ArrayType
import ir.ast.{Join, Split, UserFun, fun}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
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
  
  @Test def sortInt(): Unit = {
    val size = 128
    val input = Array.fill(size)(util.Random.nextInt(32))
    val N = SizeVar("N")
    
    val kernel = fun(
      ArrayType(int, N),
      arr => InsertionSortSeq(int_compare) $ arr
    )
    
    val (output: Array[Int], runtime) = Execute(size)(kernel, input)
    println(s"Runtime: $runtime")
    
    println(output.mkString(", "))
    assertArrayEquals(input.sortWith(_ < _), output)
  }
  
  @Test def mapSort(): Unit = {
    val size = 1024
    val input = Array.fill(size)(util.Random.nextInt())
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
  
  @Test def sortArrays(): Unit = {
    val size = 128
    val input = Array.fill(size)(util.Random.nextInt(64))
    val N = SizeVar("N")
    
    val kernel = fun(
      ArrayType(int, N),
      arr =>
        Join() o InsertionSortSeq(
          fun((l, r) =>
            int_compare.apply(
              fun(x => x.at(0)) o ReduceSeq(add(int), 0) $ l,
              fun(x => x.at(0)) o ReduceSeq(add(int), 0) $ r
            )
          )
        ) o Split(8) $ arr
    )
    
    val gold = input.grouped(8).toArray.sortWith(_.sum < _.sum).flatten
    val (output: Array[Int], runtime) = Execute(size)(kernel, input)
    println(s"Runtime: $runtime")
  
    assertArrayEquals(gold, output)
  }
}
