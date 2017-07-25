package opencl.generator

import benchmarks.DbSelect
import opencl.executor.{Execute, Executor}
import org.junit.Assert.assertEquals
import org.junit.{AfterClass, BeforeClass, Test}

object TestDbSelect {
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

class TestDbSelect {
  private def gold(colA: Array[Int], colB: Array[Int], colC: Array[Int]): Vector[(Int, Int, Int)] = {
    val newC = colC.map(n => if(n == 1) 1 else 0)
    (newC, colA, colB).zipped.toVector
  }

  @Test def testNaive(): Unit = {
    val inputSize = 1024
    val colA = Array.fill(inputSize)(util.Random.nextInt(5))
    val colB = Array.fill(inputSize)(util.Random.nextInt(5))
    val colC = Array.fill(inputSize)(util.Random.nextInt(5))

    val (output, runtime) = Execute(inputSize)[Vector[(Int, Int, Int)]](DbSelect.naive, colA, colB, colC)
   
    println(s"Runtime: $runtime")
    
    assertEquals(gold(colA, colB, colC), output)
  }
  
  @Test def testDivideNConquer(): Unit = {
    val inputSize = 128*128
    val colA = Array.fill(inputSize)(util.Random.nextInt(5))
    val colB = Array.fill(inputSize)(util.Random.nextInt(5))
    val colC = Array.fill(inputSize)(util.Random.nextInt(5))
  
    val (output, runtime) = Execute(inputSize)[Vector[(Int, Int, Int)]](DbSelect.divideNConquer, colA, colB, colC)
  
    println(s"Runtime: $runtime")
  
    assertEquals(gold(colA, colB, colC), output)
  }
}