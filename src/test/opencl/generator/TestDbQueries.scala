package opencl.generator

import ir.{ArrayType, TupleType}
import ir.ast.{Join, Tuple, UserFun, fun}
import lift.arithmetic.SizeVar
import opencl.ir._
import opencl.executor.{Execute, Executor}
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import org.junit.{AfterClass, BeforeClass, Test}
import org.junit.Assert.{assertArrayEquals, assertEquals}

object TestDbQueries {
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

class TestDbQueries {
  @Test def combine(): Unit = {
    val size = 512
    val left = Array.fill(size)(util.Random.nextInt(5))
    val right = Array.fill(size)(util.Random.nextInt(5))
    
    val tuple_id = UserFun(
      "tuple_id", "x", "return x;",
      TupleType(Int, Int), TupleType(Int, Int)
    )
    
    val N = SizeVar("N")
    
    // [Int],,n,, -> [Int],,n,, -> [(Int, Int)],,n^2,,
    val combine = fun(
      ArrayType(Int, N), ArrayType(Int, N),
      (left, right) => {
        Join() o MapGlb(
          toGlobal(MapSeq(tuple_id)) o fun(lRow => {
            MapSeq(fun(rRow => Tuple(lRow, rRow))) $ right
          })
        ) $ left
      }
    );
    
    // Note: `output` is flattened
    val (output: Array[Int], runtime) = Execute(size)(combine, left, right)
    val gold = (for {x <- left; y <- right} yield Array(x, y)).flatten
    
    println(s"Runtime: $runtime")
    
    assertEquals(output.length, size * size * 2)
    assertArrayEquals(output, gold)
  }
  
  @Test def aggregation(): Unit = {
    val size = 1024
    val table = Array.fill(size)(util.Random.nextInt(4096))
    
    val N = SizeVar("N")
    
    val max = UserFun(
      "maximum", Array("x", "y"), "return max(x, y);",
      Seq(Int, Int), Int
    )
    
    // SELECT MAX(x) FROM xs;
    val aggregateMax = fun(
      ArrayType(Int, N),
      table => {
        toGlobal(MapSeq(idI)) o ReduceSeq(max, 0) $ table
      }
    )
    
    val (output: Array[Int], runtime) = Execute(size)(aggregateMax, table)
    
    println(s"Runtime: $runtime")
    
    assertEquals(table.max, output.head)
  }
}