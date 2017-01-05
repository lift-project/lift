package opencl.generator

import lift.arithmetic.SizeVar
import benchmarks.SumAbsoluteValues
import ir._
import ir.ast._
import opencl.executor._
import opencl.ir._
import org.junit.Assert._
import org.junit._
import opencl.ir.pattern._

object TestReduceWhile {
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

class TestReduceWhile {

	@Test def oneDimensionBoundedReduction() : Unit = {
    val inputSize = Math.pow(2, 14).toInt
    val arr = Array.tabulate(inputSize)((i:Int) => i)
    val gold = arr.takeWhile(_ < 100).sum

    val filter = UserFun("check", Array("acc","v"),
      "return ((v < 100));",
      Seq(Int,Int), Int
    )

    val N = SizeVar("N")

    val kernel = fun(
      ArrayType(Int, N),
      (array) => {
        toGlobal(MapSeq(idI)) o ReduceWhileSeq(
        	int_add,
        	filter, 0) $ array
      }
    )

    val code = Compile(kernel)

    println("Code: ")
    println(code)

    val (output: Array[Int], runtime) = Execute(1,1)(kernel, arr)
    println("Time: " + runtime)
    println("input[0:10]:  " + arr.take(10).toList.toString())
    println("output(0): " + output(0))
    println(s"gold: $gold")
    assert(output(0) == gold)
  }

  @Test def twoDimensionBoundedReduction() : Unit = {
    val inputSize = Math.pow(2, 14).toInt
    val splitSize = Math.pow(2, 5).toInt
    val arr = Array.tabulate(inputSize)((i:Int) => i).
      grouped(splitSize).
      toArray.
      transpose.
      flatten
    val gold = arr.grouped(splitSize).map(_.takeWhile(_ < 512).sum).toArray

    val filter = UserFun("check", Array("acc","v"),
      "return ((v < 512));",
      Seq(Int,Int), Int
    )

    val N = SizeVar("N")

    val kernel = fun(
      ArrayType(Int, N),
      (array) => {
        Join() o MapSeq(
          toGlobal(MapSeq(idI)) o 
          ReduceWhileSeq(int_add, filter, 0)
        ) o Split(splitSize) $ array
      }
    )

    val code = Compile(kernel)

    println("Code: ")
    println(code)

    val (output: Array[Int], runtime) = Execute(1,1)(kernel, arr)
    println("Time: " + runtime)
    println("input[0:10]:  " + arr.take(10).toList.toString())
    println("output[0:10]: " + output.take(10).toList.toString())
    println("gold[0:10]: " + gold.take(10).toList.toString())
    assertArrayEquals(output,gold)
  }
}
