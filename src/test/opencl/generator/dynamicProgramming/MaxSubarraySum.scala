package opencl.generator.dynamicProgramming

import ir.ast.{Get, UserFun, fun}
import ir.{ArrayTypeWSWC, TupleType}
import lift.arithmetic.ArithExpr
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import org.junit.Test
/**
  * Created by federico on 26/10/17.
  */
object MaxSubarraySum extends TestWithExecutor

class MaxSubarraySum {
  @Test
  def testMaxSubarraySum() = {
    val N = 1024
    val input1 = Array.tabulate(N)(i => if(i > 2 && i < 6) 1 else -1)
    val (dOutput, _) = Execute(128)[Array[Int]](maxSubarraySum(N), input1)
    dOutput.foreach(println)
  }

  val intID = UserFun("id_int", "x", "return x;", Int, Int)

  def maxSubarraySum(N:ArithExpr) = {
    val stepUserFun = UserFun("tallySumMax", Array("state", "x"), "{ Tuple t; t._0 = max(state._0 + x, x); t._1 = max(state._0 + x, state._1); return t; }",
      Seq(TupleType(Int,Int), Int), TupleType(Int,Int))
    fun(ArrayTypeWSWC(Int, N), input =>
      MapSeq(toGlobal(intID o Get(1))) o ReduceSeq(stepUserFun, (0, 0)) $ input
    )
  }
}