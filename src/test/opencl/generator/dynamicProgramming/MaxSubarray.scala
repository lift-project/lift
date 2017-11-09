package opencl.generator.dynamicProgramming

import ir.ast.{Get, UserFun, fun}
import ir.{ArrayTypeWSWC, TupleType}
import lift.arithmetic.ArithExpr
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import org.junit.Test

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
/**
  * Created by federico on 26/10/17.
  */
object MaxSubarray extends TestWithExecutor

class MaxSubarray {

  def testMaxSubarraySum() = {
    val N = 1024
    val input1 = Array.tabulate(N)(i => if(i > 2 && i < 6) 1 else -1)
    val (dOutput, _) = Execute(128)[Array[Int]](maxSubarraySum(N), input1)
    dOutput.foreach(println)
  }

  val intID = UserFun("id_int", "x", "return x;", Int, Int)

  def maxSubarraySum(N:ArithExpr) = {
    val stepUserFun = UserFun("tallySumMax", Array("state", "x"), "{ Tuple t; t._0 = state._0 + x; t._1 = max(state._0 + x, state._1 + x); return t; }",
      Seq(TupleType(Int,Int), Int), TupleType(Int,Int))
    fun(ArrayTypeWSWC(Int, N), input =>
      MapSeq(toGlobal(intID o Get(1))) o ReduceSeq(stepUserFun, (0, 0)) $ input
    )
  }

  val one = Integer.MAX_VALUE
  val zero = Integer.MIN_VALUE
  @Test
  def matrixVectorMult = {
    val vector = Seq(2, 7, 8)
    val matrix = Seq(
      Seq(one, zero, zero),
      Seq(zero, one, zero),
      Seq(zero, zero, one)
    )

    println(matrixVectorMultiply(matrix, vector))
  }

  @Test
  def testAdamMagicAlgorithm = {
    val vector = Seq(1, 2, 3)
    val matrix = Seq(
      Seq(zero, zero, 1),
      Seq(zero, 2, zero),
      Seq(3, zero, zero)
    )
    println(adamMagicAlgorithm(vector, matrix))
  }

  @Test
  def testAdamMagicAlgorithm2 = {
    val vector = Seq(1, 2, 3, 4, 5)
    val matrix = Seq(
      Seq(1,zero, zero, zero, zero),
      Seq(zero, 2, zero, 2, zero),
      Seq(zero, zero, 3, zero, zero),
      Seq(4, zero, zero, 4, zero),
      Seq(zero, zero, zero, 5, 5)
    )
    println(adamMagicAlgorithm(vector, matrix))
  }

  @Test
  def testAdamMagicAlgorithm3 = {
    val str = "1 1 2\n2 1 1\n3 1 1\n5 1 1\n9 1 1\n17 1 1\n2 2 3\n3 2 1\n4 2 1\n6 2 1\n10 2 1\n18 2 1\n3 3 5\n4 3 1\n5 3 1\n7 3 1\n11 3 1\n19 3 1\n4 4 7\n5 4 1\n6 4 1\n8 4 1\n12 4 1\n20 4 1\n5 5 11\n6 5 1\n7 5 1\n9 5 1\n13 5 1\n6 6 13\n7 6 1\n8 6 1\n10 6 1\n14 6 1\n7 7 17\n8 7 1\n9 7 1\n11 7 1\n15 7 1\n8 8 19\n9 8 1\n10 8 1\n12 8 1\n16 8 1\n9 9 23\n10 9 1\n11 9 1\n13 9 1\n17 9 1\n10 10 29\n11 10 1\n12 10 1\n14 10 1\n18 10 1\n11 11 31\n12 11 1\n13 11 1\n15 11 1\n19 11 1\n12 12 37\n13 12 1\n14 12 1\n16 12 1\n20 12 1\n13 13 41\n14 13 1\n15 13 1\n17 13 1\n14 14 43\n15 14 1\n16 14 1\n18 14 1\n15 15 47\n16 15 1\n17 15 1\n19 15 1\n16 16 53\n17 16 1\n18 16 1\n20 16 1\n17 17 59\n18 17 1\n19 17 1\n18 18 61\n19 18 1\n20 18 1\n19 19 67\n20 19 1\n20 20 71"
    val lines = str.split('\n')
    val map = (for(line <- lines) yield {
      val entries = line.split(" ")
      val row = entries(0).toInt
      val column = entries(1).toInt
      Seq(((row - 1, column -1), row),((column-1,row-1), column))
    }).flatten.toMap//.filter{case((r,c),v) => r != c}
    val matrix = Seq.tabulate(20, 20)((x,y) => map.getOrElse((x,y), zero))
    val vector = Seq.tabulate(20)(x => x+1)
    println(adamMagicAlgorithm(vector, matrix))
  }


  @tailrec
  private def adamMagicAlgorithm(vector:Seq[Int], matrix:Seq[Seq[Int]]):Seq[Int] = {
    val next = matrixVectorMultiply(matrix, vector)
    if(next == vector) vector else adamMagicAlgorithm(next, matrix)
  }

  def matrixVectorMultiply(matrix:Seq[Seq[Int]], vector:Seq[Int]):Seq[Int] = {
    val height = vector.length
    val width = matrix.length

    for(j <- 0 until height) yield {
      val row = matrix(j)
      println(s"With row $j")
      println(s"For for $row\n and vector $vector")
      val mins = (0 until width).map(i => Math.min(vector(i),row(i)))
      println(s"The mins are $mins\n\n")
      mins.max
    }
  }
}