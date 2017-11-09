package opencl.generator.dynamicProgramming

import ir.ArrayTypeWSWC
import ir.ast.{Pad, Slide, UserFun, Value, Zip, fun}
import lift.arithmetic.ArithExpr
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Test

/**
  * Created by federico on 25/10/17.
  */
object EditDistanceSeq extends TestWithExecutor

class EditDistanceSeq {
  @Test
  def testEditMakeSimilarity() = {
    val N = 1024
    val M = 8
    val input1 = Array.fill(N)(1)
    val input2 = Array.fill(M)(1)
    val (dOutput, _) = Execute(128)[Array[Int]](makeSimilarityTable(N, M, defaultAlgo), input1, input2)
  }

  @Test
  def testEditSolveRow() = {
    val N = 1024
    val input1 = Array.fill(N)(1)
    val input2 = Array.fill(N)(1)
    val (dOutput, _) = Execute(128)[Array[Int]](solveRow(N, defaultAlgo), input1, input2)
  }

  @Test
  def testEditSolveTable() = {
    val N = 1024
    val M = 8
    val input1 = Array.fill(M)(Array.fill(N)(1))
    val (dOutput, _) = Execute(128)[Array[Int]](solveTable(N,M, defaultAlgo), input1)
  }

  @Test
  def editDistanceWhole() = {
    val N = 1024
    val M = 8
    val input1 = Array.fill(N)(1)
    val input2 = Array.fill(M)(1)
    val (dOutput, _) = Execute(128)[Array[Int]](editDistance(N, M, defaultAlgo), input1, input2)
  }

  val intID = UserFun("id_int", "x", "return x;", Int, Int)
  case class EditDistanceConstants(subW:Int, insW:Int, delW:Int)
  val defaultAlgo = EditDistanceConstants(1, 2, 3)
  def Table(N:ArithExpr, M:ArithExpr) = ArrayTypeWSWC(ArrayTypeWSWC(Int, N),M)


  def makeSimilarityTable(N:ArithExpr, M:ArithExpr, algo:EditDistanceConstants) = {
    def similarity = UserFun("similarity", Array("x","y"), s"return x == y ? 0 : ${algo.subW};", Seq(Int,Int),Int)
    fun(ArrayTypeWSWC(Int,N), ArrayTypeWSWC(Int,M), (word1, word2) => {
      MapSeq(fun(Int, char2 =>
        MapSeq(fun(Int, char1 => similarity.apply(char1, char2))) $ word1
      )) $ word2
    })
    //similarity(x,y) = x == y ? 0 : c1
    //mkSimilarityTable(word1, word2) = map(\ch2 => map(\ch1 => similarity(ch1,ch2))
  }

  def solveRow(N:ArithExpr, algo:EditDistanceConstants) = {
    def makeYUF = UserFun("makeY",Array("p1","p2", "y"),s"return min(p1 + y, p2+${algo.delW});", Seq(Int, Int,Int), Int)
    def makeY = fun(ArrayTypeWSWC(Int,2), Int, (arr, x) => makeYUF.apply(arr.at(0), arr.at(1), x))

    def minPlus = UserFun("leftToRight", Array("x", "y"), s"return min(x+${algo.insW},y);", Seq(Int,Int),Int)
    fun(ArrayTypeWSWC(Int,N), ArrayTypeWSWC(Int,N),
      (previous, novel) => {
        ScanSeq(minPlus, Integer.MAX_VALUE) o MapSeq(makeY) $ Zip(Slide(2, 1) o Pad(1, 0, Pad.Boundary.Clamp)  $ previous, novel)
      })

    //solveRow(old,new) = Scan((x,y) => min(x + c2,y), +Inf) o map(\((p1, p2), x) => min(p1 + x, p2 + c2)) o zip(slide(old),new)
  }
  def solveTable(N:ArithExpr, M:ArithExpr, algo:EditDistanceConstants) = {
    fun(Table(N,M), table => {
      ScanSeq(solveRow(N, algo), MapSeq(toGlobal(intID)) $ Value(Integer.MAX_VALUE, ArrayTypeWSWC(Int, N))) $ table
    })
    //solveTable(table) = Scan(solveRow, [inf...])
  }

  def editDistance(N:ArithExpr, M:ArithExpr, algo:EditDistanceConstants) = {
    fun(ArrayTypeWSWC(Int, N), ArrayTypeWSWC(Int,M), (word1, word2) =>
      solveTable(N,M,algo) $ makeSimilarityTable(N,M,algo).apply(word1, word2)
    )
    // editDistance(words1, words2) = solve(makeSimilarityTable(word1, word2))
  }
}