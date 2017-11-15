package opencl.generator

import ir.ArrayTypeWSWC
import ir.ast.{Join, Split, Value, Zip, fun}
import lift.arithmetic.ArithExpr
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir.pattern._
import opencl.ir.{Float, _}
import org.junit.Test

/**
  * Created by federico on 15/11/17.
  */
object TestScanPar extends  TestWithExecutor {
  def partial_scan(N:Int, SPLIT_FACTOR:Int) = fun(ArrayTypeWSWC(Float, N),
    input => MapWrg(
      MapLcl(ScanSeq(add, 0.0f)) o Split(N/SPLIT_FACTOR)
    ) o Split(SPLIT_FACTOR) $ input
  )

  def add_all(N:Int) = fun(ArrayTypeWSWC(Float, N), Float, (arr, x) => MapLcl(fun(Float, e => add.apply(e, x))) $ arr)

  def join_phase(N:Int, M:Int) = fun(ArrayTypeWSWC(Float, N), ArrayTypeWSWC(Float, M),
    (partials, updates) => MapSeq(toGlobal(id)) o Join() o MapWrg(add_all(N/M)) $ Zip(Split(N/M) $ partials, updates)
  )
}

class TestScanPar {
  @Test
  def testWithCPUPass() = {
    val N = 1024 * 4
    val SPLIT_FACTOR = 64

    val input = Array.fill(N)(1.0f)
    val (dOutput, _) = Execute(128)[Array[Float]](TestScanPar.partial_scan(N, SPLIT_FACTOR), input)

    val localUpdatesScan = dOutput.grouped(N/SPLIT_FACTOR).map(_.last).scanLeft(0.0f)(_ + _).take(SPLIT_FACTOR).toArray

    val (finalOutput, _) = Execute(128)[Array[Float]](TestScanPar.join_phase(N, SPLIT_FACTOR), dOutput, localUpdatesScan)

    finalOutput.foreach(println)
  }
}
