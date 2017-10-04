package opencl.generator

import ir.{ArrayType, ArrayTypeWSWC}
import ir.ast.{CheckedArrayAccess, Head, Join, Split, fun}
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir.pattern._
import opencl.ir.{Float, _}
import org.junit.Test

/**
  * Created by federico on 04/10/17.
  */
object TestScanSeq extends TestWithExecutor

class TestScanSeq {

  //@Test
  def testBasic = {
    val N = 1024
    val expr  =
      fun(ArrayTypeWSWC(Float, N),
        (data) => {
        toGlobal(MapSeq(id)) o ScanSeq(add, 0.0f) $ data
      })

    val input = Array.fill(N)(1.0f)
    val (dOutput, _) = Execute(128)[Array[Float]](expr, input)

    dOutput.foreach(x => println(s"$x"))
  }

  @Test
  def testWithJoinsAndSplits = {
    val N = 1024

    val expr  =
     fun(ArrayTypeWSWC(Float, N),
       (data) => {
         toGlobal(MapSeq(id)) o Join() o MapSeq(ScanSeq(add, 0.0f)) o Split(32) $ data
       })

    val input = Array.fill(N)(1.0f)
    val (dOutput, _) = Execute(128)[Array[Float]](expr, input)

    dOutput.foreach(x => println(s"$x"))
  }

  @Test
  def testAcrossArrays = {
    val N = 1024
    val expr = fun(ArrayTypeWSWC(Float, N),
      data => toGlobal(MapSeq(id)) o
        ScanSeq(fun(Float, ArrayType(Float), (accum, array) => {add.apply(accum, (ReduceSeq(add, 0.0f) $ array).at(0))}),0.0f)
        o Split(32) $ data)

    val input = Array.fill(N)(1.0f)
    val (dOutput, _) = Execute(128)[Array[Float]](expr, input)

    dOutput.foreach(x => println(s"$x"))
  }
}
