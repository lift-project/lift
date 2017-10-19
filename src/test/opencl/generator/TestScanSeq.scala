package opencl.generator

import ir.ArrayTypeWSWC
import ir.ast.{Join, Split, Value, Zip, fun}
import lift.arithmetic.ArithExpr
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir.pattern._
import opencl.ir.{Float, _}
import org.junit.Test

/**
  * Created by federico on 04/10/17.
  */
object TestScanSeq extends TestWithExecutor

class TestScanSeq {

  @Test
  def testBasic() = {
    val N = 1024
    val expr  =
      fun(ArrayTypeWSWC(Float, N),
        (data) => {
        toGlobal(MapSeq(id)) o ScanSeq(add, 0.0f) $ data
      })

    val input = Array.fill(N)(1.0f)
    val (dOutput, _) = Execute(128)[Array[Float]](expr, input)

    val scalaResult = input.scan(0.0f)((x, y) => x + y).tail
    val zipped = scalaResult.zip(dOutput)
    zipped.foreach(x => assert(x._1 == x._2))
    zipped.foreach(println)
  }

  def testInsideMapAndSplits() = {
    val N = 1024

    val expr  =
     fun(ArrayTypeWSWC(Float, N),
       (data) => {
         toGlobal(MapSeq(id)) o Join() o MapSeq(ScanSeq(add, 0.0f)) o Split(32) $ data
       })

    val input = Array.fill(N)(1.0f)
    val (dOutput, _) = Execute(128)[Array[Float]](expr, input)
  }

  def testSplitJoin() = {
    val N = 1024

    val expr = fun(ArrayTypeWSWC(Float, N),
      data => toGlobal(MapSeq(id)) o ScanSeq(add, 0.0f) o Join() o Split(32) $ data)

    val input = Array.fill(N)(1.0f)
    val (dOutput, _) = Execute(128)[Array[Float]](expr, input)

    dOutput.foreach(x => println(s"$x"))
  }


  @Test
  def testScanAcrossArraysGlobal() = {
    val N = 1024
    val SS = 16

    def joinArray(S:ArithExpr) =
      fun(ArrayTypeWSWC(Float,S), ArrayTypeWSWC(Float,S), (arr1,arr2) => MapSeq(add) $ Zip(arr1, arr2))

    val expr = fun(ArrayTypeWSWC(Float, N),
      data => toGlobal(MapSeq(MapSeq(id))) o
        ScanSeq(
          joinArray(SS),
          toGlobal(MapSeq(id)) $ Value(1.0f, ArrayTypeWSWC(Float, SS))
        ) o Split(SS) $ data
    )

    val input = Array.fill(N)(1.0f)
    val (dOutput, _) = Execute(1024)[Array[Float]](expr, input)

    dOutput.foreach(x => println(s"$x"))
  }

  @Test
  def testScanAcrossArraysPrivate() = {
    val N = 1024
    val SS = 16

    def joinArray(S:ArithExpr) =
      fun(ArrayTypeWSWC(Float,S), ArrayTypeWSWC(Float,S), (arr1,arr2) => MapSeq(add) $ Zip(arr1, arr2))

    val expr = fun(ArrayTypeWSWC(Float, N),
      data => toGlobal(MapSeq(MapSeq(id))) o
        ScanSeq(
          joinArray(SS),
          MapSeq(id) $ Value(1.0f, ArrayTypeWSWC(Float, SS))
        ) o Split(SS) $ data
    )

    val input = Array.fill(N)(1.0f)
    val (dOutput, _) = Execute(1024)[Array[Float]](expr, input)

    dOutput.foreach(x => println(s"$x"))
  }
}
