package opencl.generator

import ir.ArrayTypeWSWC
import ir.ast.fun
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir.pattern._
import opencl.ir.{Float, _}
import org.junit.Test

/**
  * Created by federico on 04/10/17.
  */
object TestScanSeq extends TestWithExecutor{
  @Test
  def test_scan = {
    val N = 1024
    val expr  = fun(ArrayTypeWSWC(Float, N),
      data => {
        toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) $ data
      })

    val input = Array.fill(N)(1.0f)
    val (dOutput, _) = Execute(128)[Vector[Float]](expr, input)

    println(dOutput)
  }
}
