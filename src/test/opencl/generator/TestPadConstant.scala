package opencl.generator

import ir.ArrayTypeWSWC
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._

object TestPadConstant extends TestWithExecutor

class TestPadConstant {

  // use letters instead of numbers
  val next: () => Float = { var n = -1f; () => { n = n + 1f; n } }
  val a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p = next()

  @Test def padConstant1D(): Unit = {
    val input = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
    val gold = Array(0.0f,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
    println(gold.mkString(", "))

    val fct = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (domain) => MapGlb(id) o PadConstant(1, 0, 0.0f) $ domain
    )

    println(Compile(fct))

    val (output,runtime) = Execute(input.length, input.length)[Array[Float]](fct, input)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
  }

}
