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

  @Test def padConstant1DLeftOnly(): Unit = {
    val input = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
    val gold = Array(0.0f,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

    val fct = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (domain) => MapGlb(id) o PadConstant(1, 0, 0.0f) $ domain
    )

    println(Compile(fct))

    val (output,runtime) = Execute(input.length, input.length)[Array[Float]](fct, input)
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.0f)

  }

  @Test def padConstant1DRightOnly(): Unit = {
    val input = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
    val gold = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,0.0f)

    val fct = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (domain) => MapGlb(id) o PadConstant(0, 1, 0.0f) $ domain
    )

    val (output,runtime) = Execute(input.length, input.length)[Array[Float]](fct, input)

    assertArrayEquals(gold, output, 0.0f)

  }


  @Test def padConstantBothEnds(): Unit = {
    val input = Array(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
    val gold = Array(0.0f,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,0.0f)

    val fct = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (domain) => MapGlb(id) o PadConstant(1, 1, 0.0f) $ domain
    )

    val (output,runtime) = Execute(input.length, input.length)[Array[Float]](fct, input)

    assertArrayEquals(gold, output, 0.0f)

  }


  @Test def padConstant2D(): Unit = {

    val input2D = Array(Array(1,2,3), Array(4,5,6),Array(7,8,9),Array(10,11,12),Array(13,14,15))
    val gold2D = Array(0,0,0,0,0,0,1,2,3,0, 0,4,5,6,0,0,7,8,9,0,0,10,11,12,0,0,13,14,15,0,0,0,0,0,0)

    val fct2D = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Int,SizeVar("M")), SizeVar("N")),
      (domain) => MapGlb(1)(MapGlb(0) (fun(m => { toGlobal(idI) $ m}))) o PadConstant2D(1, 1, 0) $ domain
    )

    val (output2D,runtime) = Execute(2,2)[Array[Int]](fct2D, input2D)

    assertArrayEquals(gold2D, output2D)
  }


  @Test def padConstant3D(): Unit = {

    val input3D = Array(Array(Array(1,2,3), Array(4,5,6),Array(7,8,9)),
                        Array(Array(10,11,12),Array(13,14,15),Array(16,17,18)),
                        Array(Array(19,20,21),Array(22,23,24),Array(25,26,27)))
    val gold3D = Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                       0,0,0,0,0,0,1,2,3,0, 0,4,5,6,0,0,7,8,9,0,0,0,0,0,0,
                       0,0,0,0,0,0,10,11,12,0,0,13,14,15,0,0,16,17,18,0,0,0,0,0,0,
                       0,0,0,0,0,0,19,20,21,0,0,22,23,24,0,0,25,26,27,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

    val fct3D = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int,SizeVar("M")), SizeVar("N")),SizeVar("O")),
      (domain) => MapGlb(2)(MapGlb(1)(MapGlb(0) (fun(m => { toGlobal(idI) $ m})))) o PadConstant3D(1, 1, 1, 0) $ domain
    )

    val (output3D,runtime) = Execute(2,2)[Array[Int]](fct3D, input3D)

    assertArrayEquals(gold3D, output3D)
  }


}
