package cbackends.global

import cbackends.host.host_ir.CPUFunc
import ir.ast.Pad.Boundary.WrapUnsafe
import ir.ast.{Array3DFromUserFunGenerator, ArrayFromUserFunGenerator, Get, Join, Lambda, Pad, Slide, Slide2D, Slide3D, Slide3D_R, Split, Transpose, TransposeW, UserFun, Zip, \, fun}
import ir.{ArrayType, ArrayTypeWSWC, TupleType}
import lift.arithmetic.{Cst, SizeVar}
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import opencl.ir.{Float, add, dividedBy, _}
import org.junit.Assert._
import org.junit.Test
import rewriting.Rewrite
import rewriting.rules.Rules
import rewriting.utils.NumberPrinter
//import org.scalatest.expect

import scala.language.postfixOps
import scala.sys.process._

import cbackends.common.executor.Executor.{native_compile_and_run}

class TestGlobal {

  val common_path = "/home/lu/Documents/Research/lift/src/test/cbackends/global"

  val N = SizeVar("N")
  val M = SizeVar("M")
  val O = SizeVar("O")
  val K = SizeVar("K")

  val incrementF = fun(Float, x => add(Float).apply(1f, x))
  val incrementF2 = fun(Float, x => add(Float).apply(2f, x))

  val add2 = UserFun("add", Array("l", "r"),
    "{ return (l + r); }",
    Seq(Float, Float), Float)

  @Test
  def test_cpu_func(): Unit = {

    val path = s"$common_path/01.cpufunc"
    val file = "libcpufunc.cpp"

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      in => CPUFunc( MapSeq(MapSeq(incrementF))  ) o CPUFunc( MapSeq(MapSeq(incrementF)) ) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    //val actual : String = native_compile_and_run(path, file)
    val expected : String = "3 3 3 3 3 3 \n"
    //assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }
}
