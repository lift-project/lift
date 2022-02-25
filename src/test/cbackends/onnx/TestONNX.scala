package cbackends.onnx

import cbackends.common.executor.Executor.native_compile_and_run
import ir.ast.{Lambda, fun, onnx}
import lift.arithmetic.Cst
import opencl.ir.Float
import ir.ArrayType
import ir.ast.onnx.{AveragePool, ConvWithoutBias}
import org.junit.Assert._
import org.junit.{Ignore, Test}

import scala.reflect.runtime._
import scala.language.postfixOps
import scala.tools.reflect.ToolBox
import scala.io.Source
import scala.sys.process._

class TestONNX {

  val common_path = System.getProperty("user.dir") + "/src/test/cbackends/onnx"

  @Ignore // TODO: fix bugs
  @Test
  def test_conv(): Unit = {

    val path = s"$common_path/01.conv"
    val host_file = "libconv3d_host.cpp"
    val gpu_file = "libconv3d_gpu.cpp"

    ("mkdir -p " + s"$path" ) !!


    /*
    val prologue =
      """
        | import ir.{ArrayType}
        | import ir.ast.fun
        | import lift.arithmetic.Cst
        | import opencl.ir.Float
        | import ir.ast.onnx._
        |
        |
      """.stripMargin

    val expr = Source.fromFile(System.getProperty("user.dir") + "/src/test/ir/ast/onnx/lift_expr_for_onnx_model/lu_lift_expr_from_onnx.txt").getLines.mkString

    val code = prologue + expr


    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    val tb = mirror.mkToolBox()
    val tree = tb.parse(code)
    val f = tb.eval(tree).asInstanceOf[Lambda]
    */

    val f = fun(
      //ArrayType(Float,List(Cst(1),Cst(3),Cst(20),Cst(20))),
      //ArrayType(Float,List(Cst(19),Cst(20),Cst(20))),
      //ArrayType(Float,List(Cst(5),Cst(5),Cst(5))),
      //ArrayType(Float,List(Cst(5),Cst(3),Cst(4),Cst(4))),
      ArrayType(Float,List(Cst(1),Cst(8),Cst(8),Cst(8))),
      ArrayType(Float,List(Cst(1),Cst(8),Cst(6),Cst(6))),
      (X,W) => {
        onnx.Conv(
          group = 1,
          kernelShape = List(1,8,6,6),
          pads = Some(List(1,1)),
          strides = List(1,1)
        ) (X,W)
      }
    )

    ONNXCompiler ! (f, path, List(host_file, gpu_file))

    val actual : String = native_compile_and_run(path, host_file)
    //val expected : String = "432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 \n"
    val expected : String = "576 576 576 576 576 576 576 576 576 \n"
    assertEquals(expected, actual)




    println("cool")



  }


  @Ignore // TODO: fix bugs
  @Test
  def test_pool(): Unit = {

    val path = s"$common_path/02.pool"
    val host_file = "libaverage_pool_host.cpp"
    val gpu_file = "libaverage_pool_gpu.cpp"

    ("mkdir -p " + s"$path" ) !!


    val f = fun(
      //ArrayType(Float,List(Cst(1),Cst(5),Cst(19),Cst(19))),
      ArrayType(Float,List(Cst(8),Cst(8),Cst(8))),
      (Y)=> {
        AveragePool(
          auto_pad = "NOTSET",
          count_include_pad = 0,
          kernel_shape = List(8,6,6),
          pads = List(0,0,0,0,0,0),
          strides = List(1,1,1)
        ) (Y)}
    )

    ONNXCompiler ! (f, path, List(host_file, gpu_file))

    val actual : String = native_compile_and_run(path, host_file)
    val expected : String = "2 2 2 2 2 2 2 2 2 \n"
    assertEquals(expected, actual)




    println("cool")



  }

  @Ignore // TODO: fix bugs
  @Test
  def test_conv_pool_network(): Unit = {

    val path = s"$common_path/03.conv_pool_network"
    val host_file = "libconv_pool_network_host.cpp"
    val gpu_file = "libconv_pool_network_gpu.cpp"

    ("mkdir -p " + s"$path" ) !!


    val f = fun(
      ArrayType(Float,List(Cst(1),Cst(8),Cst(8),Cst(8))),
      ArrayType(Float,List(Cst(1),Cst(8),Cst(6),Cst(6))),
      (X,W) => {
        AveragePool(
          auto_pad = "NOTSET",
          count_include_pad = 0,
          kernel_shape = List(1,2,2),
          pads = List(0,0,0,0,0,0),
          strides = List(1,1,1)
        ) (
          onnx.Conv(
            autoPad = "NOTSET",
            group = 1,
            kernelShape = List(1,8,6,6),
            pads = Some(List(0,0)),
            strides = List(1,1,1)
          ) (X,W)
        )
      }
    )

    ONNXCompiler ! (f, path, List(host_file, gpu_file))

    val actual : String = native_compile_and_run(path, host_file)
    val expected : String = "576 576 576 576 \n"
    assertEquals(expected, actual)




    println("cool")



  }


}
