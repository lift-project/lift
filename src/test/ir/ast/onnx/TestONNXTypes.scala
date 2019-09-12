package ir.ast.onnx

import ir.ArrayType
import ir.ast.{Lambda, fun}
import lift.arithmetic.Cst
import opencl.ir.Float
import org.junit.{Ignore, Test}

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox
import scala.io.Source

class TestONNXTypes {

  @Ignore("not ready yet")
  @Test
  def test_Conv() : Unit = {

    //val f = fun(ArrayType(Float,List(Cst(1),Cst(3),Cst(20),Cst(20))),  ArrayType(Float,List(Cst(5),Cst(3),Cst(4),Cst(4))), (X,W)=> {ConvWithoutBias(auto_pad = "NOTSET",dilations = List(0,0),group = 1,kernel_shape = List(3,4,4),pads = List(1,1),strides = List(1,1)) (X,W)})

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


    /*
    val f = fun(
      ArrayType(Float,List(Cst(1),Cst(3),Cst(20),Cst(20))),
      ArrayType(Float,List(Cst(5),Cst(3),Cst(4),Cst(4))),
      (X,W) => {
        ConvWithoutBias(
          auto_pad = "NOTSET",
          dilations = List(0,0),
          group = 1,
          kernel_shape = List(3,4,4),
          pads = List(1,1),
          strides = List(1,1)
        ) (X,W)
      }
    )*/

    println("cool")
  }

}
