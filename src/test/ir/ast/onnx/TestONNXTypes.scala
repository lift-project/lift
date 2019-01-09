package ir.ast.onnx

import ir.{ArrayType, ArrayTypeWS}
import ir.ast.fun
import lift.arithmetic.Cst
import org.junit.Test
import opencl.ir.Float

class TestONNXTypes {

  @Test
  def test_Conv() : Unit = {

    val f = fun(ArrayType(Float,List(Cst(1),Cst(3),Cst(20),Cst(20))),  ArrayType(Float,List(Cst(5),Cst(3),Cst(4),Cst(4))), (X,W)=> {ConvWithoutBias(auto_pad = "NOTSET",dilations = List(0,0),group = 1,kernel_shape = List(3,4,4),pads = List(1,1),strides = List(1,1)) (X,W)})

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
