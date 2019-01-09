package ir.ast.onnx

import ir.ArrayTypeWS
import ir.ast.fun
import org.junit.Test

class TestONNXTypes {

  @Test
  def test_Conv() : Unit = {

    val f = fun(
      NDArrayBuilder(Float,List(1,3,20,20)),
      NDArrayBuilder(Float,List(5,3,4,4)),
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
    )
  }

}
