package ir.ast.onnx

import ir.ast.{onnx, λ}
import ir.{ArrayTypeWSWC, Type}
import lift.arithmetic.ArithExpr
import opencl.ir._
import org.junit._

/*
class testONNXOperators {


  @Test
  def minst(): Unit = {
    /* Initializers */
//    val Input3: List[List[List[List[Float]]]] =
//      List.fill(1)(List.fill(1)(List.fill(28)(List.fill(28)(util.Random.nextFloat()))))
    val Input3Type: Type = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 28), 28), 1), 1)

    val Parameter193_reshape1_shape: List[ArithExpr] = List(256, 10)
    val Pooling160_Output_0_reshape0_shape: List[ArithExpr] = List(256, 10)

//    val Parameter5: List[List[List[List[Float]]]] =
//      List.fill(8)(List.fill(1)(List.fill(5)(List.fill(5)(util.Random.nextFloat()))))
//    val Parameter6:  List[List[List[Float]]] =
//      List.fill(8)(List.fill(1)(List.fill(1)(util.Random.nextFloat())))
//    val Parameter87: List[List[List[List[Float]]]] =
//      List.fill(16)(List.fill(8)(List.fill(5)(List.fill(5)(util.Random.nextFloat()))))
//    val Parameter88: List[List[List[Float]]] =
//      List.fill(16)(List.fill(1)(List.fill(1)(util.Random.nextFloat())))
//    val Parameter193: List[List[List[List[Float]]]] =
//      List.fill(16)(List.fill(4)(List.fill(4)(List.fill(10)(util.Random.nextFloat()))))
//    val Parameter194: List[List[Float]] =
//      List.fill(1)(List.fill(10)(util.Random.nextFloat()))
    val Parameter5Type: Type = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 5), 5), 1), 8)
    val Parameter6Type: Type = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 1), 1), 8)
    val Parameter87Type: Type = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 5), 5), 8), 16)
    val Parameter88Type: Type = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 1), 1), 15)
    val Parameter193Type: Type = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 10), 4), 4), 16)
    val Parameter194Type: Type = ArrayTypeWSWC(ArrayTypeWSWC(Float, 1), 10)



    def Parameter193_reshape1 =
      λ(Parameter193 =>
        onnx.Reshape(Parameter193_reshape1_shape) $ Parameter193)

    def Convolution28_Output_0 =
      λ((Input3, Parameter5) =>
        Conv(autoPad = "SAME_UPPER", dilations = Some(List(1, 1)), group = 1,
          kernelShape = List(5, 5), strides = List(1, 1))(Input3, Parameter5))

    def Plus30_Output_0 =
      λ((Convolution28_Output_0, Parameter6) => onnx.Add()(Convolution28_Output_0, Parameter6))

    def ReLU32_Output_0 =
      λ(Plus30_Output_0 =>
        onnx.Relu() $ Plus30_Output_0)

    def Pooling66_Output_0 =
      λ(ReLU32_Output_0 =>
        onnx.MaxPool(autoPad = "NOTSET", kernelShape = List(2, 2), pads = Some(List(0, 0, 0, 0)),
          strides = List(2, 2)) $ ReLU32_Output_0)

    def Convolution110_Output_0 =
      λ((Pooling66_Output_0, Parameter87) =>
        Conv(autoPad = "SAME_UPPER", dilations = Some(List(1, 1)), group = 1,
          kernelShape = List(5, 5), strides = List(1, 1))(Pooling66_Output_0, Parameter87))

    def Plus112_Output_0 =
      λ((Convolution110_Output_0, Parameter88) =>
        onnx.Add()(Convolution110_Output_0, Parameter88))

    def ReLU114_Output_0 =
      λ(Plus112_Output_0 =>
        onnx.Relu() $ Plus112_Output_0)

    def Pooling160_Output_0 =
      λ(ReLU114_Output_0 =>
        onnx.MaxPool(autoPad = "NOTSET", kernelShape = List(5, 5), pads = Some(List(0, 0, 0, 0)),
          strides = List(3, 3)) $ ReLU114_Output_0)

    def Pooling160_Output_0_reshape0 =
      λ(Pooling160_Output_0 =>
        onnx.Reshape(Pooling160_Output_0_reshape0_shape) $ Pooling160_Output_0)

    def Times212_Output_0 =
      λ((Pooling160_Output_0_reshape0, Parameter193_reshape1) =>
        onnx.MatMul()(Pooling160_Output_0_reshape0, Parameter193_reshape1))

    def Plus214_Output_0 =
      λ((Parameter194, Times212_Output_0) => onnx.Add()(Times212_Output_0, Parameter194)
      )

    val graph =
      λ(Input3Type, Parameter5Type, Parameter6Type, Parameter87Type, Parameter88Type, Parameter193Type, Parameter194Type,
        (Input3, Parameter5, Parameter6, Parameter87, Parameter88, Parameter193, Parameter194) =>
        Plus214_Output_0(
          Parameter194,
          Times212_Output_0(
            Pooling160_Output_0_reshape0(
              Pooling160_Output_0(
                ReLU114_Output_0(
                  Plus112_Output_0(
                    Convolution110_Output_0(
                      Pooling66_Output_0(
                        ReLU32_Output_0(
                          Plus30_Output_0(
                            Convolution28_Output_0(
                              Input3,
                              Parameter5),
                            Parameter6))),
                      Parameter87),
                    Parameter88)))),
            Parameter193_reshape1(
              Parameter193))))

//    val graphReverseNotation =
//      λ(Input3Type, Parameter5Type, Parameter6Type, Parameter87Type, Parameter88Type, Parameter193Type, Parameter194Type,
//        (Input3, Parameter5, Parameter6, Parameter87, Parameter88, Parameter193, Parameter194) =>
//
//        (Parameter194,
//          (((((
//            Input3,
//            Parameter5 ) :>>
//            Convolution28_Output_0,
//            Parameter6 ) :>>
//
//            Plus30_Output_0 :>>
//            ReLU32_Output_0 :>>
//            Pooling66_Output_0,
//            Parameter87 ) :>>
//
//            Convolution110_Output_0,
//            Parameter88 ) :>>
//
//            Plus112_Output_0 :>>
//            ReLU114_Output_0 :>>
//            Pooling160_Output_0 :>>
//            Pooling160_Output_0_reshape0,
//            Parameter193 :>>
//              Parameter193_reshape1 ) :>>
//
//            Times212_Output_0) :>>
//          Plus214_Output_0
//      )


    val graphInlined =
      λ(Input3Type, Parameter5Type, Parameter6Type, Parameter87Type, Parameter88Type, Parameter193Type, Parameter194Type,
        (Input3, Parameter5, Parameter6, Parameter87, Parameter88, Parameter193, Parameter194) =>
        onnx.Add()(
          onnx.MatMul()(
            onnx.Reshape(Pooling160_Output_0_reshape0_shape) o
              onnx.MaxPool(autoPad = "NOTSET", kernelShape = List(5, 5), pads = Some(List(0, 0, 0, 0)),
                strides = List(3, 3)) o
              onnx.Relu() $
              onnx.Add()(
              Conv(autoPad = "SAME_UPPER", dilations = Some(List(1, 1)), group = 1,
                kernelShape = List(5, 5), strides = List(1, 1))(
                onnx.MaxPool(autoPad = "NOTSET", kernelShape = List(2, 2), pads = Some(List(0, 0, 0, 0)),
                  strides = List(2, 2)) o
                  onnx.Relu() $
                  Conv(autoPad = "SAME_UPPER", dilations = Some(List(1, 1)), group = 1,
                    kernelShape = List(5, 5), strides = List(1, 1))(
                    Input3,
                    Parameter5),
                Parameter87),
              Parameter88),
          onnx.Reshape(Parameter193_reshape1_shape) $
            Parameter193),
          Parameter194))

  }
}*/
