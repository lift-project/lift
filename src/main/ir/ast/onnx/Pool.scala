package ir.ast.onnx

import ir._
import ir.ast.{Lambda1, Pattern, fun}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.simplifier.SimplifyFloor
import lift.arithmetic.{ArithExpr, CeilingFunction, Cst, FloorFunction}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

abstract class Pool(override val arity: Int) extends Pattern(arity) with ONNXPattern {
  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }
}

/**
  * TODO: documentation
  * @param auto_pad
  * @param count_include_pad
  * @param kernel_shape
  * @param pads
  * @param strides
  */
case class AveragePool(auto_pad : String =  "NOTSET",
                       count_include_pad : Int = 0,
                       kernel_shape : List[ArithExpr],
                       pads : List[ArithExpr],
                       strides : List[ArithExpr]) extends Pool(1) {
  override def checkType(argType: Type, setType: Boolean): Type = argType
}

/**
  * ONNX Lift IR: MaxPool operator.
  * Corresponds to ONNX (v1.3.0)->MaxPool.
  * See https://github.com/onnx/onnx/blob/master/docs/Operators.md
  *
  * Note the difference between pads and padShape: the pad is the input parameter that specifies the number of pixels
  * to add at the beginning and the end of the corresponding axes. The shape is following:
  * [x1_begin, x2_begin, ..., x1_end, x2_end, ...]
  * padShape is inferred from pads and autoPad and has the shape [x1_total_pad, x2_total_pad, ..., xn_total_pad].
  *
  * @param autoPad (see ONNX spec)
  * @param kernelShape (see ONNX spec)
  * @param pads (see ONNX spec)
  * @param storageOrder (see ONNX spec)
  * @param strides (see ONNX spec)
  */
case class MaxPool(autoPad: String,
                   kernelShape: List[ArithExpr],
                   private val pads: List[ArithExpr],
                   storageOrder: ArithExpr,
                   strides: List[ArithExpr]) extends Pool(1) {
  // PadShape will be determined inside checkType based on input shape, autoPad and pads
  var padShape: Option[List[ArithExpr]] = None

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    val dimensionality = strides.length

    argType match {
      case ArrayTypeWS(ArrayTypeWS(spatialInputDimsT, iC), iN)
        if Type.getLengths(spatialInputDimsT).length == dimensionality =>

        val spatialInputDims = Type.getLengths(spatialInputDimsT)

        val dimIndices = (1 to dimensionality).toList

        val outputSpatialShape: List[ArithExpr] = autoPad match {
          case "NOTSET" =>
            // Explicit padding
            padShape = Some(dimIndices.map(i =>
              pads(i) + pads(dimensionality + i)))

            dimIndices.map(i =>
              SimplifyFloor((spatialInputDims(i) + padShape.get(i) - kernelShape(i)) / strides(i) + 1))

          case "VALID" =>
            // No padding
            padShape = Some(List.fill(dimensionality)(0))

            dimIndices.map(i =>
              CeilingFunction((spatialInputDims(i) - kernelShape(i) + 1) / strides(i)))

          case "SAME_UPPER" | "SAME_LOWER" =>
            // Automatic padding such that the output size matches the input size depending on stride
            val outSShape = dimIndices.map(i =>
              CeilingFunction(spatialInputDims(i) / strides(i)))

            padShape = Some(dimIndices.map(i =>
              (outSShape(i) - 1) * strides(i) + kernelShape(i) - spatialInputDims(i)))

            outSShape
        }

        ArrayTypeWSWC(ArrayTypeWSWC(
          ArrayType(Type.getBaseType(spatialInputDimsT), outputSpatialShape),
          iC), iN)


      case _ =>
        throw TypeException(f"Expected an input of the shape (N x C x D1 x D2 x .. x Dn). Got $argType")
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }
}


object MaxPool {
  /**
    * Creates an instance of onnx.MaxPool.
    * This function checks the parameters and assigns default values.
    *
    * @param autoPad (see ONNX spec)
    * @param kernelShape (see ONNX spec)
    * @param pads (see ONNX spec)
    * @param storageOrder (see ONNX spec)
    * @param strides (see ONNX spec)
    *
    * @return A lambda returning an instance of the onnx.MaxPool pattern.
    */
  def apply(autoPad: String = "NOTSET",
            kernelShape: List[ArithExpr],
            pads: Option[List[ArithExpr]] = None,
            storageOrder: ArithExpr = 0,
            strides: List[ArithExpr]): Lambda1 = {
    val dimensionality = strides.length
    assert(autoPad == "NOTSET" || autoPad == "SAME_UPPER" || autoPad == "SAME_LOWER" || autoPad == "VALID")
    assert(kernelShape.length == dimensionality)

    pads match {
      case Some(padList) =>
        assert(autoPad == "NOTSET" || autoPad == "VALID")
        assert(padList.length == dimensionality * 2)
      case None =>
    }

    fun(x => MaxPool(autoPad, kernelShape, pads.getOrElse(List.fill(dimensionality)(Cst(0))),
      storageOrder, strides)(x))
  }
}