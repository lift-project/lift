package ir.ast.onnx

import ir._
import ir.ast.{Expr, Pattern}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.ArithExpr
import sun.reflect.generics.reflectiveObjects.NotImplementedException


/**
  * ONNX Lift IR: Conv operator.
  * Corresponds to ONNX (v1.3.0)->Conv.
  * See https://github.com/onnx/onnx/blob/master/docs/Operators.md
  *
  * This is an abstract pattern inherited by ConvWithoutBias and ConvWithBias.
  *
  * @param n The number of arrays which are combined. Must be 2 or 3.
  * @param autoPad (see ONNX spec)
  * @param dilations (see ONNX spec)
  * @param group (see ONNX spec)
  * @param kernelShape (see ONNX spec)
  * @param pads (see ONNX spec)
  * @param strides (see ONNX spec)
  */
abstract class AbstractConv(n: Int,
                            autoPad: String,
                            dilations: List[Int],
                            group: Int,
                            kernelShape: Option[List[Int]],
                            pads: List[Int],
                            strides: List[Int]) extends Pattern(arity = n) {

  /**
    * Verifies the dimension sizes and the base type of the convolutional kernel weights.
    *
    * @param kCGroupSize is the size of groups output channels are divided into
    * @param iC is the number of input channels
    * @param spatialDimIT is the nested type of spatial dimensions of the inputs
    * @param spatialDimWT is the nested type of spatial dimensions of the weights
    * @return
    */
  def verifyWType(kCGroupSize: ArithExpr, iC: ArithExpr, spatialDimIT: Type, spatialDimWT: Type): Boolean = {
    // Check base types
    Type.getBaseType(spatialDimWT) == Type.getBaseType(spatialDimIT) &&
      // Check kernel channels
      kCGroupSize * group == iC &&
      // Check actual kernel spatial dimensions shape if corresponding parameter is specified
      (kernelShape.isEmpty ||
        Type.getLengths(spatialDimWT).zip(kernelShape.get).forall(sizePair => sizePair._1.eval == sizePair._2))
  }


  def computeOutType(iN: ArithExpr, kCGroupSize: ArithExpr, spatialDimIT: Type, spatialDimWT: Type): Type = {

    // For the shape formula, see Relationship 14 in
    // http://deeplearning.net/software/theano/tutorial/conv_arithmetic.html
    ArrayTypeWSWC(ArrayTypeWSWC(
      Type.buildArrayType(lengths = Type.getLengths(spatialDimIT).zip(
        Type.getLengths(spatialDimWT)).toList.zip(pads).zip(dilations).zip(strides).map({
        case ((((i, k), p), d), s) =>
          // Here we assume the parameters are such that the division doesn't have a remainder
          ((i + (2 * p) - k - (k - 1) * (d - 1)) / s) + 1
      }), elemT = Type.getBaseType(spatialDimIT)), kCGroupSize), iN)

  }


  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }
}


case class ConvWithoutBias private(autoPad: String,
                                   dilations: List[Int],
                                   group: Int,
                                   kernelShape: Option[List[Int]],
                                   pads: List[Int],
                                   strides: List[Int])
  extends AbstractConv(2, autoPad, dilations, group, kernelShape, pads, strides) {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      // X, W and B
      case TupleType(
      xT@ArrayTypeWS(ArrayTypeWS(ArrayType(spatialDimIT), iC), iN),
      wT@ArrayTypeWS(ArrayTypeWS(ArrayType(spatialDimWT), kCGroupSize), wM))
        if verifyWType(kCGroupSize, iC, spatialDimIT, spatialDimWT) =>

        computeOutType(iN, kCGroupSize, spatialDimIT, spatialDimWT)

      case _ => throw TypeException(f"Expected X and W types as per ONNX specification. Got $argType")
    }
  }
}


case class ConvWithBias private(autoPad: String,
                                dilations: List[Int],
                                group: Int,
                                kernelShape: Option[List[Int]],
                                pads: List[Int],
                                strides: List[Int])
  extends AbstractConv(3, autoPad, dilations, group, kernelShape, pads, strides) {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      // X, W and B
      case TupleType(
      xT@ArrayTypeWS(ArrayTypeWS(ArrayType(spatialDimIT), iC), iN),
      wT@ArrayTypeWS(ArrayTypeWS(ArrayType(spatialDimWT), kCGroupSize), wM),
      bT@ArrayTypeWS(spatialDimBT, bM))
        if verifyWType(kCGroupSize, iC, spatialDimIT, spatialDimWT) && wM == bM &&
          Type.getBaseType(spatialDimIT) == Type.getBaseType(spatialDimBT)=>

        computeOutType(iN, kCGroupSize, spatialDimIT, spatialDimWT)

      case _ => throw TypeException(f"Expected X, W and B types as per ONNX specification. Got $argType")
    }
  }
}


object Conv {
  /**
    * Create a lambda producing an instance of either onnx.ConvWithoutBias, or onnx.ConvWithBias pattern.
    * This function infers the number of arrays which the onnx.AbstractConv pattern
    * is applied on; it also checks the parameters and assigns default values.
    *
    * @param autoPad (see ONNX spec)
    * @param dilations (see ONNX spec)
    * @param group (see ONNX spec)
    * @param kernelShape (see ONNX spec)
    * @param pads (see ONNX spec)
    * @param strides (see ONNX spec)
    *
    * @return A lambda returning an instance of the onnx.AbstractConv pattern.
    */
  def apply(autoPad: String = "NOTSET",
            dilations: Option[List[Int]],
            group: Int = 1,
            kernelShape: Option[List[Int]],
            pads: Option[List[Int]],
            strides: List[Int])(args : Expr*): Expr = {
    assert(args.length == 2 || args.length == 3)

    args.length match {
      case 2 =>
        ConvWithoutBias(
          autoPad,
          dilations.getOrElse(strides.map(_ => 0)),
          group,
          kernelShape,
          pads.getOrElse(strides.map(_ => 0)),
          strides)(args: _*)
      case 3 =>
        ConvWithBias(
          autoPad,
          dilations.getOrElse(strides.map(_ => 0)),
          group,
          kernelShape,
          pads.getOrElse(strides.map(_ => 0)),
          strides)(args: _*)
    }
  }
}