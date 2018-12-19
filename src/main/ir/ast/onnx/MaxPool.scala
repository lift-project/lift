package ir.ast.onnx

import ir._
import ir.ast.{Expr, Pattern}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{ArithExpr, CeilingFunction, Cst, FloorFunction}
import sun.reflect.generics.reflectiveObjects.NotImplementedException


/**

  */
case class MaxPool(autoPad: String,
                   kernelShape: List[Int],
                   private val pads: List[Int],
                   storageOrder: Int,
                   strides: List[Int]) extends Pattern(arity = 1) {
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
              Cst(pads(i) + pads(dimensionality + i))))

            dimIndices.map(i =>
              FloorFunction((spatialInputDims(i) + padShape.get(i) - kernelShape(i)) / strides(i) + 1))

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
          Type.buildArrayType(outputSpatialShape, Type.getBaseType(spatialInputDimsT)),
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

    */
  def apply(autoPad: String = "NOTSET",
            kernelShape: List[Int],
            pads: Option[List[Int]],
            storageOrder: Int = 0,
            strides: List[Int])(args : Expr*): Expr = {
    val dimensionality = strides.length
    assert(autoPad == "NOTSET" || autoPad == "SAME_UPPER" || autoPad == "SAME_LOWER" || autoPad == "VALID")
    assert(kernelShape.length == dimensionality)
    assert(args.length == 1)

    pads match {
      case Some(padList) =>
        assert(autoPad == "NOTSET" || autoPad == "VALID")
        assert(padList.length * 2 == dimensionality)
      case None =>
    }

    MaxPool(autoPad, kernelShape, pads.getOrElse(List.fill(dimensionality)(0)), storageOrder, strides)(args: _*)
  }
}