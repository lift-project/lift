package ir.ast.onnx

import ir.ast.Pattern
import ir.interpreter.Interpreter.ValueMap
import ir.{ArrayType, TupleType, Type, TypeException}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
  * ONNX Lift IR: MatMul operator.
  * Corresponds to ONNX (v1.3.0)->MatMul.
  * See https://github.com/onnx/onnx/blob/master/docs/Operators.md
  */

case class MatMul() extends Pattern(arity = 2) with ONNXPattern {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {

    argType match {
      case TupleType(aT@ArrayType(_), bT@ArrayType(_)) if Type.getBaseType(aT) == Type.getBaseType(bT) =>

        val aShape = Type.getLengths(aT)
        val bShape = Type.getLengths(bT)

        val outputShape = ((aShape.length, bShape.length) match {
          case (aLen, bLen) if aLen >= 2 && bLen >= 2 =>
            // Ensure A width == B height
            if (aShape.last != bShape(bShape.length - 2))
              throw TypeException(f"Expected A width to be equal to B height")

            // To verify against numpy.matmul
            aShape.take(aLen - 2) ++ bShape.take(bLen - 2) ++ List(bShape(bShape.length - 2), aShape.last)

          case (aLen, bLen) if aLen >= 2 && bLen == 1 =>
            aShape.take(aLen - 2) ++ List(bShape.head, aShape.last)

          case (aLen, bLen) if aLen == 1 && bLen >= 2 =>
            bShape.take(bLen - 2) ++ List(bShape(bShape.length - 2), aShape.head)

          case (aLen, bLen) =>
            throw TypeException(f"Unexpected A and B lengths ($aLen, $bLen).")
        }).toList

        ArrayType(Type.getBaseType(aT), outputShape)


      case _ => throw TypeException(f"Expected the argument to be a tuple of matrices (with equal base types) " +
        f"that can be multiplied together as per ONNX spec. Got $argType")
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }

}
