package ir.ast.onnx

import ir.{ArrayType, TupleType, Type, TypeException}
import ir.ast.Pattern
import ir.interpreter.Interpreter.ValueMap
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
  * ONNX Lift IR: Add operator.
  * Corresponds to ONNX (v1.3.0)->Add.
  * See https://github.com/onnx/onnx/blob/master/docs/Operators.md
  */

case class Add() extends Pattern(arity = 2) with Broadcastable with ONNXPattern {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    val typeException = TypeException(
      f"Expected A and B to be arrays of the same or multidirectionally broadcastable lengths as per ONNX " +
        f"specification. Got $argType")

    argType match {
      case TupleType(typeA: ArrayType, typeB: ArrayType) =>
        tryBroadcastingShapes(typeA, typeB) match {
          case Some(shape) => ArrayType(Type.getBaseType(typeA), shape.toList)
          case None => throw typeException
        }
      case _ => throw typeException
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }

}
