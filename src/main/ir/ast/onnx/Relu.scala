package ir.ast.onnx

import ir.ast.Pattern
import ir.interpreter.Interpreter.ValueMap
import ir.{ArrayType, TupleType, Type, TypeException}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
  * ONNX Lift IR: Relu operator.
  * Corresponds to ONNX (v1.3.0)->Relu.
  * See https://github.com/onnx/onnx/blob/master/docs/Operators.md
  */
case class Relu() extends Pattern(arity = 1) with ONNXPattern {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {

    argType match {
      case ArrayType(_) => argType
      case _ => throw TypeException(f"Expected the argument to be an array. Got $argType")
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }

}
