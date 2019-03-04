package ir.ast.onnx

import ir.ast.{Expr, Lambda1, Pattern, fun}
import ir.interpreter.Interpreter.ValueMap
import ir.{ArrayType, TupleType, Type, TypeException}
import lift.arithmetic.{ArithExpr, Cst}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
  * ONNX Lift IR: Reshape operator.
  * Corresponds to ONNX (v1.3.0)->Reshape.
  * See https://github.com/onnx/onnx/blob/master/docs/Operators.md
  *
  * Note that this operator does not comply with the ONNX specification in the following way:
  * the new shape is passed as a parameter rather than as an input. This is done so as to ensure that the
  * new shape is statically known to be able to infer the output shape.
  * The new shape and the output shape are not necessarily the same in case the new shape uses "-1" in one of the
  * dimensions.
  *
  * @param shape The list of lengths of the new shape. One length can be unknown and indicated with (-1). See ONNX spec.
  */

class Reshape private(val shape: List[ArithExpr]) extends Pattern(arity = 1) with ONNXPattern {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {

    def sum(xs: List[ArithExpr]): ArithExpr = {
      xs match {
        case x :: tail => x + sum(tail)
        case Nil => 0
      }
    }

    argType match {
      case ArrayType(_) =>
        val oldShape: List[ArithExpr] = Type.getLengths(argType).toList

        if (shape.contains(Cst(-1))) {
          // The shape is contains unknown lengths (-1) that need to be inferred based on the other lengths
          val sumOfKnownNewLengths: ArithExpr = shape.reduceLeft((x, y) => if (x != Cst(-1)) x * y else x)
          // Make sure the array size is divisible by the sum of the known lengths
          if (sum(oldShape) % sumOfKnownNewLengths != Cst(0))
            throw TypeException(f"Expected the array size to be divisible by the sum of the known lengths of " +
              f"the new shape")

          val missingNewLength: ArithExpr = sum(oldShape) / sumOfKnownNewLengths
          val newShape = shape.map(s => if (s == Cst(-1)) missingNewLength else s)

          ArrayType(Type.getBaseType(argType), newShape)

        } else {
          // The shape is known
          if (sum(oldShape) != sum(shape))
            throw TypeException(f"Expected the array size to be divisible by the size of the new shape")

          ArrayType(Type.getBaseType(argType), shape)
        }

      case _ => throw TypeException(f"Expected the argument to be an array. Got $argType")
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }

}


object Reshape {
  /**
    * Creates an instance of onnx.Reshape.
    *
    * @param shape The list of lengths of the new shape. One length can be unknown and indicated with (-1).
    *              See ONNX spec.
    */
  def apply(shape: List[ArithExpr]): Lambda1 = {
    // Check that there is no more than one unknown dimension size
    assert(shape.map(s => if (s == Cst(-1)) 1 else 0).sum <= 1)
    fun(x => new Reshape(shape)(x))
  }
}