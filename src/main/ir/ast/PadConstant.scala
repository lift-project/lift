package ir.ast

import ir.{ArrayTypeWSWC, Type, TypeException}
import ir.interpreter.Interpreter.ValueMap
import sun.reflect.generics.reflectiveObjects.NotImplementedException

case class PadConstant(left: Int, right: Int, constant: Value)
  extends Pattern(arity = 1) {

  override def toString: String = "PadConstant(" + left + "," + right + "," + constant + ")"

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWSWC(et,s,c) => ArrayTypeWSWC(et, s + left + right, c + left + right)
      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case _: Vector[_] => throw new NotImplementedException()
    }
  }
}
