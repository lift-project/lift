package ir.ast

import lift.arithmetic._
import ir._
import ir.interpreter.Interpreter._

case class ArrayAccess(index: ArithExpr) extends Pattern(arity = 1) with isGenerable {

  override def checkType(argType: Type, setType: Boolean): Type = {
    argType match {
      case ArrayType(t) => t
      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    args.head match {
      case a: Array[_] => a(index.eval)
    }
  }

}
