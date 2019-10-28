package backends.spatial.accel.ir.pattern

import ir.ast.Pattern
import ir.interpreter.Interpreter.ValueMap
import ir.{ScalarType, Type, TypeException}

case class toLiteral() extends Pattern(arity = 1) {

  override def checkType(argType: Type, setType: Boolean): Type = {
    argType match {
      case _: ScalarType =>
      case _ => throw new TypeException(argType, "ScalarType", this)
    }

    argType
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = args.head
}
