package ir.ast.debug

import ir.Type
import ir.ast.Pattern
import ir.interpreter.Interpreter._

/**
  * TypeOperator applies given function onto the type of its parameter.
  * Useful for debugging.
  */
abstract class TypeOperator(f: Type => Unit) extends Pattern(arity = 1) {
  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    f(argType)
    argType
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    args.head
  }
}
