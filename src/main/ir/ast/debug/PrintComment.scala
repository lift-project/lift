package ir.ast.debug

import ir.Type
import ir.ast.Pattern
import ir.interpreter.Interpreter._

/**
  * A pattern for debugging Lift code.
  * An identity function that generates an OpenCL comment in the kernel.
  */
case class PrintComment(msg: String = "") extends Pattern(arity = 1) {
  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    args.head
  }
}
