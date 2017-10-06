package ir.ast.debug

import ir.Type
import ir.ast.Pattern
import ir.interpreter.Interpreter._

/**
  * A pattern for debugging Lift code.
  * Identity function that prints the Lift type of its input.
  * Generates no OpenCL code.
  */
case class PrintType(msg: String = "") extends Pattern(arity = 1) {
  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    if (msg != "")
      print(msg + ": ")
    println(argType.toString)
    argType
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    args.head
  }
}
