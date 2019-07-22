package ir.ast.debug

import ir.Type
import ir.ast.{FPattern, Lambda, Pattern}
import ir.interpreter.Interpreter._

/**
  * A pattern for debugging Lift code.
  * PrintView adds a comment into the OpenCL kernel with the optional message and 
  * a string representation of the View at the point in expression where PrintView
  * is inserted.
  */
case class PrintView(msg: String = "", f: Lambda) extends Pattern(arity = f.arity)
                                                    with FPattern {
  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    f.checkType(argType, setType)
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    f.eval(valueMap, args:_*)
  }

  override def copy(f: Lambda): Pattern = PrintView(msg, f)
}
