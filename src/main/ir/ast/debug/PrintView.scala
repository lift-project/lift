package ir.ast.debug

import ir.Type
import ir.ast.{FPattern, Lambda, Pattern, isGenerable}
import ir.interpreter.Interpreter._

/**
  * A pattern for debugging Lift code.
  * TODO
  */
case class PrintView(msg: String = "", f: Lambda) extends Pattern(arity = f.arity)
                                                    with FPattern with isGenerable {
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
