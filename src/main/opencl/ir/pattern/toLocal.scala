package opencl.ir.pattern

import ir.Type
import ir.ast._
import ir.interpreter.Interpreter._

case class toLocal(f: Lambda) extends Pattern(arity = f.arity)
                               with FPattern {
  override def copy(f: Lambda): Pattern = toLocal(f)

  override def checkType(argType: Type, setType: Boolean): Type =
    f.checkType(argType, setType)

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    f.eval(valueMap, args:_*)
  }
}
