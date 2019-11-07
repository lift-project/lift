package backends.spatial.accel.ir.pattern

import ir.Type
import ir.ast.{FPattern, Lambda, Pattern}
import ir.interpreter.Interpreter.ValueMap

case class toArgOut(f: Lambda) extends Pattern(arity = f.arity)
                            with FPattern {
  assert(f.arity == 1)

  override def copy(f: Lambda): Pattern = toArgOut(f)

  override def checkType(argType: Type, setType: Boolean): Type =
    f.checkType(argType, setType)

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    f.eval(valueMap, args:_*)
  }
}
