package backends.spatial.accel.ir.pattern

import ir.Type
import ir.ast.{FPattern, Lambda, Pattern}
import ir.interpreter.Interpreter.ValueMap

case class toDRAM(f: Lambda) extends Pattern(arity = f.arity)
                             with FPattern {
  override def copy(f: Lambda): Pattern = toDRAM(f)

  override def checkType(argType: Type, setType: Boolean): Type =
    f.checkType(argType, setType)

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    f.eval(valueMap, args:_*)
  }
}
