package opencl.ir.pattern

import ir.interpreter.Interpreter.ValueMap
import ir.{Type, TypeChecker, UndefType}
import ir.ast._

// TODO(tlutz) remove lambda and use composition operator
case class toGlobal(f: Lambda) extends Pattern(arity = f.arity)
                                with FPattern with isGenerable {
  override def copy(f: Lambda): Pattern = toGlobal(f)

  override def checkType(argType: Type, setType: Boolean): Type = {
    f.checkType(argType, setType)
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    f.eval(valueMap, args:_*)
  }
}
