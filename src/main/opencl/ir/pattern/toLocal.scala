package opencl.ir.pattern

import ir.interpreter.Interpreter._
import ir.{TypeChecker, Type}
import ir.ast._

// TODO(tlutz) remove lambda and use composition operator
case class toLocal(f: Lambda) extends Pattern(arity = f.arity)
                               with FPattern with isGenerable {
  override def copy(f: Lambda): Pattern = toLocal(f)

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    TypeChecker.checkAndSetTypeForParams(f.params, argType)
    TypeChecker.check(f.body, setType)
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    f.eval(valueMap, args:_*)
  }
}
