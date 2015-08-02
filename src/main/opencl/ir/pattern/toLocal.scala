package opencl.ir.pattern

import ir.{TypeChecker, Type}
import ir.ast._

// TODO(tlutz) remove lambda and use composition operator
case class toLocal(f: Lambda1) extends Pattern(arity = 1)
                               with FPattern with isGenerable {
  override def copy(f: Lambda): Pattern = toLocal(f)

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    f.params(0).t = argType
    TypeChecker.check(f.body, setType)
  }
}
