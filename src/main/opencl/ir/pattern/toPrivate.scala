package opencl.ir.pattern

import ir._
import ir.ast._

// TODO(tlutz) remove lambda and use composition operator
case class toPrivate(f: Lambda) extends Pattern(arity = f.arity)
                                 with FPattern with isGenerable {

  override def copy(f: Lambda): Pattern = toPrivate(f)

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    TypeChecker.checkAndSetTypeForParams(f.params, argType)
    TypeChecker.check(f.body, setType)
  }

}
