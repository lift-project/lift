package opencl.ir.pattern

import ir.ast._

// TODO(tlutz) remove lambda and use composition operator
case class toPrivate(f: Lambda1) extends Pattern(arity = 1) with FPattern with isGenerable {
  override def copy(f: Lambda): Pattern = toPrivate(f)
}
