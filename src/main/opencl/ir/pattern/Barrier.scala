package opencl.ir.pattern

import ir.ast.{isGenerable, Pattern}

case class Barrier() extends Pattern(arity = 1) with isGenerable {
  var valid = true
}