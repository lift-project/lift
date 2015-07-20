package opencl.ir.pattern

import ir.ast.{isGenerable, FPattern, Pattern, Lambda1}

// TODO(tlutz) remove lambda and use composition operator
case class toGlobal(f: Lambda1) extends Pattern(arity = 1) with FPattern with isGenerable