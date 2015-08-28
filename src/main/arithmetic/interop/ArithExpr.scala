package arithmetic.interop

import apart.arithmetic.{Cst, Var, Range}

object jCst { def create(c: Int) = Cst(c) }

object jVar {
  def create(name: String, range: Range) = Var(name, range)
  def create(name: String) = Var(name)
}
