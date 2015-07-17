package arithmetic.interop

import arithmetic.{Cst, Var}

object jCst { def create(c: Int) = Cst(c) }

object jVar {
  def create(name: String, range: arithmetic.Range) = Var(name, range)
  def create(name: String) = Var(name)
}
