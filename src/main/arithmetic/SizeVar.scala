package arithmetic

import apart.arithmetic.{Cst, StartFromRange, Var}

object SizeVar {
  def apply(name: String): Var = {
    Var(name, StartFromRange(Cst(1)))
  }
}