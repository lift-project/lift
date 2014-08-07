package generator

import ir.FunExpr

abstract class Generator {
  def generate(f: FunExpr) : String
}