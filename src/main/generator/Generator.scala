package generator

import ir.ast.Lambda

abstract class Generator {
  def generate(f: Lambda) : String
}