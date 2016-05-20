package generator

import ir.ast.Lambda

class Kernel(val code: String, val f: Lambda)

abstract class Generator {
  def generate(f: Lambda) : Kernel
}