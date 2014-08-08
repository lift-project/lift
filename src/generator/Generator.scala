package generator

import ir.Lambda

abstract class Generator {
  def generate(f: Lambda) : String
}