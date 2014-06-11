package generator

import ir.Fun

abstract class Generator {
  def generate(f: Fun) : String
}