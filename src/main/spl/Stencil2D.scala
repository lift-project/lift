package spl

import apart.arithmetic.ArithExpr
import ir.ast.{Group2D, Lambda, Pad2D}

object Stencil2D {
  def apply(neighbors: Array[Int], boundary: (ArithExpr, ArithExpr) => ArithExpr): Lambda = {
    Group2D(neighbors) o Pad2D(neighbors.map(Math.abs).max, boundary)
  }
}
