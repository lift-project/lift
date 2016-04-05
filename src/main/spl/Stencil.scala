package spl

import apart.arithmetic.ArithExpr
import ir.ast.{Lambda, Group, Pad}

object Stencil {
  def apply(neighbors: Array[Int], boundary: (ArithExpr, ArithExpr) => ArithExpr): Lambda = {
    Group(neighbors) o Pad(neighbors.map(Math.abs).max, boundary)
  }
}

