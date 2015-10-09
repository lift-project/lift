package spl

import apart.arithmetic.ArithExpr
import ir.ast.{Lambda, Group, Pad}

object Stencil {
  def apply(neighbors: Array[Int], boundary: (ArithExpr, ArithExpr) => ArithExpr): Lambda = {
    Group(neighbors.map(_+neighbors.map(Math.abs).max)) o Pad(neighbors.map(Math.abs).max, boundary)
  }
}

