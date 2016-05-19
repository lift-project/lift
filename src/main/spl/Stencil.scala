package spl

import apart.arithmetic.ArithExpr
import ir.ast.{Lambda, Group, Pad}

object Stencil {
  def apply(neighbors: Array[Int], boundary: (ArithExpr, ArithExpr) => ArithExpr): Lambda = {
    val max = neighbors.map(Math.abs).max
    Group(neighbors.map(_ + max)) o Pad(max, boundary)
  }
}

