package ir.ast

import lift.arithmetic.{ArithExpr, Cst}
import lift.arithmetic.ArithExpr._

object Drop {
  def apply(left: ArithExpr, right: ArithExpr): Lambda = {
    if (ArithExpr.isSmaller(left, Cst(0)).getOrElse(false) || ArithExpr.isSmaller(right, Cst(0)).getOrElse(false))
      throw new IllegalArgumentException("Drop only supports statically evaluable positive values")

    Pad(-1 * left, -1 * right, Pad.Boundary.Identity)
  }
}