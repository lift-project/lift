package ir.ast

import ir.{ArrayType, Capacity, Size}

abstract class ArrayConstructors(val at: ArrayType with Size with Capacity) extends Expr {
  this.t = at // set type
}
