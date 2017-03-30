package ir.ast

import ir.ArrayType

abstract class ArrayConstructors(val at: ArrayType) extends Expr {
  this.t = at // set type
}
