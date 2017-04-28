package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap

case class ArrayFromValue(value: Value,
                          override val at: ArrayType with Size with Capacity) extends ArrayConstructors(at) {
  override def copy: Expr = ArrayFromValue(value, at)

  override def eval(valueMap: ValueMap): Any = {
    Array.fill(at.size.eval)(value.eval(valueMap))
  }

}
