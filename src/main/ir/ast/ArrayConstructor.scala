package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap

case class ArrayConstructor(value: Value, at: ArrayType) extends Expr {

  override def copy: Expr = ArrayConstructor(value, at)

  override def eval(valueMap: ValueMap): Any = {
    Array.fill(at.len.eval)(value.eval(valueMap))
  }

}
