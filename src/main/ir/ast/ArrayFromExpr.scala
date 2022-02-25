package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap

case class ArrayFromExpr(e: Expr) extends ArrayConstructors(ArrayTypeWSWC(e.t,1,1)) {
  override def copy: Expr = ArrayFromExpr(e)

  override def eval(valueMap: ValueMap): Any = {
    Array.fill(at.size.eval)(e.eval(valueMap))
  }

}
