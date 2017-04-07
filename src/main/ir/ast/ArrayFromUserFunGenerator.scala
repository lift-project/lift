package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap

case class ArrayFromUserFunGenerator(f: UserFun,
                                     override val at: ArrayType) extends ArrayConstructors(at) {
  override def copy: Expr = ArrayFromUserFunGenerator(f, at)

  override def eval(valueMap: ValueMap): Any = {
    val n = at.len.eval
    Array.tabulate(n)( i => f.eval(valueMap, i, n) )
  }

}