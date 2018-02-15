package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.ArithExpr
import core.generator.GenericAST._

case class ArrayFromGenerator(f: (ArithExpr, ArithExpr) => ExpressionT,
                              override val at: ArrayType with Size with Capacity) extends ArrayConstructors(at) {
  override def copy: Expr = ArrayFromGenerator(f, at)

  override def eval(valueMap: ValueMap): Any = {
    val n = at.size.eval
    Array.tabulate(n)( i => f(i, n) )
  }

}
