package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.Expression

case class ArrayFromGenerator(f: (ArithExpr, ArithExpr) => Expression,
                              override val at: ArrayType) extends ArrayConstructors(at) {
  override def copy: Expr = ArrayFromGenerator(f, at)

  override def eval(valueMap: ValueMap): Any = {
    val n = at.len.eval
    Array.tabulate(n)( i => f(i, n) )
  }

}
