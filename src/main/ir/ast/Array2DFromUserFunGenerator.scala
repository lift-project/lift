package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap

case class Array2DFromUserFunGenerator(f: UserFun,
                                       override val at: ArrayType) extends ArrayConstructors(at) {
  override def copy: Expr = Array2DFromUserFunGenerator(f, at)

  override def eval(valueMap: ValueMap): Any = {
    at match {
      case ArrayType(ArrayType(_, n_), m_) =>
        val n = n_.eval
        val m = m_.eval
        Array.tabulate(m, n)( (i, j) => f.eval(valueMap, i, j, m, n) )
      case _ =>
        throw new Exception(s"Expected Array(Array(_, _), _)")
    }
  }

}