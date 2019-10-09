package backends.spatial.common

import _root_.ir.Type
import _root_.ir.ScalarType
import lift.arithmetic.{ArithExpr, Cst, Predicate}
import opencl.generator.NotPrintableExpression

object Printer {

  def toString(t: Type): String = {
    t match {
      case ScalarType(name, _)    => name
      case _                      => throw new NotImplementedError()
    }
  }

  def toString(e: ArithExpr): String = {
    e match {
      case Cst(c)                                  => c.toString
      case _                                       => throw new NotImplementedError()
    }
  }

  def toString(p: Predicate): String = {
    throw new NotImplementedError()
  }
}
