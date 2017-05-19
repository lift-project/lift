package ir.ast

import lift.arithmetic.{?, ArithExpr, Var, Cst}
import ir._
import ir.interpreter.Interpreter.ValueMap

/**
 * Indexed array UnsafeArrayAccess pattern
 *
 * Code for this pattern can be generated
 */

case class UnsafeArrayAccess(index: Expr) extends Pattern(arity = 1)
  with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
		TypeChecker.check(index)
	  argType match {
	      case ArrayTypeWS(t, Cst(1)) =>
					ArrayTypeWSWC(t, Cst(1)) // match the definition of searches/reductions

	      case ArrayType(t) => ArrayTypeWSWC(t, Cst(1))

	      case _ => throw new TypeException(argType, "ArrayType", this)
	    }
	}
  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    args.head match {
      case a: Array[_] => a(index.eval(valueMap).asInstanceOf[Int])
    }
  }
}
