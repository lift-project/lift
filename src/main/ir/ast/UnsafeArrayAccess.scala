package ir.ast

import apart.arithmetic.{?, ArithExpr, Var, Cst}
import ir._

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
	      case ArrayType(t, Cst(1)) =>
	        ArrayType(t, 1) // match the definition of searches/reductions

	      case ArrayType(t, n) => ArrayType(t, 1)

	      case _ => throw new TypeException(argType, "ArrayType")
	    }
	}
}
