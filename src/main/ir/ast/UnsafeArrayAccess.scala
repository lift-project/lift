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
					t // break with the definition of searches/reductions, as we have no "partial" accesses

	      case ArrayType(t) =>
          t

	      case _ => throw new TypeException(argType, "ArrayType")
	    }
	}
  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    args.head match {
      case a: Array[_] => a(index.eval(valueMap).asInstanceOf[Int])
    }
  }
}
