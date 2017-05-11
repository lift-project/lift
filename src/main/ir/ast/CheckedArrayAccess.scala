package ir.ast

import lift.arithmetic.{?, ArithExpr, Var, Cst}
import ir._
import ir.interpreter.Interpreter.ValueMap

/**
  * Indexed array CheckedArrayAccess pattern
  *
  * Code for this pattern can be generated
  */

case class CheckedArrayAccess(index: Expr) extends Pattern(arity = 2)
  with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    val ixT = TypeChecker.check(index)
    if (ixT != opencl.ir.Int)
      throw TypeException(s"Index type must be integral, got type ${ixT} instead!")
//    TypeChecker.check(default)
    argType match {
      case TupleType(defaultT, ArrayType(elemT)) =>
        if(defaultT != elemT)
          throw TypeException(s"Default access result type ($defaultT) must match array element type ($elemT)")
        ArrayTypeWSWC(elemT, Cst(1))
      case TupleType(defaultT, ArrayTypeWS(elemT, Cst(1))) =>
        if(defaultT != elemT)
          throw TypeException(s"Default access result type ($defaultT) must match array element type ($elemT)")
        ArrayTypeWSWC(elemT, Cst(1))
        // TODO: Should we handle ArrayTypeWSWC?
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

object CheckedArrayAccess {
  def apply(index: Expr, default: Expr): Lambda1 = fun((x) => CheckedArrayAccess(index)(default, x))
}
