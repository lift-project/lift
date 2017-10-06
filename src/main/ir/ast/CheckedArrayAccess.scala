package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap

/**
  * Indexed array CheckedArrayAccess pattern
  *
  * Code for this pattern can be generated
  */
case class CheckedArrayAccess(index: Expr) extends Pattern(arity = 2) {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    val ixT = TypeChecker.check(index)
    if (ixT != opencl.ir.Int)
      throw TypeException(s"Index type must be integral, got type $ixT instead!")

    argType match {
      case TupleType(defaultT, ArrayType(elemT)) =>
        if(defaultT != elemT)
          throw TypeException(
            s"In:\n$this\n" +
            s"Default access result type ($defaultT) must match array element type ($elemT)"
          )
          elemT
      case _ => throw new TypeException(argType, "TupleType(α, ArrayType(α))", this)
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
