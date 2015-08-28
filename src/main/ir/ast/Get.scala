package ir.ast

import ir.{TypeException, TupleType, Type}

case class Get(n: Int) extends Pattern(arity = 1) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case tt: TupleType => tt.elemsT(n)
      case _ => throw new TypeException(argType, "TupleType")
    }
  }

}

object Get {
  def apply(e: Expr, n: Int): Expr = Get(n)(e)
}