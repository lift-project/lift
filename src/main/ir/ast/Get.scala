package ir.ast

import ir.interpreter.Interpreter._
import ir.{TupleType, Type, TypeException, UndefType}

case class Get(n: Int) extends Pattern(arity = 1) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case tt: TupleType => tt.elemsT(n)
      case _ => throw new TypeException(argType, "TupleType", this)
    }
  }


  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    (args.head, n) match {
      case (t: (_,_), 0) => t._1
      case (t: (_,_), 1) => t._2

      case (t: (_,_,_), 0) => t._1
      case (t: (_,_,_), 1) => t._2
      case (t: (_,_,_), 2) => t._3

      case (t: (_,_,_,_), 0) => t._1
      case (t: (_,_,_,_), 1) => t._2
      case (t: (_,_,_,_), 2) => t._3
      case (t: (_,_,_,_), 3) => t._4

      case _ => throw new NotImplementedError()
    }
  }
}

object Get {
  def apply(e: Expr, n: Int): Expr = Get(n)(e)
}