package ir.ast

import ir.{ArrayTypeWSWC, Type, TypeException}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.ArithExpr
import sun.reflect.generics.reflectiveObjects.NotImplementedException

case class PadFunction(left: Int, right: Int, fun: (ArithExpr, ArithExpr) => Expr)
  extends Pattern(arity = 1) {

  override def toString: String = "PadFunction(" + left + "," + right + "," + fun.toString + ")"

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWSWC(et,s,c) => ArrayTypeWSWC(et, s + left + right, c + left + right)
      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case _: Vector[_] => throw new NotImplementedException()
    }
  }
}
/*
object PadFunction2D {
   def apply(top: Int, bottom: Int, left: Int, right: Int, constant: Value): Lambda = {
    Map(PadFunction(left, right, constant)) o PadFunction(top, bottom, constant)
  }

  def apply(left: Int, right: Int, constant: Value): Lambda = {
    Map(PadFunction(left, right, constant)) o PadFunction(left, right, constant)
  }

}

object PadFunction3D {
  def apply(x: Int, y: Int, z: Int, constant: Value): Lambda = {
    Map(Map(PadFunction(x, x, constant)) o PadFunction(y, y, constant)) o PadFunction(z, z, constant)
  }
}
 */
