package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.ArithExpr
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.collection.immutable

/**
  * A primitive allowing for a certain number of places in memory to be "skipped" before writing.
  *
  * Author: Larisa Stoltzfus (larisa.stoltzfus@ed.ac.uk)
  * Changes by: Naums Mogers
  */

case class SkipW(left: ArithExpr, right: ArithExpr) extends Pattern(arity = 1) {

  // first component: memory to be replaced
  // second component: memory to replace the first component with
  var replacementMap: immutable.Map[Memory, Memory] = immutable.Map[Memory, Memory]()

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWSWC(t, s, c) => ArrayTypeWSWC(t, s + left + right, c + left + right)
      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[Vector[_]] = {
    assert(args.length == arity)
    args.head match {
      case a: Vector[_] => throw new NotImplementedException()
    }
  }
  override def toString: String = "SkipW(" + left + ")"
}

