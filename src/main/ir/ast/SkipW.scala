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

case class SkipW(left: ArithExpr) extends Pattern(arity = 1) {

  // first component: memory to be replaced
  // second component: memory to replace the first component with
  var replacementMap: immutable.Map[Memory, Memory] = immutable.Map[Memory, Memory]()

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWSWC(t, s, c) => ArrayTypeWSWC(t, s+left, c+left)
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

object SkipW {
 /*
  def apply(left: Int) : Lambda = {
    if (left < 0 )
      throw new IllegalArgumentException("Drop only supports positive values")
    SkipW(left)
  }
  */
}

object SkipW2D {

  def apply(top: Int, left: Int, bottom: Int, right: Int) : Lambda = {
    if (left < 0 || top < 0)
      throw new IllegalArgumentException("Skip2D only supports positive values")
    Map(SkipW(left,right)) o SkipW(top,bottom)
  }

  def apply(left: Int) : Lambda = {
    if (left < 0 )
      throw new IllegalArgumentException("Skip2D only supports positive values")
    SkipWND(2)(left)
  }
}

object SkipW3D {

  def apply(xl: Int, yl: Int,zl: Int): Lambda = {
    if (xl < 0 || yl < 0 || zl < 0)
      throw new IllegalArgumentException("Skip3D only supports positive values")
    Map(Map(SkipW(xl)) o SkipW(yl)) o SkipW(zl)
  }

  def apply(x: Int): Lambda = {
    if (x < 0 )
      throw new IllegalArgumentException("Skip3D only supports positive values")
    Map(Map(SkipW(x)) o SkipW(x)) o SkipW(x)
  }

}

object SkipWND {
  def apply(dim: Int)(offset: Int): Lambda = {
    if (offset < 0 )
      throw new IllegalArgumentException("SkipWND only supports positive values")
    GenerateIR.applyInEveryDimUntilDim(SkipW(offset), dim)
  }
}

