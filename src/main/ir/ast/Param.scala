package ir.ast

import lift.arithmetic.ArithExpr
import ir.interpreter.Interpreter.ValueMap
import ir.{Type, TypeException}

/**
 * Parameters to functions and lambdas, i.e.: x, y, ...
 */
class Param(name: String = "default") extends Expr with Cloneable {

  /**
   * Debug string representation
   */
  override def toString: String =
    name match {
    case _ if name == "default" =>
      //this.t + " " +
//        "p" + (hashCode() & 0xff).toString
        "p" + (hashCode() & 0xffff).toString
    case _  => this.t + " " + name
    }

  /**
   * Perform a copy of `this`
   */
  override def copy: Param = this.clone().asInstanceOf[Param]

  /**
   * Vectorize the current parameter
   * @param n The vector width
   * @return A new vectorized parameter
   */
  def vectorize(n: ArithExpr): Param = this match {
    case _: VectorParam =>
      throw TypeException("Cannot vectorize a vectorized parameter")
    case x => new VectorParam(x, n)
  }

  override def eval(valueMap: ValueMap): Any = {
    valueMap get this match {
      case Some(x) => x
      case None => throw new Error("This expression is not evaluable")
    }
  }
  // These methods allow for writing an easy access syntax for tuples
  def _0 = Get(this, 0)
  def _1 = Get(this, 1)
  def _2 = Get(this, 2)
  def _3 = Get(this, 3)
  def _4 = Get(this, 4)
  def _5 = Get(this, 5)
  def _6 = Get(this, 6)
  def _7 = Get(this, 7)
  def _8 = Get(this, 8)
  def _9 = Get(this, 9)
}

object Param {
  /**
   * Create a new parameter without any information set yet, i.e.,
   * type, memory, ....
   * @return A newly constructed parameter
   */
  def apply(): Param = new Param

  /**
   * Create a new parameter with the given type.
   * @param t The type the parameter should be set to
   * @return A newly constructed parameter with the given type
   */
  def apply(t: Type): Param = {
    val p = Param()
    p.t = t
    p
  }

  def apply(t: Type, name: String): Param = {
    val p = new Param(name)
    p.t = t
    p
  }
}

/**
 * A vectorized parameter constructed from an existing parameter
 * TODO (michel): is this class really required? Can we get rid of it?
 * @param p An existing parameter
 * @param n The vector width
 */
class VectorParam(val p: Param, val n: ArithExpr) extends Param {
  t = p.t.vectorize(n) // set the type
  override def toString: String = "v" + p.toString + "_" + n
}
