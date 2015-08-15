package ir.ast

import apart.arithmetic.ArithExpr
import ir.interpreter.Interpreter.ValueMap
import ir.{Type, TypeException}


/**
 * Parameters to functions and lambdas, i.e.: x, y, ...
 */
class Param() extends Expr with Cloneable {

  /**
   * Debug string representation
   */
  override def toString = "p" + (hashCode() & 0xff)

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
    case v:VectorParam =>
      throw new TypeException("Cannot vectorize a vectorized parameter")
    case x => new VectorParam(x, n)
  }

  override def eval(valueMap: ValueMap): Any = {
    valueMap get this match {
      case Some(x) => x
    }
  }
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

  @deprecated("used Param.vectorize(n)")
  def vectorize(p: Param, n: ArithExpr): Param = p.vectorize(n)
}

/**
 * A vectorized parameter constructed from an existing parameter
 * TODO (michel): is this class really required? Can we get rid of it?
 * @param p An existing parameter
 * @param n The vector width
 */
class VectorParam(val p: Param, n: ArithExpr) extends Param {
  t = p.t.vectorize(n) // set the type
  override def toString = "v" + p.toString + "_" + n
}
