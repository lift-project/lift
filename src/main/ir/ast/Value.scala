package ir.ast

import apart.arithmetic.ArithExpr
import ir.Type

/**
 * Values, i.e.: 4, 128, ...
 * This is a subclass of Param, as it can be seen as a variable (== parameter) with a fixed value.
 * This slightly simplifies the implementation of the type checking, memory allocation, ...
 * @param value The represented value in a string representation
 */
case class Value(var value: String) extends Param {

  /**
   * Debug string representation
   */
  override def toString = value

  /**
   * Perform a copy of `this`
   */
  override def copy: Value = this.clone().asInstanceOf[Value]

  /**
   * Vectorize the current value.
   * @param n The vector width
   * @return A vectorized value
   */
  override def vectorize(n: ArithExpr): Value = Value(value, t.vectorize(n))
}

object Value {
  /**
   * Create a new value with the given type, but without a given value
   * @param t The type the value should be set to
   * @return A newly constructed value with the given type
   */
  def apply(t: Type): Value = {
    val v = Value("")
    v.t = t
    v
  }

  /**
   * Create a new value with the given value (as a string) and type
   * @param value The value as a string the newly constructed value should represent
   * @param t The type the value should be set to
   * @return A newly constructed value with the given type and value
   */
  def apply(value: String, t: Type): Value = {
    val v = Value(value)
    v.t = t
    v
  }

  /**
   * Set the type of the given value to the given type and return it
   * @param v The value to set the type for
   * @param t The type the value should be set to
   * @return The value `v` with the type set to `t`
   */
  def apply(v: Value, t: Type): Value = {
    v.t = t
    v
  }

  /**
   * Behaves like the identity and returns the given value `v`
   * @param v A value
   * @return The value `v`
   */
  def apply(v: Value) = v

  @deprecated("Replaced by Value.vectorize(n)")
  def vectorize(v: Value, n: ArithExpr): Value = v.vectorize(n)
}
