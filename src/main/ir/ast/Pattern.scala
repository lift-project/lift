package ir.ast

import ir.interpreter.Interpreter.ValueMap

/**
 * Abstract base class for all patterns (i.e., primitives defined in our
 * language)
 */
abstract class Pattern(override val arity: Int) extends FunDecl(arity) {
  def eval(valueMap: ValueMap, args: Any*): Any
}

object Pattern {
  def unapply(l: Lambda): Option[(Pattern)] = l match {
    case Lambda(_, FunCall(x, _),_) if x.isInstanceOf[Pattern] => Some(x.asInstanceOf[Pattern])
    case _ => None
  }
}

/**
 * Trait for all patterns which have a nested lambda (e.g., Map or Reduce)
 */
trait FPattern {
  def f: Lambda
  def copy(f: Lambda): Pattern
}

object FPattern {
  def unapply(arg: FPattern): Option[Lambda] = Some(arg.f)
}
