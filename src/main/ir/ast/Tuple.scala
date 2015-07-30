package ir.ast

/**
 * Tuple pattern.
 * Code for this pattern can be generated.
 *
 * The tuple pattern has the following high-level semantics:
 * <code>Tuple(2)(a, b) = (a, b)</code>
 * The definitions for `n > 2` are accordingly.
 *
 * The tuple pattern has the following type:
 * `Tuple(2) : a -> b -> (a, b)`
 * The definitions for `n > 2` are accordingly.
 *
 * @param n The number of elements which are combined. Must be >= 2
 */
case class Tuple(n: Int) extends FunDecl(arity = n) with isGenerable

object Tuple {
  /**
   * Create am instance of the Tuple pattern.
   * This function infers the number of elements which are combined with the tuple
   * pattern.
   *
   * @param args The elements to be combined with the tuple pattern.
   * @return An instance of the tuple pattern combining the elements given by `args`
   */
  def apply(args : Expr*) : Expr = {
    assert(args.length >= 2)
    Tuple(args.length)(args:_*)
  }
}
