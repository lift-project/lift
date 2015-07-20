package ir.ast

import arithmetic.ArithExpr

/**
 * Iterate pattern.
 * Code for this pattern can be generated.
 *
 * The iterate pattern has the following high-level semantics:
 *   `Iterate(0, f)( xs ) = xs`
 *   `Iterate(n, f)( xs ) = Iterate(n-1, f)( f(xs) )`
 *
 * The iterate pattern has the following type:
 *   `Iterate(n, f) : [a],,m,, -> [a],,F^n^(m),,`
 * where `n: Int` and `f: [a],,k,, -> [a],,F(k),,`.
 *
 * We know the following algorithmic rewrite rules for the iterate pattern
 * (so far):
 *  - Iterate(n+m, f) => Iterate(n, f) o Iterate(m, f)
 *
 * @param n Number of times to iterate
 * @param f Lambda to be iterated
 */
case class Iterate(n: ArithExpr, f: Lambda1)
  extends Pattern(arity = 1) with FPattern with isGenerable {

  /**
   * Function call. This method returns an object representing the function call
   * of `this` with `args`.
   * This method will fail at runtime if the number of given `args` is `!= 1`.
   * @param args The arguments to call the function (`this`) with.
   * @return An object (of type IterateCall) representing the function call of
   *         `this` with `args`.
   */
  override def apply(args: Expr*): IterateCall = iterateCall(args: _*)

  /**
   * Alternative function call operator syntax. Calls `this.apply(arg)`.
   * @param arg The argument to call the function with.
   * @return An object (of type IterateCall) representing the function call of
   *         `this` with `arg`.
   */
  override def $(arg: Expr): IterateCall = iterateCall(arg)

  // helper method creating an IterateCall linked to this
  private def iterateCall(args: Expr*): IterateCall = {
    assert(args.length == 1)
    new IterateCall(this, args(0))
  }
}

object Iterate {
  def apply(n: ArithExpr): ((Lambda1) => Iterate)  = (f: Lambda1) => Iterate(n ,f)

  def varName(): String = "iterSize"
}
