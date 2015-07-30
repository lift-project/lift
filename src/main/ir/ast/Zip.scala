package ir.ast

/**
 * Zip pattern.
 * Code for this pattern can be generated.
 *
 * The zip pattern has the following high-level semantics:
 *   <code>Zip(2)( [x,,1,,, ..., x,,n,,], [y,,1,,, ..., y,,n,,] )
 *      = [ (x,,1,,, y,,1,,), ..., (x,,n,,, y,,n,,) ]</code>
 * The definitions for `n > 2` are accordingly.
 *
 * The zip pattern has the following type:
 *   `Zip(2) : [a],,i,, -> [b],,i,, -> [a x b],,i,,`
 * The definitions for `n > 2` are accordingly.
 *
 * @param n The number of arrays which are combined. Must be >= 2.
 */
case class Zip(n : Int) extends FunDecl(arity = n) with isGenerable

object Zip {
  /**
   * Create an instance of the zip pattern.
   * This function infers the number of arrays which are combined with the zip
   * pattern.
   *
   * @param args The arrays to be combined with the zip pattern.
   * @return An instance of the zip pattern combining the arrays given by `args`
   */
  def apply(args : Expr*) : Expr = {
    assert(args.length >= 2)
    Zip(args.length)(args:_*)
  }
}
