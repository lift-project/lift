package ir.ast

import arithmetic.Var

/**
 * Abstract class for the (full) reduce pattern.
 *
 * The full reduction is modeled as a special case of the parital reduction,
 * therefore, this class inherits from the AbstractPartRed class.
 *
 * An object of the reduce pattern has to be instantiated with a given
 * lambda `f`, therefore, it is not possible to have a like `Red()`.
 *
 * @param f A lambda to be applied in the partial reduction
 */
abstract class AbstractReduce(f:Lambda2) extends AbstractPartRed(f)

/**
 * Concrete class for the reduce pattern.
 * No code can be generated for this pattern.
 *
 * The reduce pattern has the following high-level semantics:
 *   `Reduce(f)( id, [x,,1,,, ..., x,,n,,] ) =
 *      id f (x,,1,, f (... ( ... f x,,n,,) ...))`
 * where `f` is written in infix notation.
 *
 * The reduce pattern has the following type:
 *   `Reduce(f) : a -> [a],,i,, -> [a],,1,,`
 * where `f : (a x a) -> a`.
 *
 * We know the following algorithmic rewrite rules for the reduce pattern
 * (so far):
 *  - `Reduce(f) => Reduce(f) o PartRed(f)``
 *
 * @param f A lambda to be applied as the binary reduction operator in the
 *          reduction
 */
case class Reduce(f: Lambda2) extends AbstractReduce(f) {
  /**
   * Function call. This method returns an object representing the function call
   * of `this` with `args`.
   * This method will fail at runtime if the number of given `args` is != 2.
   * @param args The arguments to call the function (`this`) with.
   * @return An object (of type FunCall) representing the function call of
   *         `this` with `args`.
   */
  override def apply(args: Expr*) : ReduceCall = reduceCall(args:_*)

  // helper method creating a ReduceCall instance linked to this
  private def reduceCall(args: Expr*): ReduceCall = {
    assert(args.length == 2)
    new ReduceCall(Var("i"), this, args(0), args(1))
  }
}

object Reduce {
  def apply(f: Lambda2, init: Value): Lambda1 = fun((x) => Reduce(f)(init, x))
}
