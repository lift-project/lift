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
abstract class AbstractReduce(override val f: Lambda2,
                              override val loopVar: Var)
  extends AbstractPartRed(f, loopVar)

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
 *  - `Reduce(f) => Reduce(f) o PartRed(f)`
 *
 * @param f A lambda to be applied as the binary reduction operator in the
 *          reduction
 */
case class Reduce(override val f: Lambda2) extends AbstractReduce(f, Var(""))

object Reduce {
  def apply(f: Lambda2, init: Value): Lambda1 = fun((x) => Reduce(f)(init, x))
}
