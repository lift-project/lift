package ir.ast

import apart.arithmetic.{?, ArithExpr, Var}

/**
 * Abstract class for the partial reduce pattern.
 *
 * An object of the partial reduce pattern has to be instantiated with a given
 * lambda `f`, therefore, it is not possible to have a like `PartRed()`.
 *
 * @param f A lambda to be applied in the partial reduction
 */
abstract class AbstractPartRed(val f: Lambda,
                               val loopVar: Var) extends Pattern(arity = 2)
                                                         with FPattern {
  assert(f.params.length == 2)

  var iterationCount: ArithExpr = ?
}

/**
 * Concrete class for the partial reduce pattern.
 * No code can be generated for this class.
 *
 * The partial reduce pattern has the following high-level semantics:
 *   `PartRed(f)( id, [x,,1,,, ..., x,,n,,] ) = ` TODO (fill this out)
 * where `f` is written in infix notation.
 *
 * TODO: Currently this has NOT this type. fix this.
 * The partial reduce pattern has the following type:
 *   `PartRed(f) : a -> n -> [a],,i,, -> [a],,n,,`
 * where `f: (a x a) -> a`.
 *
 * We know the following algorithmic rewrite rules for the partial reduce
 * pattern (so far):
 *  - PartRed(f) => Reduce(f)
 *  - PartRed(f) => PartRed(f) o Reorder
 *  - PartRed(f) => Iterate(k, PartRed(f))
 *  - PartRed(f) => Join() o Map(PartRed(f)) o Split(k)
 *
 * @param f A lambda to be applied as the binary reduction operator in the
 *          partial reduction
 */
case class PartRed(override val f: Lambda2) extends AbstractPartRed(f, Var("")) {
  override def copy(f: Lambda): Pattern = PartRed(f)
}

object PartRed {
  def apply(f: Lambda2, init: Value): Lambda1 = fun((x) => PartRed(f)(init, x))
}
