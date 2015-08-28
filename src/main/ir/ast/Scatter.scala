package ir.ast

import ir.Type

/**
 * Scatter pattern. Performs a reorder on the previous write.
 * Code for this pattern can be generated.
 *
 * The scatter pattern has the following high-level semantics:
 * `Scatter(idx)( [x,,1,,, ..., x,,n,,] ) = [ y,,1,,, ..., y,,n,,], where y,,idx(i),, = x,,i,,`
 *
 * The scatter pattern has the following type:
 * `Scatter(IndexFunction) : [a],,I,, -> [a],,I,,`
 *
 * @param idx The function to use for reordering
 */
case class Scatter(idx: IndexFunction) extends Pattern(arity = 1)
                                       with isGenerable {

  override def checkType(argType: Type, setType: Boolean): Type = argType

}
