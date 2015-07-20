package ir.ast

/**
 * Gather pattern. Performs a reorder on the next read.
 * Code for this pattern can be generated.
 *
 * The gather pattern has the following high-level semantics:
 * `Gather(idx)( [x,,1,,, ..., x,,n,,] ) = [ y,,1,,, ..., y,,n,,], where y,,i,, = x,,idx(i),,`
 *
 * The gather pattern has the following type:
 * `Gather(IndexFunction) : [a],,I,, -> [a],,I,,`
 *
 * @param idx The function to use for reordering
 */
case class Gather(idx: IndexFunction) extends Pattern(arity = 1) with isGenerable

