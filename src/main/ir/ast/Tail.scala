package ir.ast

/**
 * Tail pattern.
 * Code for this pattern can be generated.
 *
 * The tail pattern has the following high-level semantics:
 * `Tail()( [x,,1,,, x,,2,,, ..., x,,n,,] ) = [x,,2,,, ..., x,,n,,]`
 *
 * The tail pattern has the following type:
 * `Tail() : [a],,I+1,, -> [a],,I,,`
 */
case class Tail() extends Pattern(arity = 1) with isGenerable
