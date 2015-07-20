package ir.ast

/**
 * Transpose pattern. Performs the transpose on the next read.
 * Code for this pattern can be generated.
 *
 * The transpose pattern has the following high-level semantics:
 * `Transpose()([ [x,,1,,, ..., x,,n,,], [y,,1,,, ..., y,,n,,], ..., [z,,1,,, ..., z,,n,,] ]) = [ [x,,1,,, y,,1,,, ..., z,,1,,], [x,,2,,, y,,2,,, ..., z,,2,,], ..., [x,,n,,, y,,n,, ..., z,,n,,,] ]`
 *
 * The transpose pattern has the following type:
 * `Transpose() : [ [a],,I,, ],,J,, -> [ [a],,J,, ],,I,,`
 */
case class Transpose() extends Pattern(arity = 1) with isGenerable
