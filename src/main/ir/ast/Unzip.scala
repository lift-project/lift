package ir.ast

/**
 * Unzip pattern.
 * Code for this pattern can be generated.
 *
 * The unzip pattern has the following high-level semantics:
 * `Unzip()( [ (x,,1,,, y,,1,,), ..., (x,,n,,, y,,n,,) ]) = ([x,,1,,, ..., x,,n,,], [y,,1,, , ..., y,,n,,] )`
 * The definitions for `n > 2` are accordingly.
 *
 * The unzip pattern has the following type:
 * `Unzip() : [a x b],,i,, -> ([a],,i,,, [b],,i,,)`
 * The definitions for `n > 2` are accordingly.
 */
case class Unzip() extends FunDecl(arity = 1) with isGenerable

