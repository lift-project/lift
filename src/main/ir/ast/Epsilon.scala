package ir.ast

/**
 * Empty pattern (Epsilon).
 * Code for this pattern can be generated.
 *
 * The epsilon pattern has the following high-level semantics:
 *   `Epsilon() = nothing`
 *
 * The join pattern has the following type:
 *   `Epsilon() : A -> A`
 *
 * We know the following algorithmic rewrite rules for the epsilon pattern
 * (so far):
 *  - `Epsilon() o x => x`
 *  - `x o Epsilon() => x`
 *  - `Map(Epsilon()) => Epsilon()`
 */
case class Epsilon() extends Pattern(arity = 1) with isGenerable
