package ir.ast

/**
 * Join pattern.
 * Code for this pattern can be generated.
 *
 * The join pattern has the following high-level semantics:
 *   `Join()( [ [x,,1,,, ..., x,,n,,], ..., [x,,m-n+1,,, ..., x,,m,,] ] ) =
 *    [x,,1,,, ..., x,,m,,]`
 *
 * The join pattern has the following type:
 *   `Join() : [ [a],,i,, ],,j,, -> [ a ],,i x j,,`
 *
 * We know the following algorithmic rewrite rules for the join pattern
 * (so far):
 *  - `Join() o Split(chunkSize) | Split(chunkSize) o Join() => id`
 */
case class Join() extends Pattern(arity = 1) with isGenerable
