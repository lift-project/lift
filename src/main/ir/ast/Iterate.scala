package ir.ast

import arithmetic.{RangeUnknown, Var, ?, ArithExpr}
import ir.{UnallocatedMemory, Memory}

/**
 * Iterate pattern.
 * Code for this pattern can be generated.
 *
 * The iterate pattern has the following high-level semantics:
 *   `Iterate(0, f)( xs ) = xs`
 *   `Iterate(n, f)( xs ) = Iterate(n-1, f)( f(xs) )`
 *
 * The iterate pattern has the following type:
 *   `Iterate(n, f) : [a],,m,, -> [a],,F^n^(m),,`
 * where `n: Int` and `f: [a],,k,, -> [a],,F(k),,`.
 *
 * We know the following algorithmic rewrite rules for the iterate pattern
 * (so far):
 *  - Iterate(n+m, f) => Iterate(n, f) o Iterate(m, f)
 *
 * @param n Number of times to iterate
 * @param f Lambda to be iterated
 */
case class Iterate(n: ArithExpr, f: Lambda1) extends Pattern(arity = 1)
                                                     with FPattern
                                                     with isGenerable {
  var iterationCount: ArithExpr = ?

  var swapBuffer: Memory = UnallocatedMemory

  var indexVar = Var("i", RangeUnknown)
}

object Iterate {
  def apply(n: ArithExpr): ((Lambda1) => Iterate) =
    (f: Lambda1) => Iterate(n ,f)

  def varName(): String = "iterSize"
}
