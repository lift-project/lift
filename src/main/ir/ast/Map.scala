package ir.ast

import apart.arithmetic.{?, ArithExpr, Var}

/**
 * Abstract class for map patterns.
 *
 * An object of the map pattern has to be instantiated with a given lambda `f`,
 * therefore, it is not possible to have a term like `Map()`.
 *
 * @param f A lambda to be applied to every element of the input array
 */
abstract class AbstractMap(val f: Lambda,
                           val name: String,
                           val loopVar: Var) extends Pattern(arity = 1)
                                                     with FPattern {
  assert(f.params.length == 1)

  var iterationCount: ArithExpr = ?
}

/**
 * Concrete class for the map pattern.
 * No code can be generated for this pattern.
 *
 * The map pattern has the following high-level semantics:
 *   `Map(f) $ [x,,1,,, ..., x,,n,,] = [f(x,,1,,), ..., f(x,,n,,)]`
 *
 * The map pattern has to following type:
 *  `Map(f) : [a],,i,, -> [b],,i,,`
 * where `f: a -> b`.
 *
 * We know the following algorithmic rewrite rules for the map pattern (so far):
 *  - `Map(f)          => Join() o Map(Map(f)) o Split(I)`
 *  - `Map(f) o Map(g) => Map(f o g)`
 *
 * Lower level rewrite rules are described for the corresponding low-level
 * patterns.
 *
 * @param f A lambda to be applied to every element of the input array
 */
case class Map(override val f: Lambda) extends AbstractMap(f, "Map", Var(""))
