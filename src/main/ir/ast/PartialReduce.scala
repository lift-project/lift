package ir.ast

import apart.arithmetic.{?, ArithExpr, Var}
import ir._

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

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case TupleType(initT, ArrayType(elemT, _)) =>
        f.params(0).t = initT // initial elem type
        f.params(1).t = elemT // array element typ
        TypeChecker.check(f.body, setType) // check the body

        ArrayType(initT, 1)

      case _ => throw new TypeException(argType, "TupleType(_, _)")
    }
  }
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
case class PartRed(override val f: Lambda) extends AbstractPartRed(f, Var("")) {
  override def copy(f: Lambda): Pattern = PartRed(f)

  /**
   * Indicating if it is possible to generate code for this function
   * declaration.
   * Might be overwritten by a subclass or by mixing in the `isGenerable` trait.
   */
  override def isGenerable: Boolean = false
}

object PartRed {
  def apply(f: Lambda2, init: Value): Lambda1 = fun((x) => PartRed(f)(init, x))
}
