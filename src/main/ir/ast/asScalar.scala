package ir.ast

import ir.interpreter.Interpreter.ValueMap
import ir._

/**
 * asScalar pattern. (a.k.a., joinVec).
 * Code for this pattern can be generated.
 *
 * The asScalar pattern has the following high-level semantics:
 *   `asScalar()( [ <x,,1,,, ..., x,,n,,>, ..., <x,,m-n+1,,, ..., x,,m,,> ] ) =
 *    [x,,1,,, ..., x,,m,,]`
 *
 * The asScalar pattern has the following type:
 *   `asScalar(): [ < a >,,i,, ],,j,, -> [ a ],,i x j,,`
 *
 * We know the following algorithmic rewrite rules for the asScalar pattern
 * (so far):
 *  - `asScalar() o asVector(n) | asVector(n) o asScalar() => id`
 */
case class asScalar() extends Pattern(arity = 1) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case at@ArrayTypeWSWC(VectorType(_, _),_,_) => Type.asScalarType(at)
      case _ =>
        throw new TypeException(argType, "ArrayType(VectorType(_, _), _)", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any =
    Join().eval(valueMap, args:_*)
}
