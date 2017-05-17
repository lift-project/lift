package ir.ast

import lift.arithmetic.ArithExpr
import ir.interpreter.Interpreter._
import ir._

/**
 * asVector pattern. (a.k.a., splitVec).
 * Code for this pattern can be generated.
 *
 * The asVector pattern has the following high-level semantics:
 *   `asVector(n)( [x,,1,,, ..., x,,m,,] ) =
 *      [ <x,,1,,, ..., x,,n,,>, ..., <x,,m-n+1,,, ..., x,,m,,> ]`
 *
 * The asVector pattern has the following type:
 *   `asVector(n) : [ a ],,n x j,, -> [ < a >,,n,, ],,j,,`
 *
 * We know the following algorithmic rewrite rules for the asVector pattern
 * (so far):
 *  - `asScalar() o asVector(n) | asVector(n) o asScalar() => id`
 *
 * @param n The vector length used to split the input array in to.
 *          The size of the input array must be a multiple of `len`.
 */
case class asVector(n: ArithExpr) extends Pattern(arity = 1) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case at@ArrayTypeWSWC(ScalarType(_, _), _,_) => at.vectorize(n)
      case _ => throw new TypeException(argType, "ArrayType(ScalarType, _)", this)
    }
  }


  override def eval(valueMap: ValueMap, args: Any*): Any =
    Split(n).eval(valueMap, args:_*)
}
