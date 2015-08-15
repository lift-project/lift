package ir.ast

import ir.interpreter.Interpreter._
import ir.{TypeException, ArrayType, Type}

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
case class Join() extends Pattern(arity = 1) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(ArrayType(t, m), n) =>
        ArrayType(t, n * m)

      case _ => throw new TypeException(argType, "ArrayType(ArrayType(_, _), _)")
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Array[_] = {
    assert(args.length == arity)
    args.head match {
      case a: Array[Array[Any]] => a.flatten
    }
  }
}
