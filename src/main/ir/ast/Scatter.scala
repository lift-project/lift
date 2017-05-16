package ir.ast

import ir.{ArrayType, Type, TypeException, UndefType}
import ir.interpreter.Interpreter.ValueMap

/**
 * Scatter pattern. Performs a reorder on the previous write.
 * Code for this pattern can be generated.
 *
 * The scatter pattern has the following high-level semantics:
 * `Scatter(idx)( [x,,1,,, ..., x,,n,,] ) = [ y,,1,,, ..., y,,n,,], where y,,idx(i),, = x,,i,,`
 *
 * The scatter pattern has the following type:
 * `Scatter(IndexFunction) : [a],,I,, -> [a],,I,,`
 *
 * @param idx The function to use for reordering
 */
case class Scatter(idx: IndexFunction) extends Pattern(arity = 1)
                                       with isGenerable {

  override def checkType(argType: Type, setType: Boolean): Type = {
    // Scatter expects an array
    argType match {
      case ArrayType(_) => argType
      case _ => throw new TypeException(argType, "ArrayType(_)", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)

    args.head match {
      case a: Array[_] =>
        (0 to a.length).map(i => a(idx.f(i, Type.fromAny(a)).eval))
    }
  }
}
