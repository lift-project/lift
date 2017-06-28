package ir.ast

import lift.arithmetic.Cst
import ir.interpreter.Interpreter.ValueMap
import ir._

/**
 * Tail pattern.
 * Code for this pattern can be generated.
 *
 * The tail pattern has the following high-level semantics:
 * `Tail()( [x,,1,,, x,,2,,, ..., x,,n,,] ) = [x,,2,,, ..., x,,n,,]`
 *
 * The tail pattern has the following type:
 * `Tail() : [a],,I+1,, -> [a],,I,,`
 */
case class Tail() extends Pattern(arity = 1) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWS(t, Cst(1)) =>
        // TODO: think about this ... throw exception instead?
        ArrayTypeWSWC(t, 1)

      case ArrayTypeWSWC(t, s, c) => ArrayTypeWSWC(t, s - 1, c-1)

      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }


  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    args.head match {
      case a: Array[_] => a.tail
    }
  }
}
